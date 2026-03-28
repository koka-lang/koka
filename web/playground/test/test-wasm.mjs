#!/usr/bin/env node
/**
 * test-wasm.mjs
 *
 * Tests the WASM Koka compiler (koka-playground.wasm) and LSP server
 * (koka-lsp-wasm.wasm) by compiling the all.kk sample program.
 *
 * Usage:
 *   node web/test/test-wasm.mjs
 *
 * Expects:
 *   - web/playground/public/koka-playground.wasm (built via util/playground.kk)
 *   - lib/std/ (stdlib sources)
 *   - precompiled/ (precompiled .kki/.mjs files)
 *   - samples/ (sample .kk files)
 */

import fs from 'fs';
import path from 'path';
import { WASI, File, Directory, PreopenDirectory, OpenFile, ConsoleStdout } from '@bjorn3/browser_wasi_shim';

const ROOT = path.resolve(import.meta.dirname, '..', '..', '..');

// ── Build unified WASI filesystem from real filesystem ──────────────────

function loadDirToMap(fsDir, vfsPrefix) {
  const files = new Map();
  if (!fs.existsSync(fsDir)) return files;
  function walk(dir, prefix) {
    for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
      const fsPath = path.join(dir, entry.name);
      const vfsPath = prefix + '/' + entry.name;
      if (entry.isDirectory()) {
        walk(fsPath, vfsPath);
      } else {
        files.set(vfsPath, fs.readFileSync(fsPath, 'utf-8'));
      }
    }
  }
  walk(fsDir, vfsPrefix);
  return files;
}

function buildDirectoryTree(files) {
  const root = new Map();
  const encoder = new TextEncoder();
  for (const [filePath, content] of files) {
    const parts = filePath.split('/').filter(Boolean);
    let current = root;
    for (let i = 0; i < parts.length - 1; i++) {
      if (!current.has(parts[i])) current.set(parts[i], new Directory(new Map()));
      const dir = current.get(parts[i]);
      current = dir.contents;
    }
    const filename = parts[parts.length - 1];
    if (filename) current.set(filename, new File(encoder.encode(content)));
  }
  return new Directory(root);
}

function collectFiles(dir, prefix, out, decoder) {
  for (const [name, entry] of dir.contents) {
    const p = prefix ? prefix + '/' + name : name;
    if (entry instanceof File) out.set(p, decoder.decode(entry.data));
    else if (entry instanceof Directory) collectFiles(entry, p, out, decoder);
  }
}

// ── Test: WASM standalone compiler ──────────────────────────────────────

async function testWasmCompiler() {
  const wasmPath = path.join(ROOT, 'web', 'playground', 'public', 'koka-playground.wasm');
  if (!fs.existsSync(wasmPath)) {
    console.error('ERROR: koka-playground.wasm not found. Run: stack run koka -- -e util/playground.kk -- --backend=all');
    process.exit(1);
  }

  console.log('=== Testing WASM standalone compiler ===');
  console.log('Loading WASM module...');
  const wasmBytes = fs.readFileSync(wasmPath);
  const wasmModule = await WebAssembly.compile(wasmBytes);

  // Build VFS
  const allFiles = new Map();
  for (const [k, v] of loadDirToMap(path.join(ROOT, 'lib', 'std'), '/share/lib/std')) allFiles.set(k, v);

  // Load precompiled .kki/.mjs
  const preDir = path.join(ROOT, 'precompiled');
  if (fs.existsSync(preDir)) {
    for (const f of fs.readdirSync(preDir)) {
      const content = fs.readFileSync(path.join(preDir, f), 'utf-8');
      allFiles.set('/lib/js-debug/' + f, content);
    }
  }

  // Load sample sources
  for (const [k, v] of loadDirToMap(path.join(ROOT, 'samples'), '/samples')) allFiles.set(k.replace('/samples/', '/'), v);

  // all.kk source
  const allKk = fs.readFileSync(path.join(ROOT, 'samples', 'all.kk'), 'utf-8');

  const rootDir = buildDirectoryTree(allFiles);
  const encoder = new TextEncoder();
  const stdinFile = new File(encoder.encode(allKk));

  const stdoutLines = [];
  const stderrLines = [];

  const wasi = new WASI(
    ['koka-playground',
     '--sharedir=/share', '--libdir=/lib', '--target=js',
     '--builddir=/.koka', '--include=/share/lib', '--include=/',
     '--console=ansi', '-v1', 'all'],
    [],
    [
      new OpenFile(stdinFile),
      ConsoleStdout.lineBuffered(line => stdoutLines.push(line)),
      ConsoleStdout.lineBuffered(line => { stderrLines.push(line); }),
      new PreopenDirectory('/', rootDir.contents),
    ],
    { debug: false },
  );

  console.log('Running compiler...');
  const instance = new WebAssembly.Instance(wasmModule, {
    wasi_snapshot_preview1: wasi.wasiImport,
  });

  try {
    wasi.start(instance);
  } catch (e) {
    if (!(e instanceof Error && e.message?.includes('exit'))) {
      console.error('WASM exception:', e);
    }
  }

  // Parse result
  const jsonLine = stdoutLines.filter(l => l.startsWith('{')).pop() ?? '';
  let success = false;
  try {
    success = JSON.parse(jsonLine).success === true;
  } catch { /* */ }

  // Collect generated files
  const generatedFiles = new Map();
  const kokaDir = rootDir.contents.get('.koka');
  if (kokaDir instanceof Directory) {
    collectFiles(kokaDir, '', generatedFiles, new TextDecoder());
  }

  const mjsCount = [...generatedFiles.keys()].filter(k => k.endsWith('.mjs')).length;

  if (success && mjsCount > 0) {
    console.log(`PASS: Compilation succeeded, generated ${mjsCount} .mjs files`);
  } else {
    console.error(`FAIL: success=${success}, mjs files=${mjsCount}`);
    if (stderrLines.length > 0) {
      console.error('Stderr (last 10 lines):');
      stderrLines.slice(-10).forEach(l => console.error('  ' + l));
    }
    process.exit(1);
  }
}

// ── Run tests ───────────────────────────────────────────────────────────

async function main() {
  await testWasmCompiler();
  console.log('\nAll tests passed.');
}

main().catch(err => {
  console.error(err);
  process.exit(1);
});
