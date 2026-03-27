/**
 * wasm-worker.ts
 *
 * Web Worker that runs the WASM Koka compiler off the main thread.
 * Communicates via postMessage:
 *
 * Main → Worker:
 *   { type: 'init', wasmUrl: string }
 *   { type: 'compile', moduleName: string, sourceText: string, files: [path, content][] }
 *
 * Worker → Main:
 *   { type: 'ready' }
 *   { type: 'log', text: string }
 *   { type: 'result', success: boolean, stdout: string, stderr: string, generatedFiles: [path, content][] }
 *   { type: 'error', message: string }
 */

import {
  WASI,
  File,
  OpenFile,
  Directory,
  PreopenDirectory,
  ConsoleStdout,
} from '@bjorn3/browser_wasi_shim';
import { buildUnifiedTree, collectFilesFromDir } from './wasi-fs';

let wasmModule: WebAssembly.Module | null = null;
let compilerFlags: string[] = [];

self.onmessage = async (e: MessageEvent) => {
  const msg = e.data;

  if (msg.type === 'init') {
    try {
      compilerFlags = msg.compilerFlags ?? [];
      const response = await fetch(msg.wasmUrl);
      const bytes = await response.arrayBuffer();
      wasmModule = await WebAssembly.compile(bytes);
      self.postMessage({ type: 'ready' });
    } catch (err) {
      self.postMessage({ type: 'error', message: String(err) });
    }
    return;
  }

  if (msg.type === 'compile') {
    if (!wasmModule) {
      self.postMessage({ type: 'error', message: 'WASM module not loaded' });
      return;
    }

    try {
      const allFiles = new Map<string, string>(msg.files);
      const extraArgs: string[] = msg.extraArgs ?? [];
      const result = runCompiler(wasmModule, msg.moduleName, msg.sourceText, allFiles, extraArgs);
      self.postMessage({
        type: 'result',
        success: result.success,
        stdout: result.stdout,
        stderr: result.stderr,
        generatedFiles: Array.from(result.generatedFiles.entries()),
      });
    } catch (err) {
      self.postMessage({ type: 'error', message: String(err) });
    }
    return;
  }
};

function runCompiler(
  module: WebAssembly.Module,
  moduleName: string,
  sourceText: string,
  allFiles: Map<string, string>,
  extraArgs: string[] = [],
): { success: boolean; stdout: string; stderr: string; generatedFiles: Map<string, string> } {
  // Build a single unified WASI filesystem from all VFS files.
  // Using one root mount avoids cross-mount copy issues.
  const rootDir = buildUnifiedTree(allFiles);

  const stdinFile = new File(new TextEncoder().encode(sourceText));

  const stdoutLines: string[] = [];
  const stderrLines: string[] = [];

  const wasi = new WASI(
    ['koka-playground', ...compilerFlags, ...extraArgs, moduleName],
    [],
    [
      new OpenFile(stdinFile),
      ConsoleStdout.lineBuffered((line) => {
        stdoutLines.push(line);
      }),
      ConsoleStdout.lineBuffered((line) => {
        stderrLines.push(line);
        self.postMessage({ type: 'log', text: line });
      }),
      new PreopenDirectory('/', rootDir.contents as Map<string, File | Directory>),
    ],
    { debug: false },
  );

  const instance = new WebAssembly.Instance(module, {
    wasi_snapshot_preview1: wasi.wasiImport,
  });

  try {
    wasi.start(instance as unknown as { exports: { memory: WebAssembly.Memory; _start: () => void } });
  } catch (e) {
    if (!(e instanceof Error && e.message?.includes('exit'))) {
      stderrLines.push(String(e));
    }
  }

  // Collect generated output files from /.koka in the unified tree
  const generatedFiles = new Map<string, string>();
  const decoder = new TextDecoder();
  const kokaDir = rootDir.contents.get('.koka');
  if (kokaDir instanceof Directory) {
    collectFilesFromDir(kokaDir, '', generatedFiles, decoder);
  }

  // The last JSON line on stdout is the compiler result; earlier lines are build logs
  const jsonLine = stdoutLines.filter(l => l.startsWith('{')).pop() ?? '';
  const stderr = stderrLines.join('\n');

  let success = false;
  try {
    const result = JSON.parse(jsonLine);
    success = result.success === true;
  } catch { /* */ }

  return { success, stdout: jsonLine, stderr, generatedFiles };
}

