/**
 * shared-compiler.ts
 *
 * Manages a single WASM compiler Web Worker shared by all <koka-editor> elements
 * on the page. Lazily loaded on first compile request.
 */

import { createWasmCompiler } from '../editor/wasm-runner';
import { runKokaModules } from '../editor/module-runner';

export interface SharedCompiler {
  compile: (moduleName: string, sourceText: string, extraArgs?: string[]) => Promise<CompileResult>;
  run: (moduleName: string, generatedFiles: Map<string, string>, entryFunction?: string) => Promise<string>;
}

export interface CompileResult {
  success: boolean;
  stdout: string;
  stderr: string;
  generatedFiles: Map<string, string>;
}

let sharedInstance: SharedCompiler | null = null;
let loading: Promise<SharedCompiler | null> | null = null;

/**
 * Get or create the shared WASM compiler.
 * All <koka-editor> elements on the page share this instance.
 */
export async function getSharedCompiler(): Promise<SharedCompiler | null> {
  if (sharedInstance) return sharedInstance;
  if (loading) return loading;

  loading = initCompiler();
  sharedInstance = await loading;
  return sharedInstance;
}

async function initCompiler(): Promise<SharedCompiler | null> {
  try {
    const wasmUrl = findWasmUrl();
    const stdlibUrl = findStdlibUrl();

    // Load stdlib sources and precompiled files
    const { sources, precompiled } = await loadStdlib(stdlibUrl);

    // Build VFS from stdlib
    const vfsFiles = new Map<string, string>();
    for (const [path, content] of sources) {
      vfsFiles.set('/share/lib/' + path, content);
    }
    for (const [f, content] of precompiled) {
      vfsFiles.set('/lib/js-debug/' + f, content);
    }

    // Create WASM compiler in a Web Worker via wasm-runner
    const { COMPILER_FLAGS } = await import('../editor/flags');
    const compileFn = await createWasmCompiler({
      wasmUrl,
      compilerFlags: COMPILER_FLAGS,
      getAllFiles: () => vfsFiles,
      onLog: (text) => console.log('[koka]', text),
    });

    return {
      compile: async (moduleName, sourceText, extraArgs?) => {
        // Add user source to VFS temporarily
        const userPath = '/' + moduleName.replace(/\./g, '/') + '.kk';
        vfsFiles.set(userPath, sourceText);

        const result = await compileFn(moduleName, sourceText, extraArgs);

        // Clean up user source
        vfsFiles.delete(userPath);

        return {
          success: result.success,
          stdout: result.stdout,
          stderr: result.stderr,
          generatedFiles: result.generatedFiles,
        };
      },
      run: (moduleName, generatedFiles, entryFunction?) =>
        runModules(moduleName, generatedFiles, precompiled, entryFunction),
    };
  } catch (err) {
    console.error('[koka-editor] Failed to initialize compiler:', err);
    return null;
  }
}

// ── URL discovery ───────────────────────────────────────────────────────────

function findWasmUrl(): string {
  const meta = document.querySelector('meta[name="koka-wasm-url"]');
  if (meta) return meta.getAttribute('content') || '';

  const cfg = (globalThis as Record<string, unknown>).kokaConfig as Record<string, string> | undefined;
  if (cfg?.wasmUrl) return cfg.wasmUrl;

  return new URL('koka-playground.wasm', window.location.href).href;
}

function findStdlibUrl(): string {
  const meta = document.querySelector('meta[name="koka-stdlib-url"]');
  if (meta) return meta.getAttribute('content') || '';

  const cfg = (globalThis as Record<string, unknown>).kokaConfig as Record<string, string> | undefined;
  if (cfg?.stdlibUrl) return cfg.stdlibUrl;

  return '';
}

// ── Stdlib loading ──────────────────────────────────────────────────────────

async function loadStdlib(baseUrl: string): Promise<{
  sources: Map<string, string>;
  precompiled: Map<string, string>;
}> {
  const sources = new Map<string, string>();
  const precompiled = new Map<string, string>();
  const prefix = baseUrl ? baseUrl.replace(/\/$/, '') + '/' : '';

  try {
    const resp = await fetch(prefix + 'stdlib-manifest.json');
    if (resp.ok) {
      const files: string[] = await resp.json();
      await Promise.all(files.map(async (f) => {
        try {
          const r = await fetch(prefix + 'lib/' + f);
          if (r.ok) sources.set(f, await r.text());
        } catch { /* skip */ }
      }));
    }
  } catch { /* no manifest */ }

  try {
    const resp = await fetch(prefix + 'precompiled-manifest.json');
    if (resp.ok) {
      const files: string[] = await resp.json();
      await Promise.all(files.map(async (f) => {
        try {
          const r = await fetch(prefix + 'precompiled/' + f);
          if (r.ok) precompiled.set(f, await r.text());
        } catch { /* skip */ }
      }));
    }
  } catch { /* no manifest */ }

  return { sources, precompiled };
}

// ── Module execution (delegates to module-runner.ts) ────────────────────────

async function runModules(
  moduleName: string,
  generatedFiles: Map<string, string>,
  precompiled: Map<string, string>,
  entryFunction?: string,
): Promise<string> {
  // Filter to .mjs files only
  const precompiledMjs = new Map<string, string>();
  for (const [name, content] of precompiled) {
    if (name.endsWith('.mjs')) precompiledMjs.set(name, content);
  }

  const outputLines: string[] = [];
  const errorLines: string[] = [];

  await runKokaModules(
    precompiledMjs,
    generatedFiles,
    moduleName,
    (text) => outputLines.push(text),
    (text) => errorLines.push(text),
    entryFunction,
  );

  if (errorLines.length > 0) {
    return [...outputLines, ...errorLines.map(e => `Error: ${e}`)].join('\n');
  }
  return outputLines.join('\n');
}
