/**
 * actions.ts
 *
 * Async compile/run logic extracted from main.ts.
 * These are plain functions, not hooks — they take dispatch + refs.
 */

import type { Dispatch, MutableRefObject } from 'react';
import type { PlaygroundAction, PlaygroundRefs, WasmCompileFn, InlayHintSettings } from './context';
import { createWasmCompiler } from './wasm-runner';
import { startLspClient } from './lsp-client';
import { COMPILER_FLAGS } from './flags';
import { runKokaModules } from './module-runner';
import { buildFileTree } from './samples';
import { kokaModuleToFilename } from './koka-lang';
import { MODULE_NAME_RE } from './koka-lang';
import AnsiToHtml from 'ansi-to-html';

const ansiConverter = new AnsiToHtml({ escapeXML: true });

// ── Helpers ──────────────────────────────────────────────────────────────

export function appendCompilerLog(dispatch: Dispatch<PlaygroundAction>, msg: string): void {
  dispatch({ type: 'APPEND_COMPILER_LOG', html: ansiConverter.toHtml(msg) });
}

export function appendConsole(dispatch: Dispatch<PlaygroundAction>, text: string, cls: 'stdout' | 'stderr' | 'info' = 'stdout'): void {
  dispatch({ type: 'APPEND_CONSOLE', text, cls });
}

// ── VFS Preloading ──────────────────────────────────────────────────────

export async function preloadStdlib(
  dispatch: Dispatch<PlaygroundAction>,
  refs: MutableRefObject<PlaygroundRefs>,
): Promise<void> {
  dispatch({ type: 'SET_STATUS', kind: 'loading', text: 'Loading stdlib...' });
  try {
    await Promise.all([
      refs.current.vfs.preloadSources('', 'stdlib-manifest.json')
        .catch((e: unknown) => { appendCompilerLog(dispatch, '[warn] stdlib preload: ' + String(e)); return 0; }),
      refs.current.vfs.preloadPrecompiled('', 'precompiled-manifest.json')
        .catch((e: unknown) => { appendCompilerLog(dispatch, '[warn] precompiled preload: ' + String(e)); return 0; }),
    ]);
    // Loaded silently
  } catch (e: unknown) {
    appendConsole(dispatch, 'Warning: could not preload stdlib: ' + String(e), 'info');
  }
  dispatch({ type: 'SET_STATUS', kind: 'ready', text: 'Ready' });
}

// ── Samples Preloading ──────────────────────────────────────────────────

/**
 * Preload all .kk sample files into VFS from the local samples/ directory.
 * This enables module resolution for samples that import each other.
 */
export async function preloadSamplesDirectory(
  dispatch: Dispatch<PlaygroundAction>,
  refs: MutableRefObject<PlaygroundRefs>,
): Promise<void> {
  if (refs.current.samplesPreloaded) return;
  refs.current.samplesPreloaded = true;

  try {
    const resp = await fetch('samples-manifest.json');
    if (!resp.ok) return;
    const files: string[] = await resp.json();

    await Promise.all(files.map(async (f) => {
      try {
        const r = await fetch('samples/' + f);
        if (r.ok) {
          const text = await r.text();
          // Place at root so "basic/caesar" resolves to "/basic/caesar.kk"
          refs.current.vfs.addFile('/' + f, text);
        }
      } catch { /* ignore individual failures */ }
    }));
  } catch {
    // Samples manifest not available
  }
}

// ── LSP Startup ─────────────────────────────────────────────────────────

export async function startLsp(
  dispatch: Dispatch<PlaygroundAction>,
  refs: MutableRefObject<PlaygroundRefs>,
  inlayHints?: InlayHintSettings,
): Promise<void> {
  if (typeof SharedArrayBuffer === 'undefined') {
    console.warn('SharedArrayBuffer not available -- LSP features disabled.');
    return;
  }
  try {
    const handle = await startLspClient({
      wasmUrl: new URL('koka-lsp.wasm', window.location.href).href,
      compilerFlags: COMPILER_FLAGS,
      vfs: refs.current.vfs,
      verbose: 1,
      onLog: (text: string) => {
        if (text.includes('WithSeverity') || text.includes('Failed to parse config')) return;
        if (text.trim()) appendCompilerLog(dispatch, '[LSP] ' + text);
      },
      onProgress: (info) => {
        dispatch({ type: 'SET_PROGRESS', progress: info });
      },
    });
    refs.current.lspHandle = handle;
    // Send initial inlay hint configuration
    if (inlayHints) sendLspConfig(refs, inlayHints);
  } catch (err) {
    console.warn('[LSP] Failed to start:', err);
    appendCompilerLog(dispatch, '[LSP] Failed to start: ' + String(err));
  }
}

// ── WASM Compiler ───────────────────────────────────────────────────────

async function ensureWasmCompiler(
  dispatch: Dispatch<PlaygroundAction>,
  refs: MutableRefObject<PlaygroundRefs>,
): Promise<WasmCompileFn | null> {
  if (refs.current.wasmCompile) return refs.current.wasmCompile;
  if (refs.current.wasmLoading) return refs.current.wasmLoading;

  refs.current.wasmLoading = (async () => {
    try {
      dispatch({ type: 'SET_STATUS', kind: 'loading', text: 'Loading WASM compiler...' });
      const fn = await createWasmCompiler({
        wasmUrl: new URL('koka-playground.wasm', window.location.href).href,
        compilerFlags: COMPILER_FLAGS,
        getAllFiles: () => refs.current.vfs.getAllFiles(),
        onLog: (text) => appendCompilerLog(dispatch, '[WASM] ' + text),
      });
      refs.current.wasmCompile = fn;
      dispatch({ type: 'SET_STATUS', kind: 'ready', text: 'Compiler ready' });
      return fn;
    } catch (e) {
      appendCompilerLog(dispatch, '[WASM] Compiler not available: ' + String(e));
      appendConsole(dispatch, 'WASM compiler failed to load: ' + String(e), 'stderr');
      refs.current.wasmLoading = null;
      return null;
    }
  })();

  return refs.current.wasmLoading;
}

// ── Compile ─────────────────────────────────────────────────────────────

async function compileWithLsp(
  dispatch: Dispatch<PlaygroundAction>,
  refs: MutableRefObject<PlaygroundRefs>,
  moduleName: string,
  verbosity: number,
  entryFunction?: string,
): Promise<string | null> {
  if (!refs.current.lspHandle) return null;
  try {
    const filePath = '/' + moduleName.replace(/\./g, '/') + '.kk';
    const args: string[] = [`-v${verbosity}`];
    const entryfn = entryFunction || 'main';
    const extraArgs = args.join(' ');
    const result = await refs.current.lspHandle.compile(filePath, entryfn, extraArgs);
    if (!result.success) return null;
    for (const [path, content] of result.generatedFiles) {
      refs.current.vfs.addFile('/.koka/' + path, content);
    }
    // LSP compile complete
    return moduleName;
  } catch (err) {
    console.warn('LSP compile error:', err);
    return null;
  }
}

async function compileWithWasm(
  dispatch: Dispatch<PlaygroundAction>,
  refs: MutableRefObject<PlaygroundRefs>,
  moduleName: string,
  sourceText: string,
  verbosity: number,
  entryFunction?: string,
): Promise<string | null> {
  const compiler = await ensureWasmCompiler(dispatch, refs);
  if (!compiler) {
    appendConsole(dispatch, 'WASM compiler not available.', 'stderr');
    return null;
  }
  const extraArgs: string[] = [`-v${verbosity}`];
  if (entryFunction && entryFunction !== 'main') {
    extraArgs.push(`--main-entry=${entryFunction}`);
  }
  const result = await compiler(moduleName, sourceText, extraArgs);
  if (!result.success) {
    appendConsole(dispatch, '=== Compilation Errors ===', 'stderr');
    try {
      const parsed = JSON.parse(result.stdout);
      for (const err of parsed.errors ?? []) appendConsole(dispatch, err, 'stderr');
    } catch {
      appendConsole(dispatch, result.stdout || '(no error details)', 'stderr');
    }
    return null;
  }
  for (const [path, content] of result.generatedFiles) {
    refs.current.vfs.addFile('/.koka/' + path, content);
  }
  // Generated files stored silently
  return moduleName;
}

async function compile(
  dispatch: Dispatch<PlaygroundAction>,
  refs: MutableRefObject<PlaygroundRefs>,
  sourceText: string,
  backend: string,
  verbosity: number,
  entryFunction?: string,
): Promise<string | null> {
  const moduleMatch = sourceText.match(MODULE_NAME_RE);
  const moduleName = moduleMatch ? moduleMatch[1] : 'main';

  if (backend === 'lsp') {
    if (refs.current.lspHandle) {
      const result = await compileWithLsp(dispatch, refs, moduleName, verbosity, entryFunction);
      if (result !== null) return result;
    } else {
      for (let i = 0; i < 10 && !refs.current.lspHandle; i++) {
        await new Promise(r => setTimeout(r, 500));
      }
      if (refs.current.lspHandle) {
        const result = await compileWithLsp(dispatch, refs, moduleName, verbosity, entryFunction);
        if (result !== null) return result;
      }
    }
    return compileWithWasm(dispatch, refs, moduleName, sourceText, verbosity, entryFunction);
  }
  return compileWithWasm(dispatch, refs, moduleName, sourceText, verbosity, entryFunction);
}

// ── Run ─────────────────────────────────────────────────────────────────

async function run(
  dispatch: Dispatch<PlaygroundAction>,
  refs: MutableRefObject<PlaygroundRefs>,
  moduleName: string,
  entryFunction?: string,
): Promise<void> {
  const generatedMjs = refs.current.vfs.getGeneratedMjs();
  if (generatedMjs.size === 0) {
    appendConsole(dispatch, 'No .mjs output found after compilation.', 'stderr');
    return;
  }
  const expectedFilename = kokaModuleToFilename(moduleName) + '.mjs';
  for (const [path, code] of generatedMjs) {
    const filename = path.split('/').pop() ?? path;
    if (filename === expectedFilename || filename === 'main.mjs') {
      refs.current.jsEditor?.setValue(code);
      // Generated file info goes to compiler log, not console
      break;
    }
  }
  await runKokaModules(
    refs.current.vfs.precompiledMjs,
    generatedMjs,
    kokaModuleToFilename(moduleName),
    (text) => appendConsole(dispatch, text, 'stdout'),
    (text) => appendConsole(dispatch, text, 'stderr'),
    entryFunction,
  );

  // Update the VFS section in the file browser
  refreshVfsSection(dispatch, refs);
}

function refreshVfsSection(
  dispatch: Dispatch<PlaygroundAction>,
  refs: MutableRefObject<PlaygroundRefs>,
): void {
  const allFiles = refs.current.vfs.getAllFiles();
  // Collect generated output files, stripping the build dir prefix for a flat listing
  const outputFiles = new Map<string, string>();
  for (const [path, content] of allFiles) {
    if (path.includes('.koka/') && (path.endsWith('.mjs') || path.endsWith('.kki'))) {
      // Strip everything up to the last js-debug-XXXXX/ directory
      const match = path.match(/js-debug-[^/]+\/(.+)$/);
      const displayPath = match ? match[1] : path.split('/').pop() ?? path;
      outputFiles.set(displayPath, content);
    }
  }
  const tree = buildFileTree(outputFiles);
  dispatch({ type: 'UPDATE_SECTION', title: 'Output', entries: tree });
}

// ── Compile & Run ───────────────────────────────────────────────────────

export async function compileAndRun(
  dispatch: Dispatch<PlaygroundAction>,
  refs: MutableRefObject<PlaygroundRefs>,
  backend: string,
  verbosity: number,
  entryFunction?: string,
): Promise<void> {
  const sourceText = refs.current.sourceEditor?.getValue();
  if (!sourceText) return;

  dispatch({ type: 'CLEAR_CONSOLE' });
  dispatch({ type: 'SET_STATUS', kind: 'running', text: 'Compiling...' });

  try {
    const moduleName = await compile(dispatch, refs, sourceText, backend, verbosity, entryFunction);
    if (moduleName === null) {
      dispatch({ type: 'SET_STATUS', kind: 'error', text: 'Compilation failed' });
      return;
    }
    appendConsole(dispatch, 'Compilation successful!', 'info');
    dispatch({ type: 'SET_STATUS', kind: 'running', text: 'Running...' });
    await run(dispatch, refs, moduleName, entryFunction);
  } catch (err: unknown) {
    const msg = err instanceof Error ? err.message : String(err);
    appendConsole(dispatch, 'ERROR: ' + msg, 'stderr');
    dispatch({ type: 'SET_STATUS', kind: 'error', text: 'Error' });
  }

  dispatch({ type: 'SET_STATUS', kind: 'ready', text: 'Compiler ready' });
}

// ── LSP Configuration ───────────────────────────────────────────────────

export async function sendLspConfig(
  refs: MutableRefObject<PlaygroundRefs>,
  inlayHints: InlayHintSettings,
): Promise<void> {
  const client = refs.current.lspHandle?.client;
  if (!client) return;
  await client.sendNotification('workspace/didChangeConfiguration', {
    settings: {
      koka: {
        languageServer: {
          inlayHints: {
            showImplicitArguments: inlayHints.showImplicitArguments,
            showInferredTypes: inlayHints.showInferredTypes,
            showFullQualifiers: inlayHints.showFullQualifiers,
          },
        },
      },
    },
  });
  // Trigger Monaco to re-request inlay hints with the new config.
  // A short delay lets the LSP process the config change first.
  setTimeout(() => {
    const editor = refs.current.sourceEditor;
    if (editor) {
      // Toggle a whitespace edit to force Monaco to re-request all providers
      const model = editor.getModel();
      if (model) {
        const pos = model.getFullModelRange().getEndPosition();
        model.applyEdits([{ range: { startLineNumber: pos.lineNumber, startColumn: pos.column, endLineNumber: pos.lineNumber, endColumn: pos.column }, text: ' ' }]);
        model.applyEdits([{ range: { startLineNumber: pos.lineNumber, startColumn: pos.column, endLineNumber: pos.lineNumber, endColumn: pos.column + 1 }, text: '' }]);
      }
    }
  }, 100);
}
