/**
 * module-runner.ts
 *
 * Loads and executes Koka-generated ES modules in the browser using blob URLs.
 *
 * Koka compiles to ES modules with relative imports like:
 *   import * as $std_core from './std_core.mjs'
 *
 * Since we can't serve these as actual files from the playground, we resolve
 * all imports by rewriting them to blob: URLs, processing modules in
 * topological order (dependencies before dependents).
 */

import { kokaIdentToJsExport } from './koka-lang';

/** Map of filename (e.g. "std_core.mjs") to JS source text. */
interface ModuleSet {
  [filename: string]: string;
}

/**
 * Load and execute Koka-generated ES modules.
 *
 * @param precompiledMjs  Standard library .mjs modules (Map of filename -> source)
 * @param generatedMjs    Freshly compiled .mjs modules (Map of filename -> source)
 * @param mainModuleName  Name of the entry module, without extension (e.g. "main")
 * @param onOutput        Callback for stdout lines
 * @param onError         Callback for stderr lines
 */
/**
 * @param entryFunction  Optional entry function name (e.g. 'example/counter').
 *                       If not provided, calls 'main'. Koka encodes names:
 *                       '/' → '_fs_', '-' → '_dash_', '_' → '__'
 */
export async function runKokaModules(
  precompiledMjs: Map<string, string>,
  generatedMjs: Map<string, string>,
  mainModuleName: string,
  onOutput: (text: string) => void,
  onError: (text: string) => void,
  entryFunction?: string,
): Promise<void> {
  // Combine all modules: precompiled std + freshly generated
  const allModules: ModuleSet = {};

  for (const [name, code] of precompiledMjs) {
    // Normalise to bare filename
    allModules[normalizeName(name)] = code;
  }

  for (const [path, code] of generatedMjs) {
    // path may be a full VFS path like /.koka/v3.2.4/.../main.mjs
    allModules[normalizeName(path)] = code;
  }

  const mainFilename = mainModuleName.endsWith('.mjs')
    ? mainModuleName
    : mainModuleName + '.mjs';

  if (!allModules[mainFilename]) {
    onError(`No module named "${mainFilename}" found in generated output.`);
    return;
  }

  // ── Resolve imports topologically via blob URLs ───────────────────────────
  //
  // Each module's source text contains lines like:
  //   import * as $std_core from './std_core.mjs'
  //
  // We rewrite those to blob: URLs once the dependency is resolved.

  const blobUrls: Record<string, string> = {};
  let remaining = Object.keys(allModules);
  const MAX_ROUNDS = 50;

  for (let round = 0; round < MAX_ROUNDS && remaining.length > 0; round++) {
    const nextRemaining: string[] = [];

    for (const name of remaining) {
      const code = allModules[name];
      let allResolved = true;

      const rewritten = code.replace(
        /from\s+['"]\.\/([^'"]+)['"]/g,
        (_match, importName: string) => {
          if (blobUrls[importName]) {
            return `from '${blobUrls[importName]}'`;
          } else if (allModules[importName] !== undefined) {
            // Dependency exists but not yet resolved — try next round
            allResolved = false;
            return _match;
          }
          // Unknown import — leave as-is (may fail at runtime)
          return _match;
        },
      );

      if (allResolved) {
        const blob = new Blob([rewritten], { type: 'application/javascript' });
        blobUrls[name] = URL.createObjectURL(blob);
      } else {
        nextRemaining.push(name);
      }
    }

    remaining = nextRemaining;
  }

  if (remaining.length > 0) {
    onOutput(`Warning: could not resolve all imports for: ${remaining.join(', ')}`);
  }

  if (!blobUrls[mainFilename]) {
    onError(`Could not create blob URL for main module — unresolved imports.`);
    // Clean up any URLs we did create
    for (const url of Object.values(blobUrls)) URL.revokeObjectURL(url);
    return;
  }

  // ── Set up output capture ────────────────────────────────────────────────
  //
  // Koka's std/core/console detects "browser" and writes to a DOM element
  // (#koka-console-out) instead of console.log. We create a hidden element
  // to capture that output, plus patch console.log for any direct calls.

  // Create hidden capture element
  let kokaConsoleOut = document.getElementById('koka-console-out');
  const createdConsoleOut = !kokaConsoleOut;
  if (!kokaConsoleOut) {
    const konsolDiv = document.createElement('div');
    konsolDiv.id = 'koka-console';
    konsolDiv.style.display = 'none';
    kokaConsoleOut = document.createElement('div');
    kokaConsoleOut.id = 'koka-console-out';
    konsolDiv.appendChild(kokaConsoleOut);
    document.body.appendChild(konsolDiv);
  }
  kokaConsoleOut.innerHTML = '';

  // Also patch console.log for any direct output
  const origLog   = console.log;
  const origError = console.error;
  const origWarn  = console.warn;

  console.log = (...args: unknown[]) => {
    origLog(...args);
    onOutput(args.map(String).join(' '));
  };
  console.error = (...args: unknown[]) => {
    origError(...args);
    onError(args.map(String).join(' '));
  };
  console.warn = (...args: unknown[]) => {
    origWarn(...args);
    onOutput('[warn] ' + args.map(String).join(' '));
  };

  try {
    const mod = await import(/* @vite-ignore */ blobUrls[mainFilename]);
    // Determine which export to call.
    // Koka's asciiEncode for exports (isModule=false): '/' → '_fs_', '-' → '_dash_', '_' → '__'
    const fnName = entryFunction ? kokaIdentToJsExport(entryFunction) : 'main';
    const fn = mod[fnName] ?? mod.main;
    if (typeof fn === 'function') {
      await fn();
    }

    // Collect output from the DOM element (Koka browser runtime writes there)
    // The runtime uses innerHTML with <br> for newlines, so convert back
    if (kokaConsoleOut && kokaConsoleOut.innerHTML) {
      const lines = kokaConsoleOut.innerHTML
        .replace(/<br\s*\/?>/gi, '\n')
        .replace(/<[^>]*>/g, '')  // strip any other HTML tags
        .replace(/&amp;/g, '&')
        .replace(/&lt;/g, '<')
        .replace(/&gt;/g, '>')
        .replace(/&quot;/g, '"')
        .replace(/&apos;/g, "'");
      for (const line of lines.split('\n')) {
        if (line) onOutput(line);
      }
    }
  } catch (e: unknown) {
    const msg = e instanceof Error ? e.message : String(e);
    onError('[runtime error] ' + msg);
    if (e instanceof Error && e.stack) {
      const shortStack = e.stack.split('\n').slice(0, 4).join('\n');
      onError(shortStack);
    }
  } finally {
    console.log   = origLog;
    console.error = origError;
    console.warn  = origWarn;
    // Clean up the hidden console element
    if (createdConsoleOut) {
      document.getElementById('koka-console')?.remove();
    }
    for (const url of Object.values(blobUrls)) URL.revokeObjectURL(url);
  }
}

/** Extract the bare filename from a path or return as-is. */
function normalizeName(nameOrPath: string): string {
  const slash = nameOrPath.lastIndexOf('/');
  return slash === -1 ? nameOrPath : nameOrPath.slice(slash + 1);
}
