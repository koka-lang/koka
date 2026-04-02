/**
 * lsp-client.ts
 *
 * Connects Monaco to the Koka WASM LSP server running in a Web Worker.
 * Uses SharedArrayBuffer + Atomics for stdin delivery, and postMessage
 * for stdout (response) retrieval.
 *
 * The LSP server uses Content-Length framed JSON-RPC over stdio.
 * This module:
 *   - Creates the worker and initializes the WASM LSP
 *   - Bridges vscode-jsonrpc MessageReader/MessageWriter to the worker
 *   - Creates a MonacoLanguageClient connected to the transport
 */

import {
  AbstractMessageReader,
  AbstractMessageWriter,
  type DataCallback,
  type Message,
  type Disposable,
  type MessageReader,
  type MessageWriter,
} from 'vscode-jsonrpc/browser.js';
import { MonacoLanguageClient } from 'monaco-languageclient';
import { ExecuteCommandRequest } from 'vscode-languageclient/browser.js';
import { Uri } from 'vscode';
import type { KokaVFS } from './vfs';

// ── Message Reader (worker stdout → client) ─────────────────────────────────

class WorkerMessageReader extends AbstractMessageReader implements MessageReader {
  private callback: DataCallback | null = null;
  private readonly worker: Worker;
  /** Pending generated files from koka/compile responses, keyed by response ID */
  public pendingGeneratedFiles: Map<number, Map<string, string>> = new Map();

  constructor(worker: Worker) {
    super();
    this.worker = worker;
    this.worker.addEventListener('message', (e: MessageEvent) => {
      if (e.data.type === 'response' && this.callback) {
        try {
          const msg = JSON.parse(e.data.data) as Message;
          // If this response includes generated files, stash them
          if (e.data.generatedFiles) {
            const id = (msg as { id?: number }).id;
            if (id !== undefined) {
              this.pendingGeneratedFiles.set(id, new Map(e.data.generatedFiles));
            }
          }
          this.callback(msg);
        } catch (err) {
          this.fireError(err as Error);
        }
      }
    });
  }

  listen(callback: DataCallback): Disposable {
    this.callback = callback;
    return {
      dispose: () => { this.callback = null; },
    };
  }
}

// ── Message Writer (client → worker stdin via SharedArrayBuffer) ─────────────

class WorkerMessageWriter extends AbstractMessageWriter implements MessageWriter {
  private flagView: Int32Array | null = null;
  private sharedBuffer: SharedArrayBuffer | null = null;
  private readonly encoder = new TextEncoder();

  setSharedBuffer(sharedBuffer: SharedArrayBuffer): void {
    this.sharedBuffer = sharedBuffer;
    this.flagView = new Int32Array(sharedBuffer, 0, 2);
  }

  async write(msg: Message): Promise<void> {
    if (!this.sharedBuffer || !this.flagView) {
      throw new Error('SharedArrayBuffer not initialized');
    }

    const json = JSON.stringify(msg);
    const body = this.encoder.encode(json);
    const header = this.encoder.encode(`Content-Length: ${body.byteLength}\r\n\r\n`);

    const totalLen = header.byteLength + body.byteLength;

    // Wait until the worker has consumed the previous message
    while (Atomics.load(this.flagView, 0) !== 0) {
      // Spin briefly — the worker should consume quickly
      await new Promise(r => setTimeout(r, 1));
    }

    // Write the full LSP message (header + body) into the shared buffer
    const dataView = new Uint8Array(this.sharedBuffer, 8);
    if (totalLen > dataView.byteLength) {
      throw new Error(
        `LSP message too large for shared buffer: ${totalLen} bytes > ${dataView.byteLength} bytes capacity`
      );
    }
    dataView.set(header, 0);
    dataView.set(body, header.byteLength);

    // Set length and signal data ready
    Atomics.store(this.flagView, 1, totalLen);
    Atomics.store(this.flagView, 0, 1);
    Atomics.notify(this.flagView, 0);
  }

  end(): void {
    // Nothing to clean up
  }
}

// ── Public API ──────────────────────────────────────────────────────────────

export interface LspClientOptions {
  /** URL to the koka-lsp.wasm file */
  wasmUrl: string;
  /** Compiler flags (--sharedir, --libdir, --target, etc.) */
  compilerFlags: readonly string[];
  /** The VFS containing stdlib sources and precompiled files */
  vfs: KokaVFS;
  /** Called for LSP server log messages (stderr) */
  onLog?: (text: string) => void;
  /** Called when progress begins/updates/ends */
  onProgress?: (info: { title: string; message?: string; percentage?: number } | null) => void;
  /** Verbosity level (0=quiet, 1=phases, 2=detail, 3=trace) */
  verbose?: number;
}

export interface LspCompileResult {
  success: boolean;
  /** Path to the generated executable (from the LSP response) */
  exePath: string | null;
  /** Generated files from the WASI filesystem (path → content) */
  generatedFiles: Map<string, string>;
}

/**
 * Compile a file via the LSP's koka/compile command.
 * Returns generated files from the WASI filesystem.
 */
export async function compileViaLsp(
  client: MonacoLanguageClient,
  reader: WorkerMessageReader,
  filePath: string,
  fnName: string,
  additionalArgs: string = '',
): Promise<LspCompileResult> {
  // Snapshot the set of known IDs before the request so we can identify the new one after
  const idsBefore = new Set(reader.pendingGeneratedFiles.keys());

  const result = await client.sendRequest(ExecuteCommandRequest.type, {
    command: 'koka/compileFunction',
    arguments: [filePath, fnName, additionalArgs],
  });

  // Find the response ID that appeared after our request
  let generatedFiles = new Map<string, string>();
  for (const id of reader.pendingGeneratedFiles.keys()) {
    if (!idsBefore.has(id)) {
      generatedFiles = reader.pendingGeneratedFiles.get(id) ?? generatedFiles;
      reader.pendingGeneratedFiles.delete(id);
      break;
    }
  }

  const exePath = typeof result === 'string' ? result : null;
  return {
    success: exePath !== null,
    exePath,
    generatedFiles,
  };
}

export interface LspClientHandle {
  client: MonacoLanguageClient;
  /** Compile a file via the LSP and return generated files */
  compile: (filePath: string, fnName: string, additionalArgs?: string) => Promise<LspCompileResult>;
}

/**
 * Start the WASM LSP server in a Web Worker and connect MonacoLanguageClient.
 * Returns a handle with the client and a compile function.
 */
export async function startLspClient(
  options: LspClientOptions,
): Promise<LspClientHandle> {
  // Check for SharedArrayBuffer support
  if (typeof SharedArrayBuffer === 'undefined') {
    throw new Error(
      'SharedArrayBuffer not available. The page must be served with ' +
      'Cross-Origin-Opener-Policy: same-origin and ' +
      'Cross-Origin-Embedder-Policy: require-corp headers.'
    );
  }

  const worker = new Worker(
    new URL('../workers/lsp-worker.ts', import.meta.url),
    { type: 'module' },
  );

  // Collect VFS files for the LSP server's filesystem
  const vfsFiles = options.vfs.getAllFiles();

  // Set up reader and writer
  const reader = new WorkerMessageReader(worker);
  const writer = new WorkerMessageWriter();

  // Forward log messages
  worker.addEventListener('message', (e: MessageEvent) => {
    if (e.data.type === 'log' && options.onLog) {
      options.onLog(e.data.text);
    }
    if (e.data.type === 'error') {
      console.error('[LSP Worker Error]', e.data.message);
    }
  });

  // Initialize: send WASM URL and files, wait for ready + shared buffer
  await new Promise<void>((resolve, reject) => {
    const handler = (e: MessageEvent) => {
      if (e.data.type === 'ready') {
        writer.setSharedBuffer(e.data.sharedBuffer);
        worker.removeEventListener('message', handler);
        resolve();
      } else if (e.data.type === 'error') {
        worker.removeEventListener('message', handler);
        reject(new Error(e.data.message));
      }
    };
    worker.addEventListener('message', handler);
    worker.postMessage({
      type: 'init',
      wasmUrl: options.wasmUrl,
      compilerFlags: [...options.compilerFlags],
      files: Array.from(vfsFiles.entries()),
      verbose: options.verbose ?? 0,
    });
  });

  // initServices() must have been called before this point (in main.ts)
  const client = new MonacoLanguageClient({
    name: 'Koka Language Server',
    clientOptions: {
      documentSelector: [{ language: 'koka' }],
      markdown: {
        isTrusted: true,
        supportHtml: true,
      },
      middleware: {
        handleDiagnostics: (uri, diagnostics, next) => {
          // The LSP sends diagnostics with file:// URIs (e.g. file:///main.kk)
          // but Monaco models use inmemory:// URIs. Remap to match.
          const uriStr = uri.toString();
          if (uriStr.startsWith('file:///')) {
            const path = uriStr.slice('file://'.length); // e.g. "/main.kk"
            next(Uri.parse(`inmemory://playground${path}`), diagnostics);
          } else {
            next(uri, diagnostics);
          }
        },
        executeCommand: async (command: string, args: any[], next: any) => {
          if (command === 'koka/signature-help/set-context') {
            await next(command, args);
            const editor = (await import('@codingame/monaco-vscode-editor-api')).editor;
            const activeEditor = editor.getEditors()[0];
            if (activeEditor) {
              activeEditor.trigger('koka', 'editor.action.triggerParameterHints', {});
            }
          } else {
            return next(command, args);
          }
        },
      },
    },
    messageTransports: { reader, writer },
  });

  // Forward window/logMessage notifications to the onLog callback
  // These contain compiler phase info (parse, check, etc.) with ANSI colors.
  // Must be registered before start() so we capture initial type-check phases.
  if (options.onLog) {
    const log = options.onLog;
    client.onNotification('window/logMessage', (params: { message: string; type: number }) => {
      const msg = params.message.trim();
      if (msg) log(msg);
    });
  }

  // Track work-done progress and forward to callback
  if (options.onProgress) {
    const onProgress = options.onProgress;
    const progressTitles = new Map<number | string, string>();
    let clearTimer: ReturnType<typeof setTimeout> | null = null;

    client.onNotification('$/progress', (params: { token: number | string; value: { kind: string; title?: string; message?: string; percentage?: number } }) => {
      const { token, value } = params;
      if (clearTimer) { clearTimeout(clearTimer); clearTimer = null; }
      if (value.kind === 'begin') {
        progressTitles.set(token, value.title ?? 'Working...');
        onProgress({ title: value.title ?? 'Working...', message: value.message, percentage: value.percentage });
      } else if (value.kind === 'report') {
        const title = progressTitles.get(token) ?? 'Working...';
        onProgress({ title, message: value.message, percentage: value.percentage });
      } else if (value.kind === 'end') {
        const title = progressTitles.get(token) ?? 'Done';
        progressTitles.delete(token);
        if (progressTitles.size === 0) {
          // Show final message briefly before clearing
          onProgress({ title, message: value.message ?? 'done', percentage: 100 });
          clearTimer = setTimeout(() => onProgress(null), 2000);
        }
      }
    });
  }

  await client.start();

  // Send dark/light theme to the LSP server for colored markdown
  try {
    const isDark = document.body.classList.contains('vscode-dark') ||
                   window.matchMedia('(prefers-color-scheme: dark)').matches;
    await client.sendRequest(ExecuteCommandRequest.type, {
      command: 'koka/set-colors',
      arguments: [{ mode: isDark ? 'dark' : 'light' }],
    });
  } catch {
    // koka/set-colors is optional — ignore errors
  }

  return {
    client,
    compile: (filePath: string, fnName: string, additionalArgs?: string) =>
      compileViaLsp(client, reader, filePath, fnName, additionalArgs ?? ''),
  };
}
