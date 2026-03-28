/**
 * lsp-worker.ts
 *
 * Web Worker that runs the Koka LSP server (WASM) off the main thread.
 * Uses SharedArrayBuffer + Atomics for blocking stdin reads.
 *
 * Main → Worker:
 *   { type: 'init', wasmUrl: string, files: [path, content][] }
 *
 * Worker → Main:
 *   { type: 'ready' }
 *   { type: 'response', data: string }  // raw LSP JSON-RPC message (no headers)
 *   { type: 'log', text: string }
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

// ── WASI fd interface stubs ──────────────────────────────────────────────────
// Base class with no-op/error stubs for the WASI fd interface.
// BlockingStdinFile overrides fd_read; LspStdoutCapture overrides fd_write.

class WasiFdStubs {
  fd_fdstat_get() { return { ret: 0, fdstat: null }; }
  fd_close() { return 0; }
  fd_read(_len: number): { ret: number; data: Uint8Array } { return { ret: 8, data: new Uint8Array(0) }; }
  fd_write(_data: Uint8Array): { ret: number; nwritten: number } { return { ret: 8, nwritten: 0 }; }
  fd_seek(_offset: bigint, _whence: number) { return { ret: 8, offset: 0n }; }
  fd_tell() { return { ret: 0, offset: 0n }; }
  fd_sync() { return 0; }
  fd_filestat_get() { return { ret: 0, filestat: null }; }
  fd_pread(_len: number, _offset: bigint) { return { ret: 8, data: new Uint8Array(0) }; }
  fd_pwrite(_data: Uint8Array, _offset: bigint) { return { ret: 8, nwritten: 0 }; }
  fd_allocate(_offset: bigint, _len: bigint) { return 0; }
  fd_fdstat_set_flags(_flags: number) { return 0; }
  fd_fdstat_set_rights(_base: bigint, _inheriting: bigint) { return 0; }
  fd_filestat_set_size(_size: bigint) { return 0; }
  fd_filestat_set_times(_a: bigint, _m: bigint, _flags: number) { return 0; }
  fd_prestat_get() { return { ret: 8, prestat: null }; }
  fd_readdir_single(_cookie: bigint) { return { ret: 8, dirent: null }; }
  path_create_directory(_path: string) { return 8; }
  path_filestat_get(_flags: number, _path: string) { return { ret: 8, filestat: null }; }
  path_filestat_set_times(_f: number, _p: string, _a: bigint, _m: bigint, _fl: number) { return 8; }
  path_lookup(_path: string, _flags: number) { return { ret: 8, inode_obj: null }; }
  path_link(_path: string, _inode: unknown, _force: boolean) { return 8; }
  path_open(..._args: unknown[]) { return { ret: 8, fd_obj: null }; }
  path_readlink(_path: string) { return { ret: 8, data: null }; }
  path_remove_directory(_path: string) { return 8; }
  path_unlink(_path: string) { return { ret: 8, inode_obj: null }; }
  path_unlink_file(_path: string) { return 8; }
}

// ── Blocking stdin via SharedArrayBuffer ────────────────────────────────────

/**
 * A custom File-like object for WASI stdin that blocks via Atomics.wait
 * until the main thread sends data.
 *
 * SharedArrayBuffer layout:
 *   Int32[0] = flag (0 = empty, 1 = data ready)
 *   Int32[1] = data length in bytes
 *   Uint8[8..] = data bytes
 */
class BlockingStdinFile extends WasiFdStubs {
  private flagView: Int32Array;
  private sharedBuffer: SharedArrayBuffer;
  private pendingData: Uint8Array = new Uint8Array(0);
  private offset = 0;
  /** Reference to stdout capture for tracking compile request IDs */
  public stdoutCapture: LspStdoutCapture | null = null;
  private incomingBuffer = '';
  private readonly textDecoder = new TextDecoder();

  constructor(sharedBuffer: SharedArrayBuffer) {
    super();
    this.sharedBuffer = sharedBuffer;
    this.flagView = new Int32Array(sharedBuffer, 0, 2);
  }

  /** Scan incoming data for koka/compile request IDs */
  private trackCompileRequests(data: Uint8Array): void {
    if (!this.stdoutCapture) return;
    this.incomingBuffer += this.textDecoder.decode(data, { stream: true });
    // Try to find JSON-RPC messages containing koka/compile
    // Simple heuristic: look for the pattern in the accumulated buffer
    const re = /"id"\s*:\s*(\d+).*"method"\s*:\s*"workspace\/executeCommand".*"koka\/compile/g;
    let m: RegExpExecArray | null;
    while ((m = re.exec(this.incomingBuffer)) !== null) {
      this.stdoutCapture.compileRequestIds.add(parseInt(m[1], 10));
    }
    // Also check for koka/compileFunction
    const re2 = /"id"\s*:\s*(\d+).*"method"\s*:\s*"workspace\/executeCommand".*"koka\/compileFunction/g;
    while ((m = re2.exec(this.incomingBuffer)) !== null) {
      this.stdoutCapture.compileRequestIds.add(parseInt(m[1], 10));
    }
    // Keep buffer bounded — only keep last 4KB
    if (this.incomingBuffer.length > 4096) {
      this.incomingBuffer = this.incomingBuffer.slice(-2048);
    }
  }

  fd_read(len: number): { ret: number; data: Uint8Array } {
    // If we have leftover data from a previous read, return that first
    if (this.offset < this.pendingData.length) {
      const available = this.pendingData.length - this.offset;
      const toRead = Math.min(len, available);
      const data = this.pendingData.slice(this.offset, this.offset + toRead);
      this.offset += toRead;
      return { ret: 0, data };
    }

    // Check if new data is available (non-blocking first)
    let flag = Atomics.load(this.flagView, 0);
    if (flag !== 1) {
      // Short timeout to let WASI runtime run (e.g. sendServer)
      Atomics.wait(this.flagView, 0, 0, 10);
      flag = Atomics.load(this.flagView, 0);
      if (flag !== 1) {
        return { ret: 0, data: new Uint8Array(0) };
      }
    }

    // Data available — read it
    const dataLen = Atomics.load(this.flagView, 1);
    const dataView = new Uint8Array(this.sharedBuffer, 8, dataLen);
    this.pendingData = new Uint8Array(dataView);
    this.offset = 0;

    Atomics.store(this.flagView, 0, 0);
    Atomics.notify(this.flagView, 0);

    // Track incoming koka/compile requests so stdout can attach generated files
    this.trackCompileRequests(this.pendingData);

    const toRead = Math.min(len, this.pendingData.length);
    const data = this.pendingData.slice(0, toRead);
    this.offset = toRead;
    return { ret: 0, data };
  }

}

// ── Stdout capture with LSP message parsing ─────────────────────────────────

/**
 * Captures stdout bytes and parses LSP Content-Length framed messages.
 * Forwards complete JSON-RPC messages back to the main thread via postMessage.
 *
 * When a koka/compile response is detected, also collects generated files
 * from the WASI filesystem and sends them alongside the response.
 */
class LspStdoutCapture extends WasiFdStubs {
  private buffer = '';
  private readonly decoder = new TextDecoder();
  /** Set of request IDs that are koka/compile or koka/compileFunction */
  public compileRequestIds: Set<number> = new Set();
  /** Reference to the unified WASI root directory */
  public rootDir: Directory | null = null;

  fd_write(data: Uint8Array): { ret: number; nwritten: number } {
    this.buffer += this.decoder.decode(data, { stream: true });
    this.parseMessages();
    return { ret: 0, nwritten: data.byteLength };
  }

  private parseMessages(): void {
    while (true) {
      const headerEnd = this.buffer.indexOf('\r\n\r\n');
      if (headerEnd === -1) break;

      const headerSection = this.buffer.substring(0, headerEnd);
      const match = headerSection.match(/Content-Length:\s*(\d+)/i);
      if (!match) {
        this.buffer = this.buffer.substring(headerEnd + 4);
        continue;
      }

      // Note: Content-Length is bytes but we use string length. This works for
      // ASCII/Latin-1 content but may desync on multi-byte UTF-8 in JSON-RPC bodies.
      const contentLength = parseInt(match[1], 10);
      const bodyStart = headerEnd + 4;
      const bodyEnd = bodyStart + contentLength;

      if (this.buffer.length < bodyEnd) break;

      const body = this.buffer.substring(bodyStart, bodyEnd);
      this.buffer = this.buffer.substring(bodyEnd);

      // Check if this is a response to a koka/compile request
      try {
        const msg = JSON.parse(body);
        if (msg.id !== undefined && this.compileRequestIds.has(msg.id)) {
          this.compileRequestIds.delete(msg.id);
          // Collect generated files from the WASI filesystem
          const generatedFiles = new Map<string, string>();
          const textDecoder = new TextDecoder();
          if (this.rootDir) {
            // Collect from all relevant subdirs of the unified root
            const libDir = this.rootDir.contents.get('lib');
            if (libDir instanceof Directory) {
              collectFilesFromDir(libDir, '/lib', generatedFiles, textDecoder);
            }
            const kokaDir = this.rootDir.contents.get('.koka');
            if (kokaDir instanceof Directory) {
              collectFilesFromDir(kokaDir, '/.koka', generatedFiles, textDecoder);
            }
          }
          // Send response with generated files attached
          self.postMessage({
            type: 'response',
            data: body,
            generatedFiles: Array.from(generatedFiles.entries()),
          });
          continue;
        }
      } catch { /* not JSON, send as-is */ }

      self.postMessage({ type: 'response', data: body });
    }
  }

}

// ── Shared buffer for stdin communication ───────────────────────────────────

// 1MB shared buffer: [flag:i32, length:i32, data:uint8...]
const SHARED_BUFFER_SIZE = 1024 * 1024;
const sharedBuffer = new SharedArrayBuffer(SHARED_BUFFER_SIZE);
const flagView = new Int32Array(sharedBuffer, 0, 2);

// ── Worker message handler ──────────────────────────────────────────────────

let wasmModule: WebAssembly.Module | null = null;

self.onmessage = async (e: MessageEvent) => {
  const msg = e.data;

  if (msg.type === 'init') {
    try {
      // Load and compile WASM module
      const response = await fetch(msg.wasmUrl);
      const bytes = await response.arrayBuffer();
      wasmModule = await WebAssembly.compile(bytes);

      // Build a single unified WASI filesystem from all VFS files.
      // Using one root mount avoids cross-mount copy issues.
      const allFiles = new Map<string, string>(msg.files);
      const rootDir = buildUnifiedTree(allFiles);

      // Create blocking stdin and capturing stdout
      const stdinFile = new BlockingStdinFile(sharedBuffer);
      const stdoutCapture = new LspStdoutCapture();
      // Wire up reference to root for collecting generated files after compile
      stdoutCapture.rootDir = rootDir;
      stdinFile.stdoutCapture = stdoutCapture;

      const wasi = new WASI(
        ['koka-lsp', '--language-server', '--lsstdio',
         ...msg.compilerFlags,
         `-v${msg.verbose ?? 1}`],
        [],
        [
          stdinFile as unknown as OpenFile,           // fd 0: stdin
          stdoutCapture as unknown as OpenFile,        // fd 1: stdout
          ConsoleStdout.lineBuffered((line: string) => {
            self.postMessage({ type: 'log', text: line });
          }),                                          // fd 2: stderr
          new PreopenDirectory('/', rootDir.contents as Map<string, File | Directory>),
        ],
        { debug: false },
      );

      // Send the shared buffer to the main thread so it can write to stdin
      self.postMessage({ type: 'ready', sharedBuffer });

      // Start the LSP server (this blocks forever, reading from stdin)
      const instance = new WebAssembly.Instance(wasmModule, {
        wasi_snapshot_preview1: wasi.wasiImport,
      });

      try {
        const result = wasi.start(instance as unknown as { exports: { memory: WebAssembly.Memory; _start: () => void } });
        self.postMessage({ type: 'log', text: `LSP server exited with code ${result}` });
      } catch (err) {
        const msg = err instanceof Error ? err.message : String(err);
        self.postMessage({ type: 'log', text: `LSP server exception: ${msg}` });
        if (!(err instanceof Error && msg.includes('exit'))) {
          self.postMessage({ type: 'error', message: msg });
        }
      }
    } catch (err) {
      self.postMessage({ type: 'error', message: String(err) });
    }
    return;
  }

  // 'request' messages are handled by the main thread writing directly
  // to the SharedArrayBuffer — they don't come through onmessage.
};
