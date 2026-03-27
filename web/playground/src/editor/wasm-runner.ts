/**
 * wasm-runner.ts
 *
 * Runs the Koka WASM compiler in a Web Worker for non-blocking compilation
 * with real-time log streaming.
 */

export interface WasmCompilerConfig {
  /** URL to the .wasm file */
  wasmUrl: string;
  /** Compiler flags (--sharedir, --libdir, --target, etc.) */
  compilerFlags: readonly string[];
  /** Returns ALL current VFS files at call time */
  getAllFiles: () => Map<string, string>;
  /** Called for each compiler log line (real-time, from worker) */
  onLog?: (text: string) => void;
}

export interface WasmCompileResult {
  success: boolean;
  stdout: string;
  stderr: string;
  generatedFiles: Map<string, string>;
}

/**
 * Create a WASM compiler that runs in a Web Worker.
 * Returns a compile function that streams log output in real time.
 */
export async function createWasmCompiler(config: WasmCompilerConfig): Promise<
  (moduleName: string, sourceText: string, extraArgs?: string[]) => Promise<WasmCompileResult>
> {
  // Create worker from the wasm-worker module
  const worker = new Worker(
    new URL('../workers/wasm-worker.ts', import.meta.url),
    { type: 'module' },
  );

  // Initialize: load and compile the WASM module in the worker
  await new Promise<void>((resolve, reject) => {
    const handler = (e: MessageEvent) => {
      if (e.data.type === 'ready') {
        worker.removeEventListener('message', handler);
        resolve();
      } else if (e.data.type === 'error') {
        worker.removeEventListener('message', handler);
        reject(new Error(e.data.message));
      }
    };
    worker.addEventListener('message', handler);
    worker.postMessage({ type: 'init', wasmUrl: config.wasmUrl, compilerFlags: [...config.compilerFlags] });
  });

  // Return compile function
  return (moduleName: string, sourceText: string, extraArgs?: string[]): Promise<WasmCompileResult> => {
    return new Promise((resolve, reject) => {
      const handler = (e: MessageEvent) => {
        const msg = e.data;

        if (msg.type === 'log') {
          // Real-time log line from the worker
          if (config.onLog) config.onLog(msg.text);
          return;
        }

        if (msg.type === 'result') {
          worker.removeEventListener('message', handler);
          resolve({
            success: msg.success,
            stdout: msg.stdout,
            stderr: msg.stderr,
            generatedFiles: new Map(msg.generatedFiles),
          });
          return;
        }

        if (msg.type === 'error') {
          worker.removeEventListener('message', handler);
          reject(new Error(msg.message));
          return;
        }
      };

      worker.addEventListener('message', handler);

      // Send current VFS state + source to worker
      const allFiles = config.getAllFiles();
      worker.postMessage({
        type: 'compile',
        moduleName,
        sourceText,
        files: Array.from(allFiles.entries()),
        extraArgs: extraArgs ?? [],
      });
    });
  };
}
