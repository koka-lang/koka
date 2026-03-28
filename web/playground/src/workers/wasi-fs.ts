/**
 * wasi-fs-utils.ts
 *
 * Shared WASI filesystem utilities used by both lsp-worker and wasm-worker
 * to build directory trees and collect output files.
 */

import { File, Directory } from '@bjorn3/browser_wasi_shim';

/**
 * Build a WASI Directory tree from a flat map of relative paths to content.
 */
function buildDirectoryTree(files: Map<string, string>): Directory {
  const root = new Map<string, File | Directory>();
  const encoder = new TextEncoder();

  for (const [path, content] of files) {
    const parts = path.split('/').filter(Boolean);
    let current = root;

    for (let i = 0; i < parts.length - 1; i++) {
      const part = parts[i];
      if (!current.has(part)) {
        current.set(part, new Directory(new Map()));
      }
      const dir = current.get(part);
      if (dir instanceof Directory) {
        current = dir.contents as Map<string, File | Directory>;
      }
    }

    const filename = parts[parts.length - 1];
    if (filename) {
      current.set(filename, new File(encoder.encode(content)));
    }
  }

  return new Directory(root);
}

/**
 * Recursively collect all files from a WASI Directory into a flat map.
 */
export function collectFilesFromDir(
  dir: Directory, prefix: string,
  out: Map<string, string>, decoder: TextDecoder,
): void {
  for (const [name, entry] of dir.contents) {
    const path = prefix ? prefix + '/' + name : name;
    if (entry instanceof File) {
      out.set(path, decoder.decode(entry.data));
    } else if (entry instanceof Directory) {
      collectFilesFromDir(entry, path, out, decoder);
    }
  }
}

/**
 * Build a unified WASI directory tree from all VFS files.
 * All files are placed under a single root, preserving their full paths.
 * This avoids cross-mount issues that occur with multiple PreopenDirectory mounts.
 */
export function buildUnifiedTree(allFiles: Map<string, string>): Directory {
  // Strip leading '/' from VFS paths for buildDirectoryTree
  const normalized = new Map<string, string>();
  for (const [path, content] of allFiles) {
    normalized.set(path.startsWith('/') ? path.slice(1) : path, content);
  }
  return buildDirectoryTree(normalized);
}
