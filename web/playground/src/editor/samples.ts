/**
 * samples.ts
 *
 * Sample program content, excluded samples list, file tree types/utilities,
 * and sample loading from the deployed samples/ directory.
 */

// ── File tree types and utilities ────────────────────────────────────────────

export interface FileEntry {
  name: string;
  path: string;
  type: 'file' | 'directory';
  children?: FileEntry[];
  /** URL to fetch file content from */
  download_url?: string;
  /** File content if available (e.g. from VFS) */
  content?: string;
}

/** Build a tree structure from flat VFS paths */
export function buildFileTree(files: Map<string, string>, filter?: (path: string) => boolean): FileEntry[] {
  const root: FileEntry = { name: '', path: '', type: 'directory', children: [] };

  for (const [path, content] of files) {
    if (filter && !filter(path)) continue;

    const parts = path.split('/').filter(Boolean);
    let current = root;

    for (let i = 0; i < parts.length; i++) {
      const part = parts[i];
      const isLast = i === parts.length - 1;

      if (isLast) {
        current.children!.push({
          name: part,
          path: path,
          type: 'file',
          content: content,
        } as FileEntry & { content: string });
      } else {
        let dir = current.children!.find(
          (c) => c.type === 'directory' && c.name === part,
        );
        if (!dir) {
          dir = { name: part, path: parts.slice(0, i + 1).join('/'), type: 'directory', children: [] };
          current.children!.push(dir);
        }
        current = dir;
      }
    }
  }

  sortFileEntries(root.children!);
  return root.children!;
}

/** Sort file entries: directories first, then alphabetical. Recurses into children. */
export function sortFileEntries(entries: FileEntry[]): void {
  entries.sort((a, b) => {
    if (a.type !== b.type) return a.type === 'directory' ? -1 : 1;
    return a.name.localeCompare(b.name);
  });
  for (const e of entries) {
    if (e.children) sortFileEntries(e.children);
  }
}

// ── Sample content ───────────────────────────────────────────────────────────

export const EXCLUDED_SAMPLES = new Set([
  'samples/all.kk',
  'samples/basic/rbtree.kk',
  'samples/basic/rbtree-fbip.kk',
  'samples/learn/lazycons.kk',
  'samples/learn/implicits-talk.kk',
]);

export const DEFAULT_SOURCE = `module main

fun main()
  println("Hello, Koka!")
`;

export const ALL_WEB_SAMPLE = `// Run all web-compatible sample programs
module all

import basic/caesar
import basic/fibonacci
import basic/garsia-wachs

import learn/basic
import learn/handler
import learn/with
import learn/qualifiers
import learn/implicits
import learn/contexts
import learn/fip

import handlers/ambient
import handlers/basic
import handlers/nim
import handlers/vec
import handlers/yield
import handlers/parser
import handlers/scoped
import handlers/unix

import handlers/named/ask
import handlers/named/ask-poly
import handlers/named/file
import handlers/named/file-scoped
import handlers/named/heap
import handlers/named/unify

fun run( name : string, action : () -> <console|e> a ) : <console|e> ()
  println("run " ++ name ++ "\\n--------------------------")
  action()
  println("")

pub fun main()
  // basic
  run("caesar",caesar/main)
  run("fibonacci",fibonacci/main)
  run("garsia-wachs",garsia-wachs/main)

  // learn
  run("basic",learn/basic/main)
  run("contexts",contexts/main)
  run("handler",learn/handler/main)
  run("fip",fip/main)
  run("implicits",implicits/main)
  run("qualifiers",qualifiers/main)
  run("with",learn/with/main)

  // named handlers
  run("ask-poly",handlers/named/ask-poly/main)
  run("heap",handlers/named/heap/main)
  run("unify",handlers/named/unify/main)
  run("ask",handlers/named/ask/main)

  // handlers
  run("ambient",ambient/main)
  run("nim",nim/main)
  run("parser",parser/main)
  run("scoped",scoped/main)
  run("vec",vec/main)
  run("yield",handlers/yield/main)
  run("unix",unix/main)
`;

// ── Sample loading ───────────────────────────────────────────────────────────

/**
 * Load the sample file listing from the local samples-manifest.json.
 * Returns a tree of FileEntry objects for the file browser.
 */
export async function loadKokaSamples(): Promise<FileEntry[]> {
  const resp = await fetch('samples-manifest.json');
  if (!resp.ok) throw new Error(`Failed to fetch samples manifest: ${resp.status}`);
  const files: string[] = await resp.json();

  // Build a tree from flat paths like "basic/caesar.kk"
  const root: FileEntry = { name: '', path: '', type: 'directory', children: [] };

  for (const filePath of files) {
    const parts = filePath.split('/');
    let current = root;

    for (let i = 0; i < parts.length; i++) {
      const part = parts[i];
      const isLast = i === parts.length - 1;

      if (isLast) {
        current.children!.push({
          name: part,
          path: 'samples/' + filePath,
          type: 'file',
          download_url: 'samples/' + filePath,
        });
      } else {
        let dir = current.children!.find(c => c.type === 'directory' && c.name === part);
        if (!dir) {
          dir = {
            name: part,
            path: 'samples/' + parts.slice(0, i + 1).join('/'),
            type: 'directory',
            children: [],
          };
          current.children!.push(dir);
        }
        current = dir;
      }
    }
  }

  sortFileEntries(root.children!);
  return root.children!;
}
