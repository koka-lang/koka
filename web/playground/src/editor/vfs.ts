/**
 * vfs.ts
 *
 * In-memory Virtual File System for the Koka playground.
 * Stores stdlib sources, precompiled .kki/.mjs files, and compiler output.
 * The WASM workers build their WASI filesystem from VFS snapshots.
 */

export interface VFSEntry {
  content: string;
  /** Unix epoch milliseconds */
  time: number;
}

export class KokaVFS {
  private files: Map<string, VFSEntry> = new Map();
  /** Set of directory paths that have been explicitly created */
  private dirs: Set<string> = new Set();
  /**
   * Set of VFS paths that came from precompiled files.
   * Used to distinguish precompiled .mjs from freshly generated ones.
   */
  private precompiledPaths: Set<string> = new Set();

  /**
   * Precompiled .mjs content keyed by filename (e.g. "std_core.mjs").
   * Used by the module runner to supply runtime modules.
   */
  public precompiledMjs: Map<string, string> = new Map();


  constructor() {
    // Always ensure the root exists
    this.dirs.add('/');
  }

  // ── Public helpers ────────────────────────────────────────────────────────

  /** Add / overwrite a file in the VFS. */
  addFile(path: string, content: string, time?: number): void {
    const key = this.normalize(path);
    this.files.set(key, { content, time: time ?? Date.now() });
    // Ensure all parent directories exist
    this.ensureParents(key);
  }

  /** Explicitly remove a file. */
  removeFile(path: string): void {
    this.files.delete(this.normalize(path));
  }

  /** Return all files in the VFS. */
  getAllFiles(): Map<string, string> {
    const out = new Map<string, string>();
    for (const [k, v] of this.files) {
      out.set(k, v.content);
    }
    return out;
  }

  /**
   * Return only newly generated .mjs files — those written by the compiler
   * during the current compile run, not preloaded precompiled files.
   */
  getGeneratedMjs(): Map<string, string> {
    const out = new Map<string, string>();
    for (const [k, v] of this.files) {
      if (k.endsWith('.mjs') && !this.precompiledPaths.has(k)) {
        out.set(k, v.content);
      }
    }
    return out;
  }

  // ── Preloading ────────────────────────────────────────────────────────────

  /**
   * Fetch a JSON manifest (array of relative paths) and load all listed files
   * into the VFS at `/share/lib/<path>`.
   *
   * @param baseUrl       URL prefix for fetching files (e.g. '/lib' or '')
   * @param manifestPath  URL/path to the JSON manifest file
   */
  async preloadSources(baseUrl: string, manifestPath: string): Promise<number> {
    const resp = await fetch(manifestPath);
    if (!resp.ok) throw new Error(`Failed to fetch manifest ${manifestPath}: ${resp.status}`);
    const manifest: string[] = await resp.json();

    const prefix = baseUrl ? baseUrl.replace(/\/$/, '') + '/lib/' : 'lib/';
    await Promise.all(manifest.map(async (f) => {
      try {
        const r = await fetch(prefix + f);
        if (r.ok) {
          const text = await r.text();
          this.addFile('/share/lib/' + f, text);
        }
      } catch {
        // ignore individual file failures
      }
    }));

    return manifest.length;
  }

  /**
   * Fetch a precompiled manifest and load:
   *   - `.kki` files into `/lib/js-debug/<filename>` with a far-future timestamp
   *     so the compiler treats them as fresh cache entries.
   *   - `.mjs` files into `precompiledMjs` for use by the module runner,
   *     and also into the VFS so the compiler can find them.
   *
   * @param baseUrl       URL prefix for fetching files (e.g. '' or '/precompiled')
   * @param manifestPath  URL/path to the JSON manifest file
   */
  async preloadPrecompiled(baseUrl: string, manifestPath: string): Promise<number> {
    const resp = await fetch(manifestPath);
    if (!resp.ok) throw new Error(`Failed to fetch manifest ${manifestPath}: ${resp.status}`);
    const manifest: string[] = await resp.json();

    // Far-future timestamp so the compiler considers these files fresh cache
    const kkiTime = Date.now() + 365 * 24 * 60 * 60 * 1000; // 1 year ahead

    const prefix = baseUrl ? baseUrl.replace(/\/$/, '') + '/precompiled/' : 'precompiled/';
    await Promise.all(manifest.map(async (f) => {
      try {
        const r = await fetch(prefix + f);
        if (!r.ok) return;
        const text = await r.text();

        if (f.endsWith('.kki')) {
          const vfsPath = '/lib/js-debug/' + f;
          this.files.set(this.normalize(vfsPath), { content: text, time: kkiTime });
          this.ensureParents(this.normalize(vfsPath));
          this.precompiledPaths.add(this.normalize(vfsPath));
        }

        if (f.endsWith('.mjs')) {
          // Store for module runner
          this.precompiledMjs.set(f, text);
          // Also place in VFS so compiler can reference it if needed
          const vfsPath = '/lib/js-debug/' + f;
          this.files.set(this.normalize(vfsPath), { content: text, time: kkiTime });
          this.ensureParents(this.normalize(vfsPath));
          this.precompiledPaths.add(this.normalize(vfsPath));
        }
      } catch {
        // ignore individual file failures
      }
    }));

    return manifest.length;
  }

  // ── VFS operations ────────────────────────────────────────────────────────

  readFile(path: string): string | null {
    const key = this.normalize(path);
    const entry = this.files.get(key);
    if (entry !== undefined) return entry.content;
    return null;
  }

  fileExists(path: string): boolean {
    return this.files.has(this.normalize(path));
  }

  /** Returns the modification time in milliseconds, or 0 if not found. */
  fileTime(path: string): number {
    return this.files.get(this.normalize(path))?.time ?? 0;
  }

  writeFile(path: string, content: string): void {
    this.addFile(path, content);
  }

  /** List direct children (files and directories) of a directory. */
  listDir(path: string): string[] {
    const dir = this.normalizeDir(path);
    const children = new Set<string>();

    for (const key of this.files.keys()) {
      if (key.startsWith(dir)) {
        const rest = key.slice(dir.length);
        const slash = rest.indexOf('/');
        children.add(slash === -1 ? rest : rest.slice(0, slash));
      }
    }
    for (const d of this.dirs) {
      if (d !== dir && d.startsWith(dir)) {
        const rest = d.slice(dir.length);
        const slash = rest.indexOf('/');
        if (slash === -1) children.add(rest);
        else children.add(rest.slice(0, slash));
      }
    }
    return [...children].sort();
  }

  createDir(path: string): void {
    this.dirs.add(this.normalizeDir(path));
    this.ensureParents(this.normalizeDir(path));
  }

  dirExists(path: string): boolean {
    const dir = this.normalizeDir(path);
    if (this.dirs.has(dir)) return true;
    // A directory implicitly exists if any file lives under it
    for (const key of this.files.keys()) {
      if (key.startsWith(dir)) return true;
    }
    return false;
  }

  fileSize(path: string): number {
    const entry = this.files.get(this.normalize(path));
    if (entry === undefined) return 0;
    // UTF-16 length is close enough for the compiler's purposes
    return entry.content.length;
  }

  // ── Internal helpers ──────────────────────────────────────────────────────

  /** Normalise a file path: ensure leading slash, collapse . / .., unify separators. */
  private normalize(path: string): string {
    // Replace back-slashes with forward slashes
    const p = path.replace(/\\/g, '/');
    // Split and resolve . and ..
    const parts = p.split('/');
    const resolved: string[] = [];
    for (const part of parts) {
      if (part === '' || part === '.') continue;
      if (part === '..') {
        resolved.pop();
      } else {
        resolved.push(part);
      }
    }
    return '/' + resolved.join('/');
  }

  /** Like normalize but always ends with a trailing slash. */
  private normalizeDir(path: string): string {
    const n = this.normalize(path);
    return n.endsWith('/') ? n : n + '/';
  }

  private ensureParents(normalizedPath: string): void {
    const parts = normalizedPath.split('/').slice(1); // remove leading ''
    let current = '';
    // Walk every parent segment (not the file itself)
    for (let i = 0; i < parts.length - 1; i++) {
      current += '/' + parts[i];
      this.dirs.add(current + '/');
    }
  }
}
