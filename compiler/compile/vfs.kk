//---------------------------------------------------------------------------
// Copyright 2026, Tim Whiting.
//
// This is free software; you can redistribute it and/or modify it under the
// terms of the Apache License, Version 2.0. A copy of the License can be
// found in the LICENSE file at the root of this distribution.
//---------------------------------------------------------------------------
// In-memory overlay over the filesystem, for sources that have no file on disk.
//
// Two consumers want this:
//   * the REPL -- a typed-in expression is compiled as a module whose source
//     only ever exists in memory
//   * a future language server -- an editor buffer with unsaved edits must be
//     compiled from the buffer, not from the stale file on disk
//
// Modelled on upstream's `VFS`/`vfsFind`/`withVFS` (Compile/Build.hs) but NOT a
// literal port; the contract below is chosen for how this port actually reads
// files. Deliberate differences:
//
//   * Upstream threads the map through its `Build` monad's `Env`. There is no
//     such monad here, so this is an ambient effect. `no-vfs` answers `Nothing`
//     for everything, so installing it changes nothing until a file is added.
//
//   * Keys are canonicalized with `realpath` on both sides. Upstream's
//     `lookupVFS` only `normalize`s the path string and relies on whoever fills
//     the overlay to key it by real path (the language server's `fromLspUri`).
//     The port reaches the same source through several spellings (as-written
//     relative import, include-root-relative, absolute), so a lookup that only
//     normalizes would miss.
//
//   * Upstream's wsl `/@virtual///` special case in `searchSourceFile` is not
//     reproduced; it is a Windows path workaround, not part of the model.
module compiler/compile/vfs

import std/os/path
import std/os/dir
import std/os/file
import compiler/common/file

// contents plus the modification time to report for them
pub struct vfile
  content: string
  mtime: int

// Takes the path AS WRITTEN, not a canonicalized key. Canonicalizing costs a
// `realpath` -- an `open`/`getattrlist`/`stat` round trip per call -- and only
// the handler that actually holds an overlay can use it, so only that handler
// pays for it. Canonicalizing in the caller would make every `is-file` and
// `file-mtime` in a batch build (where the overlay is `no-vfs`, always
// `Nothing`) pay for a key that is never used: MEASURED 74ms of `xrealpath`
// on a ~1.0s warm rebuild, all of it serial syscall time.
pub effect vfs-lookup
  fun vfs-find(p: path): maybe<vfile>

// no overlay: every read falls through to the real filesystem
pub fun no-vfs(action: () -> <vfs-lookup|e> a): e a
  with handler
    fun vfs-find(_p) Nothing
  action()

// Layer `files` over whatever overlay is already installed. Paths are given as
// written and canonicalized here, so callers need not pre-normalize.
pub fun with-vfs(files: list<(path, vfile)>, action: () -> <vfs-lookup,io|e> a): <vfs-lookup,io|e> a
  val entries = files.map(fn((p, v)) (p.vfs-key, v))
  with override
    fun vfs-find(p)
      // An EMPTY overlay cannot match, so it must not canonicalize either --
      // `isolate-diagnostics` installs `with-vfs` on every module compile and
      // passes an empty list for a batch build.
      if entries.is-nil then vfs-find(p)
      else match entries.lookup(fn(k) k == p.vfs-key)
        Just(v) -> Just(v)
        Nothing -> vfs-find(p)      // fall through to the outer overlay
  action()

// A virtual path carries this prefix so it can never collide with a real file.
pub val virtual-mount = "/@virtual"

pub fun virtual-path(rel: string): path
  (virtual-mount ++ "/" ++ rel).path

pub fun is-virtual(p: path): bool
  p.string.starts-with(virtual-mount).bool

// `realpath` resolves symlinks and `..`, but only for paths that EXIST -- a
// virtual path never does, and neither does a not-yet-written output file, so
// fall back to the path as given when it fails.
fun vfs-key(p: path): io string
  if p.is-virtual then p.string
  else match try({ p.realpath })
    Ok(r) -> r.string
    Error -> p.string

//----------------------------------------------------------------------
// Reads. Each answers from the overlay when it has the path, else from disk.
// These are drop-in replacements for `is-file` / `file-mtime` /
// `read-text-file` at the build's source-reading points.
//----------------------------------------------------------------------

pub fun vfs/is-file(p: path): <vfs-lookup,io> bool
  match vfs-find(p)
    Just -> True
    Nothing -> dir/is-file(p)

pub fun vfs/file-mtime(p: path): <vfs-lookup,io> int
  if p.string.is-empty then 0
  else match vfs-find(p)
    Just(v) -> v.mtime
    Nothing -> file/file-mtime(p)

pub fun vfs/read-text-file(p: path): <vfs-lookup,io> string
  match vfs-find(p)
    Just(v) -> v.content
    Nothing -> file/read-text-file(p)
