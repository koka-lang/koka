//-----------------------------------------------------------------------------
// Copyright 2024, Microsoft Research, Daan Leijen. Tim Whiting.
//
// This is free software; you can redistribute it and/or modify it under the
// terms of the Apache License, Version 2.0. A copy of the License can be
// found in the LICENSE file at the root of this distribution.
//---------------------------------------------------------------------------
// Updated as of 7/23/24, though parts are incomplete (Commit: 336bf9b)

module compiler/common/file

import std/os/path
import std/os/file
import compiler/platform/config
import std/os/env
import std/core/unsafe
import std/os/process

pub fun is-literal-doc(fname: path)
  fname.string.ends-with(source-ext ++ ".md").bool ||
  fname.string.ends-with(source-ext ++ ".mdk").bool

// A file name without any of its extensions (`dir/foo.kk.md` is `dir/foo`), as upstream `noexts`
pub fun noexts(fname: string): string
  match fname.split("/").reverse
    Cons(base, dirs) -> Cons(base.list.take-while(fn(c) c != '.').string, dirs).reverse.join("/")
    Nil -> fname

// The source files module `stem` may be in, in search order: `.kk`, then a literate `.kk.md`
pub fun source-files(dir: path, stem: string): list<path>
  [dir / (stem ++ source-ext).path, dir / (stem ++ source-ext ++ ".md").path]

// -------------------------
// File names
// -------------------------
pub alias file-name = path

// Remove the extension and directory part.
// Private: a public `basename` would be ambiguous with `std/os/path`'s at every call on a `path`.
fun basename(p: file-name): file-name
  p.stemname.path

// extname - Get the file extension (already part of std/os/path)

// Append `ext` unless the path already ends in it: `ensure-ext("a.c", ".kk")` is `a.c.kk`.
pub fun ensure-ext(p: file-name, ext: string): file-name
  val e = p.extname
  if (if e.is-empty then "" else "." ++ e) == ext then p
  else (p.string ++ ext).path

// dirname - Return the directory prefix (including last separator if present) (already part of std/os/path)

// notdir -- Remove the directory prefix (already part of std/os/path as nodir)

// notext -- Remove the extension (already part of std/os/path as noext)

// undelim-paths - Split a string of paths into a list of file names (already part of std/os/path as paths)

// split-path - Split a path into its directory parts (already part of std/os/path as dirparts)

fun on-windows(): bool
  with unsafe-total
  get-os-name() == "windows"

// join-path / join-paths - Join a list of file names into a single path (already part of std/os/path as (/))

// normalize - Normalize path separators (.path on string already does this)

// Replace every path separator in `s` with `sep`, keeping a backslash-escaped space (a unix shell escape).
// Works on any string, such as a whole command line, without parsing it as a path.
pub fun string/normalize-with(s: string, sep: char): string
  fun norm(cs: list<char>, acc: list<char>): list<char>
    match cs
      Nil -> acc.reverse
      Cons('\\', Cons(c, rest)) | c.is-white -> norm(rest, Cons(c, Cons('\\', acc)))
      Cons(c, rest) -> norm(rest, Cons(if c.config/is-path-sep then sep else c, acc))
  norm(s.list, []).string

pub fun path/normalize-with(p: file-name, sep: char): string
  p.string.normalize-with(sep)

// Is this a file separator? Both are, regardless of host OS.
pub fun is-path-sep(c: char): bool
  c == '/' || c == '\\'

// Is this a delimiter in a list of paths? `;` always, and `:` except on Windows, where it follows a drive letter.
pub fun is-path-delimiter(c: char): bool
  c == ';' || (c == ':' && !on-windows())

// `run-system-raw` and `run-system` are private: public, they would be ambiguous with `std/os/process`'s `run-system`.
fun run-system-raw(cmd: string): _ ()
  val res = process/run-system(cmd)
  if res == 0 then ()
  else
    throw("raw command failed:\n " ++ cmd)

// Run a shell command with its path separators made native.
fun run-system(cmd: string): _ ()
  val ncmd = cmd.normalize-with(partsep().head-char.default('/'))
  val res = process/run-system(ncmd)
  if res == 0 then ()
  else
    throw("command failed:\n " ++ ncmd)

// Quote one argument for the shell, so an argument with spaces or quotes reaches the program unchanged.
// Upstream's `runCmd` passes its arguments directly (`rawSystem`); `std/os/process` only runs a command line through the shell.
fun shell-arg(arg: string): ndet string
  if on-windows() then "\"" ++ arg.replace-all("\"", "\\\"") ++ "\""
  else "'" ++ arg.replace-all("'", "'\\''") ++ "'"

fun command-line(cmd: string, args: list<string>): ndet string
  Cons(cmd, args).map(shell-arg).join(" ")

pub fun run-cmd(cmd: string, args: list<string>): _ ()
  val res = process/run-system(command-line(cmd, args))
  if res == 0 then ()
  else
    throw("command failed (exit code " ++ res.show ++ ")")

// TODO: `env` is ignored and only stdout is returned: `std/os/process` cannot set a child's environment or capture its stderr.
pub fun run-cmd-read(env: env, cmd: string, args: list<string>): _ string
  match process/run-system-read(command-line(cmd, args))
    Ok(s) -> s
    Error(s) -> throw("command failed (exit code " ++ s.show ++ ")")

// TODO: `env` is ignored: `std/os/process` cannot set a child's environment.
pub fun run-cmd-env(env: env, cmd: string, args: list<string>): _ ()
  run-cmd(cmd, args)

fun build-env(env: env): _ maybe<env>
  if env.is-empty then return Nothing
  val old = get-env()
  Just(env ++ old.filter(fn((k,_)) env.lookup(_ == k).is-nothing))

// File modification time, in nanoseconds since the epoch.
//
// A syscall, because a process spawn MEASURED 8ms and the rebuild walk asks
// per module AND per dependency. Cheap mtimes are also what makes it viable
// to check interface freshness before deciding whether a source needs
// scanning at all.
//
// A missing or unreadable file reports 0, which makes a missing artifact
// infinitely stale without needing a special case at the call sites.
extern import
  c file "file-inline.c"

extern file-mtime-prim( p : string ) : ndet int
  c "kk_compiler_file_mtime"

pub fun file-mtime( p : path ) : io int
  file-mtime-prim(p.string)

// Read at most `max-bytes` from the head of a file. For scanning a header out
// of a file whose tail is irrelevant -- an interface's import block is the
// first thing in it, and interfaces run to megabytes.
extern read-head-prim( p : string, max-bytes : int ) : ioc string
  c "kk_compiler_read_head"

pub fun read-head( p : path, max-bytes : int = 65536 ) : io string
  read-head-prim(p.string, max-bytes)

// Monotonic-ish CPU millisecond clock for coarse timing (KOKA_TIMING).
// Named `now-ms` to avoid clashing with driver.kk's private `cpu-ms`.
extern import
  c include-inline "#include <time.h>"

pub extern now-ms(): ndet int
  c inline "kk_integer_from_int((kk_intx_t)(clock()/(CLOCKS_PER_SEC/1000)), kk_context())"

// Wall-clock epoch NANOSECONDS -- the same unit and origin as `file-mtime`, so
// the two are directly comparable. An in-memory source (a language server's
// edited buffer) needs a timestamp on this scale or staleness comparisons
// against on-disk interfaces are meaningless.
pub extern epoch-nanos(): ndet int
  c "kk_compiler_epoch_nanos"

// Atomic text-file write: write to a unique temp file in the target
// directory, then rename over the destination. A rename within one file
// system is atomic, so concurrent koka processes sharing a build cache never
// observe a half-written interface or C file (8 sweep workers do exactly
// this). It runs three times per compiled module (.kki, .c, .h), inside the
// codegen phase, so it is a syscall pair rather than shell commands.
extern write-atomic-prim( p : string, content : string ) : ioc bool
  c "kk_compiler_write_atomic"

pub fun write-text-file-atomic( p : path, content : string ) : io ()
  // false on any failure (or on WASI, which has no temp files); a direct
  // write is still correct for a single writer
  if write-atomic-prim(p.string, content) then ()
  else write-text-file(p, content)

// Not ported yet:
// fun file-time-compare(p1: file-name, p2: file-name): io order
// fun max-file-time
// fun max-file-times
// fun does-file-exist-and-not-empty

// read-text-file (already part of std/os/file)
// write-text-file (already part of std/os/file)

pub fun copy-text-file(src: path, dest: path): _ ()
  file/write-text-file(dest, file/read-text-file(src))

pub fun copy-text-file-with(src: path, dest: path, transform: (string) -> _ string): _ ()
  file/write-text-file(dest, transform(file/read-text-file(src)))

// Not ported yet (`std/os/file` has no binary file I/O):
// copy-binary-file
// copy-binary-if-newer

// Not ported yet:
// fun copy-text-if-newer
// fun copy-text-if-newer-with
// fun remove-file-if-exists

// get-program-path (already in std/os/path as app-path())

// Path parts are joined with `/`, which every OS accepts, as upstream's `joinPaths` does.
pub fun common-path-prefix(p1: path, p2: path): string
  p1.dirparts.zip(p2.dirparts).take-while(fn((a,b)) a == b).map(fst).join("/")

pub fun default/relative-to-path(pprefix: path, p: path): path
  match maybe/relative-to-path(pprefix, p)
    Just(r) -> r
    Nothing -> p

pub fun maybe/relative-to-path(pprefix: path, p: path): maybe<path>
  if pprefix.string == "" then Just(p)
  else
    val prefs = pprefix.dirparts
    val ps = p.dirparts
    if prefs.is-prefix-of(ps) then
      Just(ps.drop(prefs.length).join("/").path)
    else
      Nothing

// is-absolute (already part of std/os/path)

// Find a maximal prefix path given a path and a list of root paths. Returns the root path and relative path.
pub fun find-maximal-prefix-path(p: path, roots: list<path>): maybe<(path, path)>
  val rels = roots.map(fn(r)
        match r.maybe/relative-to-path(p)
          Just(rel) -> [(r, rel)]
          Nothing -> []
      ).concat
  match rels
    Nil -> Nothing
    Cons(x, xx) -> Just(xx.maximum(x, ?cmp = fn((r1, _), (r2, _)) r1.string.cmp(r2.string)))

// Get the maximal relative path
pub fun get-maximal-prefix-path(p: path, roots: list<path>): (path, path)
  match find-maximal-prefix-path(p, roots)
    Just(rel) -> rel
    Nothing -> ("".path,p)

//----------------------------------
// File searching
//----------------------------------
// TODO: Need file exists function
// fun search-paths
// fun search-paths-suffixes
// fun search-paths-canonical

// Make a file path relative to a set of given paths: return the (maximal) root and stem
// if it is not relative to the paths, return dirname/notdir 
pub fun make-relative-to-paths(p: path, roots: list<path>): (path, path)
  match find-maximal-prefix-path(p, roots)
    Just(rel) -> rel
    Nothing -> (p.dirname.path, p.nodir)


pub fun get-env-paths(n: string): ndet list<path>
  get-env-value(n).default("").paths

// get-env-var (already part of std/os/env as get-env-value)

// real-path (already part of std/os/path as realpath I think - may not have same behavior)

// TODO: Need file exists function
// fun search-program
