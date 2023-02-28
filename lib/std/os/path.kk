/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* File system paths.

The path functions are system independent: backward slashes (`'\\'`)
are treated as a forward slash to separate directories. Windows style
root names like ``c:\`` or ``//server`` are also recognised.

A list of paths can be separated with either a colon (``:``) or
semi-colon (``;``).

A `:path` is created using the `path` functions. Use `string` to convert
back to a normalized path string. A path consists of a _root_ name
(``/``, ``c:\``), the directory (``foo/bar``) and finally the _base_ name.
The base name itself consists of the _stem_ and the _extension_. The
extension is always the part that follows the last occurence of a dot (`'.'`)
in the base name.

A `:path` is always normalized. For a sequence of directories, any
empty directory or ``.`` directory is ignored.
A directory followed by ``..`` is also ignored -- this is the [Plan 9](https://9p.io/sys/doc/lexnames.html)
interpretation of paths where ``..`` is considered lexically.
If parent directories should be resolved through symbolic links,
the `realpath` function should be used (which has the `:io` effect though).
*/
module std/os/path

import std/text/parse

extern import
  cs file "path-inline.cs"
  js file "path-inline.js"

// A `:path` represents a file system path.\
abstract value struct path
  root : string = ""
  parts: list<string> = [] // directory parts in reverse order


// Return the base name of a path (stem name + extension)\
// `"/foo/bar.txt".path.basename === "bar.txt"` \
// `"/foo".path.basename === "foo"`
pub fun basename( p : path ) : string
  p.parts.head("")

// Return the directory part of a path (including the rootname)
// `"/foo/bar.txt".path.dirname === "/foo"` \
// `"/foo".path.dirname === "/"`
pub fun dirname( p : path ) : string
  p.root ++ p.parts.tail.reverse.join("/")

// Return the extension of path (without the preceding dot (`'.'`))\
// `"/foo/bar.svg.txt".path.extname === "txt"`
pub fun extname( p : path ) : string
  p.basename.split-base.snd

// Return the stem name of path.\
// `"/foo/bar.svg.txt".path.extname === "foo.svg"`
pub fun stemname( p : path ) : string
  p.basename.split-base.fst

// Return the root name of path.
// `"c:\\foo".path.rootname === "c:/"`\
// `"/foo".path.rootname === "/"`
pub fun rootname( p : path ) : string
  p.root

fun split-base( basename : string ) : (string,string)
  match(basename.find-last("."))
    Just(slice) -> (slice.before.string,slice.after.string)
    Nothing     -> (basename,"")


fun split-parts( parts : list<string> ) : (string,list<string>)
  (parts.head(""), parts.tail)

// Convert a `:path` to a normalized `:string` path.\
// If this results in an empty string, the current directory path `"."` is returned.
// `"c:/foo/test.txt".path.string -> "c:/foo/test.txt"`\
// `"c:\\foo\\test.txt".path.string -> "c:/foo/test.txt"`\
// `"/foo//./bar/../test.txt".path.string -> "/foo/test.txt"`
pub fun string( p : path ) : string
  val s = p.root ++ p.parts.reverse.join("/")
  if s.is-empty then "." else s

// Show a path as a string.
pub fun show( p : path ) : string
  p.string.show


// Is a path empty?
pub fun is-empty( p : path ) : bool
  p.root.is-empty && p.parts.is-nil

// Is a path relative?
pub fun is-relative( p : path ) : bool
  p.root.is-empty

// Is a path absolute?
pub fun is-absolute( p : path ) : bool
  !p.is-relative

// Create a normalized `:path` from a path string.
pub fun path( s : string ) : path
  if s.is-empty return Path("",[])
  val t = s.replace-all("\\","/")
  match t.starts-with(proot)
    Nothing          -> path-parts("",t)
    Just((eof,rest)) -> path-parts(rest.before.string ++ (if eof then "/" else ""),rest.string)

fun path-parts( root : string, s : string, dirs : list<string> = [] ) : path
  val parts = s.split("/").push-parts(dirs)
  Path(root,parts)

fun push-parts( parts : list<string>, dirs : list<string> ) : list<string>
  match parts
    Cons(part,rest) -> push-parts(rest, push-part(part,dirs))
    Nil -> dirs

fun push-part( dir : string, dirs : list<string> ) : list<string>
  if dir=="." || dir==""       then dirs
  elif dir==".." && dirs.is-cons then dirs.tail
  else Cons(dir,dirs)

fun proot() : parse bool
  ({ alpha(); char(':'); () }) || ({char('/'); many1({none-of("/")}); () })
  ({ char('/'); False }) || ({ eof(); True })

// Parse a list of paths seperated by colon (`':'`) or semi-colon (`';'`)
//
// Colon separated paths can be ambiguous with Windows style root names (`c:\\`)
// In particular, a single letter path followed by an absolute path, e.g. ``c:/foo:/bar`` is
// parsed as ``c:/foo`` and ``/bar``.
pub fun paths( s : string ) : list<path>
  s.replace-all(";",":").split(":").paths-collect

fun paths-collect( ps : list<string> ) : list<path>
  match ps
    Cons(root,Cons(part,rest)) // match on windows 'c:\' parts
      | root.count==1 && root.head-char.default(' ').is-alpha &&
        !part.is-empty && "/\\".contains(part.head)
      -> Cons( path(root ++ ":" ++ part), paths-collect(rest))
    Cons(part,rest) -> Cons( path(part), paths-collect(rest))
    Nil             -> Nil


// Add two paths together using left-associative operator `(/)`. \
// Keeps the root of `p1` and discards the root name of `p2`.\
// `"/a/" / "b/foo.txt"          === "/a/b/foo.txt"`\
// `"/a/foo.txt" / "/b/bar.txt"  === "/a/foo.txt/b/bar.txt"`\
// `"c:/foo" / "d:/bar"          === "c:/foo/bar"`
pub fun (/)(p1 : path, p2: path) : path
  Path(p1.root,push-parts(p2.parts.reverse, p1.parts))

// Convenience function that adds a string path.
pub fun (/)(p1 : path, p2: string) : path
  p1 / p2.path

// Convenience function that adds two strings into a path.
pub fun (/)(p1 : string, p2: string) : path
  p1.path / p2.path

// Return the first path if it is not empty, otherwise return the second one.
pub fun (||)(p1 : path, p2: path) : path
  if p1.is-empty then p2 else p1

// Combine multiple paths using `(/)`.
pub fun combine( ps : list<path> ) : path
  match ps
    Nil -> Path()
    Cons(p,pp) -> pp.foldl(p,(/))


// Remove the directory and root and only keep the base name (file name) portion of the path.\
// `nodir("foo/bar.ext".path) === "bar.ext"`
pub fun nodir( p : path ) : path
  p(root="",parts=p.parts.take(1))

// Remove the basename and only keep the root and directory name portion of the path.\
// `nobase("foo/bar.ext".path) == "foo")`
pub fun nobase( p : path ) : path
  p( parts = p.parts.tail)

// Remove the extension from a path.
pub fun noext( p : path ) : path
  p.change-ext("")

// Change the extension of a path.
// Only adds a dot if the extname does not already start with a dot.
pub fun change-ext( p : path, extname : string ) : path
  val (basename,dirs) = p.parts.split-parts
  val (stemname,_)    = basename.split-base
  val newext = if (extname.starts-with(".").bool) then extname else "." ++ extname
  path-parts(p.root, stemname ++ newext, dirs)

// If a path has no extension, set it to the provided one.
pub fun default-ext( p : path, newext : string ) : path
  if p.extname.is-empty then p.change-ext(newext) else p

// Change the stem name of a path
pub fun change-stem( p : path, stemname : string ) : path
  val ext = p.extname
  p.change-base( stemname ++ (if ext.is-empty then "" else "." ++ ext) )

// Change the base name of a path
pub fun change-base( p : path, basename : string ) : path
  val q = p.nobase
  path-parts(q.root, basename, q.parts)


// Return a list of all directory components (excluding the root but including the basename).\
// `"/foo/bar/test.txt".path.dirparts === ["foo","bar","test.txt"]`
pub fun dirparts(p : path) : list<string>
  p.parts.reverse

// Return the last directory component name (or the empty string).\
// `"c:/foo/bar/tst.txt".path.parentname === "bar"
pub fun parentname( p : path ) : string
  p.parts.tail.head("")

// Convert a path to the absolute path on the file system.
// The path is not required to exist on disk. However, if it
// exists any permissions and symbolic links are resolved fully.\
// `".".realpath` (to get the current working directory)\
// `"/foo".realpath` (to resolve the full root, like `"c:/foo"` on windows)
pub fun realpath( p : path ) : io path
  realpath(p.string)

// Returns the current working directory.\
// Equal to `".".realpath`.
pub fun cwd()
  ".".realpath

// Convert a path to the absolute path on the file system.\
// The overload on a plain string is necessary as it allows
// for unnormalized paths with `".."` parts. For example
// `"/foo/symlink/../test.txt"` may resolve to `"/bar/test.txt"` if
// ``symlink`` is a symbolic link to a sub directory of `"/bar"`.
pub fun realpath( s : string ) : io path
  xrealpath(s).path

extern xrealpath( p : string ) : io string
  c  "kk_os_realpath"
  cs "System.IO.Path.GetFullPath"
  js "_get_realpath"


// Return the OS specific directory separator (`"/"` or `"\\"`)
pub extern partsep() : ndet string
  c  "kk_os_dir_sep"
  cs "System.IO.Path.DirectorySeparatorChar.ToString"
  js "_get_partsep"

// Return the OS specific path separator (`';'` or `':'`)
pub extern pathsep() : ndet string
  c  "kk_os_path_sep"
  cs "System.IO.Path.PathSeparator.ToString"
  js "_get_pathsep"

// Return the path to the currently executing application.
pub fun app-path() : io path
  xapp-path().path

extern xapp-path() : io string
  c "kk_os_app_path"
  cs inline "System.Reflection.Assembly.GetEntryAssembly().Location"
  js "_get_apppath"

// Return the base directory that contains the currently running application.
// First tries `app-path().nobase`; if that ends in the ``bin`` or ``exe`` directory it
// returns the parent of that directory.
pub fun appdir() : io path
  val p = app-path().nobase
  if p.basename=="bin" || p.basename=="exe" then p.nobase else p


// Return the home directory of the current user.
pub fun homedir() : io path
  xhomedir().path

extern xhomedir() : io string
  c  "kk_os_home_dir"
  cs "_Path.GetHomeDir"
  js "_get_homedir"

// Return the temporary directory for the current user.
pub fun tempdir() : io path
  xtempdir().path

extern xtempdir() : io string
  c  "kk_os_temp_dir"
  cs "System.IO.Path.GetTempPath"
  js "_get_tempdir"

