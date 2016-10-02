/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
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
A directory followed by ``..`` is also ignored -- this is the Plan 9
interpretation of paths where ``..`` is considered lexically. 
If parent directories should be resolved through symbolic links, 
the `realpath` function should be used (which has the `:io` effect though).  
*/
module std/path

import std/regex

extern include {
  cs file "path-inline.cs"
  js file "path-inline.js"
}

// A `:path` represents a file system path.\
abstract struct path(
  root : string = "",
  dirs : list<string> = [],
  stem : string = "",
  ext  : string = ""                 
)

// Return the base name of a path (stem name + extension)
public fun basename( p : path ) : string {
  p.stem + (if (p.ext.empty?) then "" else "." + p.ext)
}

// Return the directory part of a path (including the rootname)
public fun dirname( p : path ) : string {
  p.root + p.dirs.join("/")
}

// Return the extension of path.
public fun extname( p : path ) : string {
  p.ext
}

// Return the stem name of path.
public fun stemname( p : path ) : string {
  p.stem
}

// Return the root name of path.
public fun rootname( p : path ) : string {
  p.root
}

// Convert a `:path` to a normalized `:string` path.\
// If this results in an empty string, the current directory path `"."` is returned.
public fun string( p : path ) : string {
  val s = p.root + p.dirs.join-end("/") + p.basename
  if (s.empty?) then "." else s
}

// Show a path
public fun show( p : path ) : string {
  p.string.show
}



// Is a path empty?
public fun empty?( p : path ) : bool {
  p.root.empty? && p.dirs.nil? && p.stem.empty? && p.ext.empty?
}

// Is a path relative?
public fun relative?( p : path ) : bool {
  p.root.empty?
}

// Is a path absolute?
public fun absolute?( p : path ) : bool {
  !p.relative?
}

// Create a normalized path from a path string.\
// `"c:/foo/test.txt" -> "c:/foo/test.txt"`\
// `"c:\\foo\\test.txt" -> "c:/foo/test.txt"`\
// `"/foo//./bar/../test.txt" -> "/foo/test.txt"`
public fun path( s : string ) : path {
  match(s.find(rx-root)) {
    Nothing    -> path-dirs("",s)
    Just(capr) -> path-dirs(capr.groups[1] + "/",capr.slice.after.string)
  }
}
fun path-dirs( root : string, s : string ) : path {  
  val (dirs,base) = s.split(rx-dirsep).list.path-split([])
  path-base(root,dirs,base)
}
fun path-split( parts : list<string>, dirs : list<string> ) : (list<string>,string)  {
  match(parts) {
    Nil -> (dirs.reverse,"")
    Cons(part,rest) -> {
      if (part=="." || part=="")      then path-split(rest,dirs)
      elif (part==".." && dirs.cons?) then path-split(rest,dirs.drop(1))
      elif (rest.cons?)               then path-split(rest, Cons(part,dirs))
      else (dirs.reverse,part)
    }
  }
}
fun path-base( root: string, dirs : list<string>, base : string ) : path {
  match(base.find-last(".")) {
    Nothing     -> Path(root,dirs,base,"")
    Just(slice) -> Path(root,dirs,slice.before.string,slice.after.string)
  }
}

val rx-dirsep = regex(@"[/\\]")
val rx-root= regex(@"^([a-zA-Z]:|(?:\\\\|//)[^/\\]+)?(?:[/\\]|$)")

// Parse a list of paths seperated by colon (`':'`) or semi-colon (`';'`)
//
// Colon separated paths can be ambiguous with Windows style root names (`c:\\`) 
// In particular, a single letter path followed by an absolute path, e.g. ``c:/foo:/bar`` is
// parsed as ``c:/foo`` and ``/bar``. 
public fun paths( s : string ) : list<path> {
  s.split(rx-pathsep).list.paths-split
}
fun paths-split( ps : list<string> ) : list<path> {
  match(ps) {
    Cons(root,Cons(colon,Cons(part,rest))) // match on windows 'c:\' parts    
      | colon==":" && root.count==1 && root.head-char.default(' ').alpha? && 
        part.notempty? && "/\\".contains(part.head)
      -> Cons( path(root+":"+part), paths-split(rest))
    Cons(part,rest) 
      | part==";" || part==":" || part==""
      -> paths-split(rest)
    Cons(part,rest) -> Cons( path(part), paths-split(rest))
    Nil             -> Nil
  }
}
val rx-pathsep = regex(@"([;:])")

// Add two paths together. \
// Keeps the root of `p1` and the basename of `p2` but discards the root of `p2`.\
// `"/a/" + "b/foo.txt"          === "/a/b/foo.txt"`\
// `"/a/foo.txt" + "/b/bar.txt"  === "/a/foo.txt/b/bar.txt"`\
// `"c:/foo" + "d:/bar"          === "c:/foo/bar"`
public fun (+)(p1 : path, p2: path) : path {
  val dirs = p1.dirs + [p1.basename] + p2.dirs
  Path(p1.root,dirs,p2.stem,p2.ext)
}

// Combine multiple paths using `(+)`.
public fun combine( ps : list<path> ) : path {
  ps.foldl(Path(),(+))
}

// Remove the directory and root and only keep the base name (file name) portion of the path.\
// `nodir("foo/bar.ext".path) === "bar.ext"`
public fun nodir( p : path ) : path {
  p(root="",dirs=[])
}

// Remove the basename and only keep the root and directory name portion of the path.\
// `nobase("foo/bar.ext".path) == "foo")`
public fun nobase( p : path ) : path {
  p(stem="",ext="")
}

// Remove the extension from a path.
public fun noext( p : path ) : path {
  p.change-ext("")
}

// Change the extension of a path.
public fun change-ext( p : path, extname : string ) : path {
  path-base(p.root,p.dirs,p.stem + (if (extname.starts-with(".").bool) then extname else "." + extname))
}

// If a path has no extension, set it to the provided one.
public fun default-ext( p : path, extname : string ) : path {
  if (p.ext.empty?) then p.change-ext(extname) else p
}

// Change the stem name of a path
public fun change-stem( p : path, stemname : string ) : path {
  p.change-base( stemname + (if (p.ext.empty?) then "" else "." + p.ext) )
}

// Change the base name of a path
public fun change-base( p : path, basename : string ) : path {
  path-base(p.root,p.dirs,basename)
}


// Return a list of all directory components (excluding the root) 
public function dirnames(p : path) : list<string> {
  p.dirs
}

// Return the last directory component (or the empty string)
public function last-dirname( p : path ) : string {
  p.dirs.reverse.head.default("")
}

// Drop the last directory component (if it exists) 
public function drop-lastdir( p : path ) : path {
  p(dirs = p.dirs.init)
}

// Convert a path to the absolute path on the file system.
// The path is not required to exist on disk. However, if it
// exists any permissions and symbolic links are resolved fully.\
// `".".realpath` (to get the current working directory)\
// `"/foo".realpath` (to resolve the full root, like `"c:/foo"` on windows)
public fun realpath( p : path ) : io path {
  realpath(p.string)
}

// Returns the current working directory.\
// Equal to `".".realpath`.
public fun cwd() {
  ".".realpath
}

// Convert a path to the absolute path on the file system.\
// The overload on a plain string is necessary as it allows 
// for unnormalized paths with `".."` parts. For example
// `"/foo/symlink/../test.txt"` may resolve to `"/bar/test.txt"` if
// ``symlink`` is a symbolic link to a sub directory of `"/bar"`.
public fun realpath( s : string ) : io path {
  xrealpath(s).path
}

extern xrealpath( p : string ) : io string {
  cs "System.IO.Path.GetFullPath"
  js "_get_realpath"
}


// Return the OS specific directory separator (`"/"` or `"\\"`)
public extern dirsep() : ndet string {
  cs "System.IO.Path.DirectorySeparatorChar.ToString"
  js "_get_dirsep"
}

// Return the OS specific path separator (`';'` or `':'`)
public extern pathsep() : ndet string {
  cs "System.IO.Path.PathSeparator.ToString"
  js "_get_pathsep"
}

// Return the path to the currently executing application.
public fun app-path() : io path {
  xapp-path().path
}

extern xapp-path() : io string {
  cs inline "System.Reflection.Assembly.GetEntryAssembly().Location"
  js "_get_apppath"
}

// Return the base directory that contains the currently running application.
// First tries `app-path().nobase`; if that ends in the ``bin`` or ``exe`` directory it
// returns the parent of that directory.
public fun appdir() : io path {
  val p = app-path().nobase
  if (p.last-dirname=="bin" || p.last-dirname=="exe") then p.drop-lastdir else p
}


// Return the home directory of the current user.
public fun homedir() : io path {
  xhomedir().path
}

extern xhomedir() : io string {
  cs "_Path.GetHomeDir"
  js "_get_homedir"
}

// Return the temporary directory for the current user.
public fun tempdir() : io path {
  xtempdir().path
}

extern xtempdir() : io string {
  cs "System.IO.Path.GetTempPath"
  js "_get_tempdir"
}
