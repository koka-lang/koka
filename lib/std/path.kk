/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/


/* File system paths
*/
module std/path

import std/regex

extern import {
  js "fs"
}

// A `:path` represents a file system path.
abstract struct path(
  root : string = "",
  dirs : list<string> = [],
  stem : string = "",
  ext  : string = ""                 
)

// Append `end` to each element of the list `xs` and join them all together.\
// `join-end([],end) === ""`\
// `join-end(["a","b"],"/") === "a/b/"`
public fun join-end( xs : list<string>, end : string) : string {
  xs.join(end) + (if (xs.nil?) then "" else end)
}


// Return the base name of a path (stemname + extension)
public fun basename( p : path ) : string {
  p.stem + (if (p.ext.empty?) then "" else "." + p.ext)
}

// Return the directory part of a path (including the root)
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

// Convert a `:path` to a normalized `:string` path.
public fun string( p : path ) : string {
  p.root + p.dirs.join-end("/") + p.basename
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

// Create a normalized path from a string.
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


// Convert a path to the absolute path on the file system
public fun realpath( p : path ) : io path {
  realpath(p.string)
}

// Convert a path to the absolute path on the file system
public fun realpath( s : string ) : io path {
  xrealpath(s).path
}

extern xrealpath( p : string ) : io string {
  cs inline "System.IO.Path.GetFullPath(#1)"
  js inline "fs.realpathSync(#1)"
}


// Return the OS specific directory separator (`"/"` or `"\\"`)
public extern get-dirsep() : ndet string {
  cs inline "System.IO.Path.DirectorySeparatorChar.ToString()"
  js inline "path.sep"
}

// Return the OS specific path separator (`';'` or `':'`)
public extern get-pathsep() : ndet string {
  cs inline "System.IO.Path.PathSeparator.ToString()"
  js inline "path.delimiter"
}

// Return the path to the currently executing program.
public fun program-path() : io path {
  xprogram-path().path
}

extern xprogram-path() : io string {
  cs inline "System.Reflection.Assembly.GetEntryAssembly().Location"
  js inline "((function(){ var m = module; if (m==null) return ''; while(m.parent) { m = m.parent; }; return (m.filename ? m.filename : ''); })())"
}