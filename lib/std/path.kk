/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/


/* File system paths
*/
public module std/path

extern include {
  js file "path-inline.js"
}

// Change the extension of a path
fun change-ext( path : string, ext : string ) : string {
  path.noext + (if (ext.starts-with(".")) then "" else ".") + ext
}

// If a path has no extension, set it to the provided one.
fun default-ext( fname, ext ) {
  if (fname.extname=="") then fname.change-ext(ext) else fname
}


// Remove the extension from a path
fun noext( path : string ) : string {
  val ext = path.extname
  path.trim-right(ext)
}


// Join arguments using the platform specific directory separator  
// Note: unlike NodeJS the resulting path is not yet normalized
fun combine( path1 : string, path2 : string ) : string {
  val p2 = path2.trim-left(sep)
  val p1 = path1.trim-right(sep)
  if (p1=="") then p2 else p1 + sep + p2
}

// Join a list of paths 
fun combine( paths : list<string> ) : string {
  match(paths) {
    Nil -> ""
    Cons(p,ps) -> ps.foldl( p, combine )
  }
}

// Return the full normalized path
extern normalize( p : string ) : io string {
  cs inline "System.IO.Path.GetFullPath(#1)"
  js inline "path.normalize(#1)"
}

// Return the directory name portion of the path (excluding the directory separator).
// Return an empty string if no directory part exists in the path.  
// `dirname("foo/bar.ext") == "foo")`
fun dirname( p : string ) : string {
  val d = p.xdirname
  if (d=="." && !(p.starts-with("."))) then "" else d
}

private extern xdirname( p : string ) : string {
  cs inline "System.IO.Path.GetDirectoryName(#1)"
  js inline "path.dirname(#1)"
}

// Return the base name (file name) portion of the path.
// `basename("foo/bar.ext") == "bar.ext"`
extern basename( p : string ) : string {
  cs inline "System.IO.Path.GetFileName(#1)"
  js inline "path.basename(#1)"
}

// Return the extension (including the `.`)  
// `extname("foo.ext") == ".ext"`, `extname("bla.") == "."`, `extname("bla") == ""` 
extern extname( p : string ) : string {
  cs inline "System.IO.Path.GetExtension(#1)"
  js inline "path.extname(#1)"
}

// Return the stem name portion of the path, i.e. without directory or extension.
// `stemname("foo/bar.ext") == "bar"`
public fun stemname( p : string ) : string {
  p.basename.noext
}

// Append to the stem name of a path
public fun append-stem( path : string, extra : string ) : string {
  combine(path.dirname, path.stemname + extra + path.extname)
}

private extern pathsep() : string {
  cs inline "System.IO.Path.DirectorySeparatorChar.ToString()"
  js inline "path.sep"
}

// Platform specific directory separator (`/` or `\`)
val sep : string = pathsep()

private extern pathdelim() : string {
  cs inline "System.IO.Path.PathSeparator.ToString()"
  js inline "path.delimiter"
}

// Platform specific path delimiter when specifying a list o paths (`:` or `;`)
val delimiter : string = pathdelim()


extern program-path() : io string {
  cs inline "System.Reflection.Assembly.GetEntryAssembly().Location"
  js inline "((function(){ var m = module; if (m==null) return ''; while(m.parent) { m = m.parent; }; return (m.filename ? m.filename : ''); })())"
}