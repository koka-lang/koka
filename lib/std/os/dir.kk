/*---------------------------------------------------------------------------
  Copyright 2020, Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/


/* Directory operations.
*/
module std/os/dir

import std/os/path

extern import {
  c file "dir-inline.c"
}

// Recursively list all the entries under a directory.
public fun list-directory-recursive( dir : path, max-depth : int = 1000 ) : <fsys,div> list<path> {
  if (max-depth < 0) return []
  val all  = list-directory(dir)
  val dirs = all.filter(is-directory)
  all ++ dirs.flatmap(fn(sub){ list-directory-recursive(sub,(max-depth - 1)) })
}

public fun copy-directory( dir : path, to : path ) : <fsys,pure> () {
  ensure-dir(to)
  val all = list-directory(dir)
  val (dirs,files) = all.partition(is-directory)
  files.foreach( fn(f){ copy-file( f, to / f.nodir ) } )
  dirs.foreach( fn(d){ copy-directory( d, to / d.nodir ) } )
}

// Ensure a directory path exists
public fun ensure-dir( dir : path ) : <fsys,exn> () {
  match(ensure-dir-err(dir.string, -1)) {
    Error(exn) -> Error(exn.prepend("unable to create directory " ++ dir.show)).throw
    Ok         -> ()
  }
}

// List directory contents (excluding `.` and `..`).
// Returns a list of full paths (not just the names in the directory).
public fun list-directory( dir : path ) : fsys list<path> {
  match(prim-list-dir(dir.string)) {
    Error()      -> []
    Ok(contents) -> contents.list.map(fn(name){ dir / name.path })
  }
}

// Is the path a valid directory?
public fun is-directory( dir : path ) : fsys bool {
  prim-is-dir(dir.string)
}

// Is the path a valid file?
public fun is-file( path : path ) : fsys bool {
  prim-is-file(path.string)
}

// Copy a file to a directory
public fun copy-file-to-dir( from : path, dir : path ) : <fsys,exn> () {
  copy-file(from, dir / from.nodir)
}

// Copy a file.
public fun copy-file( from : path, to : path, preserve-mtime : bool = True ) : <fsys,exn> () {
  match (prim-copy-file(from.string, to.string, preserve-mtime )) {
    Error(exn) -> Error(exn.prepend("unable to copy " ++ from.show ++ " to " ++ to.show)).throw
    _ -> ()
  }
}

private fun prepend( exn : exception, pre : string ) : exception {
  Exception(pre ++ ": " ++ exn.message, exn.info)
}

extern ensure-dir-err( path : string, mode : int ) : fsys error<()> {
  c "kk_os_ensure_dir_error"
}

extern prim-copy-file( from : string, to : string, preserve-mtime : bool ) : fsys error<()> {
  c "kk_os_copy_file_error"
}

extern prim-list-dir( dir : string ) : fsys error<vector<string>> {
  c "kk_os_list_directory_prim"
}

extern prim-is-dir( dir : string ) : fsys bool {
  c "kk_os_is_directory"
}

extern prim-is-file( path : string ) : fsys bool {
  c "kk_os_is_file"
}
