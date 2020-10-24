/*---------------------------------------------------------------------------
  Copyright 2020, Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/


/* File operations.
*/
module std/os/file

import std/os/path
//import std/num/ddouble
//import std/time/duration
//import std/time/instant
//import std/time/utc

include extern {
  c file "file-inline.c"
  js file "file-inline.js"
}

// Recursively list all the entries under a directory.
public fun list-directory-recursive( dir : path, max-depth : int = 1000 ) : <file,div> list<path> {
  if (max-depth < 0) return []
  val all  = list-directory(dir)
  val dirs = all.filter(is-directory)
  all + dirs.flatmap(fn(sub){ list-directory-recursive(sub,(max-depth - 1)) })
}

public fun copy-directory( dir : path, to : path ) : <file,pure> () {
  ensure-dir(to)
  val all = list-directory(dir)
  val (dirs,files) = all.partition(is-directory)
  files.foreach( fn(f){ copy-file( f, to + f.nodir ) } )
  dirs.foreach( fn(d){ copy-directory( d, to + d.nodir ) } )
}

// Read a text file synchronously (using UTF8 encoding)
public fun read-text-file( path : path ) : <file,exn> string {
  match(read-text-file-err(path.string)) {
    Error(exn)  -> Error(exn.prepend("unable to read text file " + path.show)).throw
    Ok(content) -> content
  }
}

// Write a text file synchronously (using UTF8 encoding)
public fun write-text-file( path : path, content : string, create-dir : bool = True ) : <file,exn> ()  {
  if (create-dir) then ensure-dir(path.nobase) 
  match(write-text-file-err(path.string,content)) {
    Error(exn) -> Error(exn.prepend("unable to write text file " + path.show)).throw
    _ -> ()
  }
}

// Ensure a directory path exists
public fun ensure-dir( dir : path ) : <file,exn> () {
  match(ensure-dir-err(dir.string, -1)) {
    Error(exn) -> Error(exn.prepend("unable to create directory " + dir.show)).throw
    Ok         -> ()
  }
}

// List directory contents (excluding `.` and `..`).
// Returns a list of full paths (not just the names in the directory).
public fun list-directory( dir : path ) : file list<path> {
  match(prim-list-dir(dir.string)) {
    Error()      -> []
    Ok(contents) -> contents.list.map(fn(name){ dir + name.path })
  }
}

// Is the path a valid directory?
public fun is-directory( dir : path ) : file bool {
  prim-is-dir(dir.string)
}

// Is the path a valid file?
public fun is-file( path : path ) : file bool {
  prim-is-file(path.string)
}

// Copy a file to a directory
public fun copy-file-to-dir( from : path, dir : path ) : <file,exn> () {
  copy-file(from, dir + from.nodir)
}

// Copy a file.
public fun copy-file( from : path, to : path ) : <file,exn> () {
  match (prim-copy-file(from.string, to.string)) {
    Error(exn) -> Error(exn.prepend("unable to copy " + from.show + " to " + to.show)).throw
    _ -> ()
  }
}

private fun prepend( exn : exception, pre : string ) : exception {
  Exception(pre + ": " + exn.message, exn.info)
}

extern read-text-file-err( path : string ) : file error<string> {
  c "kk_os_read_text_file_error"
  js "_read_text_file_error"
  //cs inline "System.IO.File.ReadAllText(#1,System.Text.Encoding.UTF8)"
}

extern write-text-file-err( path : string, content : string ) : file error<()> {
  c "kk_os_write_text_file_error"
  js "_write_text_file_error"
  //cs inline "System.IO.File.WriteAllText(#1,#2,System.Text.Encoding.UTF8)"
}

extern ensure-dir-err( path : string, mode : int ) : file error<()> {
  c "kk_os_ensure_dir_error"
}

extern prim-copy-file( from : string, to : string ) : file error<()> {
  c "kk_os_copy_file_error"
}

extern prim-list-dir( dir : string ) : file error<vector<string>> {
  c "kk_os_list_directory_prim"
}

extern prim-is-dir( dir : string ) : file bool {
  c "kk_os_is_directory"
}

extern prim-is-file( path : string ) : file bool {
  c "kk_os_is_file"
}

public extern run-command( cmd : string ) : file error<string> {
  c "kk_os_run_command_error"
}

public extern run-system( cmd : string ) : file int {
  c "kk_os_run_system_prim"
}
