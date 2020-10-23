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

// Read a text file synchronously (using UTF8 encoding)
public fun read-text-file( path : path ) : file error<string> {
  match(read-text-file-err(path.string)) {
    Error(exn) -> Error(exn.prepend("unable to read text file " + path.show))
    ok -> ok
  }
}

// Write a text file synchronously (using UTF8 encoding)
public fun write-text-file( path : path, content : string, create-dir : bool = True ) : file error<()> {
  match( if (create-dir) then ensure-path(path.nobase) else Ok(()) ) {
    Ok() -> match(write-text-file-err(path.string,content)) {
              Error(exn) -> Error(exn.prepend("unable to write text file " + path.show))
              ok -> ok    
            }
    err  -> err
  }
}

// Ensure a directory path exists
public fun ensure-path( path : path ) : file error<()> {
  match(ensure-path-err(path.string, -1)) {
    Error(exn) -> Error(exn.prepend("unable to create directory " + path.show))
    ok -> ok    
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

extern ensure-path-err( path : string, mode : int ) : file error<()> {
  c "kk_os_ensure_path_error"
  //js "_write_text_file_error"
  //cs inline "System.IO.File.WriteAllText(#1,#2,System.Text.Encoding.UTF8)"
}
