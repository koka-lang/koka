/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* File operations.
*/
module std/os/file

import std/os/path
import std/os/dir

extern import
  c file "file-inline.c"
  js file "file-inline.js"

// Read a text file synchronously (using UTF8 encoding)
pub fun read-text-file( path : path ) : <fsys,exn> string
  match read-text-file-err(path.string)
    Error(exn)  -> throw-exn(exn.prepend("unable to read text file " ++ path.show))
    Ok(content) -> content


// Write a text file synchronously (using UTF8 encoding)
pub fun write-text-file( path : path, content : string, create-dir : bool = True ) : <fsys,exn> ()
  if create-dir then ensure-dir(path.nobase)
  match(write-text-file-err(path.string,content))
    Error(exn) -> throw-exn(exn.prepend("unable to write text file " ++ path.show))
    _ -> ()


fun prepend( exn : exception, pre : string ) : exception
  Exception(pre ++ ": " ++ exn.message, exn.info)


extern read-text-file-err( path : string ) : fsys error<string>
  c "kk_os_read_text_file_error"
  js "_read_text_file_error"
  //cs inline "System.IO.File.ReadAllText(#1,System.Text.Encoding.UTF8)"

extern write-text-file-err( path : string, content : string ) : fsys error<()>
  c "kk_os_write_text_file_error"
  js "_write_text_file_error"
  //cs inline "System.IO.File.WriteAllText(#1,#2,System.Text.Encoding.UTF8)"

