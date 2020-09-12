/*---------------------------------------------------------------------------
    Copyright 2012 Microsoft Corporation.

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

extern import {
  c  "file-inline.c"
  js "fs"
}

// Read a text file synchronously (using UTF8 encoding)
public fun read-text-file( path : string ) : <file,exn> string {
  match(read-text-file-either(path)) {
    Left(errno) -> Exception("unable to read text file " + path.show, System(errno.show)).throw
    Right(s)    -> s
  }
}

// Write a text file synchronously (using UTF8 encoding)
public fun write-text-file( path : string, content : string ) : <file,exn> () {
  match(write-text-file-either(path,content)) {
    Left(errno) -> Exception("unable to write text file " + path.show, System(errno.show)).throw
    Right       -> ()
  }
}

extern read-text-file-either( path : string ) : file either<int,string> {
  c "kk_os_read_text_file_either"
  js inline "($std_core.host()=='node' ? fs.readFileSync(#1,{encoding: 'utf8'}) : '')"
  //cs inline "System.IO.File.ReadAllText(#1,System.Text.Encoding.UTF8)"
}

extern write-text-file-either( path : string, content : string )  : file either<int,()> {
  c "kk_os_write_text_file_either"
  js inline "($std_core.host()=='node' ? fs.writeFileSync(#1,#2,{encoding: 'utf8'}) : null)"
  //cs inline "System.IO.File.WriteAllText(#1,#2,System.Text.Encoding.UTF8)"
}
