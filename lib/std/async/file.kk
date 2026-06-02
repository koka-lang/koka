/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/


/* Asynchronous file operations.

   Todo.
*/
module async/file

import std/num/int32
import std/os/path
import async/uv/file
import async/async

pub type filemode
  // Open for reading, fails if the file does not exist ("r")
  Read
  // Open for writing; truncates an existing file, or creates the file if it does not yet exist ("w")
  Write       
  // Append writes to the end of the file; creates the file if it does not yet exist ("a")
  Append
  // Open for reading and writing; truncates an existing file, or creates the file if it does not yet exist ("w+")
  ReadWrite

abstract struct fd( fd : ssize_t )

fun filemode/flags( fmode : filemode ) : int32
  val m = match fmode
            Read      -> 0
            Write     -> 1
            Append    -> 2
            ReadWrite -> 3
  fd-flags-from-filemode(m.int32)

pub fun file-open( fpath : path, fmode : filemode ) : <exn,async> fd
  with try-exn-prepend(fpath.show)
  Fd(await("file-open", fn(evloop,resume) fd-open-setup(evloop,fpath.string,fmode.flags,0x666.int32,resume)))
  
pub fun file-close( Fd(fd) : fd ) : <exn,async> ()
  await("file-close", fn(evloop,resume) uv/file/fd-close(evloop,fd,resume))
  ()

pub fun test()
  val fd = file-open("readme.md".path, Read)
  println("opened")
  file-close(fd)
  println("done")