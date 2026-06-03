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

import std/core/undiv
import std/num/int32
import std/num/int64
import std/os/path
import async/uv/file
import async/async
import async/channel
import async/interleave

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

pub fun fd-open( fpath : path, fmode : filemode ) : <exn,async> fd
  with try-exn-prepend(fpath.show)
  Fd(await("fd-open", fn(evloop,resume) fd-open-setup(evloop,fpath.string,fmode.flags,0x666.int32,resume)))
  
pub fun fd-close( Fd(fd) : fd ) : <exn,async> ()
  await("fd-close", fn(evloop,resume) uv/file/fd-close(evloop,fd,resume))
  ()

pub fun fd-read( Fd(fd) : fd, recommended-len : int = 64*1024 ) : <exn,async> bytes
  val len = if (recommended-len < 0 || recommended-len > 64*1024) then 64*1024 else recommended-len
  if (len==0) then empty else
    val buf = unsafe-bytes(len)  
    val read = await("fd-read", fn(evloop,resume) uv/file/fd-read(evloop,fd,buf,(-1).int64,resume))
    buf.adjust-length(read.int)  // note: we must refer to `buf` here to keep it alive during the async operation!

pub fun fd-read-all( fd : fd, recommended-len : int = 64*1024 ) : <exn,async> bytes
  fun read-all(())
    val bs = fd-read(fd,recommended-len)
    if bs.is-empty
      then []
      else Cons(bs,read-all(pretend-decreasing(())))  // a file cannot be infinite so no divergence
  read-all(()).join

pub fun fd-read-to-channel( fd : fd, ch : channel<bytes>, recommended-len : int = 64*1024 ) : <exn,async> ()
  fun read-all(())
    val bs = fd-read(fd,recommended-len)
    val eof = bs.is-empty
    ch.schedule-emit(bs)     
    if eof
      then () 
      else read-all(pretend-decreasing(()))  // a file cannot be infinite so no divergence
  read-all(())

pub fun test()
  val fd = fd-open("readme.md".path, Read)
  println("opened")
  val bs = fd-read-all(fd,16)
  println("read: " ++ bs.string)
  fd-close(fd)
  println("done")

pub fun test-channel()
  val ch = channel()
  interleaved {
    val fd = fd-open("readme.md".path, Read)
    with finally 
      fd-close(fd)
    println("opened")
    fd-read-to-channel(fd,ch,16)
    println("done")  
  } {
    fun recv()
      val bs = ch.receive
      println("recv: " ++ bs.length.show ++ ": " ++ bs.string)
      recv()
    recv() 
  }
  println("program done.")