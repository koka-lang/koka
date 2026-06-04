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

// Read the full contents of a file handle.
pub fun fd-read-all( fd : fd, recommended-len : int = 64*1024 ) : <exn,async> bytes
  fun read-all(())
    val bs = fd-read(fd,recommended-len)
    if bs.is-empty
      then []
      else Cons(bs,read-all(pretend-decreasing(())))  // a file cannot be infinite so no divergence
  read-all(()).join

// Read the full contents of a file handles as a string.
pub fun fd-read-all-text( fd : fd, recommended-len : int = 64*1024 ) : <exn,async> string
  fd-read-all(fd,recommended-len).string


// Read the contents of a file handle to a channel in chunks of `recommended-len` (=64 KiB) when possible.
// Note: do not convert byte chunks to a string directly since the may start/end with a partial
// utf-8 sequence -- use `fd-read-text-to-channel` instead in such case.
pub fun fd-read-to-channel( fd : fd, ch : channel<bytes>, recommended-len : int = 64*1024 ) : <exn,async> ()
  fun emit-all(())
    val bs  = fd-read(fd,recommended-len)
    val eof = bs.is-empty
    ch.schedule-emit(bs)     // continue directly with the next read request
    if eof
      then () 
      else emit-all(pretend-decreasing(()))  // a file cannot be infinite so no divergence
  emit-all(())

// Read the contents of a file handle to a channel as a string fragments.
// Correctly handles partial utf-8 sequences.
pub fun fd-read-text-to-channel( fd : fd, ch : channel<string>, recommended-len : int = 64*1024 ) : <exn,async> ()
  fun emit-all( previous-post : bytes ) : _ ()
      val bs  = fd-read(fd,recommended-len)
      val eof = bs.is-empty
      if eof then 
        if !previous-post.is-empty() then ch.schedule-emit(previous-post.string)
        ch.schedule-emit("") 
      else 
        val (full,post) = bs.partial-string(previous-post)
        if !full.is-empty() then ch.schedule-emit(full)
        emit-all( pretend-decreasing(post) )
  emit-all(empty)
  
fun test-read()
  val fd = fd-open("readme.md".path, Read)
  println("opened")
  val bs = fd-read-all(fd,16)
  println("read: " ++ bs.string)
  fd-close(fd)
  println("done")

fun test-channel()
  val ch = channel()
  interleaved {
    val fd = fd-open("readme.md".path, Read)
    with finally 
      fd-close(fd)
    println("opened")
    fd-read-text-to-channel(fd,ch,16)
    println("done")  
  } {
    fun recv()
      val s = ch.receive
      println("recv: " ++ s.count.show ++ ": " ++ s)
      recv()
    recv() 
  }
  println("program done.")