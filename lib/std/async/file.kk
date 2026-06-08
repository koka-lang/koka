/*---------------------------------------------------------------------------
  Copyright 2020-2026, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/


/* Asynchronous file operations.

*/
module async/file

import std/core/undiv
import std/num/int32
import std/num/int64
import std/os/path

import async/async
import async/channel
import async/interleave

import async/uv/file

//-----------------------------------
// path based api (prefixed with `file-`)
//-----------------------------------

// Read the full contents of a file.
pub fun file-read-text( fpath : path ) : <ndet,fsys,exn,async> string
  val fd = fd-open(fpath, Read)
  with finally  
    fd.fd-close
  fd.fd-read-all-text()

// Emit the full contents of a file to a string channel.
// Tries to read in chunks of `chunk-len` (=64000)
pub fun file-read-text-to-channel( fpath : path, ch : channel<string>, chunk-len : int = -1 ) : <ndet,fsys,exn,async> ()
  with fd = fd-open(fpath, Read) 
  fd.fd-read-text-to-channel(ch,chunk-len)

// Emit the full contents of a file to a bytes channel.
// Tries to read in chunks of `chunk-len` (=64000)
pub fun file-read-to-channel( fpath : path, ch : channel<bytes>, chunk-len : int = -1 ) : <ndet,fsys,exn,async> ()
  with fd = fd-open(fpath, Read)
  fd.fd-read-to-channel(ch,chunk-len)



//-----------------------------------
// more primitive file descriptor based api (prefixed with `fd-`)
//-----------------------------------

// The mode that a (non-seeking) file is opened in.
pub type filemode
  // Open for reading, fails if the file does not exist ("r")
  Read
  // Open for writing; truncates an existing file, or creates the file if it does not yet exist ("w")
  Write       
  // Append writes to the end of the file; creates the file if it does not yet exist ("a")
  Append
  // Open for reading and writing; truncates an existing file, or creates the file if it does not yet exist ("w+")
  ReadWrite

abstract struct fd( fd : ssize_t, fpath : path )

pub fun fd/show( Fd(fd,fpath) : fd ) : string
  fpath.show ++ "(=" ++ fd.int.show ++ ")"

fun filemode/flags( fmode : filemode ) : int32
  val m = match fmode
            Read      -> 0
            Write     -> 1
            Append    -> 2
            ReadWrite -> 3
  fd-flags-from-filemode(m.int32)

// Open a file descriptor for a given `path` and file mode `fmode`.
pub fun fd-open( fpath : path, fmode : filemode ) : <exn,fsys,async> fd
  with try-exn-prepend("open " ++ fpath.show)
  Fd(await("fd-open", fn(evloop,resume) fd-open-setup(evloop,fpath.string,fmode.flags,0x666.int32,resume)),fpath)
  

// Close a file descriptor.
pub fun fd-close( Fd(fd) : fd ) : <exn,fsys,async> ()
  await("fd-close", fn(evloop,resume) fd-close-setup(evloop,fd,resume))
  ()

// Open a file descriptor for a given `path` and file mode `fmode` and pass it to `action`.
// Closes the file descriptor when `action` returns or finalizes.
pub fun scoped/fd-open( fpath : path, fmode : filemode, action : ( fd : fd ) -> <exn,fsys,async|e> a )  : <exn,fsys,async|e> a
  val fd = fd-open(fpath,fmode)
  with finally
    fd-close(fd)
  action(fd)

pub fun fd-read( Fd(fd,fpath) : fd, chunk-len : int = -1 ) : <exn,ndet,async> bytes
  with try-exn-prepend("read " ++ fpath.show)
  val len = if (chunk-len < 0 || chunk-len > 64000) then 64000 else chunk-len
  if (len==0) then empty else
    val buf = unsafe-bytes(len)  
    val read = await("fd-read", fn(evloop,resume) fd-read-setup(evloop,fd,buf,(-1).int64,resume))
    buf.adjust-length(read.int)  // note: we must refer to `buf` here to keep it alive during the async operation!

// Read the full contents of a file handle.
pub fun fd-read-all( fd : fd, chunk-len : int = -1 ) : <exn,ndet,async> bytes
  fun read-all(())
    val bs = fd-read(fd,chunk-len)
    if bs.is-empty
      then []
      else Cons(bs,read-all(pretend-decreasing(())))  // a file cannot be infinite so no divergence
  read-all(()).join

// Read the full contents of a file handles as a string.
pub fun fd-read-all-text( fd : fd, chunk-len : int = -1 ) : <exn,ndet,async> string
  fd-read-all(fd,chunk-len).string


// Read the contents of a file handle to a channel in chunks of `chunk-len` (=64 KiB) when possible.
// Emits a final empty `:bytes` on end-of-file.
// Note: do not convert byte chunks to a string directly since the may start/end with a partial
// utf-8 sequence -- use `fd-read-text-to-channel` instead in such case.
pub fun fd-read-to-channel( fd : fd, ch : channel<bytes>, chunk-len : int = -1 ) : <exn,ndet,async> ()
  fun emit-all(())
    val bs  = fd-read(fd,chunk-len)
    val eof = bs.is-empty
    ch.schedule-emit(bs)     // continue directly with the next read request
    if eof
      then () 
      else emit-all(pretend-decreasing(()))  // a file cannot be infinite so no divergence
  emit-all(())

// Read the contents of a file handle to a channel as string fragments (of about the `chunk-len` (=64 KiB)).
// Emits a final empty string on end-of-file.
// Correctly handles partial utf-8 sequences.
pub fun fd-read-text-to-channel( fd : fd, ch : channel<string>, chunk-len : int = -1 ) : <exn,ndet,async> ()
  fun emit-all( previous-post : bytes ) : _ ()
      val bs  = fd-read(fd,chunk-len)
      val eof = bs.is-empty
      if eof then 
        if !previous-post.is-empty() then ch.schedule-emit(previous-post.string)
        ch.schedule-emit("") 
      else 
        val (full,post) = bs.partial-string(previous-post)
        if !full.is-empty() then ch.schedule-emit(full)
        emit-all( pretend-decreasing(post) )                // a file cannot be infinite so no divergence
  emit-all(empty)


//-----------------------------------
// Test
//-----------------------------------

fun test-read()
  with fd = fd-open("readme.md".path, Read)
  println("opened")
  val bs = fd-read-all(fd,16)
  println("read: " ++ bs.string)
  println("done")

fun test-channel()
  val ch = channel()
  interleaved {
    with fd = fd-open("readme.md".path, Read)
    println("opened")
    fd-read-to-channel(fd,ch,16)
    println("done reading")      
  } {
    fun recv()
      val bs = ch.receive
      println("recv: " ++ bs.show )
      // println("recv: " ++ s.count.show ++ ": " ++ s)
      if !bs.is-empty then recv()
    recv() 
    println("done receiving")
  }
  println("program done.")