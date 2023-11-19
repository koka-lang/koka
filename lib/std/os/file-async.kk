/*---------------------------------------------------------------------------
  Copyright 2012-2023, Microsoft Research, Daan Leijen. Brigham Young University, Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/// This module provides a set of functions for asynchronous file I/O.
module std/os/file-async

pub import std/async
pub import std/os/file-uv
import std/num/int64
import std/os/path

pub fun read-as-string(path: path)
  val length = stat(path).size
  val fd = open(path, o_RDONLY, 0.int32)
  val res = await fn(cb)
    read(fd, length.int.int32, 0.int32) fn(bytes1)
      match bytes1
        Ok((bytes, bytesRead)) ->
          cb(Ok(bytes))
          if bytesRead.int64 != length then
            // TODO: Actually read it all?
            cb(Error(Exception("Not Full Read", ExnInternal("Not Full Read"))))
          else ()
        Error(e) -> 
          cb(Error(e))
    Just({ fd.close(fn(_) ())})
  res.to-string

pub fun read-file(path: path)
  val stream = stream-file(path)
  val done = ref(False)
  val str = ref("")
  while { ! !done }
    match stream.receive
      Ok(bytes) -> str := !str ++ bytes.to-string
      Error(Exception("EOF",_ )) -> done := True
      Error(e) -> 
        done := True
        throw-exn(e)
  !str

pub fun open(path: path, flags: int32, mode: int32): <async,iouv> uvFile
  await fn(cb)
    open(path.string, flags, mode) fn(fd)
      cb(fd)
    Nothing

pub fun stream-file(path: path, chunkSize: int32 = 0x1024.int32)
  val ch = channel()
  val file = open(path, o_RDONLY, 0.int32)
  await-to-channel(
    (fn(cb)
      read-file_(file, chunkSize, 0.int32, cb)        
      Just({ file.close(fn(_) ())})
    ),
    ch,
    fn(e) e
  )

fun read-file_(fd: uvFile, chunkSize: int32, offset: int32, cb: (error<bytes>, bool) -> iouv-noexn ())
  read(fd, chunkSize, offset) fn(bytes1)
    match bytes1
      Ok((bytes, bytesRead)) ->
        cb(Ok(bytes), False)
        if bytesRead.int32 == chunkSize then
          read-file_(fd, chunkSize, offset+bytesRead.int32, cb)
        else
          cb(Error(Exception("EOF", ExnInternal("EOF"))), True)
      Error(e) -> 
        cb(Error(e), True)

fun write-to-file(fd: uvFile, bts: bytes, offset: int64, cb: (error<()>) -> iouv-noexn ()): iouv-noexn ()
  write(fd, bts, offset) fn(bytesWritten)
    match bytesWritten 
      Ok(bytes) -> 
        if bytes == bts.bytes-length.int then cb(Ok(()))
        else write-to-file(fd, bts.bytes-skip-count(bytes.ssize_t), offset + bytes.int64, cb)
      Error(e) -> cb(Error(e))

fun write-to-file(fd: uvFile, bts: bytes, offset: int64): iouv-noexn error<()>
  val bytesWritten = write-sync(fd, bts, offset) 
  match bytesWritten 
    Ok(bytes) -> 
      if bytes == bts.bytes-length.int then Ok(())
      else write-to-file(fd, bts.bytes-skip-count(bytes.ssize_t), offset + bytes.int64)
    Error(e) -> Error(e)

pub fun write-to-file(fd: uvFile, s: string): <async,iouv> ()
  await fn(cb)
    write-to-file(fd, s.from-string, 0.int64, cb)
    Just({fd.close(fn(_) ())})

pub fun write-to-file-sync(fd: uvFile, s: string): iouv-noexn error<()>
  write-to-file(fd, s.from-string, 0.int64)

pub fun close(fd: uvFile): <async,iouv> ()
  await fn(cb)
    fd.close(fn(_) cb(Ok(())))
    Nothing

pub fun unlink(path: path): <async,iouv> ()
  await fn(cb)
    unlink(path.string) fn(_) cb(Ok(()))
    Nothing

pub inline fun delete(path: path): <async,iouv> ()
  unlink(path)

pub fun mkdir(path: path, mode: int32 = 0.int32): <async,iouv> ()
  await fn(cb)
    mkdir(path.string, mode) fn(_) cb(Ok(()))
    Nothing

pub inline fun create-dir(path: path, mode: int32 = 0.int32): <async,iouv> ()
  mkdir(path, mode)

pub fun make-temp-dir(p: path)
  await fn(cb)
    mkdtemp(p.string ++ "XXXXXX") fn(p1)
      match p1
        Ok(p1') -> cb(Ok(p1'.path))
        Error(e) -> cb(Error(e))
    Nothing

pub inline fun create-temp-dir(path: path): <async,iouv> path
  make-temp-dir(path)

pub fun make-temp-file(p: path)
  await fn(cb)
    mkstemp(p.string ++ "XXXXXX") fn(p1)
      match p1
        Ok((file, p1')) -> cb(Ok((p1'.path, file)))
        Error(e) -> cb(Error(e))
    Nothing

pub inline fun create-temp-file(path: path): <async,iouv> (path, uvFile)
  make-temp-file(path)

// TODO: Add recursive delete && force delete with children
pub fun rmdir(path: path): <async,iouv> ()
  await fn(cb)
    rmdir(path.string) fn(_) cb(Ok(()))
    Nothing

pub inline fun remove-dir(path: path): <async,iouv> ()
  rmdir(path)

pub inline fun delete-directory(path: path): <async,iouv> ()
  rmdir(path)

fun dir-list(req: uvFsReq): iouv-noexn list<dirent>
  match scandir-next(req)
    Ok(dirent) -> Cons(dirent, dir-list(req))
    Error(_) -> Nil

pub fun list-directory(path: path): <async,iouv> list<dirent>
  await fn(cb)
    scandir(path.string) fn(e)
      match e
        Ok(req) -> cb(Ok(dir-list(req)))
        Error(e) -> cb(Error(e))
    Nothing // TODO: Does req need cleanup?

pub fun stat(path: path): <async,iouv> fstat
  await fn(cb)
    stat(path.string) fn(e)
      cb(e)
    Nothing

pub fun lstat(path: path): <async,iouv> fstat
  await fn(cb)
    lstat(path.string) fn(e)
      cb(e)
    Nothing

pub fun stat(fd: uvFile): <async,iouv> fstat
  await fn(cb)
    fstat(fd) fn(e)
      cb(e)
    Nothing

pub fun rename(path: path, newPath: path): <async,iouv> ()
  await fn(cb)
    rename(path.string, newPath.string) fn(e)
      cb(e)
    Nothing

pub fun fsync(fd: uvFile): <async,iouv> ()
  await fn(cb)
    fsync(fd) fn(e)
      cb(e)
    Nothing

pub fun fdatasync(fd: uvFile): <async,iouv> ()
  await fn(cb)
    fdatasync(fd) fn(e)
      cb(e)
    Nothing

pub inline fun flush(fd: uvFile): <async,iouv> ()
  fsync(fd)

pub fun truncate(fd: uvFile, offset: int64): <async,iouv> ()
  await fn(cb)
    ftruncate(fd, offset) fn(e)
      cb(e)
    Nothing

pub fun copyfile(path: path, newPath: path, flags: int32 = 0.int32): <async,iouv> ()
  await fn(cb)
    copyfile(path.string, newPath.string, flags) fn(e)
      cb(e)
    Nothing

pub fun redirect(outFd: uvFile, inFd: uvFile, inOffset: int64, length: int64): <async,iouv> int
  await fn(cb)
    sendfile(outFd, inFd, inOffset, length.int.ssize_t) fn(e)
      cb(e)
    Nothing

pub fun access(path: path, mode: int32): <async,iouv> bool
  await fn(cb)
    access(path.string, mode) fn(e)
      match e
        Ok(_) -> cb(Ok(True))
        _ -> cb(Ok(False))
    Nothing

pub inline fun can-access(path: path, mode: int32): <async,iouv> bool
  access(path, mode)

pub fun chmod(path: path, mode: int32): <async,iouv> ()
  await fn(cb)
    chmod(path.string, mode) fn(e)
      cb(e)
    Nothing

pub fun chmod(fd: uvFile, mode: int32): <async,iouv> ()
  await fn(cb)
    fchmod(fd, mode) fn(e)
      cb(e)
    Nothing

pub fun update-time(path: path, access-time: float64, modified-time: float64): <async,iouv> ()
  await fn(cb)
    utime(path.string, access-time, modified-time) fn(e)
      cb(e)
    Nothing

pub fun update-time(fd: uvFile, access-time: float64, modified-time: float64): <async,iouv> ()
  await fn(cb)
    futime(fd, access-time, modified-time) fn(e)
      cb(e)
    Nothing

pub fun link-update-time(path: path, access-time: float64, modified-time: float64): <async,iouv> ()
  await fn(cb)
    lutime(path.string, access-time, modified-time) fn(e)
      cb(e)
    Nothing

pub fun link(path: path, newPath: path): <async,iouv> ()
  await fn(cb)
    link(path.string, newPath.string) fn(e)
      cb(e)
    Nothing

pub fun symlink(path: path, newPath: path, flags: int32 = 0.int32): <async,iouv> ()
  await fn(cb)
    symlink(path.string, newPath.string, flags) fn(e)
      cb(e)
    Nothing

pub fun readlink(path: path): <async,iouv> path
  await fn(cb)
    readlink(path.string) fn(e)
      match e
        Ok(p) -> cb(Ok(p.path))
        Error(e) -> cb(Error(e))
    Nothing

// Follows all symlinks to get the real path (max depth 32)
pub fun real-path(path: path): <async,iouv> path
  await fn(cb)
    realpath(path.string) fn(e)
      match e
        Ok(p) -> cb(Ok(p.path))
        Error(e) -> cb(Error(e))
    Nothing

// Only works on unix systems
pub fun chown(path: path, uid: int32, gid: int32): <async,iouv> ()
  await fn(cb)
    chown(path.string, uid, gid) fn(e)
      cb(e)
    Nothing

// Only works on unix systems
pub fun chown(fd: uvFile, uid: int32, gid: int32): <async,iouv> ()
  await fn(cb)
    fchown(fd, uid, gid) fn(e)
      cb(e)
    Nothing

// Only works on unix systems
pub fun link-chown(path: path, uid: int32, gid: int32): <async,iouv> ()
  await fn(cb)
    lchown(path.string, uid, gid) fn(e)
      cb(e)
    Nothing