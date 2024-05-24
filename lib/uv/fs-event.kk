/*---------------------------------------------------------------------------
  Copyright 2024 Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

module uv/fs-event
import uv/utils
pub import uv/file
pub import std/num/int32

extern import
  c file "inline/fs-event.c"

// FS Event handles allow the user to monitor a given path for changes, for example, if the file was renamed or there was a generic change in it.
// This handle uses the best backend for the job on each platform.
value struct uv-fs-event {internal: any}

// Event types that uv_fs_event_t handles monitor.
pub type uv-fs-event-kind
  UV_RENAME // 1
  UV_CHANGE // 2

pub fun event-kind/show(k: uv-fs-event-kind): string
  match k
    UV_RENAME -> "rename"
    UV_CHANGE -> "change"

pub fun int/event-kinds(i: int32): list<uv-fs-event-kind>
  val kinds = Nil
  val kinds' = if i.and(1.int32) != 0.int32 then Cons(UV_RENAME, kinds) else kinds
  return (if i.and(2.int32) != 0.int32 then Cons(UV_CHANGE, kinds') else kinds')

// Flags that can be passed to uv_fs_event_start() to control its behavior.
pub type uv-fs-event-flags
  // By default, if the fs event watcher is given a directory name, we will
  // watch for all events in that directory. This flags overrides this behavior
  // and makes fs_event report only changes to the directory entry itself. This
  // flag does not affect individual files watched.
  // This flag is currently not implemented yet on any backend.
  UV_FS_EVENT_WATCH_ENTRY // 1
  // By default uv_fs_event will try to use a kernel interface such as inotify
  // or kqueue to detect events. This may not work on remote file systems such
  // as NFS mounts. This flag makes fs_event fall back to calling stat() on a
  // regular interval.
  // This flag is currently not implemented yet on any backend.
  UV_FS_EVENT_STAT // 2
  //  By default, event watcher, when watching directory, is not registering
  // (is ignoring) changes in its subdirectories.
  // This flag will override this behaviour on platforms that support it.v
  UV_FS_EVENT_RECURSIVE // 4

pub fun flags/int(l: list<uv-fs-event-flags>): int32
  match l
    Cons(UV_FS_EVENT_WATCH_ENTRY, l') -> 1.int32.or(flags/int(l'))
    Cons(UV_FS_EVENT_STAT, l') -> 2.int32.or(flags/int(l'))
    Cons(UV_FS_EVENT_RECURSIVE, l') -> 4.int32.or(flags/int(l'))
    Nil -> 0.int32

// Initialize the handle.
pub extern uv-fs-event-init(): error<uv-fs-event>
  c "kk_uv_fs_event_init"

// Start the handle with the given callback, which will watch the specified path for changes.
// flags can be an ORed mask of `uv_fs_event_flags`.
pub extern uv-extern-fs-event-start(^event: uv-fs-event, path: string, flags: int32, cb: error<(string, int32)> -> io-noexn ()): io-noexn error<()>
  c "kk_uv_fs_event_start"

// Start the handle with the given callback, which will watch the specified path for changes.
// flags can be an list of `uv_fs_event_flags`.
pub fun uv-fs-event-start(^event: uv-fs-event, path: string, flags: list<uv-fs-event-flags>, cb: error<(string, list<uv-fs-event-kind>)> -> io-noexn ()): io-noexn error<()>
  uv-extern-fs-event-start(event, path, flags/int(flags)) fn(e)
    match e
      Error(e) -> cb(Error(e))
      Ok((p, i)) -> cb(Ok((p, event-kinds(i))))

// Stop the handle, the callback will no longer be called.
pub extern uv-fs-event-stop(event: uv-fs-event): io-noexn error<()>
  c "kk_uv_fs_event_stop"

// Get the path being monitored by the handle.
pub extern uv-fs-event-getpath(^event: uv-fs-event): io-noexn error<string>
  c "kk_uv_fs_event_getpath"
