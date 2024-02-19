/*---------------------------------------------------------------------------
  Copyright 2023 Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

module std/os/event-loop

import std/num/int32

extern import
  c { conan="libuv[>=1.47.0]"; vcpkg="libuv"; library="uv" }
// TODO: wasm {}

extern import
  c file "event-loop-inline"


pub value struct uv-handle { internal: intptr_t }

// Handles a uv loop
pub fun default-event-loop(action)
  if host() == "libc" then
    handle-loop(action)
  else // TODO: Support event loop on other platforms
    action()

val @initialize = init-uv-alloc()

// Runs a UV loop
fun handle-loop(action)
  init-loop()
  val res = action() 
  run-loop()
  close-loop()
  res

abstract extend type exception-info
  pub con AsyncExn( status-code : uv-status-code )

extern run-loop(): io-event ()
  c "kk_uv_loop_run"
  wasm "kk_emscripten_loop_run"
  js inline ""
  cs inline ""

extern init-loop(): io-event ()
  c "kk_uv_loop_init"
  wasm inline "kk_Unit"
  js inline ""
  cs inline ""

// initializes only the allocators (not an event loop)
// needed for some file operations which alloc - kk_uv_fs_mkdtemp, kk_uv_fs_mkstemp
pub extern init-uv-alloc(): ()
  c "kk_uv_alloc_init"
  wasm inline "kk_Unit"
  cs inline ""
  js inline ""

extern close-loop(): io-event ()
  c "kk_uv_loop_close"
  wasm inline "kk_Unit"
  cs inline ""
  js inline ""

pub extern int/status-code(code: int32): uv-status-code
  c "kk_uv_status_to_status_code"
  wasm inline "kk_std_os_event_dash_loop_UV__OK"
  cs inline ""
  js inline ""

pub extern close(hnd: uv-handle, callback: () -> io-event ()): io-event ()
  c "kk_uv_close"
  wasm inline "kk_Unit"
  cs inline ""
  js inline ""

pub inline fun result/untry(code: uv-status-code, a: a): exn a
  if code.is-uv_OK then a 
  else throw-exn(Exception(code.message, AsyncExn(code))) // TODO: Errno refactoring

pub inline fun unit/untry(code: uv-status-code): exn ()
  if code.is-uv_OK then ()
  else throw-exn(Exception(code.message, AsyncExn(code)))


pub type uv-status-code
  UV_OK
  UV_E2BIG
  UV_EACCES
  UV_EADDRINUSE
  UV_EADDRNOTAVAIL
  UV_EAFNOSUPPORT
  UV_EAGAIN
  UV_EAI_ADDRFAMILY
  UV_EAI_AGAIN
  UV_EAI_BADFLAGS
  UV_EAI_BADHINTS
  UV_EAI_CANCELED
  UV_EAI_FAIL
  UV_EAI_FAMILY
  UV_EAI_MEMORY
  UV_EAI_NODATA
  UV_EAI_NONAME
  UV_EAI_OVERFLOW
  UV_EAI_PROTOCOL
  UV_EAI_SERVICE
  UV_EAI_SOCKTYPE
  UV_EALREADY
  UV_EBADF
  UV_EBUSY
  UV_ECANCELED
  UV_ECHARSET
  UV_ECONNABORTED
  UV_ECONNREFUSED
  UV_ECONNRESET
  UV_EDESTADDRREQ
  UV_EEXIST
  UV_EFAULT
  UV_EFBIG
  UV_EHOSTUNREACH
  UV_EINTR
  UV_EINVAL
  UV_EIO
  UV_EISCONN
  UV_EISDIR
  UV_ELOOP
  UV_EMFILE
  UV_EMSGSIZE
  UV_ENAMETOOLONG
  UV_ENETDOWN
  UV_ENETUNREACH
  UV_ENFILE
  UV_ENOBUFS
  UV_ENODEV
  UV_ENOENT
  UV_ENOMEM
  UV_ENONET
  UV_ENOPROTOOPT
  UV_ENOSPC
  UV_ENOSYS
  UV_ENOTCONN
  UV_ENOTDIR
  UV_ENOTEMPTY
  UV_ENOTSOCK
  UV_ENOTSUP
  UV_EOVERFLOW
  UV_EPERM
  UV_EPIPE
  UV_EPROTO
  UV_EPROTONOSUPPORT
  UV_EPROTOTYPE
  UV_ERANGE
  UV_EROFS
  UV_ESHUTDOWN
  UV_ESPIPE
  UV_ESRCH
  UV_ETIMEDOUT
  UV_ETXTBSY
  UV_EXDEV
  UV_UNKNOWN
  UV_EOF
  UV_ENXIO
  UV_EMLINK
  UV_ENOTTY
  UV_EFTYPE
  UV_EILSEQ
  UV_ESOCKTNOSUPPORT
  UV_EUNATCH

pub fun message(code: uv-status-code): string
  match code
    UV_OK -> "no error"
    UV_E2BIG -> "argument list too long"
    UV_EACCES -> "permission denied"
    UV_EADDRINUSE -> "address already in use"
    UV_EADDRNOTAVAIL -> "address not available"
    UV_EAFNOSUPPORT -> "address family not supported"
    UV_EAGAIN -> "resource temporarily unavailable"
    UV_EAI_ADDRFAMILY -> "address family not supported"
    UV_EAI_AGAIN -> "temporary failure"
    UV_EAI_BADFLAGS -> "bad ai_flags value"
    UV_EAI_BADHINTS -> "invalid value for hints"
    UV_EAI_CANCELED -> "request canceled"
    UV_EAI_FAIL -> "permanent failure"
    UV_EAI_FAMILY -> "ai_family not supported"
    UV_EAI_MEMORY -> "out of memory"
    UV_EAI_NODATA -> "no address"
    UV_EAI_NONAME -> "unknown node or service"
    UV_EAI_OVERFLOW -> "argument buffer overflow"
    UV_EAI_PROTOCOL -> "resolved protocol is unknown"
    UV_EAI_SERVICE -> "service not available for socket type"
    UV_EAI_SOCKTYPE -> "socket type not supported"
    UV_EALREADY -> "connection already in progress"
    UV_EBADF -> "bad file descriptor"
    UV_EBUSY -> "resource busy or locked"
    UV_ECANCELED -> "operation canceled"
    UV_ECHARSET -> "invalid Unicode character"
    UV_ECONNABORTED -> "software caused connection abort"
    UV_ECONNREFUSED -> "connection refused"
    UV_ECONNRESET -> "connection reset by peer"
    UV_EDESTADDRREQ -> "destination address required"
    UV_EEXIST -> "file already exists"
    UV_EFAULT -> "bad address in system call argument"
    UV_EFBIG -> "file too large"
    UV_EHOSTUNREACH -> "host is unreachable"
    UV_EINTR -> "interrupted system call"
    UV_EINVAL -> "invalid argument"
    UV_EIO -> "i/o error"
    UV_EISCONN -> "socket is already connected"
    UV_EISDIR -> "illegal operation on a directory"
    UV_ELOOP -> "too many symbolic links encountered"
    UV_EMFILE -> "too many open files"
    UV_EMSGSIZE -> "message too long"
    UV_ENAMETOOLONG -> "name too long"
    UV_ENETDOWN -> "network is down"
    UV_ENETUNREACH -> "network is unreachable"
    UV_ENFILE -> "file table overflow"
    UV_ENOBUFS -> "no buffer space available"
    UV_ENODEV -> "no such device"
    UV_ENOENT -> "no such file or directory"
    UV_ENOMEM -> "not enough memory"
    UV_ENONET -> "machine is not on the network"
    UV_ENOPROTOOPT -> "protocol not available"
    UV_ENOSPC -> "no space left on device"
    UV_ENOSYS -> "function not implemented"
    UV_ENOTCONN -> "socket is not connected"
    UV_ENOTDIR -> "not a directory"
    UV_ENOTEMPTY -> "directory not empty"
    UV_ENOTSOCK -> "socket operation on non-socket"
    UV_ENOTSUP -> "operation not supported on socket"
    UV_EOVERFLOW -> "value too large to be stored in data type"
    UV_EPERM -> "operation not permitted"
    UV_EPIPE -> "broken pipe"
    UV_EPROTO -> "protocol error"
    UV_EPROTONOSUPPORT -> "protocol not supported"
    UV_EPROTOTYPE -> "protocol wrong type for socket"
    UV_ERANGE -> "result too large"
    UV_EROFS -> "read-only file system"
    UV_ESHUTDOWN -> "cannot send after transport endpoint shutdown"
    UV_ESPIPE -> "invalid seek"
    UV_ESRCH -> "no such process"
    UV_ETIMEDOUT -> "connection timed out"
    UV_ETXTBSY -> "text file is busy"
    UV_EXDEV -> "cross-device link not permitted"
    UV_UNKNOWN -> "unknown error"
    UV_EOF -> "end of file"
    UV_ENXIO -> "no such device or address"
    UV_EMLINK -> "too many links"
    UV_ENOTTY -> "inappropriate ioctl for device"
    UV_EFTYPE -> "inappropriate file type or format"
    UV_EILSEQ -> "illegal byte sequence"
    UV_ESOCKTNOSUPPORT -> "socket type not supported"
    UV_EUNATCH -> "protocol driver not attached"