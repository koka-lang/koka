/*---------------------------------------------------------------------------
  Copyright 2023 Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

module std/os/stream
pub import std/os/uv
pub import std/data/bytes

extern import
  c file "stream-inline.c"

pub value struct uvStream { internal : intptr_t };

pub inline extern uv-handle(tcp: uvStream): io-noexn uvHandle
  c inline "kk_std_os_uv__new_UvHandle(#1.internal, kk_context())"

pub extern shutdown(hnd: uvStream, callback: (uvStatusCode) -> io-event ()): io-event ()
  c "kk_uv_shutdown"

pub extern listen(stream: uvStream, backlog: int32, callback: (uvStatusCode) -> io-event ()): io-event ()
  c "kk_uv_listen"

pub extern accept(server: uvStream, client: uvStream): io-noexn uvStatusCode
  c "kk_uv_accept"

pub extern read-start(stream: uvStream, callback: (bytes) -> io-event ()): io-event ()
  c "kk_uv_read_start"

pub extern read-stop(stream: uvStream): io-noexn uvStatusCode
  c "kk_uv_read_stop"

pub extern write(stream: uvStream, data: list<bytes>, callback: (uvStatusCode) -> io-event ()): io-event ()
  c "kk_uv_write"

pub extern send/write(hnd: uvStream, data: list<bytes>, send-handle: uvStream, callback: (uvStatusCode) -> io-event ()): io-event ()
  c "kk_uv_write2"

pub extern try-write(hnd: uvStream, data: list<bytes>): io-noexn uvStatusCode
  c "kk_uv_try_write"

pub extern send/try-write(hnd: uvStream, data: list<bytes>, send-handle: uvStream): io-noexn uvStatusCode
  c "kk_uv_try_write2"

pub inline extern is-readable(hnd: uvStream): io-noexn bool
  c "kk_uv_is_readable"

pub inline extern is-writable(hnd: uvStream): io-noexn bool
  c "kk_uv_is_writable"

pub inline extern set-blocking(hnd: uvStream, blocking: bool): io-noexn uvStatusCode
  c "kk_uv_stream_set_blocking"

pub inline extern get-write-queue-size(hnd: uvStream): io-noexn int32
  c "kk_uv_stream_get_write_queue_size"