/*---------------------------------------------------------------------------
  Copyright 2012-2023, Microsoft Research, Daan Leijen. Brigham Young University, Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

module std/os/stream
pub import std/os/uv

extern import
  c file "stream-inline.c"

pub value struct uvStream { internal : intptr_t };

pub inline extern uvHandle(tcp: uvStream): iouv-noexn uvHandle
  c inline "kk_std_os_uv__new_UvHandle(#1.internal, kk_context())"

pub extern shutdown(hnd: uvStream, callback: (uvStatusCode) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_shutdown"

pub extern listen(stream: uvStream, backlog: int32, callback: (uvStatusCode) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_listen"

pub extern accept(server: uvStream, client: uvStream): iouv-noexn uvStatusCode
  c "kk_uv_accept"

pub extern read-start(stream: uvStream, callback: (bytes) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_read_start"

pub extern read-stop(stream: uvStream): iouv-noexn uvStatusCode
  c "kk_uv_read_stop"

pub extern write(stream: uvStream, data: list<bytes>, callback: (uvStatusCode) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_write"

pub extern write(hnd: uvStream, data: list<bytes>, send-handle: uvStream, callback: (uvStatusCode) -> iouv-noexn ()): iouv-noexn ()
  c "kk_uv_write2"

pub extern try-write(hnd: uvStream, data: list<bytes>): iouv-noexn uvStatusCode
  c "kk_uv_try_write"

pub extern try-write(hnd: uvStream, data: list<bytes>, send-handle: uvStream): iouv-noexn uvStatusCode
  c "kk_uv_try_write2"

pub inline extern is-readable(hnd: uvStream): iouv-noexn bool
  c "kk_uv_is_readable"

pub inline extern is-writable(hnd: uvStream): iouv-noexn bool
  c "kk_uv_is_writable"

pub inline extern set-blocking(hnd: uvStream, blocking: bool): iouv-noexn uvStatusCode
  c "kk_uv_stream_set_blocking"

pub inline extern get-write-queue-size(hnd: uvStream): iouv-noexn int32
  c "kk_uv_stream_get_write_queue_size"