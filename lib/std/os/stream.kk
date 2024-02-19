/*---------------------------------------------------------------------------
  Copyright 2023 Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

module std/os/stream
pub import std/os/event-loop
pub import std/data/bytes

extern import
  c file "stream-inline.c"

pub value struct uv-stream { internal : intptr_t };

pub inline extern uv-handle(tcp: uv-stream): io-noexn uv-handle
  c inline "kk_std_os_event_dash_loop__new_uv_handle(#1.internal, kk_context())"

pub extern shutdown(hnd: uv-stream, callback: (uv-status-code) -> io-event ()): io-event ()
  c "kk_uv_shutdown"

pub extern listen(stream: uv-stream, backlog: int32, callback: (uv-status-code) -> io-event ()): io-event ()
  c "kk_uv_listen"

pub extern accept(server: uv-stream, client: uv-stream): io-noexn uv-status-code
  c "kk_uv_accept"

pub extern read-start(stream: uv-stream, callback: (bytes) -> io-event ()): io-event ()
  c "kk_uv_read_start"

pub extern read-stop(stream: uv-stream): io-noexn uv-status-code
  c "kk_uv_read_stop"

pub extern stream/write(stream: uv-stream, data: list<bytes>, callback: (uv-status-code) -> io-event ()): io-event ()
  c "kk_uv_write"

pub extern send/write(hnd: uv-stream, data: list<bytes>, send-handle: uv-stream, callback: (uv-status-code) -> io-event ()): io-event ()
  c "kk_uv_write2"

pub extern stream/try-write(hnd: uv-stream, data: list<bytes>): io-noexn uv-status-code
  c "kk_uv_try_write"

pub extern send/try-write(hnd: uv-stream, data: list<bytes>, send-handle: uv-stream): io-noexn uv-status-code
  c "kk_uv_try_write2"

pub inline extern is-readable(hnd: uv-stream): io-noexn bool
  c "kk_uv_is_readable"

pub inline extern is-writable(hnd: uv-stream): io-noexn bool
  c "kk_uv_is_writable"

pub inline extern set-blocking(hnd: uv-stream, blocking: bool): io-noexn uv-status-code
  c "kk_uv_stream_set_blocking"

pub inline extern write-queue-size(hnd: uv-stream): io-noexn int32
  c "kk_uv_stream_get_write_queue_size"