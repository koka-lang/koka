/*---------------------------------------------------------------------------
  Copyright 2023 Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

module uv/stream
pub import uv/event-loop
pub import std/core/bytes
pub import uv/utils

extern import
  c file "inline/stream.c"

pub value struct uv-stream { internal : any };

// Casts the stream to the underlying uv handle type
pub inline extern uv-handle(tcp: uv-stream): io-noexn uv-handle
  c inline "kk_uv_utils__new_Uv_handle(#1.internal, kk_context())"

// Shutdown the outgoing (write) side of a duplex stream.
// It waits for pending write requests to complete.
// The stream handle should refer to a initialized stream.
// The `callback` is called after shutdown is complete.
pub extern shutdown(hnd: uv-stream, callback: (uv-status-code) -> io-noexn ()): io-noexn ()
  c "kk_uv_shutdown"

// Start listening for incoming connections.
// backlog indicates the number of connections the kernel might queue, same as listen(2).
// When a new incoming connection is received the `callback` is called.
pub extern listen(stream: uv-stream, backlog: int32, callback: (uv-status-code) -> io-noexn ()): io-noexn ()
  c "kk_uv_listen"

// This call is used in conjunction with `listen()` to accept incoming connections.
// Call this function after receiving a connection callback to accept the connection.
// Before calling this function the client handle must be initialized. < 0 return value indicates an error.
// When the connection callback is called it is guaranteed that this function will complete successfully the first time.
// If you attempt to use it more than once, it may fail.
// It is suggested to only call this function once per connection callback.
pub extern accept(server: uv-stream, client: uv-stream): io-noexn uv-status-code
  c "kk_uv_accept"

// Read data from an incoming stream. The `callback` will be made several times until there is no more data to read or `read-stop()` is called.
// Returns `UV_EALREADY` when called twice, and `UV_EINVAL` when the stream is closing.
pub extern read-start(stream: uv-stream, callback: (bytes) -> io-noexn ()): io-noexn ()
  c "kk_uv_read_start"

// Stop reading data from the stream. The read callback will no longer be called.
// This function is idempotent and may be safely called on a stopped stream.
// A non-zero return indicates that finishing releasing resources may be pending on the next input event on that TTY on Windows, and does not indicate failure.
pub extern read-stop(stream: uv-stream): io-noexn uv-status-code
  c "kk_uv_read_stop"

// Write data to stream. Buffers are written in order.
pub extern stream/write(stream: uv-stream, data: list<bytes>, callback: (uv-status-code) -> io-noexn ()): io-noexn ()
  c "kk_uv_write"

// Extended write function for sending handles over a pipe.
// The pipe must be initialized with ipc == 1.
// send_handle must be a TCP, pipe and UDP handle on Unix, or a TCP handle on Windows, which is a server or a connection (listening or connected state). Bound sockets or pipes will be assumed to be servers.
pub extern send/write(hnd: uv-stream, data: list<bytes>, send-handle: uv-stream, callback: (uv-status-code) -> io-noexn ()): io-noexn ()
  c "kk_uv_write2"

// Same as `stream/write`, but will not queue a write request if it cannot be completed immediately
// Will return either:
// * > 0: number of bytes written (can be less than the supplied buffer size).
// * < 0: negative error code (UV_EAGAIN is returned if no data can be sent immediately).
pub extern stream/try-write(hnd: uv-stream, data: list<bytes>): io-noexn int32
  c "kk_uv_try_write"

// Same as `stream/try_write` an extended write function for sending handles over a pipe
// Try to send a handle is not supported on Windows, where it returns UV_EAGAIN.
pub extern send/try-write(hnd: uv-stream, data: list<bytes>, send-handle: uv-stream): io-noexn int32
  c "kk_uv_try_write2"

// Returns whether the stream is readable
pub inline extern is-readable(hnd: uv-stream): io-noexn bool
  c "kk_uv_is_readable"

// Returns whether the stream is writable
pub inline extern is-writable(hnd: uv-stream): io-noexn bool
  c "kk_uv_is_writable"

// Enable or disable blocking mode for a stream.
//
// When blocking mode is enabled all writes complete synchronously. The interface remains unchanged otherwise,
//  e.g. completion or failure of the operation will still be reported through a callback which is made asynchronously.
// Currently only works on Windows for uv pipe handles. On UNIX platforms, all uv_stream_t handles are supported.
pub inline extern set-blocking(hnd: uv-stream, blocking: bool): io-noexn uv-status-code
  c "kk_uv_stream_set_blocking"

// Returns the write queue size of a uv stream
pub inline extern write-queue-size(hnd: uv-stream): io-noexn int32
  c "kk_uv_stream_get_write_queue_size"
