/*---------------------------------------------------------------------------
  Copyright 2023 Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

pub import uv/event-loop
import std/num/int32
pub import uv/utils

extern import
  c file "inline/signal.c"

// A uv signal handle
value struct uv-signal {internal: any }

// Create and initilize a uv signal handle
extern uv-signal-init(): io-noexn uv-signal
  c "kk_uv_signal_alloc"

// Start the handle with the given callback, watching for the given signal.
extern uv-signal-start(h: uv-signal, cb: (uv-signal) -> io-noexn (), signal: int32): io-noexn int32
  c "kk_uv_signal_start"

// Same functionality as uv-signal-start() but the signal handler is reset the moment the signal is received.
extern uv-signal-start-one-shot(h: uv-signal, cb: () -> io-noexn (), signal: int32): io-noexn int32
  c "kk_uv_signal_start_oneshot"

// Stop the handle, the callback will no longer be called.
extern uv-signal-stop(h: uv-signal): io-noexn int32
  c "kk_uv_signal_stop"

// Get the signal number the handle is monitoring
pub extern signum(^h: uv-signal): io-noexn int32
  c "kk_uv_signal_num"

// The sigint signal
pub val sSIGINT = ssSIGINT()
extern ssSIGINT(): int32
  c inline "SIGINT"

// Start watching for the given signal, calling the callback on the next event loop
pub fun signal-start(signal: int32, cb: (uv-signal) -> io-noexn ()): io-noexn uv-status-code
  uv-signal-start(uv-signal-init(), cb, signal).status-code

// Start watching for the given signal, calling the callback on the next event loop, and resetting the signal handler after the first callback
pub fun signal-start-oneshot(signal: int32, cb: () -> io-noexn ()): io-noexn uv-status-code
  uv-signal-start-one-shot(uv-signal-init(), cb, signal).status-code

// Stop watching for the signal
pub fun signal-stop(h: uv-signal): io-noexn uv-status-code
  uv-signal-stop(h).status-code
