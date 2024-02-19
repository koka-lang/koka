/*---------------------------------------------------------------------------
  Copyright 2023 Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

pub import std/os/event-loop
import std/num/int32

extern import
  c file "signal-inline.c"

value struct uv-signal {internal: intptr_t}

extern uv-signal-init(): io-noexn uv-signal
  c "kk_uv_signal_alloc"
extern uv-signal-start(h: uv-signal, cb: (uv-signal) -> io-event (), signal: int32): io-event int32
  c "kk_uv_signal_start"
extern uv-signal-start-one-shot(h: uv-signal, cb: () -> io-event (), signal: int32): io-event int32
  c "kk_uv_signal_start_oneshot"
extern uv-signal-stop(h: uv-signal): io-noexn int32
  c "kk_uv_signal_stop"
pub extern signum(h: uv-signal): io-noexn int32
  c "kk_uv_signal_num"

pub val sSIGINT = ssSIGINT()
extern ssSIGINT(): int32
  c inline "SIGINT"

pub fun signal-start(signal: int32, cb: (uv-signal) -> io-event ()): io-event uv-status-code
  uv-signal-start(uv-signal-init(), cb, signal).status-code

pub fun signal-start-oneshot(signal: int32, cb: () -> io-event ()): io-event uv-status-code
  uv-signal-start-one-shot(uv-signal-init(), cb, signal).status-code

pub fun signal-stop(h: uv-signal): io-noexn uv-status-code
  uv-signal-stop(h).status-code