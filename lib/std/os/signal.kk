/*---------------------------------------------------------------------------
  Copyright 2023 Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

pub import std/os/uv
import std/num/int32

extern import
  c file "signal-inline.c"

value struct uvSignal {internal: intptr_t}

extern uvSignalInit(): io-noexn uvSignal
  c "kk_uv_signal_alloc" 
extern uvSignalStart(h: uvSignal, cb: (uvSignal) -> io-event (), signal: int32): io-event int32
  c "kk_uv_signal_start"
extern uvSignalStartOneShot(h: uvSignal, cb: () -> io-event (), signal: int32): io-event int32
  c "kk_uv_signal_start_oneshot"
extern uvSignalStop(h: uvSignal): io-noexn int32
  c "kk_uv_signal_stop"
pub extern signum(h: uvSignal): io-noexn int32
  c "kk_uv_signal_num"

pub val sSIGINT = ssSIGINT()
extern ssSIGINT(): int32
  c inline "SIGINT"

pub fun signal-start(signal: int32, cb: (uvSignal) -> io-event ()): io-event uvStatusCode
  uvSignalStart(uvSignalInit(), cb, signal).status-code

pub fun signal-start-oneshot(signal: int32, cb: () -> io-event ()): io-event uvStatusCode
  uvSignalStartOneShot(uvSignalInit(), cb, signal).status-code

pub fun signal-stop(h: uvSignal): io-noexn uvStatusCode
  uvSignalStop(h).status-code