pub import std/os/uv
import std/num/int32

extern import
  c file "signal-inline.c"

value struct uvSignal {internal: intptr_t}

extern uvSignalInit(): iouv-noexn uvSignal
  c "kk_uv_signal_alloc" 
extern uvSignalStart(h: uvSignal, cb: (uvSignal) -> iouv-noexn (), signal: int32): iouv-noexn int32
  c "kk_uv_signal_start"
extern uvSignalStartOneShot(h: uvSignal, cb: () -> iouv-noexn (), signal: int32): iouv-noexn int32
  c "kk_uv_signal_start_oneshot"
extern uvSignalStop(h: uvSignal): iouv-noexn int32
  c "kk_uv_signal_stop"
pub extern signum(h: uvSignal): iouv-noexn int32
  c "kk_uv_signal_num"

pub val sSIGINT = ssSIGINT()
extern ssSIGINT(): int32
  c inline "SIGINT"

pub fun signal-start(signal: int32, cb: (uvSignal) -> iouv-noexn ()): iouv-noexn uvStatusCode
  uvSignalStart(uvSignalInit(), cb, signal).toStatusCode

pub fun signal-start-oneshot(signal: int32, cb: () -> iouv-noexn ()): iouv-noexn uvStatusCode
  uvSignalStartOneShot(uvSignalInit(), cb, signal).toStatusCode

pub fun signal-stop(h: uvSignal): iouv-noexn uvStatusCode
  uvSignalStop(h).toStatusCode