/*---------------------------------------------------------------------------
  Copyright 2023 Tim Whiting.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
module uv/timer
pub import std/time/duration
pub import std/time/timestamp
import std/num/ddouble
import std/num/int64
import uv/utils

extern import
  c  file "inline/timer"
  cs file "inline/timer.cs"
  js file "inline/timer.js"

abstract struct timer (
  internal: any
)

pub extern timer-init(): io-noexn timer
  c inline "kk_timer_init(kk_context())"
  js inline "_init_timer()"
  cs inline ""

// Start the timer. timeout and repeat are in milliseconds.
//
// If timeout is zero, the callback fires on the next event loop iteration.
// If repeat is non-zero, the callback fires first after timeout milliseconds and then repeatedly after repeat milliseconds.
pub extern timer-start(^t: timer, timeout: int64, repeat: int64, cb: () -> io-noexn ()): io-noexn error<()>
  c  "kk_timer_start"
  js inline "_start_timer(#1,#2,#3,#4)"
  cs inline ""

pub extern timer-stop(t: timer): io-noexn ()
  c  "kk_timer_stop"
  js inline "_stop_timer(#1)"
  cs inline ""

// Stop the timer, and if it is repeating restart it using the repeat value as the timeout.
// If the timer has never been started before it returns UV_EINVAL
extern timer-again(^t: timer): io-noexn error<()>
  c  "kk_timer_again"
  wasm inline "kk_std_core_exn__new_Ok(kk_unit_box(kk_Unit), kk_context())"
  js inline ""
  cs inline ""

// Set the repeat interval value in milliseconds.
//
// The timer will be scheduled to run on the given interval,
// regardless of the callback execution duration, and will follow
// normal timer semantics in the case of a time-slice overrun.
//
// For example, if a 50ms repeating timer first runs for 17ms,
// it will be scheduled to run again 33ms later. If other tasks
// consume more than the 33ms following the first timer callback,
// then the next timer callback will run as soon as possible.
//
// NOTE: If the repeat value is set from a timer callback it does not immediately take effect.
// If the timer was non-repeating before, it will have been stopped. If it was repeating,
// then the old repeat value will have been used to schedule the next timeout
extern timer-set-repeat(^t: timer, repeat: int64): io-noexn ()
  c  "kk_timer_set_repeat"
  wasm inline "kk_Unit"
  js inline ""
  cs inline ""

extern timer-get-repeat(^t: timer): io-noexn int64
  c  "kk_timer_get_repeat"
  wasm inline "-1"
  js inline ""
  cs inline ""

// Get the timer due value or 0 if it has expired. -1 is returned on unsupported platforms
// The time is relative to uv_now()
extern timer-get-due-in(^t: timer): io-noexn int64
  c  "kk_timer_get_due_in"
  wasm inline "-1"
  js inline ""
  cs inline ""

// Creates a timer that repeats every `d` duration and calls `f` with the timer as argument.
//
// The timer stops repeating when `f` returns `False`.
pub fun timer(d: duration, f: (timer) -> io-noexn bool): io-noexn timer
  val ms = d.milli-seconds.int64
  val t = timer-init()
  t.timer-start(ms, ms) fn()
    if !f(t) then
      t.timer-stop()
    ()
  t
