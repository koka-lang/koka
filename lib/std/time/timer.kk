/*----------------------------------------------------------------------------
   Copyright 2012-2021, Microsoft Research, Daan Leijen

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/

/* High resolution timer.
*/
module std/time/timer

pub import std/os/uv
import std/num/float64
import std/num/ddouble
import std/time/duration
import std/time/instant

extern import
  c  file "timer-inline.c"
  cs file "timer-inline.cs"
  js file "timer-inline.js"

// -----------------------------------------------------------
// Ticks
// -----------------------------------------------------------

// Return a high-resolution time stamp in fractional SI seconds.
// The duration is guaranteed to be monotonically increasing
// and have at least millisecond resolution.
pub fun ticks() : ndet duration
  val (secs,frac) = xticks()
  duration(secs,frac)

extern xticks() : ndet (float64,float64)
  c  "kk_timer_ticks_tuple"
  cs "_Timer.Ticks"
  js "_ticks"

// Return the smallest time difference in seconds that `ticks` can measure.
pub fun ticks-resolution() : ndet duration
  duration(xticks-resolution())

// Return the smallest time difference in seconds that `ticks` can measure.
extern xticks-resolution() : ndet float64
  c  "kk_timer_dresolution"
  cs "_Timer.TicksResolution"
  js "_ticks_resolution"

// Return the number of fractional seconds that it takes to evaluate `action`.
pub fun elapsed( action : () -> <ndet|e> a ) : <ndet|e> (duration,a)
  val t0 = ticks()
  val x = action()
  val t1 = ticks()
  (t1 - t0, x)

// Measure the number of fractional seconds that it takes to evaluate `action`, and print `msg` postfixed with the
// measured time in millisecond resolution.
pub fun print-elapsed( action : () -> <ndet,console|e> a, msg : string = "elapsed" ) : <ndet,console|e> a
  val (t,x) = elapsed(action)
  println( msg ++ " " ++ t.show(3) )
  x

abstract struct timer (
  internal: intptr_t
)

pub extern timer-init(): iouv-noexn timer
  c inline "kk_timer_init(kk_context())"

// Start the timer. timeout and repeat are in milliseconds.
//
// If timeout is zero, the callback fires on the next event loop iteration. 
// If repeat is non-zero, the callback fires first after timeout milliseconds and then repeatedly after repeat milliseconds.
pub extern timer-start(t: timer, timeout: int64, repeat: int64, cb: () -> iouv-noexn ()): iouv-noexn error<()>
  c  "kk_timer_start"

pub extern timer-stop(t: timer): iouv-noexn ()
  c  "kk_timer_stop"

// Stop the timer, and if it is repeating restart it using the repeat value as the timeout.
// If the timer has never been started before it returns UV_EINVAL
extern timer-again(t: timer): iouv-noexn error<()>
  c  "kk_timer_again"

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
extern timer-set-repeat(t: timer, repeat: int64): iouv-noexn ()
  c  "kk_timer_set_repeat"

extern timer-get-repeat(t: timer): iouv-noexn int64
  c  "kk_timer_get_repeat"

// Get the timer due value or 0 if it has expired.
// The time is relative to uv_now()
extern timer-get-due-in(t: timer): iouv-noexn int64
  c  "kk_timer_get_due_in"

// Creates a timer that repeats every `d` duration and calls `f` with the timer as argument.
// 
// The timer stops repeating when `f` returns `False`.
pub fun timer(d: duration, f: (timer) -> iouv-noexn bool): iouv-noexn timer
  val ms = d.milli-seconds.int64
  val t = timer-init()
  t.timer-start(ms, ms) fn()
    if !f(t) then
      t.timer-stop()
    ()
  t