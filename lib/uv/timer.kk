
module uv/timer
pub import std/time/duration
pub import std/time/timestamp
import std/num/ddouble
import std/num/int64
import uv/utils

extern import
  c  file "inline/timer"
  js file "inline/timer.js"

abstract struct timer
  internal: any

// Initialize a timer handle
pub extern timer-init(): io-noexn timer
  c inline "kk_timer_init(kk_context())"
  js inline "_init_timer()"

// Start the timer. 
//
// The parameters `timeout` and `repeat` are in milliseconds.
// If `timeout` is zero, the callback fires on the next event loop iteration.
// If `repeat` is non-zero
//   The C backend calls the callback first after `timeout` milliseconds and then repeatedly after `repeat` milliseconds.
//   JavaScript / WASM backend calls the callback repeatedly every `repeat` milliseconds
pub extern start(^t: timer, timeout: int64, repeat: int64, cb: () -> io-noexn ()): io-noexn error<()>
  c  "kk_timer_start"
  js inline "_start_timer(#1,#2,#3,#4)" 

// Stop the timer.
pub extern stop(^t: timer): io-noexn ()
  c  "kk_timer_stop"
  js inline "_stop_timer(#1)"

// Release the callback, which if it references the timer will allow it to be garbage collected.
pub extern release-callback(t: timer): io-noexn ()
  c  "kk_timer_finish"
  js inline ""

// Stop the timer, and if it is repeating restart it using the repeat value as the timeout.
// If the timer has never been started before it returns UV_EINVAL
extern again(^t: timer): io-noexn error<()>
  c  "kk_timer_again"
  js inline ""

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
extern set-repeat(^t: timer, repeat: int64): io-noexn ()
  c  "kk_timer_set_repeat"
  js inline ""

extern get-repeat(^t: timer): io-noexn int64
  c  "kk_timer_get_repeat"
  js inline ""

// Get the timer due value or 0 if it has expired. -1 is returned on unsupported platforms
// The time is relative to uv_now()
extern get-due-in(^t: timer): io-noexn int64
  c  "kk_timer_get_due_in"
  js inline ""

// Creates a timer that repeats every `d` duration and calls `f` with the timer as argument.
//
// The timer stops repeating when `f` returns `False`.
pub fun timer(d: duration, f: (timer) -> io-noexn bool): io timer
  val ms = d.milli-seconds.int64
  val t = timer-init()
  val res = t.start(ms, ms) fn()
    if !f(t) then
      t.stop()
      t.release-callback()
    ()
  match res
    Ok() -> t
    Error(exn) -> exn.throw-exn
