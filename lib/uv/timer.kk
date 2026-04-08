
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

// Creates a timer that repeats every `d` duration and calls `f` with the timer as argument.
//
// The timer stops repeating when `f` returns `False`.
pub fun timer(d: duration, f: (timer) -> io-noexn bool): io timer
  val ms = d.milli-seconds.int64
  val t = timer-init()
  val res = t.start(ms, ms) fn()
    if !f(t) then
      t.stop()
    ()
  match res
    Ok() -> t
    Error(exn) -> exn.throw-exn
