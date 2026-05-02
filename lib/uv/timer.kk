
module uv/timer
pub import std/time/duration
pub import std/time/timestamp
import std/num/ddouble
import std/num/int32
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

// Start a repeating timer. The callback fires after `interval` milliseconds
// and then repeats every `interval` milliseconds.
//
// Backend note: libuv's `uv_timer_start` accepts independent `timeout` and
// `repeat` values, while JS/WASM `setInterval` only takes a single period.
// To keep behavior identical across backends we expose only the case where
// they are the same (the least common denominator).
pub extern start(^t: timer, interval: int64, cb: () -> io-noexn ()): io-noexn error<()>
  c  "kk_timer_start"
  js "_start_timer"

// Stop the timer.
pub extern stop(^t: timer): io-noexn ()
  c  "kk_timer_stop"
  js inline "_stop_timer(#1)"

// Internal: schedule a one-shot timer. The callback fires once after
// `timeout` milliseconds; the timer's callback is automatically released
// after firing.
extern start-once(^t: timer, timeout: int64, cb: () -> io-noexn ()): io-noexn error<()>
  c  "kk_timer_start_once"
  js "_start_timer_once"

// Schedules `cb` to be invoked once after `ms` milliseconds.
// Returns a handle that can be passed to `clear-timeout` to cancel the timer
// before it fires; once it fires the handle's resources are released
// automatically (no explicit clear needed).
pub fun set-timeout(cb: () -> io-noexn (), ms: int32): io-noexn any
  val t = timer-init()
  match t.start-once(ms.int64, cb)
    Ok()       -> t.@toany
    Error(exn) -> impossible("set-timeout failed: " ++ exn.message)

// Cancels a timer scheduled by `set-timeout`. Safe to call after the timer
// has already fired (it becomes a no-op then).
pub extern clear-timeout(t: any): io-noexn ()
  c  "kk_clear_timeout"
  js inline "_stop_timer(#1)"

// Creates a timer that repeats every `d` duration and calls `f` with the timer as argument.
//
// The timer stops repeating when `f` returns `False`.
pub fun timer(d: duration, f: (timer) -> io-noexn bool): io timer
  val ms = d.milli-seconds.int64
  val t = timer-init()
  val res = t.start(ms) fn()
    if !f(t) then
      t.stop()
    ()
  match res
    Ok() -> t
    Error(exn) -> exn.throw-exn
