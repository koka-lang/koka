/* Asynchronous timers and the `std/async` API.

   These examples assume an event loop is running -- the `main` function
   wraps every example in `default-async-uv` which sets up libuv on the
   C backend and is a no-op on the JavaScript backend.

   Prerequisites:
   - learn/basic
   - learn/with
*/
module async/timer

import std/async
import std/num/int32
import std/num/int64
import std/time/duration
import std/time/timestamp


// A repeating timer that prints a message every second.
// `timer(d, f)` invokes `f` on each tick; returning `False` stops it.
pub fun example1()
  val n = ref(0)
  timer(500.milli-seconds) fn(_t)
    n := !n + 1
    println("tick " ++ (!n).show)
    !n < 3 // stop after the third tick
  wait(2.seconds) // give the timer time to fire


// A one-shot timeout via `set-timeout` from `uv/timer`.
// The callback is released automatically after firing; 
// calling `clear-timeout` after the timeout elapses is a no-op.
pub fun example2()
  val tid = set-timeout(fn() println("fired (and self-cleaned)"), 200.int32)
  wait(500.milli-seconds)
  // Cancelling after firing is a safe no-op:
  clear-timeout(tid)


// `wait` cooperates with the event loop so other strands can run while
// it sleeps. Use `interleaved` to drive two strands concurrently.
pub fun example3()
  fun stranda()
    wait(100.milli-seconds)
    "from A"
  fun strandb()
    wait(50.milli-seconds)
    "from B"
  val (a,b) = interleaved(stranda, strandb)
  println("got: " ++ a ++ ", " ++ b)


// `firstof` races two actions and returns the first to complete; the
// loser is automatically cancelled. Combined with `wait`, this lets us
// build a `timeout` style operation.
pub fun example4()
  fun fast()
    wait(50.milli-seconds)
    "fast"
  fun slow()
    wait(500.milli-seconds)
    "slow"
  val r = firstof(fast, slow)
  println("winner: " ++ r)


// `timeout(duration, action)` is the prebuilt version of the firstof pattern:
// returns `Just(value)` if the action completes within `duration`, otherwise
// `Nothing` (and the action is cancelled).
pub fun example5()
  match timeout(100.milli-seconds, { wait(50.milli-seconds); 42 })
    Just(v) -> println("in time: " ++ v.show)
    Nothing -> println("timed out")
  match timeout(50.milli-seconds, { wait(200.milli-seconds); 42 })
    Just(v) -> println("in time: " ++ v.show)
    Nothing -> println("timed out")


// Manual control: `timer-init` + `start` + `stop` for the cases where
// you need to manage the timer lifecycle yourself.
pub fun example6()
  val t = timer-init()
  val n = ref(0)
  val res = t.start(100.int64) fn() n := !n + 1
  match res
    Ok()       -> ()
    Error(exn) -> impossible("manual timer failed to start: " ++ exn.message)
  wait(350.milli-seconds)
  t.stop()
  println("manual timer fired " ++ (!n).show ++ " times")


pub fun main()
  with default-async-uv
  // `example1` and `example6` use `timer(...)` and `throw-exn` which
  // surface `exn` in their type. We catch with `try` so the outer
  // event-loop action stays in `io-noexn`.
  val _ = try { example1(); example6() }
  example2()
  example3()
  example4()
  example5()
