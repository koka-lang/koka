/* Asynchronous timers and the `std/async` API.

*/
module basic

import std/async
import std/time/duration
import std/time/timestamp


// `wait` cooperates with the event loop so other strands can run while
// it sleeps. Use `interleaved` to drive two strands concurrently.
pub fun example1()
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
pub fun example2()
  fun fast()
    wait(50.milli-seconds)
    "fast"
  fun slow()
    wait(500.milli-seconds)
    "slow"
  val r = firstof(slow,fast)
  println("winner: " ++ r)


pub fun main()
  example1()
  example2()
