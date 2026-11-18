// A `timeout` in a strand that has awaited cancels only its own action, not the
// other strands or the caller.
import std/async
import std/time/duration

fun nested-timeout()
  val (a,b) = interleaved {
    wait(100.milli-seconds)
    "a"
  } {
    yield()
    val fast = timeout(1.seconds) { yield(); "fast" }
    val slow = timeout(10.milli-seconds) { wait(1.seconds); "slow" }
    fast.default("timed out") ++ "," ++ slow.default("timed out")
  }
  println("nested: " ++ a ++ "," ++ b)

// an outer timeout still cancels the strands of an interleaving
fun outer-timeout()
  val r = timeout(10.milli-seconds) {
    interleaved { wait(1.seconds); "a" } { yield(); wait(1.seconds); "b" }
  }
  println("outer: " ++ (if r.is-just then "not canceled" else "canceled"))

// a strand that is canceled can still use a `timeout` in its finalizer
fun finalizer-timeout()
  val r = timeout(10.milli-seconds) {
    with finally
      val x = timeout(1.seconds) { yield(); "flushed" }
      println("cleanup: " ++ x.default("timed out"))
    wait(1.seconds)
  }
  println("finalizer: " ++ (if r.is-just then "not canceled" else "canceled"))

fun main()
  nested-timeout()
  outer-timeout()
  finalizer-timeout()
  println("done, canceled: " ++ is-canceled().show)
