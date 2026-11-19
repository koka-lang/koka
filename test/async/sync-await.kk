// An await whose callback is invoked synchronously, from inside its own
// setup, must still behave like an asynchronous completion -- in particular
// inside `firstof` and `interleaved` (previously the continuation after the
// `firstof` could be lost, or the strand that finished first ignored).
import std/async
import std/time/duration

fun sync-await( x : int ) : async int
  await1-noexn("sync") fn(resume) resume(x)

fun never() : async int
  await1-noexn("never") fn(_resume) ()

pub fun main()
  async
    val x = sync-await(1)
    val y = sync-await(2)
    println("plain: " ++ (x + y).show)
  async
    val b = firstof({ sync-await(5) }, { wait(1.milli-seconds); 3 })
    println("sync strand wins: " ++ b.show)
  async
    val b = firstof({ val _ = sync-await(1); never() }, { 3 })
    println("sync then never vs immediate: " ++ b.show)
  async
    val b = firstof({ val _ = sync-await(1); wait(5.milli-seconds); 1 }, { 3 })
    println("sync then wait vs immediate: " ++ b.show)
  async
    val (a, b) = interleaved({ sync-await(1) }, { sync-await(2) })
    println("interleaved: " ++ (a + b).show)
  println("done")
