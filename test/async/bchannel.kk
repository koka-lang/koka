// Bounded cross-thread channel (backpressure over xchannel).
//
// The main loop owns a `bchannel` with a small window; a `spawn-thread` worker
// opens the producer half on its own loop and emits far more items than the
// window, so it repeatedly fills its local backlog and suspends awaiting
// grants. Grants are minted by `receive` on the main loop as batches are
// consumed, so the window bounds unconsumed items end-to-end. Values arrive in
// order; `close` on both halves lets each loop drain so the program exits.
import std/async
import std/async/thread     // portable: spawn-thread / xchannel underneath
import std/async/bchannel

fun main()
  val b : bchannel<int> = bchannel(8)     // tiny window: forces backpressure
  val c = b.connect                        // the only value that crosses the spawn
  spawn-thread(fn()
    val s = c.sender                       // producer half, on the worker's loop
    list(1, 100).foreach fn(i)
      s.emit(i)                            // suspends when the backlog fills
    s.close                                // flush + await the full window back
  )
  val got = list(1, 100).map(fn(_) b.receive)
  println("received: " ++ got.length.show ++ ", in order: " ++ (got == list(1, 100)).show)
  println("sum: " ++ got.sum.show)
  b.close
