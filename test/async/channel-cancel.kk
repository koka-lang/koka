// A canceled `receive` leaves nothing behind in the channel: a later value
// goes to a receiver that is still waiting, and a strand whose receive was
// canceled is never resumed again.
import std/async
import std/time/duration

// A value emitted after a receive timed out is kept for the next receive.
fun lost-value()
  val ch : channel<int> = channel()
  val r = timeout(10.milli-seconds){ ch.receive }
  ch.emit(42)
  println("after a timed out receive: " ++ r.show ++ ", then " ++ ch.try-receive.show)

// Waiting receivers get values in order, skipping one that was canceled.
fun fifo()
  val ch : channel<int> = channel()
  val xs = interleaved([
    { ch.receive },
    { firstof({ ch.receive }, { -1 }) },   // canceled at once
    { ch.receive },
    { ch.emit(1); ch.emit(2); 0 }
  ])
  println("receivers: " ++ xs.show)

// The interleaving that ran a canceled receive is still running when the
// value arrives: the ended strand must not run again.
fun no-second-resume()
  // (`interleaved` then discontinues, as one of its strands was canceled)
  val ch : channel<int> = channel()
  interleaved {
    with finally { println("strand 1: ended") }
    val v = ch.receive
    println("strand 1: got " ++ v.show)
  } {
    cancel-outstanding()   // cancels strand 1's receive
    ch.emit(7)
    wait(20.milli-seconds)
    println("strand 2: done, the channel holds " ++ ch.try-receive.show)
  }
  ()

fun main()
  lost-value()
  fifo()
  no-second-resume()   // last: it ends `main`
