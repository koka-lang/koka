// A canceled `receive` should not consume a later value on the channel
import std/async
import std/time/duration

fun main()
  val ch : channel<int> = channel()
  val r = timeout(10.milli-seconds){ ch.receive }
  println("timed out: " ++ r.show)
  ch.emit(42)
  println("after timeout: " ++ ch.try-receive.show)

  // waiting receivers get values in order, skipping a canceled one
  // (the timeout only guards against hanging)
  val xs = timeout(500.milli-seconds)
    interleaved([
      { ch.receive },
      { firstof({ ch.receive }, { -1 }) },  // canceled right away
      { ch.receive },
      { ch.receive },
      { ch.emit(1); ch.emit(2); ch.emit(3); 0 }
    ])
  println("receivers: " ++ xs.show)
  println("remaining: " ++ ch.try-receive.show)
