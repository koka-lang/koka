// Integration test for the uv/timer module: a repeating timer that stops
// after N fires, plus a one-shot timeout via std/async/wait.
import std/async
import std/time/duration

fun main()
  wait(50.milli-seconds)
  println("after first wait")
  wait(200.milli-seconds)
  println("after second wait")  
