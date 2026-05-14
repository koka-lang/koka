// Integration test for the uv/timer module: a repeating timer that stops
// after N fires, plus a one-shot timeout via std/async/wait.
import std/async
import std/time/duration

fun main()
  with default-async-uv
  val res = try
    val n = ref(0)
    timer(50.milli-seconds) fn(_t)
      n := !n + 1
      println("tick " ++ (!n).show)
      !n < 3 // stop after 3 fires
    wait(50.milli-seconds)
    println("after first wait")
    wait(200.milli-seconds)
    println("after second wait")
  println(res.show)
