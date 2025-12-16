import std/async
import uv/event-loop
import std/time/duration
import std/time/timestamp

fun main()
  with default-async-uv
  val res = try
    val x = ref(0)
    timer(1000.milli-seconds) fn(t)
      "Hello From Timer".println
      x := !x + 1
      if !x == 3 then False else True
    wait(1.seconds)
    "Before timeout".println
    wait(5.seconds)
    "Hello After Timeout".println
  println("Done!")
  