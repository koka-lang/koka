import std/time
import std/async

fun main()
  val x = ref(0)
  timer(1000.milli-seconds) fn(t)
    "Hello From Timer".println;
    x := !x + 1
    if !x == 3 then False else True
  wait(1.seconds)
  "Before timeout".println
  wait(10.seconds)
  "Hello After Timeout".println