// --------------------------------------------------------
// Async demo
// --------------------------------------------------------
import std/async
import std/time/duration

fun main() {
  println("wait 1")
  wait(1.seconds)
  println("wait 2")
  wait(1.seconds)
  println("wait 3")
  wait(1.seconds)
  println("done")
}