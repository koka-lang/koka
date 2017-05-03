// --------------------------------------------------------
// Async demo
// --------------------------------------------------------
import std/async

fun main() {
  println("wait 1")
  yield(1.seconds)
  println("wait 2")
  yield(1.seconds)
  println("wait 3")
  yield(1.seconds)
  println("done")
}