// --------------------------------------------------------
// Async demo
// --------------------------------------------------------
import std/async
import std/time/duration
import std/os/readline

fun main() {
  val name = timeout(2.seconds) {
    println("just waiting..")
    wait(1.seconds)
    trace("done waiting")
    "waited"
  }
  println("hi '" + name.default("<timed out>") + "'")
}