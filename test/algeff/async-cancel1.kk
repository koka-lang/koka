// --------------------------------------------------------
// Async demo
// --------------------------------------------------------
import std/async
import std/time/duration
import std/os/readline

fun main() {
  val name = timeout(1.seconds) {
    println("what is your name?")
    readline()
  }
  println("hi '" + name.default("<timed out>") + "'")
}