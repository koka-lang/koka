// --------------------------------------------------------
// Async demo
// --------------------------------------------------------
import std/async
import std/time/duration
import std/async/readline
import std/debug

fun main() {
  // launch();
  val name = timeout(2.seconds) {
    println("what is your name?")
    readline()
  }
  //launch();
  println("hi '" ++ name.default("<timed out>") ++ "'")
}