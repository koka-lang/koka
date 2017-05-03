// --------------------------------------------------------
// Async demo: do concurrently
// --------------------------------------------------------
import std/async
import std/time/duration
import std/os/readline

fun test() {
  println("starting")
  fun f() {
    println("what is your name?")
    readline()
  }
  fun g() {
    println("and waiting at least 3 secs")
    wait(3.seconds)
    println("(done waiting)")
    "timing"
  }
  val xs = interleaved( [f,g] )
  println("hi " + xs.join(","))  
}

fun main() {
  test()
}