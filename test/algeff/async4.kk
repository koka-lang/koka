// --------------------------------------------------------
// Async demo: do concurrently
// --------------------------------------------------------
import std/readline

fun test() {
  println("starting")
  fun f() {
    println("what is your name?")
    readline()
  }
  fun g() {
    println("and waiting at least 3 secs")
    wait(3000)
    println("(done waiting)")
    "timing"
  }
  val xs = async-all( [f,g] )
  println("hi " + xs.join(","))  
}

fun main() {
  test()
}