// --------------------------------------------------------
// Async demo: do concurrently
// --------------------------------------------------------
import std/async
import std/os/readline

fun test() {
  println("let's start")
  val xs = async-all( [
    { println("what is your name?")
      readline()
    },
    { println("what is your age?")
      readline()
    }
  ])
  println("hi " + xs.join(","))  
}

fun main() {
  test()
}