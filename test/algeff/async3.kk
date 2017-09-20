// --------------------------------------------------------
// Async demo: do concurrently
// --------------------------------------------------------
import std/async
import std/os/readline

fun test() {
  println("let's start")
  val (name,age) = xinterleaved( 
    { println("what is your name?")
      readline()
    },
    { println("what is your age?")
      readline()
    }
  )
  println("hi " + name + ": " + age )
}

fun main() {
  test()
}
