// --------------------------------------------------------
// Async demo: do concurrently
// --------------------------------------------------------
import std/async
import std/time/duration
import std/os/readline

fun main() {
  println("starting")
  val xs = interleaved( [
    { println("what is your name?")
      readline()
    },
    { println("and waiting at least 3 secs")
      wait(3.seconds)
      println("(done waiting)")
      "timing"
    }
  ])
  println("hi " + xs.join(", "))  
}
