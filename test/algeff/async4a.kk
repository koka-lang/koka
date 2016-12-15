// --------------------------------------------------------
// Async demo: do concurrently
// --------------------------------------------------------
import std/readline

fun main() {
  println("starting")
  val xs = async-all( [
    { println("what is your name?")
      readline()
    },
    { println("and waiting at least 3 secs")
      wait(3000)
      println("(done waiting)")
      "timing"
    }
  ])
  println("hi " + xs.join(", "))  
}
