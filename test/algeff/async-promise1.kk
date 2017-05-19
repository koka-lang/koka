// --------------------------------------------------------
// Async demo
// --------------------------------------------------------
import std/async
import std/time/duration
import std/os/readline

fun main() {
  val p = promise()
  timeout(3.seconds) {
    interleaved {
      println("what is your name?")
      p.resolve( readline() )
    }
    { 
      println("waiting for resolve..")
      val name = p.await()
      println("the name was: " + name)
    }
  }
  ()
}