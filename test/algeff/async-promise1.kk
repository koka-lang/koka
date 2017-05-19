// --------------------------------------------------------
// Async demo
// --------------------------------------------------------
import std/async
import std/time/duration
import std/os/readline

fun main() {
  val p = promise()
  val mbx = timeout(3.seconds) {
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
  println("exited " + (if (mbx.bool) then "normally" else "with timeout"))
}