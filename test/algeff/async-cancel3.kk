// --------------------------------------------------------
// Async demo
// --------------------------------------------------------
import std/async
import std/time/duration
import std/os/readline

fun main() {
  val mbx = timeout(2.seconds) {
    interleaved {
      println("what is your name?")
      readline()
    }
    {
      println("just waiting..")
      wait(1.seconds)
      println("done waiting")
      "waited"
    }
  }
  println("done: " + mbx.default(("nothing","")).fst)
}