// --------------------------------------------------------
// Async demo
// --------------------------------------------------------
import std/async
import std/time/duration
import std/os/readline

fun main() {
    val mbx = timeout(4.seconds) {
        interleaved {
              println("what is your name?")
              wait(3.seconds)
              println("done waiting name")
              "waited1"
        }  {
              println("just waiting..")
              wait(3.seconds)
              println("done waiting")
              "waited"
        }
    }
    println("***done: " + mbx.default(("nothing","")).fst)
}
