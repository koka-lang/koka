// --------------------------------------------------------
// Async demo
// --------------------------------------------------------
import std/async
import std/time/duration
import std/os/readline

fun main() {
  val mbx = timeout(2.seconds) {
    interleaved([
    {
      println("just waiting..")
      wait(1.seconds)
      println("done waiting")
      "waited"
    },
    {
        println("just waiting2..")
        wait(4.seconds)
        println("done waiting2")
        "waited2"
    },{
      println("what is your name?")
      readline()
    }])

  }
  println("done: " + mbx.default(["nothing"]).join(","))
}
