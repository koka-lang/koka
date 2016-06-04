// --------------------------------------------------------
// Async demo: do concurrently
// --------------------------------------------------------
import std/readline

function test() {
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

function main() {
  test()
}