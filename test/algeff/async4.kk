// --------------------------------------------------------
// Async demo: do concurrently
// --------------------------------------------------------
import std/readline

function test() {
  println("starting")
  function f() {
    println("what is your name?")
    readline()
  }
  function g() {
    println("and waiting at least 3 secs")
    wait(3000)
    println("(done waiting)")
    "timing"
  }
  val xs = async-all( [f,g] )
  println("hi " + xs.join(","))  
}

function main() {
  test()
}