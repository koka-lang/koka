// --------------------------------------------------------
// Async demo: do concurrently
// --------------------------------------------------------
import std/readline
import std/file

function test() {
  println("starting")
  function f() {
    println("what is your name?")
    readline()
  }
  function g() {
    println("and reading a file")
    val content = read-text-file("package.json")    
    println("(done reading file)")
    content
  }
  val xs = async-all( [f,g] )
  println("hi " + xs.join(","))  
}

function main() {
  test()
}