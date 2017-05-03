// --------------------------------------------------------
// Async demo: do concurrently
// --------------------------------------------------------
import std/async
import std/os/readline
import std/os/path
import std/os/file

fun test() {
  println("starting")
  fun f() {
    println("what is your name?")
    readline()
  }
  fun g() {
    println("and reading a file")
    val content = read-text("package.json".path)    
    println("(done reading file)")
    content
  }
  val xs = interleaved( [f,g] )
  println("hi " + xs.join(","))  
}

fun main() {
  test()
}