// --------------------------------------------------------
// Async demo: do concurrently
// --------------------------------------------------------
import std/readline
import std/file

fun test() {
  println("starting")
  fun f() {
    println("what is your name?")
    readline()
  }
  fun g() {
    println("and reading a file")
    val content = read-text-file("package.json")    
    println("(done reading file)")
    content
  }
  val xs = async-all( [f,g] )
  println("hi " + xs.join(","))  
}

fun main() {
  test()
}