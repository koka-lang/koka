// --------------------------------------------------------
// Async demo
// --------------------------------------------------------
import std/async
import std/async/readline

fun main() {
  println("what is your name?")
  val name = readline()
  println("and your age?")
  val age = readline()
  println("hi " ++ name ++ ", you are " ++ age)
}