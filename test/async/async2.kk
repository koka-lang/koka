// --------------------------------------------------------
// Async demo: combine with ambiguity
// --------------------------------------------------------
import std/async
import std/os/readline

effect amb {
  control flip() : bool
}
 
val amb = handler {
  return x -> [x]
  control flip()   -> resume(False) ++ resume(True)
}


fun test() {
  println("what is your name?")
  val name = readline()
  if (flip()) then println("and your age?") else println("and your gender?")
  val rest = readline()
  println("hi " ++ name ++ ", you are " ++ rest)  
}

fun main() {
  amb(test)
  () 
}
