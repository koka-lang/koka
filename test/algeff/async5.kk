// --------------------------------------------------------
// Async demo: do concurrently
// --------------------------------------------------------
import std/readline

effect amb {
  flip() : bool
}
 
val amb = handler {
  return x -> [x]
  flip()   -> resume(False) + resume(True)
}

fun dowait(s : int) {
  val secs = s.show + " seconds"
  println("and waiting at least " + secs)
  wait(s * 1000)
  println("(done waiting " + secs + ")")
  "waited " + secs
}

fun test() {
  println("starting")
  fun f() {
    println("what is your name?")
    readline()
  }
  fun g() {
    wait(1000)
    if (flip()) then dowait(2) else dowait(3)
  }
  val xs = async-all( [f,g] )
  println("hi " + xs.join(","))  
}

fun main() {
  amb(test); () 
}