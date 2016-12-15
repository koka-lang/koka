// --------------------------------------------------------
// Async demo: do concurrently
// --------------------------------------------------------
import std/readline


fun dowait(s : int) {
  val secs = s.show + " seconds"
  println("and waiting at least " + secs)
  wait(s * 1000)
  println("(done waiting " + secs + ")")
  "waited " + secs
}

fun test() {
  catch {
    println("starting")
    fun f() {
      println("what is your name?")
      readline()
      //error("ouch")
    }
    fun g() {
      dowait(3)
    }
    val xs = async-all( [f,g] )
    println("hi " + xs.join(","))  
  } 
   fun(exn) { 
    println("exception thrown: " + exn.show) 
  } 
}

fun main() {
  test()
}