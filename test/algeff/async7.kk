// --------------------------------------------------------
// Async demo: do concurrently
// --------------------------------------------------------
import std/async
import std/time/duration
import std/os/readline


fun dowait(s : duration) {
  val secs = s.show + " seconds"
  println("and waiting at least " + secs)
  wait(s)
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
      dowait(3.seconds)
    }
    val xs = interleaved( [f,g] )
    println("hi " + xs.join(","))  
  } 
   fun(exn) { 
    println("exception thrown: " + exn.show) 
  } 
}

fun main() {
  test()
}