// --------------------------------------------------------
// Async test: 
// tests if nested interleaved operations properly
// restore only the part of the handler stack up to the next
// inner-async-handler; test should output 0, 2b, 1, 2a, 1  
// --------------------------------------------------------
import std/async
import std/time/duration
import std/os/readline
effect state {
  fun get() : string
  fun append( x : string ) : ()
}

val state = handler(s) {
  return x -> (x,s)
  get()    -> resume(s,s)
  append(s')  -> resume(s + ", " + s',())
}


effect amb {
  flip() : bool
}
 
val amb = handler {
  return x -> [x]
  flip()   -> resume(False) + resume(True)
}

fun dowait(s : duration) {
  val secs = s.show
  println("and waiting at least " + secs)
  wait(s)
  println("(done waiting " + secs + ")")
  "waited " + secs
}

fun test() {
  println("starting")
  interleaved {
    amb{
      println("strand 1")
      if (flip()) then dowait(1.seconds) else dowait(1500.milli-seconds)
      append("1")
    }
  } {
    println("strand 2")
    interleaved {
      println("strand 2a")
      dowait(2.seconds)
      append("2a")
    } {
      println("strand 2b")
      append("2b")
    }
  }
  println("final state: " + get())
}

fun main() {
  state("0",test); () 
}