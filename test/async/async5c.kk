// --------------------------------------------------------
// Async test: 
// tests if nested interleaved operations properly
// restore only the part of the handler stack up to the next
// inner-async-handler; test should output 0, 2b, 1, 2a, 1  
// --------------------------------------------------------
import std/async
import std/time/duration
import std/os/readline
import std/debug

effect state {
  fun get() : string
  fun append( x : string ) : ()
}

fun state(init,action) {
  var s := init
  with {
    return(x)     { (x,s) }
    fun get()     { s }
    fun append(s'){ s := s ++ ", " ++ s' }
  }
  action()
}


effect amb {
  ctl flip() : bool
}
 
val amb = handler {
  return x         -> [x]
  ctl flip()   -> resume(False) ++ resume(True)
}


fun dowait(s : duration) {
  val secs = s.show
  println("and waiting at least " ++ secs)
  wait(s)
  println("(done waiting " ++ secs ++ ")")
  "waited " ++ secs
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
  println("final state: " ++ get())
}

fun main() {
  launch()
  state("0",test); () 
}