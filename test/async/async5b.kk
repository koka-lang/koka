// --------------------------------------------------------
// Async test:
// tests if nested interleaved operations properly
// restore only the part of the handler stack up to the next
// inner-async-handler; test should output 0, 2b, 1, 2a (including 1)
// --------------------------------------------------------
import std/async
import std/time/duration

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


fun show-int( i : int ) : string {
  show(i)
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
    println("strand 1")
    dowait(1.seconds)
    append("1")
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
  state("0",test); ()
}
