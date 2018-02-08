// --------------------------------------------------------
// Async demo: do concurrently
// --------------------------------------------------------
import std/async
import std/time/duration
import std/os/readline

effect amb {
  fun flip() : bool
}

val amb = handler {
  return x -> [x]
  flip()   -> resume(False) + resume(True)
}

fun dowait(s : duration) {
  val secs = s.show + " seconds"
  println("and waiting at least " + secs)
  wait(s)
  println("(done waiting " + secs + ")")
  "waited " + secs
}

fun test() {
  try {
    println("starting")
    fun f() {
      println("what is your name?")
      readline()
      error("ouch")
    }
    fun g() {
      wait(1.seconds)
      if (flip()) then dowait(2.seconds) else dowait(3.seconds)
    }
    val xs = interleaved( [f,g] )
    println("hi " + xs.join(","))
  }
  fun(exn) { println("exception thrown: " + exn.show) }
}

fun main() {
  amb(test); ()
}
