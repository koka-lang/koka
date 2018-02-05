// --------------------------------------------------------
// Async demo: do concurrently
// --------------------------------------------------------
import std/async
import std/time/duration

effect amb {
  fun flip() : bool
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
  fun f() {
    dowait(1.seconds)
  }
  fun g() {
    wait(1.seconds)
    if (flip()) then dowait(2.seconds) else dowait(3.seconds)
  }
  val xs = interleaved( [f,g] )
  println("** hi " + xs.join(",") + "\n")
}

fun main() {
  amb(test); ()
}
