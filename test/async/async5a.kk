// --------------------------------------------------------
// Async demo: do concurrently
// --------------------------------------------------------
import std/async
import std/num/ddouble
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
    dowait(4.seconds)
  }
  fun g() {
    if (flip()) then dowait(1.seconds) else dowait(1500.milli-seconds)
  }
  fun h() {
    wait(1.seconds)
    if (flip()) then dowait(2.seconds) else dowait(3.seconds)
  }
  val xs = interleaved( [f,g,h] )
  println("hi " + xs.join(","))
}

fun main() {
  amb(test); ()
}
