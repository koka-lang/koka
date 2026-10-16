import std/num/random
import std/num/float64

// A 32-bit read followed by a float64 read must also work at odd word offsets.
fun main()
  val _ = srandom-int32()
  val x = srandom-float64()
  println(x >= 0.0 && x < 1.0)
