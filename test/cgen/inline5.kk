import std/num/float64

fun l(x : float64, y : float64)
  x + y
fun let(x, f) f(x)

fun main()
  1.1.l(1.1).l(1.1).println
  42.let(_ * 2).let(_ + 1).println


