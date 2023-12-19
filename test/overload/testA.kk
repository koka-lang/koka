import std/num/float64

fun int/plus( x : int, y : int ) : int
  x + y

fun float64/plus( x : float64, y : float64 ) : float64
  testA/float64/pi() + x + y

fun float64/pi()
  3.14

fun plus( x : char, y : char ) : char
  x

fun test1()
  plus(1,2)
