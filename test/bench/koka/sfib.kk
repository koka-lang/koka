module sfib

import std/num/int32

fun sfibx(n : int32, x1 : int32, x2 : int32) : div int32 
  if n <= 0.int32
    then x1
  elif x2 > 1000000000.int32
    then sfibx(n.dec, 1.int32, 2.int32)
    else sfibx(n.dec, x2 - x1, x1 * x2 + x1)

fun sfib(n : int)
  sfibx(n.int32,1.int32,2.int32).int

pub fun main()
  sfib(50000000).println

