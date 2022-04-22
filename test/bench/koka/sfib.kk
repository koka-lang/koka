module sfib

import std/num/int32

fun sfibx(n : int32, x1 : int32, x2 : int32) : div int32 
  if n <= zero
    then x1
    else sfibx(n.dec, x2, (x1 + x2) / (x2 - x1))

fun sfib(n : int)
  sfibx(n.int32,2.int32,8.int32).int

pub fun main()
  sfib(100000000).println