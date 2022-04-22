module pyth

import std/num/int32

pub fun pyth(n : int32 ) : int
  var count := 0
  for32(1.int32, n/3.int32) fn(x)
    val xx = x*x
    for32(x.inc, n/2.int32) fn(y)
      val yy = y*y
      for-while32( y.inc, n/2.int32) fn(z)
        val zz = z*z
        if (xx+yy == zz) then count := count + 1
        if (xx+yy < zz) || (x+y+z > n) 
          then Just(())
          else Nothing
      ()
  count

pub fun main()
  pyth(4000.int32).println