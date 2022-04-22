module pyth

import std/num/int32

pub fun pyth(n : int32 ) : console int32
  fold-int32(1.int32, n/3.int32, 0.int32) fn(x,xcount)
    val xx = x*x
    fold-int32(x.inc, n/2.int32, xcount) fn(y,ycount)
      val yy = y*y
      fold-while-int32( y.inc, n/2.int32, ycount) fn(z:int32, zcount:int32)
        val zz = z*z
        if (xx+yy == zz) then Just(zcount + 1.int32)
        elif (xx+yy >= zz) && (x+y+z <= n) then Just(zcount)
        else Nothing
  
pub fun main()
  pyth(4000.int32).int.println