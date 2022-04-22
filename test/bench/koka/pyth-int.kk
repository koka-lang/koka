module pyth-int

pub fun pyth(n : int ) : int
  fold-int(1, n/3, 0) fn(x,xcount)
    val xx = x*x
    fold-int(x.inc, n/2, xcount) fn(y,ycount)
      val yy = y*y
      fold-while-int( y.inc, n/2, ycount) fn(z, zcount)
        val zz = z*z
        if (xx+yy == zz) then Just(zcount + 1)
        elif (xx+yy >= zz) && (x+y+z <= n) then Just(zcount)
        else Nothing
  
pub fun main()
  pyth(4000).println