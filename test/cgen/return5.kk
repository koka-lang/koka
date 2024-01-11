fun pyth(n : int): int
  fold(1, n/3, 0) fn(x, xcount)
    val xx = x*x
    return fold(x.inc, n/2, xcount) fn(y, ycount)
      val yy = y*y
      return fold-while(y.inc, n/2, ycount) fn(z, zcount)
        val zz = z*z
        if (xx+yy == zz) then Just(zcount + 1)
        elif (xx+yy >= zz) && (x+y+z <= n) then Just(zcount)
        else Nothing

fun main()
  pyth(30).println