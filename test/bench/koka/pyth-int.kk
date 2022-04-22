module pyth-int

pub fun pyth(n : int ) : int 
  var count := 0
  for(1, n/3) fn(x)
    val xx = x*x
    for(x+1, n/2) fn(y)
      val yy = y*y
      for-while( y+1, n/2) fn(z)
        val zz = z*z
        if (xx+yy == zz) then count := count + 1
        if (xx+yy < zz) || (x+y+z > n) 
          then Just(())
          else Nothing
      ()
  count

pub fun main()
  pyth(4000).println