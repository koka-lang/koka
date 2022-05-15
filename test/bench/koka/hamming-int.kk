// Euclid's gcd with subtraction
fun gcd( x : int, y : int ) : div int 
  if x > y 
    then gcd( x - y, y )
  elif x < y
    then gcd( x, y - x )
  else x

fun is-hamming( x : int ) : div bool
  gcd(x,42) == 1

fun hamming-last( upto : int ) : div int
  fold-int(1,upto,0) fn(i,acc) 
    if is-hamming(i) then i else acc

fun main()
  hamming-last(300000).println