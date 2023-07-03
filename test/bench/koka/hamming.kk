import std/num/int32

// Euclid's gcd with subtraction
fun gcd( x : int32, y : int32 ) : div int32
  if x > y 
    then gcd( x - y, y )
  elif x < y
    then gcd( x, y - x )
  else x

fun is-hamming( x : int32 ) : div bool
  gcd(x,42.int32) == 1.int32

fun hamming-last( upto : int ) : div int32
  fold-int32(1.int32,upto.int32,0.int32) fn(i,acc) 
    if is-hamming(i) then i else acc

fun main()
  hamming-last(300000).int.println