fun gcd( x : int, y : int ) {
  if (x>y) then gcd(x - y, y)
  elif (x < y) then gcd(x,y - x)
  else x
}

fun main() {
  println(gcd(119,7))
}