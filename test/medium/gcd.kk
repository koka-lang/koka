fun gcd( x : int, y : int ) {
  if (x>y) gcd(x - y, y)
  elif (x<y) gcd(x,y - x)
  else x
}

fun main() {
  println(gcd(119,7))
}