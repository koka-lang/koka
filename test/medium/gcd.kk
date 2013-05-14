function gcd( x : int, y : int )
{
  if (x>y) gcd(x - y, y)
  else {
    if (x<y) gcd(x,y - x)
        else x
  }
}

function main() {
  println(gcd(119,7))
}