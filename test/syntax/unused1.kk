function onSuccess( hndler : () -> e (), action : () -> e a ) : e a
{
  val x = action()
  hndler()
  return x
}

function main() {
  1
}