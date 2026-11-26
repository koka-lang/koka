// a closure that reads `x`, called in the right-hand side of `x := e`
fun f() : int
  var x := 1
  val g = fn() x
  x := x + g()
  x
