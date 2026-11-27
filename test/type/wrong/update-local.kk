// the right-hand side of `x := e` cannot use the other local variables in the scope of `x`
fun f() : int
  var y := 1
  var x := 1
  x := x + y
  x
