// a function parameter called in the update: the update closes its effect within `ioc`, so an
// argument that reads `x` does not fit
fun f() : int
  var x := 1
  val apply = fn(h : () -> _ int) { x := x + h(); x }
  apply(fn() 0) + apply(fn() x)
