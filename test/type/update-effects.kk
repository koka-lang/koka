// The right-hand side of `x := e` can use any effect in `ioc` and define functions, as long as it
// does not use the other local variables in the scope of `x`.
fun spin(n : int) : div int
  if n <= 0 then 0 else spin(n - 1)

pub fun main()
  var x := [1,2]
  var n := 0
  x := x.map(fn(v) v * 2)
  x := Cons(spin(3) + unique() * 0, x)
  x := (if True then { println("update"); Cons(1, x) } else x)
  x := (if True then { fun g(v) v + 1; x.map(g) } else x)
  n ::= n + x.length
  if n < 0 then throw("negative")
  println(x.show ++ " " ++ n.show)
