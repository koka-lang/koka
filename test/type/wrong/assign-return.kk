// a `return` in the right-hand side of an assignment is an error
fun first-even( xs : list<int> ) : int
  var acc := 0
  acc := (match xs { Cons(x, _) | x.is-even -> return x; _ -> acc + 1 })
  acc
