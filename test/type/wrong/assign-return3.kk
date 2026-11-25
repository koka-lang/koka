// a `return` in the right-hand side of an assignment is an error, also to a reference
fun first-even( xs : list<int> ) : int
  val r = ref(0)
  r := (match xs { Cons(x, _) | x.is-even -> return x; _ -> 1 })
  !r
