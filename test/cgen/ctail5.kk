effect nondet {
  control flip() : bool
  control fail() : a
}

fun knapsack(w : int, vs : list<int> ) : <nondet,div> list<int> {
  if (w < 0)    then fail()
  elif (w == 0) then []
  else {
    val v = select(vs)
    Cons(v,knapsack(w - v, vs))
  }
}

fun select(xs) {
  match(xs) {
    Nil        -> fail()
    Cons(x,xx) -> if (flip()) then x else select(xx)
  }
}

val solutions = handler {
  return  x      -> [x]
  control fail() -> []
  control flip() -> resume(True) + resume(False)
}

fun show( xss : list<list<int>> ) : string {
  xss.show-list(fn(xs) { xs.show-list(core/show) } )
}

fun main() {
  solutions{ knapsack(3,[3,2,1]) }
}
