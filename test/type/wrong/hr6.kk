function g (x : (forall <a> a -> a) -> int) {
  x(id)
}

function f(x : int -> int) {
  x(1)
}

val wrong = g(f)  // invariant
