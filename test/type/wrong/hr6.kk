fun g (x : (forall <a> a -> a) -> int) {
  x(id)
}

fun f(x : int -> int) {
  x(1)
}

val wrong = g(f)  // invariant
