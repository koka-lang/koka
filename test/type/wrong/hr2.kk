fun auto(f:forall<a> a -> e a) : e (forall<a> a -> e a) {
  f(f)
}

fun wrong(f) {
  auto(f)
}