fun test(y) {
  val xs = [1]
  match(xs) {
    Cons(x) -> /* dup x; drop xs; drop y */ x
    _       -> /* drop xs */ y
  }
}
