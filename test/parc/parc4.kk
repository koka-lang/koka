fun foo1() {
  head([1],1)
  /*
  val xs = [1]
  match(xs) {
    Cons(x) -> /* dup x; drop xs */ x
    _ -> /* drop xs */ 1
  }
  */
}

fun foo2(y) {
  val xs = [1]
  match(xs) {
    Cons(x) -> /* dup x; drop xs; drop y */ x
    _       -> /* drop xs */ y
  }
}
