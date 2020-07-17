fun foo1() {
  head([1],1)
}

fun foo2(y) {
  val xs = [1]
  match(xs) {
    Cons(x) -> x
    _       -> y
  }
}
