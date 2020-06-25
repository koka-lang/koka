type foo<a> {
  Fun( f:a -> a)
}

fun foo() : foo<int> {
  Fun(inc)
}

fun test() {
  match(foo()) {
    Fun(f) -> f(1)
  }
}

fun test2() {
  match([1]) {
    Cons(x) -> inc(x)
  }
}
