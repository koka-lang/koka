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
