fun id(x) {
  x
}

fun constant(x:a,y:b) :a {
  x
}

fun apply(f,x) {
  f(x)
}

fun map-acc( xs, f, acc ) {
  match(xs) {
    Nil -> acc.reverse
    Cons(x,xx) -> xx.map-acc(f,Cons(f(x),acc))
  }
}
