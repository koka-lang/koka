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


/*
fun map-acc-reuse( xs, f, acc ) {
  match(xs) {
    Nil -> acc.reverse
    Cons(x,xx) -> {
      dup(x); dup(xx) 
      ys = drop_reuse(xs)
      y = f(x)
      match (yielding) { 
        True  -> drop(ys); drop(y); yield_extend( ... )
        False -> xx.map-acc(y, at(ys, Cons(f(z),Cons(f(x),acc))
      }
    }
  }
}
*/
