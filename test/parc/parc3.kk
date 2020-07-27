// Test value boxing
fun foo(x, y) {
  /* drop y */
  x
}

fun match1(xs:list<maybe<int>>, y:int) {
  match(xs) {
    Cons(Just(x)) | x == y -> /* dup x; drop xs; drop y */ x
    Cons(Nothing) -> /* drop xs; drop y */ 2
    _ -> /* drop xs */ y
  }
}
