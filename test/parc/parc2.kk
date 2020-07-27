fun match1(xs:list<int>, y:int, z :int) {
  /* val t = */
  match(xs) {
    Cons(x) | x == z -> /* dup x; drop xs; drop y */ x
    Cons(x) | x == y -> /* drop xs; */ y
    Cons | y == z -> /* drop xs; drop y */ 2
    _ -> /* drop xs */ y
  }
  /* drop z; */
  /* t */
}
