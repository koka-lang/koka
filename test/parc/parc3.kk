fun test(xs:list<int>, y:int, z :int) {
  match(xs) {
    Cons(x) | x == z -> /* dup x; drop xs; drop y; drop z */ x
    Cons(x) | x == y -> /* drop xs; drop z */ y
    Cons | y == z -> /* drop xs; drop y; drop z */ 2
    _ -> /* drop xs; drop z */ y
  }
}
