fun test(xs:list<int>, y:int) {
  match(xs) {
    Cons(x,_) ->
      /* dup x; drop y */
      match(xs) {
        Cons -> /* drop xs */ x
        Nil -> /* drop x, xs */ 2
      }
    Cons -> /* drop y, xs */ 2
    _ -> /* drop xs */ y
  }
}
