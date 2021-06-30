fun test(xs:list<int>) {
  match(xs) {
    ys as Cons -> ys
    Nil -> /* drop xs */ []
  }
}
