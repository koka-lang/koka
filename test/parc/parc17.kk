fun test(xs:list<int>) {
  match(xs) {
    Cons as ys -> ys
    Nil -> /* drop xs */ []
  }
}
