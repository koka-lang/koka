fun test(xs:list<a>, ys:list<a>): list<a> {
  match(xs) {
    Cons(z) -> match(ys) {
                 Cons(z /* z0 */) -> /* dup z0; drop xs; drop ys */ [z /* z0 */]
                 Nil -> /* drop ys */ xs
               }
    Nil -> /* drop xs */ ys
  }
}
