fun test(xs:list<int>) {
  match(xs) {
    Cons(ys as xs /* xs0 */) -> /* dup xs0; drop xs */ xs /* xs0 */
    Nil -> /* drop xs */ 1
  }
}
