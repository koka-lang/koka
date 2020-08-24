fun test(xs:list<int>) {
  match(xs) {
    Cons(xs /* xs0 */) as ys -> /* dup xs0; drop xs */ xs /* xs0 */
    Nil -> /* drop xs */ 1
  }
}
