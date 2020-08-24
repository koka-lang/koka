// TRICKY: need to dup x because xs is consumed by Cons-ctor
fun test(xs:list<int>) {
  match(xs) {
    Cons(x) -> /* dup(x) */ Cons(x /* box(x) */, xs)
    Nil -> /* drop xs */ Nil
  }
}
