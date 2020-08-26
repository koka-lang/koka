
fun mapx( xs : list<a>, f : a -> b ) : list<b> {
  match(xs) {
    Cons(x,xx) -> Cons(f(x),xx.mapx(f))
    Nil        -> Nil
  }
}
