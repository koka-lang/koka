// Concatenate the result lists from applying a function to all elements.
fun xflatmap( xs: list<a>, f : a -> e list<b> ) : e list<b>
{
  fun flatmap-pre( ys, zs ) {
    match(ys) {
      Cons(y,yy) -> Cons(y,flatmap-pre(yy,zs))
      Nil -> match(zs) {
        Cons(z,zz) -> flatmap-pre(f(z),zz)
        Nil -> Nil
      }
    }      
  }
  flatmap-pre([],xs)
}

// Concatenate the `Just` result elements from applying a function to all elements.
fun xflatmap-maybe( xs : list<a>, f : a -> e maybe<b> ) : e list<b> {
  match(xs) {
    Cons(x,xx) -> match(f(x)) {
      Just(y) -> Cons(y, xx.xflatmap-maybe(f))
      Nothing -> xx.xflatmap-maybe(f)
    }
    Nil -> Nil
  }
}
