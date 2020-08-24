fun joinsepx( xs : list<string>, sep : string ) : string
{
  fun join-acc( ys : list<string>, acc : string ) {
    match(ys) {
      Cons(y,yy) -> join-acc(yy, acc + sep + y)  // todo: use string builder
      Nil -> acc
    }
  }
  match(xs) {
    Nil -> ""
    Cons(x,xx) -> join-acc(xx,x)
  }
}
