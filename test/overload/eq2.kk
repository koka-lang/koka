
fun int/eq( x : int, y : int ) : bool
  (x == y)

fun float64/eq( x : float64, y : float64 ) : bool
  (x == y)

fun list/eq<a>( xs : list<a>, ys : list<a>, ?eq : (a,a) -> bool ) : bool
  match xs
    Cons(x,xx) -> match ys
      Cons(y,yy) | eq(x,y) -> list/eq(xx,yy)
      _ -> False
    Nil -> ys.is-nil


fun equal<a>( x : a, y : a, ?eq : (a,a) -> bool ) : bool
  eq(x,y)


fun test1()
  equal([1,2],[1,3]).println

