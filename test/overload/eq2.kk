
fun eq_int( x : int, y : int ) : bool
  (x == y)

fun eq_float( x : float64, y : float64 ) : bool
  (x == y)

fun eq_list<a>( xs : list<a>, ys : list<a>, ?eq : (a,a) -> bool ) : bool
  match xs
    Cons(x,xx) -> match ys
      Cons(y,yy) | eq(x,y) -> eq_list(xx,yy)
      _ -> False
    Nil -> ys.is-nil

fun equal<a>( x : a, y : a, ?eq : (a,a) -> bool ) : bool
  eq(x,y)


fun test1()
  equal([1,2],[1,3]).println

