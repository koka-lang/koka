
fun int/eq( x : int, y : int ) : bool
  (x == y)

fun float64/eq( x : float64, y : float64 ) : bool
  (x == y)

fun list/eq<a>( xs : list<a>, ys : list<a>, ?eq : (a,a) -> bool ) : bool
  match xs
    Cons(x,xx) -> match ys
      Cons(y,yy) | eq(x,y) -> eq(xx,yy)
      _ -> False
    Nil -> ys.is-nil

fun choose(x,y)
  if True then x else y


fun test1()
  eq(1,2)

fun test2(x)
  int/eq(x,x)

fun test3()
  eq([1,2],[1,3])

