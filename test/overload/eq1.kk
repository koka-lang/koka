
fun int/eq( x : int, y : int ) : bool
  (x == y)

fun char/eq( x : char, y : char ) : bool
  (x == y)

fun list/eq( xs : list<a>, ys : list<a>, ?eq : (a,a) -> bool ) : bool
  match xs
    Cons(x,xx) -> match ys
      Cons(y,yy) | eq(x,y) -> eq(xx,yy)
      _ -> False
    Nil -> ys.is-nil

fun example()
  eq([1,2],[1,3])


fun test1()
  eq(1,2)

fun test2(x)
  int/eq(x,x)

fun test3()
  eq([1,2],[1,3])

