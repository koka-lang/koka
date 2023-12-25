import std/num
import blah = std/num/float64

fun int/eq( x : int, y : int ) : bool
  x == y

fun float64/eq( x : float64, y : float64) : bool
  x == y

fun listint/eq( xs : list<int>, ys : list<int> ) : bool
  match xs
    Cons(x,xx) -> match ys
                    Cons(y,yy) | int/eq(x,y) -> listint/eq(xx,yy)
                    _ -> False
    Nil -> False


fun test1()
  std/num/float64/eq(1.0,2.0)
  blah/eq()
  // eq(1,2)

fun test2(y)
  eq([1],y)
