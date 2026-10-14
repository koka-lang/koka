// A recursive definition whose annotation has a wildcard effect: the body is
// checked with the effect free, and the generalized type quantifies it. The
// recursive call takes it as a type argument of its own.
fun walk( xs : list<int>, act : () -> _ a ) : _ a
  match xs
    Nil -> act()
    Cons(_, rest) -> walk(rest, act)

fun main()
  println(walk([1, 2], fn() "done"))
