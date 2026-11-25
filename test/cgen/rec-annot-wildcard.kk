// A recursive definition whose annotation has a wildcard effect: generalizing
// the group quantifies the effect in front of the signature's `a`, so the
// recursive call `walk<a>` becomes `walk<e,a>`. It must stay one flat type
// application (a nested `walk<e><a>` crashed `typeOf`).
fun walk( xs : list<int>, act : () -> _ a ) : _ a
  match xs
    Nil -> act()
    Cons(_, rest) -> walk(rest, act)

fun main()
  println(walk([1, 2], fn() "done"))
