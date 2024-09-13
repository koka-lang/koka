fun implicit-error-fallthrough(e: list<int>)
  match e
    Cons(_, Cons(2, Cons(3, Nil))) -> 100.show.println
    Cons(a, Cons(1, Cons(c, Nil))) -> (a + c).show.println
    Cons(_, Cons(b, _)) -> b.show.println
    Nil -> "Nothing".println

fun reflow(e: list<int>)
  match e
    Cons(a, Cons(1, Cons(c, Nil))) -> (a + c).show.println
    Cons(_, Cons(b, _)) -> b.show.println
    Cons(a, Nil) -> a.show.println
    Nil -> "Nothing".println

fun no-guard(e: list<int>)
  match e
    Cons(a, b) | a == 0 -> b.println
    Cons(b, c) | b == 1 -> c.println
    Nil -> e.println

fun named-subpatterns(e: list<list<int>>)
  match e
    Cons(n0 as Nil, Nil) -> n0.println
    Cons(n1 as Cons(0, Nil), Nil) -> n1.println

fun main()
  try {
    implicit-error-fallthrough([]) // Nothing
    implicit-error-fallthrough([3,2]) // 2
    implicit-error-fallthrough([2,1,2])  // 4
    implicit-error-fallthrough([1,2,3]) // 100
    implicit-error-fallthrough([1]) // pattern match failure
  } fn(err) { println(err.message) }

  reflow([]) // Nothing
  reflow([3,2]) // 2
  reflow([2,1,2]) // 4
  reflow([1]) // 1

  no-guard([0, 1]) // [1]
  no-guard([1, 2]) // [2]
  no-guard([]) // []

  named-subpatterns([[]]) // []
  named-subpatterns([[0]]) // [0]

