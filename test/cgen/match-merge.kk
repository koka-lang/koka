fun implicit-error-fallthrough(e: list<int>)
  match e
    Cons(a, Cons(1, Cons(c, Nil))) -> (a + c).show.println
    Cons(_, Cons(b, _)) -> b.show.println
    Nil -> "Nothing".println

fun reflow(e: list<int>)
  match e
    Cons(a, Cons(1, Cons(c, Nil))) -> (a + c).show.println
    Cons(_, Cons(b, _)) -> b.show.println
    Cons(a, Nil) -> a.show.println
    Nil -> "Nothing".println

fun main()
  try {
    implicit-error-fallthrough([1, 2, 3])
    reflow([1, 2, 3])
  } fn(err) { println("Error") }
  