// this gives a compiler error
// fun repeat-first(times : int, xs as Cons(x, _)) : exn list<_>
//   if times == 0 then
//     return Nil

//   Cons(x, repeat-first(unsafe-decreasing(times - 1), xs))

fun repeat-first(times : int, xs : list<_>) : list<_>
  if times == 0 then
    return Nil

  match xs
    Nil -> Nil
    Cons(x, _) -> Cons(x, repeat-first(unsafe-decreasing(times - 1), xs))

fun main()
  repeat-first(10, [1]).show.println