// https://github.com/koka-lang/koka/issues/399: imported by tail2.kk, which loads it from the .kki
import std/num/float64

fun min-by(l : list<a>, ordering : a -> float64, default : a) : a
  min-by-rec(l, ordering, default, flt-min, default)

fun min-by-rec(l : list<a>, ordering : a -> float64, default : a, smallest : float64, result : a) : a
  match l
    Nil -> result
    Cons(x, tail) ->
      if ordering(x) < smallest then
        min-by-rec(tail, ordering, default, ordering(x), x)
      else
        min-by-rec(tail, ordering, default, smallest, result)

pub noinline fun main()
  min-by([1.0, 2.0], id, 0.25)