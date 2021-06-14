import std/time/timer

fun map-int(xs : list<int>, f : int -> int) : list<int> {
  match (xs) {
    Nil -> Nil
    Cons(x, xx) -> Cons(x.f, xx.map-int(f))    
  }
}

public fun test(xs : list<int>, y : int) {
  xs.map-int(fn(x){ x + y })   // free variable
}

public fun main() {
  val xs = list(1,10000000)
  val ys = print-elapsed({ test(xs,2) }, "time")
  ys.sum.println
}
