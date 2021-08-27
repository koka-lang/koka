
fun map-poly(xs : list<a>, f : a -> e b) : e list<b> {
  match (xs) {
    Nil -> Nil
    Cons(x, xx) -> Cons(x.f, xx.map-poly(f))    
  }
}

public fun test(xs : list<a>, y : int, f : a -> int) {
  xs.map-poly(fn(x){ f(x) + y })   // free variable  
}

public fun main() {
  list(1,10).test(5, inc).sum.println
}
