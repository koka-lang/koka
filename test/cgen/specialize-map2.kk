fun map-int(xs : list<int>, f : int -> int) : list<int> {
  match (xs) {
    Nil -> Nil
    Cons(x, xx) -> Cons(x.f, xx.map-int(f))    
  }
}

public fun test(y : int) {
  [1,2,3].map-int(fn(x){ x + y })   // free variable
}

public fun main() {
  test(2).show.println
}
