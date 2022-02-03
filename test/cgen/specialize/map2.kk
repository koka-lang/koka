fun map-int(xs : list<int>, f : int -> int) : list<int> {
  match (xs) {
    Nil -> Nil
    Cons(x, xx) -> Cons(x.f, xx.map-int(f))    
  }
}

pub fun test(y : int) {
  [1,2,3].map-int(fn(x){ x + y })   // free variable
}

pub fun main() {
  test(2).show.println
}
