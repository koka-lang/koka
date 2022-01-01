fun map-int(xs : list<int>, f : int -> int) : list<int> {
  match (xs) {
    Nil -> Nil
    Cons(x, xx) -> Cons(x.f, xx.map-int(f))    
  }
}

pub fun test() {
  [1,2,3].map-int(fn(x){ x + 1 })
}

pub fun main() {
  test().show.println
}
