fun map-poly(xs : list<a>, f : a -> e b) : e list<b> {
  match (xs) {
    Nil -> Nil
    Cons(x, xx) -> Cons(x.f, xx.map-poly(f)) 
  }
}

pub fun test(xs : list<int>, y : int) {
  xs.map-poly(fn(x){ x + y })   // free variable
}

pub fun main() {
  list(1,10).test(5).show.println
}
