
pub fun test(xs : list<a>, y : int, f : a -> int) {
  xs.map(fn(x){ f(x) + y })   // free variable  
}

pub fun main() {
  list(1,10).test(5, inc).sum.println
}
