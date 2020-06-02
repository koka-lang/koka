import std/time/timer

fun foldlx(xs,z,f)
{
  match(xs) {
    Cons(x,xx) -> foldlx(xx,f(z,x),f)
    Nil        -> z
  }
}

fun fold-counter(xs : list<int>, cnt : int, m : int) : int {
  match(xs) {
    Cons(x,xx) {
      if (x%5 == 0) 
       then xx.fold-counter(cnt + 1, max(m,x))
       else xx.fold-counter(cnt, max(m,x))
    }
    Nil -> cnt
  }
}

effect count {
  function increment() : ()
}

fun countH( action ) {
  var cnt := 0
  with return x -> cnt
  with fun increment() { cnt := cnt+1 }
  action()
}

fun fold-counterC( xs : list<int>, m : int) : <count> int {
  match(xs) {
    Cons(x,xx) {
      if (x%5 == 0) then increment() 
      xx.fold-counterC(max(m,x))      
    }
    Nil -> m
  }
}

fun test-direct(n : int) {
  list(1,n).fold-counter(0,0)
}

fun test-count(n : int) {
  countH{ list(1,n).fold-counterC(0) }
}


val n = 1000000
fun main() {
  print-elapsed({test-direct(n)}, "direct")
  print-elapsed({test-count(n)},  "handler")
}
