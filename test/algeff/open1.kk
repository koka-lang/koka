// test lifting of cps functions
function myid() : total ((x:a) -> e a) {
  return fun(x){ x }
}

val hs : forall<e> list<int -> e int> = [myid()]

function bar() {
  head(hs)(10)
}

function mymap( f, xs ) {
  match(xs) {
    Nil -> Nil
    Cons(x,xx) -> Cons(f(x),mymap(f,xx))
  }
}