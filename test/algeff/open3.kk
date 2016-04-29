// test lifting of cps functions

function mymap( f, xs ) {
  match(xs) {
    Nil -> Nil
    Cons(x,xx) -> Cons(f(x),mymap(f,xx))
  }
}

function myid(x) {
  x
}

function test() {
  mymap(id,[1])
}