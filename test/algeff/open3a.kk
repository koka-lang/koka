// test lifting of cps functions

fun mymap( f, xs ) {
  match(xs) {
    Nil -> Nil
    Cons(x,xx) -> Cons(f(x),mymap(f,xx))
  }
}

fun myid(x) {
  x
}

fun test() {
  mymap(id,[42])
}

fun main() {
  test().head.maybe(0).show.println
}