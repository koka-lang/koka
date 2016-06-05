// test lifting of cps functions
fun myid() : ((x:a) -> e a) {
  return fun(x){ x }
}

type func<e::E> {
  Func (f : int -> e int)
}

val myfunc = Func(myid())

fun bar() {
  match(myfunc) {
    Func(f) -> f(42)
  }
}

fun main() {
  bar().show.println
}