// test lifting of monadic functions
// using mymap both effectful and directly
fun mymap( f, xs ) {
  match(xs) {
    Nil -> Nil
    Cons(x,xx) -> Cons(f(x),mymap(f,xx))
  }
}

effect state<s> {
  fun get() : s
  fun set(s : s) : ()
}

val state = handler(s) {
  get() -> resume(s,s)
  set(s') -> resume(s',())
}

fun incr(i) {
  val s = get()
  set(s+1)
  i+s
}


type func<e::E> {
  Func (f : (int -> e int, list<int>) -> e list<int> )
}

val myfunc = Func(mymap)

fun bar(g) {
  match(myfunc) {
    Func(f) -> f(g,[1,2])
  }
}

fun main() {
  bar(id).head(0).println
  state(41,{bar(incr)}).head(0).println
}