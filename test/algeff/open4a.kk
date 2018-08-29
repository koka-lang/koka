// test lifting of monadic functions
// using mymap both effectful and directly
effect state<s> {
  fun get() : s
  fun set(s : s) : ()
}

val state = handler(s) {
  get() -> resume(s,s)
  set(s') -> resume((),s')
}

fun incr(i) {
  val s = get()
  set(s+1)
  i+s
}


type func<e::E> {
  Func (f : (list<int>,int -> e int) -> e list<int> )
}

val myfunc = Func(map)

fun bar(g) {
  match(myfunc) {
    Func(f) -> f([1,2],g)
  }
}

fun main() {
  bar(id).last(0).println
  state(39,{bar(incr)}).last(0).println
}