 
effect state {
  fun get() : (int -> state int)
  fun set(x : int -> state int) : ()
}

val new-ref = handler(s) {
  get() -> resume(s,s)
  set(x) -> resume((),x)
}

fun recurse() {
  using new-ref(id)
  fun foo(i : int) {
    get()(i)
  }
  set(foo)
  foo(42)
}
