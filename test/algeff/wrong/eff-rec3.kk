 
effect state<a> {
  fun get() : a
  fun set(x : a) : ()
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
