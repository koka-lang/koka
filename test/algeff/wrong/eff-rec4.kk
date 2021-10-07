
effect state {
  fun get() : (int -> state int)
  fun set(x : int -> state int) : ()
}

fun new-ref(init,action) {
  var s := init
  handle(action)
    fun get()  { s }
    fun set(x) { s := x }
}

fun recurse() {
  with new-ref(id)
  fun foo(i : int) {
    get()(i)
  }
  set(foo)
  foo(42)
}
