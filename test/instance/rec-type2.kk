effect state {
  fun get() : int
}

fun foo(f) {
  handle({
    foo(f)
    f()
  }) {
    get() -> resume(42)
  }
}