effect state {
  fun get() : int
}

fun foo() {
  handle({
    foo()
  }) {
    get() -> resume(42)
  }
}