// test shadow warnings
fun f(xs) {
  fun len(xs) {
    match(xs) {
      Cons(_,xx) -> len(xx)
      _ -> 0
    }
  }
  len(xs)
}
