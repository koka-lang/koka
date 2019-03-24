// test shadow warnings
fun f(xs) {
  fun len(xs) {
    match(xs) {
      Cons(_,xs) -> len(xs)
      _ -> 0
    }
  }
  len(xs)
}
