// test shadow warnings
function f(xs) {
  function len(xs) {
    match(xs) {
      Cons(_,xs) -> len(xs)
      _ -> 0
    }
  }
  len(xs)
}
