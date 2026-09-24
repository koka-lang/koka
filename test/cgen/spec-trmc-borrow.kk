// Specializing `map(xs, ^f)` on `f` must drop `f`'s borrow from the new
// definition; otherwise TRMC's accumulator takes the borrowed slot, is copied on
// every extend, and the recursive call is no longer a tail call.
fun main()
  println(list(1, 200000).map(fn(x) x + 1).length)
