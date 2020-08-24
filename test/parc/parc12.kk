fun test(x:int):int {
  // dup x
  val y = match(x + 1 /* x' */) {
    2 -> /* drop x, x' */ 3
    _ -> /* drop x' */ x
  }
  return y
}
