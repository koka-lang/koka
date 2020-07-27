fun test(x:list<int>): list<int> {
  val y = match(x) {
    Cons -> x
    Nil -> /* drop x */ []
  }
  return y
}
