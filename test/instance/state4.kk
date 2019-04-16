fun test(xs)
{
  match(xs) {
    Nil -> 0
    Cons -> {
      var i := 0
      xs.foreach fun(x) { i := i + 1 }
      i
    }
  }
}
