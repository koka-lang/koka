fun foo1() {
  head([1],1)
}

fun foo2() {
  val xs = [1]
  val ys = ["hi"].map(println)  
  match(xs) {
    Cons(x) -> xs
    _       -> [y]
  }
}
