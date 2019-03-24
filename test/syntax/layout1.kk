fun main() {
  print("hi");  print("world")
  print("!")
}


fun map(f,xs) {
  match(xs) {
    Nil -> Nil
    Cons(x,xx) -> { val y = f(x); val yy = map(f,xx); Cons(y,yy) }
  }
}
