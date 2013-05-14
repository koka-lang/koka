fun main() {
  print("hi");  print("world")
  print("!")
}


fun map(f,xs) {
  match(xs) {
    Nil -> Nil
    Cons(x,xx) -> { y = f(x); yy = map(f,xx); Cons(y,yy) }
  }
}
