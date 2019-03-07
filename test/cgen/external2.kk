
external inline foo( x : int ) : exn int {
  inline "#1"
}

fun main() {
  foo(42)
  print("done")
}
