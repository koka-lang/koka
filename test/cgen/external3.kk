
external inline foo( x : int ) : exn int {
  inline "#1"
}

fun main() {
  [42].map(foo).head
}
