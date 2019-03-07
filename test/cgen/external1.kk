
external inline foo( x : int ) : int {
  inline "#1"
}

fun main() {
  apply(foo,42)
}
