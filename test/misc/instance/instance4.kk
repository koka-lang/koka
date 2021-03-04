ambient heap<s::S> { }

val heap = with<heap<_s>> { }

fun foo() {
  with heap
  42
}

fun main() {
  println(foo())
}
