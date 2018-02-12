effect heap<s::S> { 
}

val heap = handler<heap<_s>> { 
}

fun foo() {
  using heap
  42
}

fun main() {
  println(foo())
}
