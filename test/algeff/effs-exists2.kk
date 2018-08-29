// test operators with existential bindings

effect ex {
  fun identity(x : a) : a
}

val ex = handler {
  identity( x ) -> resume(x)
}

fun test() {
  identity(42)
}

fun main() {
  ex(test).println
}