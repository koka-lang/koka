// test operators with existential bindings

effect ex<s> {
  fun identity(x : a, i : s) : a
}

val ex = handler(s) {
  identity( x ) -> resume(x,s)
}

fun test() {
  identity(42,'a')
}

fun main() {
  ex(test).println
}