fun foo() {
  42
}

fun bar( action : () -> e int ) : int {
  action()
}

fun main() {
  bar(foo)
}