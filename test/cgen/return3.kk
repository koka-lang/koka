
fun foo() {
  val x = if (True || False) then 42 else return 32
  x
}

fun main() {
  foo().print
}
