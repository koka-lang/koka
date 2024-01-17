// Test inject over exceptions (should insert "inject-exn")
fun foo( action : () -> e int ) : e int {
  try {
    mask<exn>(action)
    throw("test")
    60
  } fn(exn) {
    52
  }
}

fun main() {
  try{ foo{ throw("should throw") }}.default(42).println
}