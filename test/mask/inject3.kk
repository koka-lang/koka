// Test inject over exceptions (should insert "inject-exn")
fun foo( action : () -> e int ) : e int {
  try fn(exn) {
    52
  } {
    mask<exn>(action)
    throw("test")
    60
  } 
}

fun main() {
  try{ foo{ throw("should throw") }}.default(42).println
}