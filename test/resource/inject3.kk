// Test inject over exceptions (should insert "inject-exn")
fun foo( action : () -> e int ) : e int {
  try {
    inject<exn>(action)  
    error("test")
    60
  }
  fun(exn) {
    52
  }
}

fun main() {
  try-default(42){ foo{ error("should throw") }}.println
}