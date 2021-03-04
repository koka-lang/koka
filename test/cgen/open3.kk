open type test {
  X
}

extend type test {
  Y
}

fun test(x) {
  match(x) {
    Y -> println("its a Y")
  }
}      

fun main() {
  test(Y).print
}