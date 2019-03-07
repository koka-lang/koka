type open test {
  X
}

type extend test {
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