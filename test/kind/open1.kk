open type test {
  X
}

extend type test {
  Y
}

function test(x) {
  match(x) {
    Y -> println("its a Y")
  }
}