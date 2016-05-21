type open test {
  X
}

type extend test {
  Y
}

function test(x) {
  match(x) {
    Y -> println("its a Y")
  }
}      

function main() {
  test(Y).print
}