type open test {
  A
}

type extend test {
  B(i : int)
}

type extend test {
  C(j : int)
}

function foo(t) {
  match(t) {
    B(i) -> i
    _    -> 42
  }
}

function main() {
  foo(C(2)).print
}