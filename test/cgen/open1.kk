type open test {
  A
}

type extend test {
  B(i : int)
}

type extend test {
  C(j : int)
}

fun foo(t) {
  match(t) {
    B(i) -> i
    _    -> 42
  }
}

fun main() {
  foo(C(2)).print
}