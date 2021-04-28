open type test {
  A
}

extend type test {
  B(i : int)
}

extend type test {
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