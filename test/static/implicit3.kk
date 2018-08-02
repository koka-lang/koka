effect test {
  fun op1(): int
  fun op2(): int
}

val h = handler {
  fun op1() { 42 }
  effect fun op2() { 1 + resume(4)}
}
