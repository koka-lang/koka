effect test {
  fun op1(): int
  fun op2(): int
  val op3: int
}

val h1 = handler {
  op1() { 42 };
  op2() -> { 1 + resume(41)}
  op3 = 42
}

val h2 = handler {
  fun op1() { 42 };
  effect fun op2() -> 1 + resume(41)
  val op3 = 42
}

fun h3(f) {
  handler {
    fun op1() { f() };
    effect fun op2() -> 1 + resume(41)
    val op3 = 42
  }
}
