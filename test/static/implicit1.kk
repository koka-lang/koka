effect state<s> {
  val get : s
}

effect test {
  val x : int
}

effect foo {
  fun b(): int
}

fun h() {
  handler(s = 2) {
    val x = { println("hello"); 3 }
  }
}

val h3 = handler(s = 3) {
  effect fun b() { resume(3, s) }
}

fun main() {
  h()({ x + x })
}
