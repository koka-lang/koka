effect state<s> {
  val get : s
}

effect test {
  val x : int
}

val h = handler(s) {
  val x = { println("hello"); s }
}

fun main() {
  h(42, { x + x })
}
