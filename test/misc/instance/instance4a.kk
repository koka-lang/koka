// Test empty effect with rank-2 wrapper

ambient heap<s::S> {
}

val heap = fn(action:forall<s> () -> <heap<s>|_e> _a) {
  with<heap<_s>> { }
  action()
}


fun foo() {
  with heap
  42
}

fun main() {
  println(foo())
}
