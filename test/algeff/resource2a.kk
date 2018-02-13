// Test empty effect with rank-2 wrapper

effect heap<s::S> { 
}

val heap = fun(action:forall<s> () -> <heap<s>|_e> _a) {
  handle<heap<_s>>(action) { }
}


fun foo() {
  using heap
  42
}

fun main() {
  println(foo())
}
