// --------------------------------------------------------
// resources
// test with safe resources using rank-2 types to prevent
// escaping the correct scope
// --------------------------------------------------------

effect heap<s::S> { }

fun heap( action : forall<s> () -> <heap<s>|e> a ) : e a {
  handle<heap<_>>(action) { }
}

effect resource ref<s::S,a::V> in heap<s> {
  fun get() : a
  fun set( value : a ) : ()
}

val new-ref = handler resource(s) {
  get() -> resume(s,s)
  set(x) -> resume((),x)
}

fun foo() {
  use r = new-ref(36)
  r  // ouch, the resource escapes the handler scope
}

fun main() {
  using heap
  val r = foo()
  r.get.println   // this should throw
}
