// Test resource handler

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

fun main() {
  using heap
  use r1 = new-ref(40)
  use r2 : ref<_,string> = new-ref("hi")
  println(r1.get + r2.get.count)
}