// --------------------------------------------------------
// resources
// check handling of a specific resource
// --------------------------------------------------------
effect heap { }

val heap = handler<heap>{ }

effect resource ref<a> in heap {
  fun get() : a       // (ref<a>) -> <heap,exn> a
  fun set( value : a ) : ()
}

val new-ref = handler resource (s) {
  get() -> resume(s,s)
  set(x) -> resume((),x)
}

fun bypass( r : ref<int>, action : () -> <heap|e> a ) : <heap|e> a {
  handle resource r (action) {
    get() -> resume(42)
    set(_x) -> resume(())
  }
}

fun main() {
  using heap
  use r = new-ref(50)
  using bypass(r)
  println( r.get() )
}
