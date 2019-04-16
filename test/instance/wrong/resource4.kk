// --------------------------------------------------------
// resources
// check if a resource is handled with its type
// --------------------------------------------------------

effect heap { }

val heap = handler<heap>{ }

effect resource ref<a> in heap {
  fun get() : a
  fun set( value : a ) : ()
}

val new-ref = handler resource (s) {
  get() -> resume(s,s)
  set(x) -> resume((),x)
}

fun wrong( r : ref<a>, action : () -> <heap,exn|e> b ) : <heap,exn|e> b {
  handle resource r (action) {
    get() -> resume(42)
    set(_x) -> resume(())
  }
}

fun main() {
  using heap
  use r = new-ref(False)
  using wrong(r)
  println( r.get() : bool )
}
