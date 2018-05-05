// --------------------------------------------------------
//  recursion through a resource  -- currently not caught!! :-(
// --------------------------------------------------------
effect heap { }

// type ediv :: (X,V,E) -> P

val heap = handler<heap>{ }

effect resource ref<a> in heap {
  fun get() : a       // (ref<a>) -> <heap,exn|e> a  with ediv(heap,a,e)
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

fun recurse() {
  use r = new-ref(id)
  fun foo(i : int) {
    println(".")
    (r.get)(i)
  }
  r.set(foo)
  foo(42)
  r.get
}

fun main() : <exn,console> int {
  using heap
  recurse()(1)
}
