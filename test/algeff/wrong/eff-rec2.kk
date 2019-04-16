// --------------------------------------------------------
//  recursion through a resource  -- currently not caught!! :-(
// --------------------------------------------------------
effect heap { }

val heap = handler<heap>{ }

effect instance ref<a> in heap {
  fun get() : a       // (ref<a>) -> <heap,exn|e> a  with ediv(heap,a,e)
  fun set( value : a ) : ()
}

fun new-ref(init,action) {
  var s := init
  with r = instance {
    fun get()  { s }
    fun set(x) { s := x }
  }
  action(r)
}

/*
fun xget( r : ref<a> ) : <heap,exn|e> a  with (ediv<handled<heap>,a,e>) {
    get(r)
}
*/

fun recurse()  {
  with r = new-ref(id)
  fun foo(i : int) {
    println(".")
    (r.get)(i)
  }
  r.set(foo)
  foo(42)
  r.get
}

fun main() : <exn,console> int {
  with heap
  recurse()(1)
}
