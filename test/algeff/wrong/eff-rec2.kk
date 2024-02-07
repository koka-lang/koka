// --------------------------------------------------------
//  recursion through a resource  -- currently not caught!! :-(
// --------------------------------------------------------
effect heap { }

val heap = handler<heap>{ }

named effect ref<a> in heap {
  fun xget() : a       // (ref<a>) -> <heap,exn|e> a  with ediv(heap,a,e)
  fun set( value : a ) : ()
}

fun new-ref(init,action) {
  var s := init
  with r = named handler {
    fun xget()  { s }
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
    (r.xget)(i)
  }
  r.set(foo)
  foo(42)
  r.xget
}

fun main() : <exn,console> int {
  with heap
  recurse()(1)
}
