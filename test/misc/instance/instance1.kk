// --------------------------------------------------------
// named instances
// check handling of a specific resource
// --------------------------------------------------------
ambient heap { }

val heap = with<heap>{ }

ambient instance ref<a> in heap {
  fun get() : a       // (ref<a>) -> <heap,exn> a
  fun set( value : a ) : ()
}

fun new-ref(init,action) {
  var s := init
  with r = instance {
    fun get()   { s }
    fun set(x)  { s := x }
  }
  action(r)
}

fun bypass( r : ref<int>, action : () -> <heap|e> a ) : <heap|e> a {
  with instance(r) {
    fun get() { 42 }
    fun set(_x) { () }
  }
  action()
}

fun main() {
  with heap
  with r = new-ref(50)
  with bypass(r)
  println( r.get() )
}
