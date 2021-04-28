// --------------------------------------------------------
// resources
// check handling of a specific resource
// --------------------------------------------------------
ambient heap {
  control new-ref(init:a) : ref<a>
}

ambient instance ref<a> in heap {
  fun get() : a       // (ref<a>) -> <heap,exn> a
  fun set( value : a ) : ()
}


fun with-ref(init,action) {
  var s := init
  with r = instance {
    fun get()   { s }
    fun set(x)  { s := x }
  }
  action(r)
}

val heap = with<heap>{
  control new-ref(init) { with-ref(init,resume) }
}

fun escape() {
  val x = new-ref(42)
  x
}

fun main() {
  with heap
  val y = escape()
  println( y.get() )
}
