effect heap<s::S> {
  control new-ref(init:a) : ref<s,a>
}

effect instance ref<s::S,a::V> in heap<s> {    // named under umbrella
  fun get() : a               // (ref<s,a>) -> <heap<s>,pure> a
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

fun heap(action : forall<s> () -> <heap<s>|e> a) : e a {  // required rank-2 signature
  with<heap>{
    control new-ref(init) { with-ref(init,resume) }
  }
  action()
}

fun dynamic-ref() {
  val x = new-ref(42)
  x
}

fun main() : <pure,console> () {
  with heap
  val r = dynamic-ref()
  println( r.get() )
}
