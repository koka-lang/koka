// Test named ambient instances

ambient heap<s::S> { }

fun heap( action: forall<s> () -> <heap<s>|e> a ) : e a {
  with<heap> {}
  action()
}

ambient instance ref<s::S,a::V> in heap<s> {
  fun get() : a
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

fun main() {
  with heap
  with r1 = new-ref(40)
  with r2 : ref<_,string> = new-ref("hi")
  println(r1.get + r2.get.count)
}
