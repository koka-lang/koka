// --------------------------------------------------------
// resources
// test with safe resources using rank-2 types to prevent
// escaping the correct scope
// --------------------------------------------------------

// Test resource handler

effect filesys<s::S> { }

fun filesys( action : forall<s> () -> <filesys<s>|e> a ) : e a {
  handle<filesys<_>>(action){ }
} 

effect resource file<s::S> in filesys<s> {
  fun read() : string
}


fun fake-file(content,action) {
  handle resource (action) {
    return x -> x
    read() -> resume(content)
  }
}


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
  using filesys
  use r1 = new-ref(36)
  use r2 : ref<_,string> = new-ref("hi")
  use f = fake-file("test")
  println( r1.get + r2.get.count + f.read.count)
}
