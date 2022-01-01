/* Shows the use of named effect handlers under an umbrella effect.
   This is used here to encode a local heap with first-class
   mutable references that can be dynamically created.
   Through rank-2 types, we can prevent references from escaping their
   heap and being accessed in another heap.

   For more info see the paper:
   "First-class named effect handlers", Daan Leijen, Ningning Xie, and Youyou Cong, 2020.

   Prerequisites:
   - syntax/var
   - syntax/with
   - handlers/basic
   - named-handlers/basic
*/
module heap-shallow

effect heap<s::S> {
  control new-ref(init:a) : ref<s,a>
}

named effect ref<s::S,a::V> in heap<s> {  // named under umbrella effect `:heap`
  fun get() : a                           // `:(ref<s,a>)   -> <heap<s>,pure> a`
  fun set( value : a ) : ()               // `:(ref<s,a>,a) -> <heap<s>,pure> ()`
  //fun wrong( f : () -> heap<s> ()) : ()
}

// private (named) handler instance for a reference
fun with-ref(init : a, action : ref<s,a> -> <heap<s>,div|e> b) : <heap<s>,div|e> b {
  var s := init
  with r = named handler {
    fun get() { s }
    fun set(x){ s := x }
    //fun wrong(r){ r.get; () }    
  }
  action(r)
}

// handler for a local heap
fun heap(action : forall<s> () -> <heap<s>,div|e> a) : <div|e> a {  // required rank-2 signature
  heapx(action)
}

fun heapx(action : () -> <heap<s>,div|e> a) : <div|e> a {  
  with rcontrol new-ref(init){ 
    with heapx
    with r = with-ref(init)
    rcontext.resume-shallow(r)
  }
  action()
}

// return a fresh reference from a function
fun dynamic-ref(init) {
  val x = new-ref(init)
  x
}

pub fun main() {
  with heap
  val r1 = new-ref(41)
  val r2 = dynamic-ref(True)
  println( r1.get() + (if (r2.get()) then 1 else 0) )
}
