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
module heap

scoped effect heap<s::S> 
  ctl new-ref(init:a) : ref<s,a>


named effect ref<s::S,a::V> in heap<s>    // named under umbrella effect `:heap`
  fun get() : a                           // `:(ref<s,a>)   -> <heap<s>,pure> a`
  fun set( value : a ) : ()               // `:(ref<s,a>,a) -> <heap<s>,pure> ()`


// private (named) handler instance for a reference
fun with-ref(init,action) 
  var s := init
  with r <- named handler 
    fun get()  s 
    fun set(x) s := x 
  action(r)


// handler for a local heap
fun heap(action : forall<s> () -> <heap<s>,div|e> a) : <div|e> a   // required rank-2 signature
  with ctl new-ref(init) with-ref(init,resume) 
  action()


// return a fresh reference from a function
fun dynamic-ref(init)
  val x = new-ref(init)
  x


pub fun main()
  with heap
  val r1 = new-ref(41)
  val r2 = dynamic-ref(True)
  println( r1.get() + (if r2.get() then 1 else 0) ) 
