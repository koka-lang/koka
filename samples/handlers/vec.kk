/* Shows how to associate a resource with a handler using rank-2 polymorphism.
   This example is inspired by the "packed ref" example from Derek Dryer's
   Milner Award lecture at POPL'18 (https://www.youtube.com/watch?v=8Xyk_dGcAwk).
*/
public module vec

// Abstract type of indices in a scope `:s` (with the scope kind `::S`)
abstract type ix<s::S> 
  Ix(index :int)

// A vector of strings  (with a parameter of kind "scope")
effect vec<s::S> 
  fun push( s : string ) : ix<s>
  fun lookup( ix : ix<s> ) : string


// Create a `vec` handler that safely only handles its own indices (parameterized by `:s`)
fun vec(action : forall<s> () -> <vec<s>|e> a ) : e a   // rank-2 signature required
  var xs : list<string> := []
  with handler
    fun push(s)
      val i = xs.length
      xs := xs ++ [s]
      Ix(i) 
    fun lookup(ix)
      xs[ix.index].default("")   // index is always valid!  
  action()


fun main() 
  with vec
  val ix = push("hello")
  println( lookup(ix) ++ " world" )


/*
// rejected by the type checker as the index escapes the scope of vec
fun escape() {
  with vec
  push("hello")
}

fun wrong() {
  with vec
  val ix = escape()
  println( lookup(ix) ++ " world")
}
*/
