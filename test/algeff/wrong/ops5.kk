// ----------------------------------------------------------------------------
// operation are covered multiple times in a handler
// ----------------------------------------------------------------------------
 
effect state {
  fun get() : int
  fun set(i : int) : ()
}

fun state(init,action) 
  var s := init
  handle(action)
    fun get()  { s }
    fun set(x) { s := x }
    fun get()  { 0 }
   