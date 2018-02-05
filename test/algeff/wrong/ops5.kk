// ----------------------------------------------------------------------------
// operation are covered multiple times in a handler
// ----------------------------------------------------------------------------
 
effect state {
  fun get() : int
  fun set(i : int) : ()
}

val state = handler(i:int) {
  return x -> x
  get()    -> resume(i,i)
  set(j)   -> resume((),j)
  get()    -> resume(i,0)
}
