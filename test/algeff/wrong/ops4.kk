// ----------------------------------------------------------------------------
// not all operations are covered in a handler
// ----------------------------------------------------------------------------
 
effect state {
  fun get() : int
  fun set(i : int) : ()
}

val state = handler(i:int) {
  return x -> x
  get()    -> resume(i,i)
}
