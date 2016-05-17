// ----------------------------------------------------------------------------
// not all operations are covered in a handler
// ----------------------------------------------------------------------------
 
effect state {
  get() : int; 
  set(i : int) : ();  
}

val state = handler(i:int) {
  return x -> x
  get()    -> resume(i,i)
}
