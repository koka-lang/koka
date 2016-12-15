// ----------------------------------------------------------------------------
// operation are covered multiple times in a handler
// ----------------------------------------------------------------------------
 
effect state {
  get() : int; 
  set(i : int) : ();  
}

val state = handler(i:int) {
  return x -> x
  get()    -> resume(i,i)
  set(j)   -> resume(j,())
  get()    -> resume(0,i)
}
