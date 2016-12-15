// ----------------------------------------------------------------------------
// Program
// ----------------------------------------------------------------------------
 
fun xor() : amb bool {
  val p = flip() 
  val q = flip()
  (p||q) && not(p&&q)
}

fun xors() : amb string {
  show(xor())
}

fun foo() : <amb,state> bool {
  val p = flip() 
  val i = get()
  set(i+1)
  if (i>=0 && p) then xor() else False
}         

fun foos() : <amb,state> string {
  show(foo())
}    
 
effect amb {
  flip() : bool
}
 
effect state {
  get() : int; 
  set(i : int) : ();  
}


fun amb_handle() {
  handler {
    return x -> [x]
    flip()   -> resume(False) + resume(True)
  }
}

fun state_handle() {
  handler(i:int) {
    return x -> x
    get()    -> resume(i,i)
    set(j,x)   -> resume((),j) 
  }
}
