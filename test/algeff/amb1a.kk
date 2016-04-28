// ----------------------------------------------------------------------------
// Program
// ----------------------------------------------------------------------------
 
function xor() : amb bool {
  val p = flip() 
  val q = flip()
  (p||q) && not(p&&q)
}

function xors() : amb string {
  show(xor())
}

function foo() : <amb,state> bool {
  val p = flip() 
  val i = get()
  set(i+1)
  if (i>0 && p) then xor() else False
}         

function foos() : _ string {
  show(foo())
}    
 
effect amb {
  flip() : bool
}
 
effect state {
  get() : int; 
  set(i : int) : ();  
}
