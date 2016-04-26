// ----------------------------------------------------------------------------
// Program
// ----------------------------------------------------------------------------
 
function xor() : amb bool {
  val p = flip() 
  val q = flip()
  p||q && not(p&&q)
}

function foo() : <amb,state> bool {
  val p = flip()
  val i = get()
  set(i+1)
  if (i>=0 && p) then xor() else False
}             

effect amb {
  flip() : bool
}
 
effect state {
  get() : int; 
  set(i : int) : ();  
}
