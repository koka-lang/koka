import effs1

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
  if (i>=0 && p) then xor() else False
}         

function foos() : <amb,state> string {
  show(foo())
}    
 
// handlers


function amb_handle() {
  handler {
    return x -> [x]
    flip()   -> resume(False) + resume(True)
  }
}

function state_handle() {
  handler(i:int) {
    return x -> x
    get()    -> resume(i,i)
    set(j)   -> resume((),j) 
  }
}


function test1() : string {
  amb_handle()(xors).join(",")
}
