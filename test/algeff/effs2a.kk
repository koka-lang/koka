import effs2

function xor() : amb bool {
  val p = flip() 
  val q = flip()
  (p||q) && not(p&&q)
}

function xors() : amb string {
  show(xor())
}

function foo() : <amb,state<int>> bool {
  val p = flip() 
  val i = get()
  set(i+1)
  if (i>0 && p) then xor() else False
}         

function foos() : <amb,state<int>> string {
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
    get()    -> { print(i); set(True); resume(i,i) }
    set(j)   -> resume((),j) 
  }
}

function test1()  {
  amb_handle()(xors).join(",")
}


function test2()  {
  state_handle()({amb_handle()(foos)},0).join(",")
}

function test3()  {
  amb_handle()({state_handle()(foos,0)}).join(",")
}
