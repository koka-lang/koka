effect amb {
  flip() : bool
}

effect state<s> {
  get()    :  s;
  set(x:s) : ()
}

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
 
function amb_handle(action : () -> <amb|e> a) : e list<a> {
  val h = handler {
    return x -> [x]
    flip()   -> resume(False) + resume(True)
  }
  h(action)
}

function state_handle(i,action) {
  val h = handler(i) {
    return x -> x
    get()    -> resume(i,i)
    set(j)   -> resume(j,())
  }
  h(i,action)
}

function test1()  {
  amb_handle(xors).join(",")
}


function test2()  {
  state_handle(0,{amb_handle(foos)}).join(",")
}

function test3()  {
  amb_handle({state_handle(0,foos)}).join(",")
}

function main() {
  [test1(),test2(),test3()].join("\n").println
}