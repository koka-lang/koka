effect amb {
  flip() : bool
}

effect state<s> {
  get()  : s;
  set(i:s) : ()
}

function xor() : amb bool {
  val p = flip() 
  val q = flip()
  (p||q) && not(p&&q)
}


function foo() : <amb,state<int>> bool {
  val p = flip() 
  val i = get()
  set(i+1)
  if (i>0 && p) then xor() else False
}         

 
// handlers

function amb_handle() {
  handler {
    return x -> [x]
    flip()   -> resume(False) + resume(True)
  }
}

function state_handle() {
  handler(i) {
    return x -> x
    get()    -> resume(i,i)
    set(j)   -> resume(j,()) 
  }
}

function test1() {
  amb_handle()(xor)
}


function test2()  {
  state_handle()(0){amb_handle()(foo)}
}

function test3()  {
  amb_handle()({state_handle()(0,foo)})
}

function main() {
  test1().show.println
  test2().show.println
  test3().show.println
}