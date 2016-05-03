import effs3

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
    set(j)   -> resume((),j)
  }
}
/*
function amb_handle() {
  makeHandler(opmatchAmb,
    fun(x) { [x] },
    fun(ops,cont) {
      match(ops) {
        Flip -> cont(False) + cont(True)
      }
    }
  )
}

function state_handle() {
  makeHandler1(opmatchState,
    fun(x,i:int) { x },
    fun(ops,cont,i:int) {
      match(ops) {
        Get -> cont(i.toAny,i)
        Set(j) -> cont(().toAny,j)
      }
    }
  )
}
*/

function test1()  {
  amb_handle()(xors).join(",")
}


function test2()  {
  state_handle()({amb_handle()(foos)},0).join(",")
}

function test3()  {
  amb_handle()({state_handle()(foos,0)}).join(",")
}
  