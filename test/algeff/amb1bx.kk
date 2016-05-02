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
  if (i>=0 && p) then xor() else False
}         

function foos() : <amb,state> string {
  show(foo())
}    
 
effect amb {
  flip() : bool
}
 
effect state {
  get() : int; 
  set(i : int) : ();  
}


function amb_handle() {
  handler {
    return x -> [x]
    flip()   -> cont(False) + cont(True)
  }
}

function state_handle() {
  handler(i:int) {
    return x -> x
    get()    -> cont(i,i)
    set(j)   -> cont((),j) 
  }
}


function amb_handlex() {
  makeHandlerAmb(
    fun(x) { [x] },
    fun(ambop,xcont) {
      match(ambop) {
        Flip -> {
          function cont(x:bool) { xcont(x.toany) }
          //print("hi")
          cont(False) + cont(True)
        }
      }
    }
  )
}

function test1() : string {
  amb_handle()(xors).join(",")
}

function makeHandlerAmb( res : a -> e b, ops : (amb_ops<c>, cont:c -> e b) -> e b ) : ((action: () -> <amb|e> a) -> e b) {
  xhandlerAmb(makeHandler( matchAmb, res, ops ))
}
external xhandlerAmb( hndler : yld<a> -> e b ) : total ((action : () -> <amb|e> a) -> e b) {
  js inline "function(_action,_k){ _k = _k || $std_core.id; return (#1)(_action($std_core.Result),_k); }"
}

function matchAmb( op : operation<a> ) : maybe<amb_ops<a>> {
  match(op) {
    Amb(ambop) -> Just(ambop)
    _          -> Nothing
  }
}


function makeHandler( matchop : operation<c> -> maybe<o<c>>, res : a -> e b, ops : (op:o<c>, cont:c -> e b) -> e b  ) :  (yld<a> -> e b) {
  function hndler(yld) {
    match(yld) {
      Yield(op,f) -> {
        function cont(x) { hndler(f(x)) }
        match(matchop(op)) {
          Just(eop) -> {            
            ops(eop,cont)
          }          
          Nothing -> {
            reyield(op,cont)
          }
        }
      }
      Result(x) -> res(x)          
    }
  }
  hndler
}


external inline reyield( op: operation<b>, f : b -> e a ) : e a {
  js inline "$std_core.Yield(#1,function(_x) { return #2(_x,_k); } )"
}

external inline toany( x : a ) : any = inline "#1"
