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
external xhandlerAmb( handler : cps<a> -> e b ) : total ((action : () -> <amb|e> a) -> e b) {
  js inline "function(_action,_k){ return (#1)(_action($std_core.Result),_k); }"
}

function matchAmb( op : operation<a> ) : maybe<amb_ops<a>> {
  match(op) {
    Amb(ambop) -> Just(ambop)
    _          -> Nothing
  }
}


function makeHandler( matchop : operation<c> -> maybe<o>, res : a -> e b, ops : (op:o, cont:c -> e b) -> e b  ) :  (cps<a> -> e b) {
  function handler(cps) {
    match(cps) {
      Yield(op,f) -> {
        match(matchop(op)) {
          Just(eop) -> {
            function cont(x) { handler(f(x)) }        
            ops(eop,cont)
          }
          Nothing -> reyield(op, fun(x) { handler(f(x)) })          
        }
      }
      Result(x) -> res(x)          
    }
  }
  handler
}


external reyield( op: operation<b>, f : b -> e a ) : e a {
  js inline "$std_core.Yield(#1,function(_x) { return #2(_x,_k); } )"
}

external inline toany( x : a ) : any = inline "#1"

/*
function state_handle2() {
  makeHandlerState(
    fun(x,i) { x },
    fun(op,xcont,i) {
      match(op) {
        Get -> {
          function cont(x:int,i0) { xcont(x.toany,i0) }
          cont(i,i)
        }
        Set(j) -> {
          function cont(x:(),i0) { xcont(x.toany,i0) }
          cont((),j)
        }
      }
    }
  )
}

function makeHandlerState( res, ops ) {
  xhandlerState(makeHandler1( matchState, res, ops ))
}
external xhandlerState( handler : (cps<a>,d) -> e b ) : total ((action : () -> <state|e> a, loc : d) -> e b) {
  js inline "function(_action,_loc,_k){ return (#1)(_action($std_core.Result),_loc,_k); }"
}


function matchState( op : operation<a> ) : maybe<state_ops<a>> {
  match(op) {
    State(x) -> Just(x)
    _        -> Nothing
  }
}



function makeHandler1( matchop : operation<c> -> maybe<o>, res : ((a,d) -> e b), ops : (op:o, cont: ((c,d) -> e b), loc:d) -> e b  ) :  ((cps<a>,d) -> e b) {
  function handler(cps,loc) {
    match(cps) {
      Yield(op,f) -> {
        match(matchop(op)) {
          Just(eop) -> {
            function cont(x,loc0) { handler(f(x),loc0) }        
            ops(eop,cont,loc)
          }
          Nothing -> reyield(op, fun(x) { handler(f(x),loc) })          
        }
      }
      Result(x) -> res(x,loc)          
    }
  }
  handler
}

*/