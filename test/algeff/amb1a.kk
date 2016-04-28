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

function amb_handle( action  ) {
  function handler(cps )  {
    match(cps) {
      Result(x) -> [x]   
      Yield(op,f) -> {
        match(op) {
          Amb(ambop) -> {
            match(ambop) {
              Flip() -> {
                function cont(x:bool) { handler(f(x)) }        
                cont(False) + cont(True)
              }
            }
          }
          _ -> reyield(op, fun(x) { handler(f(x)) } ) 
        }
      }    
    }
  }
  (fun(m) { runCpsAmb(m,handler) })(action) 
}

external runCpsAmb( action : () -> <amb|e> a, handler : cps<a> -> <cont|e> b ) : e b {
  js inline "#2(#1(Result),_k)"
}


function amb_handle2() {
  makeHandlerAmb(
    fun(x) { [x] },
    fun(ambop,xcont) {
      match(ambop) {
        Flip -> {
          function cont(x:bool) { xcont(x.toany) }
          cont(False) + cont(True)
        }
      }
    }
  )
}

function makeHandlerAmb( res : a -> <cont|e> b, ops : (amb_ops<c>, cont:c -> <cont|e> b) -> <cont|e> b ) : ((action: () -> <amb|e> a) -> e b) {
  xhandlerAmb(makeHandler( matchAmb, res, ops ))
}
external xhandlerAmb( handler : cps<a> -> <cont|e> b ) : total ((action : () -> <amb|e> a) -> e b) {
  js inline "function(_action,_k){ return (#1)(_action(Result),_k); }"
}

function matchAmb( op : operation<a> ) : maybe<amb_ops<a>> {
  match(op) {
    Amb(ambop) -> Just(ambop)
    _          -> Nothing
  }
}



function makeHandler( matchop : operation<c> -> maybe<o>, res : a -> <cont|e> b, ops : (op:o, cont:c -> <cont|e> b) -> <cont|e> b  ) :  (cps<a> -> <cont|e> b) {
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


function state_handle( ) {
  function handler(cps,i)  {
    match(cps) {
      Result(x) -> (x,i)
      Yield(op,f) -> {
        match(op) {
          State(stop) -> {
            match(stop) {
              Get() -> {
                function cont(x:int,i0) { handler(f(x.toany),i0) }        
                cont(i,i)
              }
              Set(j) -> {
                function cont(x:(),i0) { handler(f(x.toany),i0) }
                cont((),j)
              }
            }
          }
          _ -> reyield(op, fun(x) { handler(f(x),i) } ) 
        }
      }    
    }
  }
  (fun(m,i) { runCpsState(m,handler,i) })
}

external runCpsState( action : () -> <state|e> a, handler : (cps<a>,c) -> <cont|e> b, i : c ) : e b {
  js inline "#2(#1(Result),#3,_k)"
}



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

function makeHandlerState( res, ops ) { // res : (a,d) -> <cont|e> b, ops : (amb_ops<c>, cont:(c,d) -> <cont|e> b) -> <cont|e> b ) : ((action: () -> <amb|e> a) -> e b) {
  xhandlerState(makeHandler1( matchState, res, ops ))
}
external xhandlerState( handler : (cps<a>,d) -> <cont|e> b ) : total ((action : () -> <state|e> a, loc : d) -> e b) {
  js inline "function(_action,_loc,_k){ return (#1)(_action(Result),_loc,_k); }"
}


function matchState( op : operation<a> ) : maybe<state_ops<a>> {
  match(op) {
    State(x) -> Just(x)
    _        -> Nothing
  }
}



function makeHandler1( matchop : operation<c> -> maybe<o>, res : ((a,d) -> <cont|e> b), ops : (op:o, cont: ((c,d) -> <cont|e> b), loc:d) -> <cont|e> b  ) :  ((cps<a>,d) -> <cont|e> b) {
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




external reyield( op: operation<b>, f : b -> e a ) : cont a {
  js inline "Yield(#1,function(_x) { return #2(_x,_k); } )"
}

external inline toany( x : a ) : any = inline "#1"