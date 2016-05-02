public module effs1

effect amb {
  flip() : bool
}
 
effect state {
  get() : int; 
  set(i : int) : ();  
}


function makeHandlerAmb<e,c,a,b>( res : a -> e b, ops : (amb_ops<c>, cont:c -> e b) -> e b ) : ((action: () -> <amb|e> a) -> e b) {
  xhandlerAmb(makeHandler( matchAmb, res, ops ))
}

private external xhandlerAmb( hndler : yld<a> -> e b ) : total ((action : () -> <amb|e> a) -> e b) {
  js inline "function(_action,_k){ _k = _k || $std_core.id; return (#1)(_action($std_core.Result),_k); }"
}


function matchAmb( op : operation<a> ) : maybe<amb_ops<a>> {
  match(op) {
    Amb(ambop) -> Just(ambop)
    _          -> Nothing
  }
}

function makeHandler<e,o,a,b,c>( matchop : operation<c> -> maybe<o>, res : a -> e b, ops : (op:o, cont:c -> e b) -> e b  ) :  (yld<a> -> e b) {
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

function makeHandlerState<e,c,a,b,d>( res : (a,d) -> e b, ops : (state_ops<c>, cont:(c,d) -> e b, d) -> e b ) : ((action: () -> <state|e> a, d) -> e b) {
  xhandlerState(makeHandler1( matchState, res, ops ))
}
private external xhandlerState( handlerndler : (yld<a>,d) -> e b ) : total ((action : () -> <state|e> a, loc : d) -> e b) {
  js inline "function(_action,_loc,_k){ _k = _k || $std_core.id; return (#1)(_action($std_core.Result),_loc,_k); }"
}


function matchState( op : operation<a> ) : maybe<state_ops<a>> {
  match(op) {
    State(x) -> Just(x)
    _        -> Nothing
  }
}



function makeHandler1<e,o,a,b,c,d>( matchop : operation<c> -> maybe<o>, res : ((a,d) -> e b), ops : (op:o, cont: ((c,d) -> e b), loc:d) -> e b  ) :  ((yld<a>,d) -> e b) {
  function hndler(yld,loc) {
    match(yld) {
      Yield(op,f) -> {
        function cont(x,loc0) { hndler(f(x),loc0) }        
        match(matchop(op)) {
          Just(eop) -> ops(eop,cont,loc)
          Nothing   -> reyield(op, fun(x) { cont(x,loc) })          
        }
      }
      Result(x) -> res(x,loc)          
    }
  }
  hndler
}


private external inline reyield( op: operation<b>, f : b -> e a ) : e a {
  js inline "$std_core.Yield(#1,function(_x) { return #2(_x,_k); } )"
}
