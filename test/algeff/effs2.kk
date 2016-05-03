public module effs2

effect amb {
  flip() : bool
}


effect state<s> {
  get() : s; 
  set(i : s) : ();  
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

function makeHandler( matchop : operation<c> -> maybe<o>, res : a -> e b, ops : (op:o, cont:c -> e b) -> e b  ) :  (yld<a> -> e b) {
  function hndler(yld) {
    match(yld) {
      Yield(op,f) -> {
        match(matchop(op)) {
          Just(eop) -> {            
            ops(eop,fun(x) { hndler(f(x))})
          }          
          Nothing -> {
            reyield(op,fun(x) { hndler(f(x))})
          }
        }
      }
      Result(x) -> res(x)          
    }
  }
  hndler
}

function makeHandlerState<e,c,a,b,s>( res : (a,s) -> e b, ops : (state_ops<s,c>, cont:(c,s) -> e b, s) -> e b ) : ((action: () -> <state<s>|e> a, s) -> e b) {
  xhandlerState(makeHandler1( matchState, res, ops ))
}
private external xhandlerState( hndler : (yld<a>,s) -> e b ) : total ((action : () -> <state<s>|e> a, loc : s) -> e b) {
  js inline "function(_action,_loc,_k){ _k = _k || $std_core.id; return (#1)(_action($std_core.Result),_loc,_k); }"
}


function matchState( op : operation<a> ) : maybe<state_ops<s,a>> {
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
