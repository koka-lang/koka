public module effs3

effect amb {
  flip() : bool
}


effect state<s> {
  get() : s; 
  set(i : s) : ();  
}

function makeHandler0( matchop : operation<c> -> opmatch<e1,e,o>, res : a -> e b, ops : (op:o, cont:c -> e b) -> e b  ) : total ((action : () -> e1 a) -> e b) {
  function hndler(yld) {
    match(yld) {
      Yield(op,f) -> {
        match(matchop(op)) {
          OpMatch(eop) -> ops(eop,fun(x) { hndler(f(x))})
          OpNoMatch    -> reyield(op,fun(x) { hndler(f(x))})
        }
      }
      Result(x) -> res(x)          
    }
  }
  unsafeMakeHandler0(hndler)
}

function makeHandler1( matchop : operation<c> -> opmatch<e1,e,o>, res : (a,d) -> e b, ops : (op:o, cont: ((c,d) -> e b), loc:d) -> e b  ) :  total ((action: () -> e1 a, loc:d) -> e b) {
  function hndler(yld,loc) {
    match(yld) {
      Yield(op,f) -> {      
        match(matchop(op)) {
          OpMatch(eop) -> ops(eop, fun(x,loc0) { hndler(f(x),loc0) }, loc)
          OpNoMatch    -> reyield(op, fun(x) { hndler(f(x),loc) }) 
        }
      }
      Result(x) -> res(x,loc)          
    }
  }
  unsafeMakeHandler1(hndler)
}


private external unsafeMakeHandler0( hndler : yld<a> -> e b ) : total ((action : () -> e1 a) -> e b) {
  js inline "function(_action,_k){ _k = _k || $std_core.id; return (#1)(_action($std_core.Result),_k); }"
}

private external unsafeMakeHandler1( hndler : (yld<a>,d) -> e b ) : total ((action : () -> e1 a, loc : d) -> e b) {
  js inline "function(_action,_loc,_k){ _k = _k || $std_core.id; return (#1)(_action($std_core.Result),_loc,_k); }"
}

private external reyield( op: operation<b>, f : b -> e a ) : e a {
  js inline "$std_core.Yield(#1,function(_x) { return (#2)(_x,_k); } )"
}
