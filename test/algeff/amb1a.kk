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

external reyield( op: operation<b>, f : b -> e a ) : cont a {
  js inline "Yield(#1,function(_x) { return #2(_x,_k); } )"
}

external inline toany( x : a ) : any = inline "#1"