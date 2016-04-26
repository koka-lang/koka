// ----------------------------------------------------------------------------
// Program
// ----------------------------------------------------------------------------

function xor( k : bool -> <amb|e> cps<<amb|e>,x>) : <amb|e> cps<<amb|e>,x> {
  flip(fun(p) {
    flip(fun(q) {
      k( p||q && not(p&&q))
    })
  })
}

             
function foo( k : bool -> <amb,state|e> cps<<amb,state|e>,x> ) : <amb,state|e> cps<<amb,state|e>,x> {
  flip( fun(p) {
    get( fun(i) {
      set( i+1, fun(_) {
        if (i >= 0 && p) then xor(k) else k(False)
      })
    })
  })
}

// ----------------------------------------------------------------------------
// Ambiguity primitives
// ----------------------------------------------------------------------------


private external inline unsafeNoAmb : forall<e,a> ( action: ( k : a -> <amb|e> cps<<amb|e>,a> ) -> <amb|e> cps<<amb|e>,a> ) -> ((k : a -> e cps<e,a>) -> e cps<e,a>) = inline "#1"
external inline toany( x : a ) : any = inline "#1"

/*
type amb :: HX
*/
type extend operation<a> {
  Amb( : ambop<a> )
}

type ambop<a> {
  Flip
}
 

function flip( k : bool -> e cps<e,x>) : amb cps<e,x> { 
  yieldop(Amb(Flip),k)
}


effect amb 

// ----------------------------------------------------------------------------
// Ambiguity handler
// ----------------------------------------------------------------------------
function amb_result(x : a, k : list<a> -> e cps<e,x> ) : e cps<e,x> {
  k([x])
}

function amb_flip(cont : (bool, list<a> -> e cps<e,x> ) -> e cps<e,x>, k : list<a> -> e cps<e,x> ) : e cps<e,x> {
  cont(False, fun(xs) {
    cont(True, fun(ys) {
      k(xs+ys)
    })
  })
}

function amb_handle( action : ( k : a -> <amb|e> cps<<amb|e>,a> ) -> <amb|e> cps<<amb|e>,a>, k : list<a> -> e cps<e,x> ) : e cps<e,x> {
  amb_handlex( unsafeNoAmb(action)(Result), k )
}

function amb_handlex( cps : cps<e,a>, k : list<a> -> e cps<e,x> ) : e cps<e,x> {
  match(cps) {
    Result(x) -> amb_result(x,k)
    Yield(Amb(op),f) -> {
      function xcont(x:any,kc) { amb_handlex(f(x),kc) } // wrap the continuation      
      match(op) {
        Flip -> {
          function cont(x,kc) { xcont(x.toany,kc) }
          amb_flip(cont,k)
        }
      }
    }
    Yield(op,f) -> Yield(op, fun(x) { amb_handlex(f(x),k) })
  }
}


// ----------------------------------------------------------------------------
// State primitives
// ----------------------------------------------------------------------------
type state :: HX

private external inline unsafeNoState : forall<e,a> ( action: ( k : a -> <state|e> cps<<state|e>,a> ) -> <state|e> cps<<state|e>,a> ) -> ((k : a -> e cps<e,a>) -> e cps<e,a>) = inline "#1"

type extend operation<a> {
  State( : stateop<a> )
}

type stateop<a> {
  Get          // : op<int>
  Set(i:int)   // : op<()>
}

function get( k : int -> e cps<e,x> ) : state cps<e,x> {
  yieldop(State(Get),k)
}


function set( i : int, k : (()) -> e cps<e,x> ) : state cps<e,x> {
  yieldop(State(Set(i)),k)
}

// ----------------------------------------------------------------------------
// State handler
// ----------------------------------------------------------------------------

function state_handle( action : ( k : a -> <state|e> cps<<state|e>,a> ) -> <state|e> cps<<state|e>,a>, i0 : int, k0 : ((a,int)) -> e cps<e,x> ) : e cps<e,x> {
  function state_handlex( cps : cps<e,a>, i : int, k : ((a,int)) -> e cps<e,x> ) : e cps<e,x> {
    match(cps) {
      Result(x) -> k((x,i))
      Yield(State(op),f) -> {
        function xcont(ic,x:any,kc) { state_handlex(f(x), ic, kc)}
        match(op) {
          Get -> {
            function cont(ic,x:int,kc) { xcont(ic,x.toany,kc) }
            cont(i,i,k)
          }
          Set(j) -> {
            function cont(ic,x:(),kc) { xcont(ic,x.toany,kc) }
            cont(j,(),k)
          }
        }
      }
      Yield(op,f) -> {
        Yield(op, fun(x) { state_handlex(f(x),i,k) } )
      }
    }
  }
  state_handlex(unsafeNoState(action)(Result),i0,k0)
}

// ----------------------------------------------------------------------------
// Testing
// ----------------------------------------------------------------------------
                   
function test0() {
  (fun(k) { amb_handle(xor,k) }).pure.map(show).join(",")
}
          

         
function show1( x : (list<bool>,int) ) {
  "([" + x.fst.map(show).join(",") + "], " + x.snd.show + ")"
}
                   
function show2( xs : list<(bool,int)> ) {
  "[" + xs.map(fun(x){ "(" + x.fst.show + "," + x.snd.show + ")"}).join(",") + "]"
}
                   
function test1() {
  (fun(k) { state_handle( fun(k2) { amb_handle(foo,k2) }, 1, k) }).pure.show1
}

function test2() {
  (fun(k) { amb_handle( fun(k2) { state_handle(foo,1,k2) }, k ) }).pure.show2          
}