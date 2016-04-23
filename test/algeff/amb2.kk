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
type amb :: X

private external inline unsafeNoAmb : forall<e,a> ( action: ( k : a -> <amb|e> cps<<amb|e>,a> ) -> <amb|e> cps<<amb|e>,a> ) -> ((k : a -> e cps<e,a>) -> e cps<e,a>) = inline "#1"

extend type op<a> {
  Flip
}

function flip( k : bool -> <amb|e> cps<<amb|e>,x> ) : amb cps<<amb|e>,x> {
  Yield(Flip,k)
}

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
    Result(x)   -> amb_result(x,k)
    Yield(op,f) -> {
      function cont(x,kc) { amb_handlex(f(x),kc) } // wrap the continuation
      match(op) {
        Flip -> amb_flip(cont,k)
        _    -> Yield(op, fun(x) { cont(x,k) })
      }
    }
  }
}


// ----------------------------------------------------------------------------
// State primitives
// ----------------------------------------------------------------------------
type state :: X

private external inline unsafeNoState : forall<e,a> ( action: ( k : a -> <state|e> cps<<state|e>,a> ) -> <state|e> cps<<state|e>,a> ) -> ((k : a -> e cps<e,a>) -> e cps<e,a>) = inline "#1"

extend type op<a> {
  Get
  Set(i:int)
}

function get( k : int -> <state|e> cps<<state|e>,x> ) : state cps<<state|e>,x> {
  Yield(Get,k)
}


function set( i : int, k : (()) -> <state|e> cps<<state|e>,x> ) : state cps<<state|e>,x> {
  Yield(Set(i),k)
}

// ----------------------------------------------------------------------------
// State handler
// ----------------------------------------------------------------------------

function state_handle( action : ( k : a -> <state|e> cps<<state|e>,a> ) -> <state|e> cps<<state|e>,a>, i0 : int, k0 : ((a,int)) -> e cps<e,x> ) : e cps<e,x> {
  function state_handlex( cps : cps<e,a>, i : int, k : ((a,int)) -> e cps<e,x> ) : e cps<e,x> {
    match(cps) {
      Result(x)   -> k((x,i))
      Yield(Get,f) -> {
        function cont(ic,x,kc) { state_handlex(f(x),ic,kc) } // wrap the continuation
        cont(i,i,k)
      }
      Yield(Set(j),f) -> {
        function cont(ic,x,kc) { state_handlex(f(x),ic,kc) } // wrap the continuation
        cont(j,(),k)
      }
      Yield(op,f) -> {
        function cont(x) { state_handlex(f(x),i,k) } // wrap the continuation
        Yield(op,cont)
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