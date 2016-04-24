// ----------------------------------------------------------------------------
// Program
// ----------------------------------------------------------------------------

function xor( k : bool -> <amb|e> free<<amb|e>,x>) : <amb|e> free<<amb|e>,x> {
  flip(fun(p) {
    flip(fun(q) {
      k( p||q && not(p&&q))
    })
  })
}

type free<e,a> {
  Res( res : a )
  Yld( eff: effect<e,a>, f : any -> e free<e,a> )
}

open type effect<e::E,a>

// ----------------------------------------------------------------------------
// Ambiguity primitives
// ----------------------------------------------------------------------------
type amb :: X

private external inline unsafeNoAmb : forall<e,a> ( action: ( k : a -> <amb|e> free<<amb|e>,a> ) -> <amb|e> free<<amb|e>,a> ) -> ((k : a -> e free<e,a>) -> e free<e,a>) = inline "#1"

extend type effect<e::E,a> {
  Amb( mkamb : (f : any -> e free<e,a>) -> e ambop<e,a> )
}

type ambop<e,a> {
  Flip( f : bool -> e free<e,a> )
}

external inline unsafecastx( f : a -> e free<e,x> ) : any -> e free<e,x> = inline "#1"
external inline safecastx( f : any -> e free<e,x> ) : b -> e free<e,x> = inline "#1"
external unsafecast( a : any ) : b = inline "#1"

function flip( k : bool -> <amb|e> free<<amb|e>,x> ) : <amb|e> free<<amb|e>,x> {
  Yld( Amb( fun(f) { Flip(safecastx(f)) }), unsafecastx(k) )
}

// ----------------------------------------------------------------------------
// Ambiguity handler
// ----------------------------------------------------------------------------
function amb_result(x : a, k : list<a> -> e free<e,x> ) : e free<e,x> {
  k([x])
}

function amb_flip(cont : (bool, list<a> -> e free<e,x> ) -> e free<e,x>, k : list<a> -> e free<e,x> ) : e free<e,x> {
  cont(False, fun(xs) {
    cont(True, fun(ys) {
      k(xs+ys)
    })
  })
}

function amb_handle( action : ( k : a -> <amb|e> free<<amb|e>,a> ) -> <amb|e> free<<amb|e>,a>, k : list<a> -> e free<e,x> ) : e free<e,x> {
  amb_handlex( unsafeNoAmb(action)(Res), k )
}

function amb_handlex( free : free<e,a>, k : list<a> -> e free<e,x> ) : e free<e,x> {
  match(free) {
    Res(x)   -> amb_result(x,k)
    Yld(Amb(mkamb),f) -> {
      match(mkamb(f)) {
        Flip(g) -> {
          function cont(x,kc) { amb_handlex(g(x),kc) }
          amb_flip(cont,k)
        }
      }
    }
    Yld(op,f) -> Yld(op, fun(x) { amb_handlex(f(x),k) })
  }
}

external magic( x: a) : b = inline "#1"

// ----------------------------------------------------------------------------
// Testing
// ----------------------------------------------------------------------------
                   
function test0() {
  (fun(k) { amb_handle(xor,k) }).pure.map(show).join(",")
}


/* 
// ----------------------------------------------------------------------------
// State program
// ----------------------------------------------------------------------------

             
function foo( k : bool -> <amb,state|e> free<<amb,state|e>,x> ) : <amb,state|e> free<<amb,state|e>,x> {
  flip( fun(p) {
    get( fun(i) {
      set( i+1, fun(_) {
        if (i >= 0 && p) then xor(k) else k(False)
      })
    })
  })
}

// ----------------------------------------------------------------------------
// State primitives
// ----------------------------------------------------------------------------
type state :: X

private external inline unsafeNoState : forall<e,a> ( action: ( k : a -> <state|e> free<<state|e>,a> ) -> <state|e> free<<state|e>,a> ) -> ((k : a -> e free<e,a>) -> e free<e,a>) = inline "#1"

extend type op<a> {
  Get
  Set(i:int)
}

function get( k : int -> <state|e> free<<state|e>,x> ) : state free<<state|e>,x> {
  Yield(Get,k)
}


function set( i : int, k : (()) -> <state|e> free<<state|e>,x> ) : state free<<state|e>,x> {
  Yield(Set(i),k)
}

// ----------------------------------------------------------------------------
// State handler
// ----------------------------------------------------------------------------

function state_handle( action : ( k : a -> <state|e> free<<state|e>,a> ) -> <state|e> free<<state|e>,a>, i0 : int, k0 : ((a,int)) -> e free<e,x> ) : e free<e,x> {
  function state_handlex( free : free<e,a>, i : int, k : ((a,int)) -> e free<e,x> ) : e free<e,x> {
    match(free) {
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

*/