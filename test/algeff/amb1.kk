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
// Testing
// ----------------------------------------------------------------------------
                   
function test() {
  (fun(k) { amb_handle(xor,k) }).pure.map(show).join(",")
}
          