// ----------------------------------------------------------------------------
// Program
// ----------------------------------------------------------------------------

function xor() : amb bool {
  val p = flip() 
  val q = flip() 
  ( p||q && not(p&&q))  
}

// ----------------------------------------------------------------------------
// Ambiguity primitives
// ----------------------------------------------------------------------------
type amb :: X

private external inline unsafenoamb( action: () -> <amb|e> a ) : ( (a -> cps<e,x>) ) -> cps<e,x> = inline "#1"

extend type op<a> {
  Flip
}

function flip() : amb bool {
  unsafeyield(Flip)
}

// ----------------------------------------------------------------------------
// Ambiguity handler
// ----------------------------------------------------------------------------
function amb_result(x : a ) : list<a> {
  [x]
}

function amb_flip( cont : bool -> list<a> ) : list<a> {
  cont(False) + cont(True)
}

function amb_handle( action : () -> <amb|e> a ) : e list<a> {
  (fun(k) { amb_handlex( unsafenoamb(action)(Result), k ) }).pure
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
  amb_handle(xor).map(show).join(",")
}
          