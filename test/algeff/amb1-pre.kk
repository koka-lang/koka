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

private external inline unsafenoamb( action: () -> <amb|e> a ) : ( (a -> e cps<<cont|e>,x>) ) -> <cont|e> cps<<cont|e>,x> = inline "#1"

extend type op<a> {
  Flip
}

function flip() : amb bool {
  unsafeyield(Flip)
}

external reyield( op : op<a>, f : a -> <cont|e> b) : <cont|e> b = inline "Yield(#1,function(x){ (#2)(x,k) })"
external unsafenocont( f : () -> <cont|e> a) : () -> e a = inline "#1"  

// ----------------------------------------------------------------------------
// Ambiguity handler
// ----------------------------------------------------------------------------
function amb_handle( action : () -> <amb|e> a ) : e list<a> {
  function amb_handlex( cps : cps<<cont|e>,a> ) : <cont|e> list<a> {
    match(cps) {
      Result(x)   -> [x]
      Yield(Flip,f) -> {
        function cont(x : bool) { amb_handlex(f(x)) } 
        cont(False) + cont(True)
      }
      Yield(op,f) -> reyield(op, fun(x) { amb_handlex(f(x)) })
    }
  }
  (unsafenocont{ amb_handlex(unsafenoamb(action)(Result)) })
}

// ----------------------------------------------------------------------------
// Testing
// ----------------------------------------------------------------------------
                   
function test() {
  amb_handle(xor).map(show).join(",")
}
          