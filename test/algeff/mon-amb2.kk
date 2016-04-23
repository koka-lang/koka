// ----------------------------------------------------------------------------
// Ambiguity & State using a monad
// ----------------------------------------------------------------------------

// extensible type...
extend type op<a> {
  Flip 
}

// ----------------------------------------------------------------------------
// The 'free' monad
// ----------------------------------------------------------------------------
type free<e,a> {
  Op<b>( op : op<b>, comp : comp<b,e,a> )
  Unit( result : a )
}

function bind( m : free<e,a>, f : a -> e free<e,b> ) : e free<e,b> {
  match(m) {
    Unit(x)     -> f(x)
    Op(op,cop)  -> Op(op, compose(cop,f))
  }
}

function unit(x) {
  Unit(x)
}

function lift( op : op<a> ) : e free<e,a> {
  Op(op,compid)
}

function pure( f : free<e,a> ) : exn a {
  match(f) {
    Unit(x) -> x
  }
}

// ----------------------------------------------------------------------------
// Queue of composed functions
// This could be implemented in backend using more efficient primitives
// ----------------------------------------------------------------------------
type comp<a,e :: E,b> {
  Comp<c>( c : comp<a,e,c>, d : comp<c,e,b> )
  Fun( f : a -> e free<e,b> )
}

val compid : forall<e,a> comp<a,e,a> = Fun(Unit)

function apply( comp : comp<a,e,b>, x : a ) : e free<e,b> {
  match(comp) {
    Comp(c1,c2) -> {
      val (f,c) = rebalance(c1,c2)
      match(f(x)) {
        Unit(y)    -> apply(c,y)
        Op(op,cop) -> Op(op, Comp(cop,c))
      }
    }
    Fun(f) -> f(x)
  }
}

function rebalance( c : comp<a,e,c>, d : comp<c,e,b> ) : (a -> e free<e,c>, comp<c,e,b>) {
  match(c) {
    Comp(c1,c2) -> rebalance(c1,Comp(c2,d))
    Fun(f) -> (f,d)
  }
}

function compose( c : comp<a,e,c>, f : c -> e free<e,b> ) : comp<a,e,b> {
  Comp(c,Fun(f))
}


// ----------------------------------------------------------------------------
// Program
// ----------------------------------------------------------------------------

function xor() : e free<e,bool> {
  bind( flip(), fun(p) {
    bind( flip(), fun(q) {
      unit( p||q && not(p&&q))
    })
  })
}


// ----------------------------------------------------------------------------
// Ambiguity primitives
// ----------------------------------------------------------------------------

function flip() : e free<e,bool> {
  lift(Flip)
}



// ----------------------------------------------------------------------------
// Ambiguity handler
// ----------------------------------------------------------------------------

function amb_result(x : a ) : e free<e,list<a>> {
  unit([x])
}

function amb_flip( cont : bool -> e free<e,list<a>> ) : e free<e,list<a>> {
  bind( cont(False), fun(i) { 
    bind( cont(True), fun(j) {
      unit(i+j)
    })
  })
}


function amb_handler( action : () -> e free<e,a> ) : e free<e,list<a>> {
  unsafe_nodiv(amb_handlerx)( action() )
}

function amb_handlerx( free : free<<div|e>,a> ) : <div|e> free<<div|e>,list<a>> {
  match(free) {
    Unit(x) -> amb_result(x)
    Op(op,cop) -> {
      function cont(x) { amb_handlerx(apply(cop,x)) }  // wrap the continuation
      match(op) {
        Flip  -> amb_flip(cont)
        _     -> Op(op, Fun(cont))
      }
    }
  }
}

private external inline unsafe_nodiv : forall<e,a,b> ( (x : free<<div|e>,a> ) -> <div|e> free<<div|e>,b> ) -> ( ( x : free<e,a> ) -> e free<e,b>) = inline "#1"


// ----------------------------------------------------------------------------
// Testing
// ----------------------------------------------------------------------------

function test0() {
  amb_handler(xor).pure.map(show).join(",")
}

