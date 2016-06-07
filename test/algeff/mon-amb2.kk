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

fun bind( m : free<e,a>, f : a -> e free<e,b> ) : e free<e,b> {
  match(m) {
    Unit(x)     -> f(x)
    Op(op,cop)  -> Op(op, compose(cop,f))
  }
}

fun unit(x) {
  Unit(x)
}

fun lift( op : op<a> ) : e free<e,a> {
  Op(op,compid)
}

fun pure( f : free<e,a> ) : exn a {
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

fun apply( comp : comp<a,e,b>, x : a ) : e free<e,b> {
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

fun rebalance( c : comp<a,e,c>, d : comp<c,e,b> ) : (a -> e free<e,c>, comp<c,e,b>) {
  match(c) {
    Comp(c1,c2) -> rebalance(c1,Comp(c2,d))
    Fun(f) -> (f,d)
  }
}

fun compose( c : comp<a,e,c>, f : c -> e free<e,b> ) : comp<a,e,b> {
  Comp(c,Fun(f))
}


// ----------------------------------------------------------------------------
// Program
// ----------------------------------------------------------------------------

fun xor() : e free<e,bool> {
  bind( flip(), fun(p) {
    bind( flip(), fun(q) {
      unit( p||q && not(p&&q))
    })
  })
}


                   
fun foo() : e free<e,bool> {
  bind( flip(), fun(p) {
    bind( get(), fun(i) {
      bind( set(i+1), fun(_) {
          if (i >= 0 && p) then xor() else unit(False)
      })
    })
  })
}

// ----------------------------------------------------------------------------
// Ambiguity primitives
// ----------------------------------------------------------------------------

fun flip() : e free<e,bool> {
  lift(Flip)
}



// ----------------------------------------------------------------------------
// Ambiguity handler
// ----------------------------------------------------------------------------

fun amb_result(x : a ) : e free<e,list<a>> {
  unit([x])
}

fun amb_flip( cont : bool -> e free<e,list<a>> ) : e free<e,list<a>> {
  bind( cont(False), fun(i) { 
    bind( cont(True), fun(j) {
      unit(i+j)
    })
  })
}


fun amb_handler( free : free<e,a> ) : e free<e,list<a>> {
  unsafe_nodiv(amb_handlerx)( free )
}

fun amb_handlerx( free ) {
  match(free) {
    Unit(x) -> amb_result(x)
    Op(op,cop) -> {
      fun cont(x) { amb_handlerx(apply(cop,x)) }  
      match(op) {
        Flip  -> amb_flip(cont)
        _     -> Op(op, Fun(cont))
      }
    }
  }
}

private external inline unsafe_nodiv : forall<e,a,b> ( (x : free<<div|e>,a> ) -> <div|e> free<<div|e>,b> ) -> ( ( x : free<e,a> ) -> e free<e,b>) = inline "#1"
private external inline unsafe_nodiv2 : forall<e,a,b,c> ( (x : free<<div|e>,a>, c ) -> <div|e> free<<div|e>,b> ) -> ( ( x : free<e,a>, c ) -> e free<e,b>) = inline "#1"
private external inline unsafe_nodivx : forall<e,a> ( () -> <div|e> free<<div|e>,a> ) -> ( () -> e free<e,a>) = inline "#1"

// ----------------------------------------------------------------------------
// State
// ----------------------------------------------------------------------------

extend type op<a> {
  Get
  Set(i:int)
}

fun get() : e free<e,int> {
  lift(Get)
}

fun set(i : int) : e free<e,()> {
  lift(Set(i))
}


fun state_handler( free : free<e,a>, i : int ) : e free<e,(a,int)> {
  fun state_handlerx( free, i ) {
    match(free) {
      Unit(x) -> unit( (x,i) )
      Op(Get,cop) -> {        
        fun cont(i,x) { state_handlerx(apply(cop,x),i) }  
        cont(i,i)
      }
      Op(Set(j),cop) -> {
        fun cont(i,x) { state_handlerx(apply(cop,x),i) }  
        cont(j,())
      }
      Op(op,cop) -> {
        fun cont(x) { state_handlerx(apply(cop,x),i) }  
        Op(op, Fun(cont))
      }
    }
  }

  unsafe_nodiv2(state_handlerx)( free, i )
}



// ----------------------------------------------------------------------------
// Testing
// ----------------------------------------------------------------------------

fun test0() {
  amb_handler(xor()).pure.map(show).join(",")
}


                   
fun show1( x : (list<bool>,int) ) {
  "([" + x.fst.map(show).join(",") + "], " + x.snd.show + ")"
}
                   
fun show2( xs : list<(bool,int)> ) {
  "[" + xs.map(fun(x){ "(" + x.fst.show + "," + x.snd.show + ")"}).join(",") + "]"
}
                   
fun test1() = state_handler(amb_handler(foo()),1).pure.show1
fun test2() = amb_handler(state_handler(foo(),1)).pure.show2