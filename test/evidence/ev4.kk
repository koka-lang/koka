module ev4

import std/num/int32

extern include {
  js file "yld.js"
}


// -------------------------------------------
// Internal types
// -------------------------------------------

struct marker<e::E,a>(m:int32)

public type htag<h::(E,V)->V> {
  Unsafe-htag(tag:string)
}

type ev<h> {
  con Ev<e,r>(:htag<h>, :marker<e,r>, :h<e,r>, :evv<e>)
}

type evv<e::E>;


// -------------------------------------------
// Internal Markers
// -------------------------------------------

extern fresh-marker-int() : int32 {
  js inline "_marker_unique++"
}

fun fresh-marker() : marker<e,r> {
  Marker(fresh-marker-int())
}

fun (==)(m1 : marker<e1,a1>, m2: marker<e2,a2>) : bool {
  (m1.m == m2.m)
}

// -------------------------------------------
// Internal Evidence vectors
// -------------------------------------------

public extern inline evv-total() : evv<<>> {
  js inline "[]"
}

extern inline evv-equal(w1 : evv<e>, w2 : evv<e> ) : bool {
  js inline "(#1===#2)"
}

extern evv-insert( evv : evv<e1>, ev : ev<h> ) : evv<e2> {
  js "evv_insert_ev"
}

public extern ievv-lookup( htag : htag<h> ) : ev<h> {
  js inline "evv_lookup(_context.evv,#1)"
}

public extern inline ievv-select-at( i : int ) : ev<h> {
  js inline "(_context.evv)[#1]"
}

public extern inline ievv-select( i : int, htag : htag<h> ) : ev<h> {
  js inline "(#1 >= 0 ? _context.evv[#1] : evv_lookup(_context.evv,#2))"
}


extern evv-current() : evv<e> {
  js inline "_context.evv"
}

extern under( w : evv<e1>, action : () -> e1 a ) : e a {   // e1 <= e
  js inline "(function(){ const w0 = _context.evv; _context.evv = #1; const res = (#2)(); _context.evv = w0; return res; })()"
}

extern under( w : evv<e1>, action : (a) -> e1 b, x : a ) : e b {
  js inline "(function(){ const w0 = _context.evv; _context.evv = #1; const res = (#2)(#3); _context.evv = w0; return res; })()"
}

fun under( w : evv<e1>, action : (a,b) -> e1 c, x : a, y : b ) : e c {
  under(w){ action(x,y) }
}


// -------------------------------------------
// Internal Yielding
// -------------------------------------------

extern inline yielding() : e bool {
  js inline "(_context.yield !== 0)"
}

extern inline yield-extend( _x : a, next : a -> e b ) : e b {
  js inline "(_context.yield_conts.push(#2))"
}

fun yield-bind( x : a, next : a -> e b ) : e b {
  if (yielding()) then yield-extend(x,next) else next(x)
}

extern yield-prompt( w1:evv<e>, w2:evv<e2>, m: marker<e,a>, res : a ) : yld<e,a> {
  js "_yield_prompt"
}

extern yield-to(m:marker<e1,r>,clause:(a -> e1 r) -> e1 r) : a {
  js "_yield_to"
}

// -------------------------------------------
//
// -------------------------------------------

extern inline unsafe-coerce(x:a) : b { inline "#1" }

abstract type yld<e,r> {
  Pure(x:r)
  Yielding()
  Yielded<b>(clause : (b -> e r) -> e r, cont : b -> e r)
}

fun prompt( w1:evv<e>, w2:evv<e2>, m : marker<e,r>, res : r ) : e r {
  match(yield-prompt(w1,w2,m,res)) {
    Pure(x)  -> x
    Yielding -> yield-extend(res, fun(x){ prompt(w1,w2,unsafe-decreasing(m),x); })
    Yielded(clause,cont) {
      val deep = fun(x) {
        val w = evv-current()
        if (evv-equal(w1,w)) then prompt(w1,w2,unsafe-decreasing(m),cont(x)) else unsafe-total{ error("non-scoped resumption") }
      }
      clause(deep)
    }
  }
}

fun guard( w1 : evv<e>, cont : a -> e b, x : a ) : e b {
  val w2 = evv-current()
  if (evv-equal(w1,w2)) then cont(x) else unsafe-total{ error("non-scoped resumption") }
}

public fun ihandle( tag:htag<h>, h : h<e1,r>, action : () -> e2 r ) : e1 r {
  val m  = fresh-marker()
  val w1 = evv-current()
  val ev = Ev(tag,m,h,w1)
  val w2 = w1.evv-insert(ev)
  prompt(w1,w2,m,unsafe-coerce(action)())
}

extern inline unsafe-ref-to-local( r : ref<h,a> ) : local-var<h,a> { inline "#1" }
extern inline unsafe-st( action : (a) -> <div,st<h>|e> b ) : ((a) -> e b) { inline "#1" }

extern yld-local-var-create(x:a) : total (local-var<s,a>) {
  js inline "{value:#1}"
}

fun prompt-local-var(r:ref<h,a>, res : b  ) : <div,st<h>,local<h>|e> b {
  if (!yielding()) return res;
  val v = !r
  yield-extend( res, fun(x){ r := v; prompt-local-var(r,x) } )
}

fun ilocal-var(init:a, action: (l:local-var<s,a>) -> <local<s>|e> b ) : e b {
  unsafe-total {
    val r   = ref(init)
    val res = unsafe-coerce(action)(unsafe-ref-to-local(r))
    prompt-local-var(r,res)
  }
}


// -------------------------------------------
// Effect handlers
// -------------------------------------------

type op<a,b,e,r> {
  Tail(op : (a -> e b) )
  Control(op : (x:a, k: b -> e r) -> e r )
}

fun tail2(op : (a,b) -> e c ) : op<(a,b),c,e,r> {
  Tail( fun(t:(_,_)){ op(t.fst,t.snd) } )
}

public fun iperform( ev : ev<h>, op : (forall<e1,r> h<e1,r> -> op<a,b,e1,r>), x : a ) : e b {
  match(ev) {
    Ev(_tag,m,h,w) -> match(h.op) {
      Tail(clause)    -> under(w,clause,x)
      Control(clause) -> yield-to( m, fun(k){ clause(x,k) } )
    }
  }
}

type op2<a,b,c,e,r> {
  Tail2(op : (a,b) -> e c)
  Control2(op : (x:a,y:b, k: c -> e r) -> e r )
}

public fun iperform2( evx : ev<h>, op : (forall<e1,r> h<e1,r> -> op2<a,b,c,e1,r>), x : a, y : b ) : e c {
  match(evx) {
    Ev(_tag,m,h,w) -> match(h.op) {
      Tail2(clause)    -> under(w,clause,x,y)
      Control2(clause) -> yield-to(m, fun(k){ clause(x,y,k) })
    }
  }
}

// -------------------------------------------
// Example
// -------------------------------------------

type amb :: HX;

val amb-tag : htag<amb-hnd> = Unsafe-htag("amb")

type amb-hnd<e,r> {
  con Amb-hnd( op-flip : op<(),bool,e,r> , op-op2 : op<(int,double),bool,e,r> )
}

fun amb-handle( h : amb-hnd<e,r>, action : () -> <amb|e> r ) : e r {
  ihandle(amb-tag,h,action)
}

fun amb-flip( i : int ) : <amb|e> bool {
  iperform(ievv-select(i,amb-tag),op-flip,())
}

public fun amb() {
  amb-handle(
    Amb-hnd( Control( fun(x,k){
                with xs : list<bool> = yield-bind(k(True))
                with ys = yield-bind( k(False) )
                xs + ys
             }),
             tail2( fun(x,y){ True } )
    ))
  {
    with p = yield-bind( amb-flip(0) )
    [p]
  }
}



type state<a> :: V -> HX;

val state-tag : forall<a> htag<state-hnd<a>> = Unsafe-htag("state")

type state-hnd<a,e,r> {
  con State-hnd( op-get : op<(),a,e,r>, op-set : op<a,(),e,r> )
}

fun state-handle( h : state-hnd<a,e,r>, action : () -> <state<a>|e> r ) : e r {
  ihandle(state-tag,h,action)
}

fun state-get( i : int ) : <state<a>|e> a {
  iperform( ievv-select(i,state-tag), op-get, () )
}
fun state-set( i : int, x : a ) : <state<a>|e> () {
  iperform( ievv-select(i,state-tag), op-set, x )
}

public fun add42()  {
  with s = ilocal-var(0)
  state-handle(
    State-hnd( Tail( fun(x){ s } ), Tail( fun(x){ s := x }) ),
    {
      with _x = yield-bind( state-set(0,42) )
      state-get(0)
    }
  )
}

/*
handle(action) {
  flip() -> resume(True)
  op1(x) ->
  op2(x,y) ->
}
~>
amb-handle( Amb-hnd(
   Control( fun((x,y),k){k(True)}) )
   Tail()
  ,action
)
*/

/*
fun map-acc(w,xs,f,acc) {
  while(1) {
  match(xs) {
    Nil {
      acc.reverse
    }
    Cons(x,xx) {
      map-acc(xx,f, Cons(f(x),acc))
      ~>
        val y = f(x)
        if (typeof y == Yield) ...
        map-acc(xx,f,Cons(y,acc))
      }
    }
  }
}
*/
