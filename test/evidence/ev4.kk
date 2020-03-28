public module ev4

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


extern inline evv-current() : evv<e> {
  js inline "_context.evv"
}

extern inline evv-set( w : evv<e> ) : e () {
  js inline "(_context.evv = #1)"
}



// -------------------------------------------
// Internal Yielding
// -------------------------------------------

extern inline yielding() : e bool {
  js inline "(_context.yield !== 0)"
}

extern yield-extend(next : a -> e b ) : e b {
  js "_yield_extend"
}
extern yield-cont(f : (a -> e b,a) -> e b ) : e b {
  js "_yield_cont"
}

fun yield-bind( x : a, next : a -> e b ) : e b {
  if (yielding()) then yield-extend(next) else next(x)
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

fun prompt( w1:evv<e>, w2:evv<e2>, m : marker<e,r>, res : r ) : e r
{
  match(yield-prompt(w1,w2,m,res)) {
    Pure(x) -> x
    Yielding {
      yield-cont(fun(cont,res){ prompt(w1,w2,unsafe-decreasing(m),cont(res)); })
    }
    Yielded(clause,cont) {
      val deep = fun(x) {
        val w = evv-current()
        if (evv-equal(w1,w)) then prompt(w1,w2,unsafe-decreasing(m),cont(x)) else unsafe-total{ error("non-scoped resumption") }
      }
      clause(deep)
    }
  }
}

public fun ihandle( tag:htag<h>, h : h<e1,r>, action : () -> e2 r ) : e1 r {
  val m  = fresh-marker()
  val w1 = evv-current()
  val ev = Ev(tag,m,h,w1)
  val w2 = w1.evv-insert(ev)
  prompt(w1,w2,m,unsafe-coerce(action)())
}


fun prompt-local-var(r:ref<h,a>, res : b  ) : <div,st<h>,local<h>|e> b {
  if (!yielding()) return res;
  val v = !r
  yield-extend(fun(x){ r := v; prompt-local-var(r,x) } )
}

fun ilocal-var(init:a, action: (l:local-var<s,a>) -> <local<s>|e> b ) : e b {
  unsafe-total {
    val r   = ref(init)
    val res = unsafe-coerce(action)(r)
    prompt-local-var(r,res)
  }
}


// -------------------------------------------
// Effect handlers
// -------------------------------------------

type op<a,b,e,r> {
  Tail(op : (evv<e>, a) -> e b)
  Control(op : (x:a, k: b -> e r) -> e r )
}

fun under( w : evv<e>, op : a -> e b, x : a ) : e b {
  val w0 = evv-current()
  evv-set(w)
  val y = op(x)
  if (yielding()) return yield-extend(fun(yy){ evv-set(w0); yy })
  evv-set(w0)
  y
}

fun under( w : evv<e>, op : (a,b) -> e c, x : a, y : b ) : e c {
  val w0 = evv-current()
  evv-set(w)
  val z = op(x,y)
  if (yielding()) return yield-extend(fun(zz){ evv-set(w0); zz })
  evv-set(w0)
  z
}


fun tail(op : a -> e b) : op<a,b,e,r> {
  Tail( fun(w,x){ under(w,op,x) } )
}

fun tail-noyield(op : a -> e b) : op<a,b,e,r> {
  Tail( fun(w,x){ op(x) } )
}


public fun iperform( ev : ev<h>, op : (forall<e1,r> h<e1,r> -> op<a,b,e1,r>), x : a ) : e b {
  match(ev) {
    Ev(_tag,m,h,w) -> match(h.op) {
      Tail(clause)    -> unsafe-coerce(clause)(w,x)
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
      Tail2(clause)    -> unsafe-coerce(clause)(w,x,y)
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
  con Amb-hnd( op-flip : op<(),bool,e,r> )
}

fun amb-handle( h : amb-hnd<e,r>, action : () -> <amb|e> r ) : e r {
  ihandle(amb-tag,h,action)
}

fun amb-flip( i : int ) : <amb|e> bool {
  iperform(ievv-select(i,amb-tag),op-flip,())
}

val h = Amb-hnd( Control( fun(x,k){
            with xs : list<bool> = yield-bind(k(False))
            with ys = yield-bind( k(True) )
            xs + ys
         }))

public fun amb(action) {
  amb-handle(h,action)
}

fun xor() : amb bool {
  with p:bool = yield-bind(amb-flip(0))
  with q = yield-bind(amb-flip(0))
  (p!=q)
}

fun test-amb() {
  amb {
    with b = yield-bind(xor())
    [b]
  }
}

type reader<a> :: V -> HX;
val reader-tag : forall<a> htag<reader-hnd<a>> = Unsafe-htag("reader")

struct reader-hnd<a,e,r>( op-read : op<(),a,e,r> )

fun read-handle( h : reader-hnd<a,e,r>, action : () -> <reader<a>|e> r) : e r {
  ihandle(reader-tag,h,action)
}

fun read(i:int) {
  iperform(ievv-select(i,reader-tag), op-read, ())
}

fun reader(init,action) {
  read-handle( Reader-hnd( Control( fun(x,k){ k(init) } )), action)
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
fun state-set( ev : ev<state-hnd<a>>, x : a ) : <state<a>|e> () {
  iperform( ev, op-set, x )
}

fun state(init,action) {
  with s = ilocal-var(init)
  state-handle(State-hnd( tail( fun(x){ s } ), tail( fun(x){ s := x }) ), action)
}

public fun add21()  {
  with i  = yield-bind(state-get(0))
  with _x = yield-bind(state-set(ievv-select-at(0),i+21))
  state-get(0)
}

fun test-state() {
  state(21) {
    add21()
  }
}


fun state-x(init,action) {
  with s = ilocal-var(init)
  state-handle(State-hnd( tail( fun(x){
    read(-1)
  } ), tail( fun(x){ s := x }) ), action)
}

fun test-difficult() : int {
  with reader(1)
  with state-x(42)
  //state-get(-1)
  with reader(2)
  with _x = yield-bind(state-get(-1))
  read(-1)
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
