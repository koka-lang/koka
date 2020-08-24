public module ev4

import std/num/int32

extern include {
  js file "yld4.js"
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
  js inline "$marker_unique++"
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

public extern evv-lookup( htag : htag<h> ) : ev<h> {
  js inline "_evv_lookup($evv,#1)"
}

public extern inline evv-at( i : int ) : ev<h> {
  js inline "($evv)[#1]"
}

public extern inline evv-select( i : int, htag : htag<h> ) : ev<h> {
  js inline "(#1 >= 0 ? $evv[#1] : _evv_lookup($evv,#2))"
}

extern inline evv-get() : e evv<e> {
  js inline "$evv"
}

extern inline evv-equals(w : evv<e> ) : bool {
  js inline "($evv===#1)"
}

extern evv-set-insert( ev : ev<h> ) : evv<e2> {
  js inline "($evv = _evv_insert($evv,#1))"
}

extern inline evv-set( w : evv<e> ) : e () {
  js inline "($evv = #1)"
}

extern evv-show( evv : evv<e> ) : string {
  js "_evv_show"
}

extern evv-expect( m: marker<e,r>, expected : evv<e0> ) : e () {
  js "_evv_expect"
}

// -------------------------------------------
// Internal Yielding
// -------------------------------------------

extern inline yielding() : e bool {
  js inline "($yield !== null)"
}

extern inline yielding-non-final() : e bool {
  js inline "($yield !== null && !$yield.final)"
}

extern yield-extend(next : a -> e b ) : e b {
  js "_yield_extend"
}

fun yield-bind( x : a, next : a -> e b ) : e b {
  if (yielding()) then yield-extend(next) else next(x)
}

extern yield-cont(f : (a -> e1 b,a) -> e0 b ) : e0 b {
  js "_yield_cont"
}

extern yield-prompt( m: marker<e,a>, res : a ) : yld<e,a> {
  js "_yield_prompt"
}

extern yield-to( m : marker<e1,r>, clause : ((b -> e1 r) -> e1 r), final : bool ) : b {
  js "_yield_to"
}

// -------------------------------------------
//
// -------------------------------------------

extern inline unsafe-coerce(x:a) : b { inline "#1" }
extern inline cast-under0(w0:evv<e0>, w1:evv<e1>, f:() -> e1 b) : (() -> e0 b) { inline "#3" }
extern inline cast-under1(w0:evv<e0>, w1:evv<e1>, f:(a) -> e1 b) : ((a) -> e0 b) { inline "#3" }

abstract type yld<e,r> {
  Pure(x:r)
  Yielding(final:bool)
  Yield<b>(clause : (b -> e r) -> e r, cont : maybe<(() -> b) -> e r>)
}

fun guard(w : evv<e> ) : e () {
  if (evv-equals(w)) then () else unsafe-total{ error("non-scoped resumption") }
}

extern impossible( s : string ) : e a {
  js inline "(console.error(#1))"
}

fun prompt( w0:evv<e0>, w1:evv<e1>, m : marker<e0,r>, res : r ) : e0 r
{
  //evv-expect(m,w1)
  match(yield-prompt(m,res)) {
    Pure(x) -> {
      evv-set(w0)
      x
    }
    Yielding(final) {
      // evv-set(w0)  // only needed if we do evidence expected check
      if (final) then res else yield-cont(fun(cont,res){ prompt(w0,w1,unsafe-decreasing(m),cont(res)); })
    }
    Yield(clause,mbcont) {
      fun deep(x){
        guard(w0)
        match(mbcont) {
          Just(cont) -> prompt(w0,w1,unsafe-decreasing(m),cont({x}))
          Nothing    -> impossible("trying to resume a non-resumable resumption")
        }
      }
      evv-set(w0)
      clause(deep)
    }
  }
}

public fun ihandle( tag:htag<h>, h : h<e,r>, action : () -> e1 r ) : e r {
  val m  = fresh-marker()
  val w0 = evv-get()
  val ev = Ev(tag,m,h,w0)
  val w1 = evv-set-insert(ev)
  prompt(w0,w1,m,unsafe-coerce(action)())
}


// -------------------------------------------
// Local variables
// -------------------------------------------

fun prompt-local-var(r:ref<h,a>, res : b  ) : <div,st<h>,local<h>|e> b {
  if (!yielding()) return res;
  val v = !r
  yield-extend(fun(x){ r := v; prompt-local-var(r,x) } )
}

fun ilocal-var(init:a, action: (l:local-var<s,a>) -> <local<s>|e> b ) : <local<s>|e> b {
  unsafe-total {
    val r   = ref(init)
    val res = unsafe-coerce(action)(r)
    prompt-local-var(r,res)
  }
}

// -------------------------------------------
// Finally/Initially
// -------------------------------------------

fun finally( fin : () -> e (), action : () -> e a ) : e a {
  finally-prompt(fin, action());
}

fun finally-prompt(fin : () -> e (), res : a ) : e a {
  if (yielding-non-final()) return yield-cont(fun(cont,x){ finally-prompt(unsafe-decreasing(fin),cont(x)) })
  fin()
  res
}

fun unit(x) {
  ()
}

extern undefined() : a {
  js inline "undefined"
}

fun finalize(cont : (() -> a) -> e b) : e () {
  val m : marker<_e,_b> = fresh-marker()
  val w = evv-get()
  prompt(w,w,m, unit(cont( { yield-to(m,fun(_k){ () }, True)() } )))
}

// -------------------------------------------
// Perform Operations
// -------------------------------------------

abstract type op<a,b,e,r> {
  Tail(op : (evv<e>, a) -> e b)
  Control(op : (x:a, k: (b -> e r)) -> e r )
}

fun under1( w : evv<e>, op : a -> e b, x : a ) : e b {
  val w0 = evv-get()
  evv-set(w)
  val y = op(x)
  // evv-set(w0)   // only needed before yielding for evidence expected check in prompt
  if (yielding()) return yield-extend(fun(yy){ evv-set(w0); yy })
  evv-set(w0)
  y
}


fun op-control( op : (x:a, k: b -> e r) -> e r ) : op<a,b,e,r> {
  Control(op)
}

fun op-tail(op : a -> e b) : op<a,b,e,r> {
  Tail( fun(w,x){ under1(w,op,x) } )
}

fun op-tail-noyield(op : a -> e b) : op<a,b,e,r> {
  Tail( fun(w,x){ op(x) } )
}

fun op-constant(v : b) : op<(),b,e,r> {
  Tail( fun(_w,_x){ v } )
}

public fun iperform( ev : ev<h>, op : (forall<e1,r> h<e1,r> -> op<a,b,e1,r>), x : a ) : e b {
  match(ev) {
    Ev(_tag,m,h,w) -> match(h.op) {
      Tail(clause)    -> unsafe-coerce(clause)(w,x)
      Control(clause) -> yield-to(m,fun(k){ clause(x,k) },False)
    }
  }
}

type op2<a1,a2,b,e,r> {
  Tail2(op : (evv<e>,a1,a2) -> e b)
  Control2(op : (x1:a1, x2:a2, k: b -> e r) -> e r )
}

fun under2( w : evv<e>, op : (a1,a2) -> e b, x1 : a1, x2 : a2 ) : e b {
  val w0 = evv-get()
  evv-set(w)
  val z = op(x1,x2)
  if (yielding()) return yield-extend(fun(zz){ evv-set(w0); zz })
  evv-set(w0)
  z
}

fun op-control2( op : (x1:a1, x2:a2, k: b -> e r) -> e r ) : op2<a1,a2,b,e,r> {
  Control2(op)
}

fun op-tail2(op : (a1,a2) -> e b) : op2<a1,a2,b,e,r> {
  Tail2( fun(w,x,y){ under2(w,op,x,y) } )
}

fun op-tail-noyield2(op : (a1,a2) -> e b) : op2<a1,a2,b,e,r> {
  Tail2( fun(w,x,y){ op(x,y) } )
}

public fun iperform2( evx : ev<h>, op : (forall<e1,r> h<e1,r> -> op2<a,b,c,e1,r>), x : a, y : b ) : e c {
  match(evx) {
    Ev(_tag,m,h,w) -> match(h.op) {
      Tail2(clause)    -> unsafe-coerce(clause)(w,x,y)
      Control2(clause) -> yield-to(m,fun(k){ clause(x,y,k) },False)
    }
  }
}


fun under3( w : evv<e>, op : (a1,a2,a3) -> e b, x1 : a1, x2 : a2, x3 : a3 ) : e b {
  val w0 = evv-get()
  evv-set(w)
  val z = op(x1,x2,x3)
  if (yielding()) return yield-extend(fun(zz){ evv-set(w0); zz })
  evv-set(w0)
  z
}

fun op-control3( op : (x1:a1, x2:a2, x3:a3, k: b -> e r) -> e r ) : op<(a1,a2,a3),b,e,r> {
  Control( fun(x:(_,_,_),k){ op(x.fst,x.snd,x.thd,k) } )
}

fun op-tail3(op : (a1,a2,a3) -> e b) : op<(a1,a2,a3),b,e,r> {
  Tail( fun(w,x:(_,_,_)){ under3(w,op,x.fst,x.snd,x.thd) } )
}

fun op-tail-noyield3(op : (a1,a2,a3) -> e b) : op<(a1,a2,a3),b,e,r> {
  Tail( fun(w,x:(_,_,_)){ op(x.fst,x.snd,x.thd) } )
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
  iperform(evv-select(i,amb-tag),op-flip,())
}

val h = Amb-hnd( op-control( fun(x,k){
            with xs : list<_> = yield-bind(k(False))
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

fun test-amb() : list<bool> {
  amb {
    with b = yield-bind(xor())
    [b]
  }
}

// Reader with control
type reader<a> :: V -> HX;
val reader-tag : forall<a> htag<reader-hnd<a>> = Unsafe-htag("reader")

struct reader-hnd<a,e,r>( op-read : op<(),a,e,r> )

fun read-handle( h : reader-hnd<a,e,r>, action : () -> <reader<a>|e> r) : e r {
  ihandle(reader-tag,h,action)
}

fun read(i:int) {
  iperform(evv-select(i,reader-tag), op-read, ())
}

fun reader(init,action) {
  read-handle( Reader-hnd( op-control( fun(x,k){ k(init) } )), action)
}

// State
type state<a> :: V -> HX;

val state-tag : forall<a> htag<state-hnd<a>> = Unsafe-htag("state")

type state-hnd<a,e,r> {
  con State-hnd( op-get : op<(),a,e,r>, op-set : op<a,(),e,r> )
}

fun state-handle( h : state-hnd<a,e,r>, action : () -> <state<a>|e> r ) : e r {
  ihandle(state-tag,h,action)
}

fun state-get( i : int ) : <state<a>|e> a {
  iperform( evv-select(i,state-tag), op-get, () )
}
fun state-set( ev : ev<state-hnd<a>>, x : a ) : <state<a>|e> () {
  iperform( ev, op-set, x )
}

fun state(init : s, action : () -> <state<s>|e> a) : e a {
  with s = ilocal-var(init)
  state-handle(State-hnd( op-tail-noyield( fun(x){ s } ), op-tail-noyield( fun(x){ s := x }) ), { mask<local>(action) } )
}

public fun add21() : state<int> int {
  with i  = yield-bind(state-get(0))
  with _x = yield-bind(state-set(evv-at(0),i+21))
  state-get(0)
}

fun test-state() : int {
  state(21) {
    add21()
  }
}

fun inc() : state<int> int {
  with i = yield-bind(state-get(-1))
  with _x = yield-bind(state-set(evv-lookup(state-tag), i+1))
  i
}

fun xorst() {
  with p = yield-bind(xor())
  with i = yield-bind(inc())
  with j:int = yield-bind(read(-1))
  i+j
}

fun test-state-amb() : list<int> {  // 1,2,3,4
  with reader(1)
  with state(0)
  with amb
  with i = yield-bind(xorst())
  [i]
}

fun test-amb-state() : list<int> {  // 1,1,1,1
  with reader(1)
  with amb
  with state(0)
  with i = yield-bind(xorst())
  [i]
}


// Tests
fun state-x(init,action) {
  with s = ilocal-var(init)
  state-handle(State-hnd( op-tail( fun(x){ read(-1) } ), op-tail-noyield( fun(x){ s := x }) ), action)
}

fun test-difficult() : int {
  with reader(1)
  with state-x(42)
  //state-get(-1)
  with reader(2)
  with x:int = yield-bind(state-get(-1))
  with y = yield-bind(read(-1))
  (x+y)
}

fun test() {
  test-amb().show-list(show).println
  test-state().println
  test-difficult().println
  test-state-amb().show-list(show).println
  test-amb-state().show-list(show).println
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
