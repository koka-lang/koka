public module ev5

import std/num/int32

extern include {
  js file "yld5.js"
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
  js inline "{ ofs: 0, evv: [] }"
}

public extern evv-lookup( htag : htag<h> ) : ev<h> {
  js inline "_evv_lookup($evv,$evv_ofs,#1)"
}

public extern inline evv-at( i : int ) : ev<h> {
  js inline "$evv[$evv_ofs + #1]"
}

public extern inline evv-select( i : int, htag : htag<h> ) : ev<h> {
  js inline "(#1 >= 0 ? $evv[#1 + $evv_ofs] : _evv_lookup($evv,$evv_ofs,#2))"
}

extern inline evv-get() : e evv<e> {
  js inline "{ evv: $evv, ofs: $evv_ofs }"
}

extern evv-insert( evv : evv<e1>, ev : ev<h> ) : evv<e2> {
  js "_evv_insert"
}

extern evv-create( evv : evv<e1>, indices : vector<int> ) : evv<e2> {
  js "_evv_create"
}

extern inline evv-set( w : evv<e> ) : () {
  js inline "(function(){ $evv = (#1).evv; $evv_ofs = (#1).ofs; }())"
}

extern inline evv-set-ofs( ofs : int ) : int {
  js inline "(function(){ const _ofs = $evv_ofs; $evv_ofs = #1; return _ofs; }())"
}

extern evv-equals( w : evv<e> ) : bool {
  js inline "($evv === w.evv && $evv_ofs === w.ofs)"
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

extern inline yielding() : bool {
  js inline "($yield !== null)"
}

extern inline yielding-non-final() : bool {
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
          Nothing    -> unsafe-total{ error("trying to resume a non-resumable resumption") }
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
  val w1 = evv-insert(w0,ev)
  evv-set(w1)
  prompt(w0,w1,m,unsafe-coerce(action)())
}

public fun ihandler( tag:htag<h>, h : h<e,r> ) : ((action : () -> e1 r ) -> e r) {
  (fun(action){ ihandle(tag,h,action) })
}


// -------------------------------------------
// Local variables
// -------------------------------------------

fun prompt-local-var(r:ref<h,a>, res : b  ) : <div,st<h>,local<h>|e> b {
  if (!yielding()) return res;
  val v = !r
  yield-extend(fun(x){ r := v; prompt-local-var(r,x) } )    // restore the state (late) after returning from a resume
  // yield-cont(fun(cont,x){ r := v; prompt-local-var(r,cont(x)) } )  // restore state early before the resume
}

fun ilocal-var(init:a, action: (l:local-var<s,a>) -> <local<s>|e> b ) : <local<s>|e> b {
  unsafe-total {
    val r   = ref(init)
    val res = unsafe-coerce(action)(r)  // suppress effects
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

fun unit(x) { () }

fun finalize(cont : (() -> a) -> e b) : e () {
  val m : marker<_e,_b> = fresh-marker()
  val w = evv-get()
  prompt(w,w,m, unit(cont( { yield-to(m,fun(_k){ () }, True)() } )))
}

// -------------------------------------------
// Clauses
// -------------------------------------------

type clause<a,b,e,r> {
  Clause( clause: (marker<e,r>, evv<e>, a) -> e b )
}

extern inline cast-ev1( f : (marker<e1,r>,evv<e1>,a) -> e1 b) : e ((marker<e1,r>,evv<e1>,a) -> e b) { inline "#1" }
extern inline cast-ev2( f : (marker<e1,r>,evv<e1>,a1,a2) -> e1 b) : e ((marker<e1,r>,evv<e1>,a1,a2) -> e b) { inline "#1" }

public fun iperform( ev : ev<h>, op : (forall<e1,r> h<e1,r> -> clause<a,b,e1,r>), x : a ) : b {
  match(ev) {
    Ev(_tag,m,h,w) -> match(h.op) {
      Clause(f) -> cast-ev1(f)(m,w,x)
    }
  }
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


fun clause-control( op : (x:a, k: b -> e r) -> e r ) : clause<a,b,e,r> {
  Clause(fun(m,w,x){ yield-to(m, fun(k){ op(x,k) }, False) })
}

fun clause-tail(op : a -> e b) : clause<a,b,e,r> {
  Clause(fun(_m,w,x){ under1(w,op,x) })
}

fun clause-tail-noyield(op : a -> e b) : clause<a,b,e,r> {
  Clause(fun(_m,_w,x){ op(x) })
}

fun clause-value(v : b) : clause<(),b,e,r> {
  Clause(fun(_m,_w,_x){ v })
}

fun clause-never( op : a -> e r ) : clause<a,b,e,r> {
  Clause(fun(m,w,x){ yield-to(m, fun(_k){ op(x) }, True) })
}

//----------------------------------------------------------------
// 0 arguments; reuse 1 argument Clauses
//----------------------------------------------------------------

public fun iperform0( ev : ev<h>, op : (forall<e1,r> h<e1,r> -> clause<(),b,e1,r>) ) : b {
  match(ev) {
    Ev(_tag,m,h,w) -> match(h.op) {
      Clause(f) -> cast-ev1(f)(m,w,())
    }
  }
}

fun under0( w : evv<e>, op : () -> e b) : e b {
  val w0 = evv-get()
  evv-set(w)
  val y = op()
  // evv-set(w0)   // only needed before yielding for evidence expected check in prompt
  if (yielding()) return yield-extend(fun(yy){ evv-set(w0); yy })
  evv-set(w0)
  y
}

fun clause-control0( op : (k: b -> e r) -> e r ) : clause<(),b,e,r> {
  Clause(fun(m,w,_x){ yield-to(m, fun(k){ op(k) }, False) })
}

fun clause-tail0(op : () -> e b) : clause<(),b,e,r> {
  Clause(fun(_m,w,_x){ under0(w,op) })
}

fun clause-tail-noyield0(op : () -> e b) : clause<(),b,e,r> {
  Clause(fun(_m,_w,_x){ op() })
}

//----------------------------------------------------------------
// 2 arguments
//----------------------------------------------------------------

type op2<a1,a2,b,e,r> {
  Clause2( clause: (marker<e,r>, evv<e>, a1, a2) -> e b )
}

fun under2( w : evv<e>, op : (a1,a2) -> e b, x1 : a1, x2 : a2 ) : e b {
  val w0 = evv-get()
  evv-set(w)
  val z = op(x1,x2)
  if (yielding()) return yield-extend(fun(zz){ evv-set(w0); zz })
  evv-set(w0)
  z
}

fun clause-control2( op : (x1:a1, x2:a2, k: b -> e r) -> e r ) : op2<a1,a2,b,e,r> {
  Clause2(fun(m,w,x1,x2){ yield-to(m, fun(k){ op(x1,x2,k) }, False) })
}

fun clause-tail2(op : (a1,a2) -> e b) : op2<a1,a2,b,e,r> {
  Clause2(fun(m,w,x1,x2){ under2(w,op,x1,x2) })
}

fun clause-tail-noyield2(op : (a1,a2) -> e b) : op2<a1,a2,b,e,r> {
  Clause2(fun(m,w,x1,x2){ op(x1,x2) })
}

public fun iperform2( evx : ev<h>, op : (forall<e1,r> h<e1,r> -> op2<a,b,c,e1,r>), x : a, y : b ) : c {
  match(evx) {
    Ev(_tag,m,h,w) -> match(h.op) {
      Clause2(f) -> cast-ev2(f)(m,w,x,y)
    }
  }
}

//----------------------------------------------------------------
// 3 arguments: reuse 1 argument clause.
// Or should the compiler do tupling/untupling?
//----------------------------------------------------------------

fun under3( w : evv<e>, op : (a1,a2,a3) -> e b, x1 : a1, x2 : a2, x3 : a3 ) : e b {
  val w0 = evv-get()
  evv-set(w)
  val z = op(x1,x2,x3)
  if (yielding()) return yield-extend(fun(zz){ evv-set(w0); zz })
  evv-set(w0)
  z
}

fun clause-control3( op : (x1:a1, x2:a2, x3:a3, k: b -> e r) -> e r ) : clause<(a1,a2,a3),b,e,r> {
  Clause( fun(m,w,x:(_,_,_)){ yield-to(m, fun(k){ op(x.fst,x.snd,x.thd,k) }, False ) } )
}

fun clause-tail3(op : (a1,a2,a3) -> e b) : clause<(a1,a2,a3),b,e,r> {
  Clause( fun(m,w,x:(_,_,_)){ under3(w,op,x.fst,x.snd,x.thd) } )
}

fun clause-tail-noyield3(op : (a1,a2,a3) -> e b) : clause<(a1,a2,a3),b,e,r> {
  Clause( fun(m,w,x:(_,_,_)){ op(x.fst,x.snd,x.thd) } )
}

// -------------------------------------------
// Open
// -------------------------------------------

fun open-at1( i: int, f : a -> e1 b, x : a ) : e2 b {
  val ofs = evv-set-ofs(i)
  val res = unsafe-coerce(f)(x)
  if (yielding()) return yield-extend( fun(xres){ val j = evv-set-ofs(ofs); xres } );
  val j = evv-set-ofs(ofs)
  res
}

fun open1( indices : vector<int>, f : a -> e1 b, x : a ) : e2 b {
  val w = evv-get()
  evv-set( evv-create(w,indices) )
  val res = unsafe-coerce(f)(x)
  if (yielding()) return yield-extend( fun(xres){ evv-set(w); xres } );
  evv-set(w)
  res
}

// -------------------------------------------
// Example
// -------------------------------------------

type amb :: HX;

val amb-tag : htag<amb-hnd> = Unsafe-htag("amb")

type amb-hnd<e,r> {
  con Amb-hnd( op-flip : clause<(),bool,e,r> )
}

fun amb-handler( h : amb-hnd<e,r> ) : ((action:() -> <amb|e> r) -> e r) {
  ihandler(amb-tag,h)
}

fun amb-handle( h : amb-hnd<e,r>, action : () -> <amb|e> r ) : e r {
  ihandle(amb-tag,h,action)
}

fun amb-flip( i : int ) : <amb|e> bool {
  iperform(evv-select(i,amb-tag),op-flip,())
}

val h = Amb-hnd( clause-control( fun(x,k) {
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

struct reader-hnd<a,e,r>( op-read : clause<(),a,e,r> )

fun read-handle( h : reader-hnd<a,e,r>, action : () -> <reader<a>|e> r) : e r {
  ihandle(reader-tag,h,action)
}

fun read(i:int) {
  iperform(evv-select(i,reader-tag), op-read, ())
}

fun reader(init,action) {
  read-handle( Reader-hnd( clause-control( fun(x,k){ k(init) } )), action)
}

// State
type state<a> :: V -> HX;

val state-tag : forall<a> htag<state-hnd<a>> = Unsafe-htag("state")

type state-hnd<a,e,r> {
  con State-hnd( op-get : clause<(),a,e,r>, op-set : clause<a,(),e,r> )
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
  state-handle(State-hnd( clause-tail-noyield( fun(x){ s } ), clause-tail-noyield( fun(x){ s := x }) ), { mask<local>(action) } )
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
  state-handle(State-hnd(
    clause-tail( fun(x){ read(-1) } ),
    clause-tail-noyield( fun(x){ s := x }) ), action)
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
