pub module ev5

import std/num/int32

extern include {
  js file "yld5.js"
}


// -------------------------------------------
// Internal types
// -------------------------------------------

struct marker<e::E,a>(m:int32)

pub type htag<h::(E,V)->V> {
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

pub extern inline evv-total() : evv<<>> {
  js inline "{ ofs: 0, evv: [] }"
}

pub extern evv-lookup( htag : htag<h> ) : int {
  js inline "_evv_lookup($evv,$evv_ofs,#1)"
}

pub extern inline evv-at( i : int ) : ev<h> {
  js inline "$evv[$evv_ofs + #1]"
}

pub extern inline evv-select( i : int, htag : htag<h> ) : ev<h> {
  js inline "(#1 >= 0 ? $evv[#1 + $evv_ofs] : $evv[_evv_lookup($evv,$evv_ofs,#2) + $evv_ofs])"
}

extern inline evv-get() : e evv<e> {
  js inline "{ evv: $evv, ofs: $evv_ofs }"
}

extern evv-insert( evv : evv<e1>, ev : ev<h> ) : evv<e2> {
  js "_evv_insert"
}

extern evv-delete( w : evv<e1>, i : int ) : evv<e2> {
  js "_evv_delete"
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

extern yield-cont(f : (a -> e1 b, a) -> e0 r ) : e0 r {
  js "_yield_cont"
}
extern yield-final() : e r {
  js inline "undefined"
}

extern yield-prompt( m: marker<e,r> ) : yld<e,a,r> {
  js "_yield_prompt"
}

extern yield-to-prim( m : marker<e1,r>, clause : (b -> e1 r) -> e1 r ) : (() -> b) {
  js "_yield_to"
}

extern yield-final( m : marker<e1,r>, clause : ((() -> b) -> e1 r) -> e1 r ) : b {
  js "_yield_final"
}

fun yield-to( m : marker<e1,r>, clause : ((() -> b) -> e1 r) -> e1 r ) : e1 b {
  val w0 = evv-get()
  val keep0 = yield-to-prim(m, clause)
  yield-extend( fn(f){ val keep1 = evv-set(w0); f() })
}

// -------------------------------------------
//
// -------------------------------------------

extern inline unsafe-coerce(x:a) : b { inline "#1" }
extern inline cast-ev0( f:() -> e1 b) : (() -> e0 b) { inline "#1" }
extern inline cast-ev1( f:(a1) -> e1 b) : ((a1) -> e0 b) { inline "#1" }
extern inline cast-ev2( f:(a1,a2) -> e1 b) : ((a1,a2) -> e0 b) { inline "#1" }


abstract type yld<e,a,r> {
  Pure
  YieldingFinal
  Yielding
  Yield<b>(clause : ((() -> b) -> e r) -> e r, cont : maybe<(() -> b) -> e a>)
}

fun guard(w : evv<e> ) : e () {
  if (evv-equals(w)) then () else cast-ev1(error)("non-scoped resumption")
}

fun prompt( w0:evv<e0>, w1:evv<e1>, m : marker<e0,r>, ret: a -> e0 r, result : a ) : e0 r
{
  //evv-expect(m,w1)
  match(yield-prompt(m)) {
    Pure {
      evv-set(w0)
      ret(result)
    }
    YieldingFinal {
      yield-final()
    }
    Yielding {
      // evv-set(w0)  // only needed if we do evidence expected check
      yield-cont(fn(cont,res){ prompt(w0,w1,unsafe-decreasing(m),ret,cont(res)); })
    }
    Yield(clause,mbcont) {
      fun deep(f) {
        guard(w0)
        match(mbcont) {
          Just(cont) -> prompt(w0,w1,unsafe-decreasing(m),ret,cont(f))
          Nothing    -> cast-ev1(error)("trying to resume a non-resumable resumption")
        }
      }
      evv-set(w0)
      clause(deep)
    }
  }
}

pub fun ihandle( tag:htag<h>, h : h<e,r>, ret: a -> e r, action : () -> e1 a ) : e r {
  val m  = fresh-marker()
  val w0 = evv-get()
  val ev = Ev(tag,m,h,w0)
  val w1 = evv-insert(w0,ev)
  evv-set(w1)
  prompt(w0,w1,m,ret,cast-ev0(action)())
}

pub fun ihandler( tag:htag<h>, h : h<e,r>, ret: a -> e r ) : ((action : () -> e1 a ) -> e r) {
  (fn(action){ ihandle(tag,h,ret,action) })
}


pub fun inamed-handle( tag:htag<h>, h : h<e,r>, ret: a -> e r, action : ev<h> -> e a ) : e r {
  val m  = fresh-marker()
  val w0 = evv-get()
  val ev = Ev(tag,m,h,w0)
  prompt(w0,w0,m,ret,action(ev))
}

// -------------------------------------------
// mask
// -------------------------------------------

pub fun imask( i : int, action : () -> e1 a ) : e2 a {
  val w0 = evv-get()
  val w1 = evv-delete(w0,i)
  under0(w1,cast-ev0(action))
}


// -------------------------------------------
// Local variables
// -------------------------------------------

fun prompt-local-var(r:ref<h,a>, res : b  ) : <div,st<h>,local<h>|e> b {
  if (!yielding()) return res;
  val v = !r
  yield-cont(fn(cont,x){ r := v; prompt-local-var(r,cont(x)) } )  // restore state early before the resume
}

fun ilocal-var(init:a, action: (l:local-var<s,a>) -> <local<s>|e> b ) : <local<s>|e> b {
  val r   = cast-ev1(ref)(init)
  val res = cast-ev1(action)(unsafe-coerce(r))
  cast-ev2(prompt-local-var)(r,res)
}


extern inline ilocal-varx(init:a, action: (l:local-var<s,a>) -> <local<s>|e> b ) : <local<s>|e> b {
  js inline "(function(){ const _r = { value: (#1) }; const _res = (#2)(_r); if (!_yielding()) return _res; return prompt_local_var(_r,_res); })()"
}


// -------------------------------------------
// Finally/Initially
// -------------------------------------------

fun finally( fin : () -> e (), action : () -> e a ) : e a {
  finally-prompt(fin, action());
}

fun finally-prompt(fin : () -> e (), res : a ) : e a {
  if (yielding-non-final()) return yield-cont(fn(cont,x){ finally-prompt(unsafe-decreasing(fin),cont(x)) })
  fin()
  if (yielding()) return yield-extend( fn(_x){ res } )
  res
}

fun finalize(cont : (() -> b) -> e r, res : a) : e a {
  val m : marker<_e,_r> = fresh-marker()
  val w = evv-get()
  prompt(w,w,m,id, yield-bind( cont( { yield-final(m,fn(_k){ res }) } ), fn(_x) { res } ))
}

fun initially(init : (int) -> e (), action : () -> e a ) : e a {
  init(0)
  if (yielding()) return yield-extend(fn(_ret){ initially-prompt(init,action()) })
  initially-prompt(init, action() )
}

fun initially-prompt( init : (int) -> e (), res : a ) : e a {
  if (yielding-non-final()) then return unsafe-total {
    val count = ref(0);
    return yield-cont(fn(cont,x){
      val cnt = !count                   // increase counter on every resumption
      count := cnt + 1
      if (cnt == 0) then cont(x) else {  // for every resume after the first, run the initializer
        val r = inject-st{init(cnt)}
        if (yielding()) return yield-extend( fn(_ret){ cont(x) })
        cont(x)
      }
    })
  }
  res
}

// -------------------------------------------
// Resume context
// -------------------------------------------

struct resume-context<b,e::E,r>( k : (() -> b) -> e r )

fun iresume( r : resume-context<b,e,r>, x : b ) : e r {
  (r.k)({x})
}

fun ifinalize( r : resume-context<b,e,r> ) : e () {
  finalize(r.k,())
}

// -------------------------------------------
// Clauses
// -------------------------------------------

type clause<a,b,e,r> {
  Clause( clause: (marker<e,r>, evv<e>, a) -> e b )
}

extern inline cast-ev1( f : (marker<e1,r>,evv<e1>,a) -> e1 b) : e ((marker<e1,r>,evv<e1>,a) -> e b) { inline "#1" }
extern inline cast-ev2( f : (marker<e1,r>,evv<e1>,a1,a2) -> e1 b) : e ((marker<e1,r>,evv<e1>,a1,a2) -> e b) { inline "#1" }

pub fun iperform( ev : ev<h>, op : (forall<e1,r> h<e1,r> -> clause<a,b,e1,r>), x : a ) : b {
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
  if (yielding()) return yield-extend(fn(yy){ evv-set(w0); yy })
  evv-set(w0)
  y
}


fun clause-control-raw( op : (x:a, r: resume-context<b,e,r>) -> e r ) : clause<a,b,e,r> {
  Clause(fn(m,w,x){ yield-to(m, fn(k){ op(x,Resume-context(k)) } ) } )
}

fun get( ref: ref<h,a>) : <read<h>,div|e> a {
  !ref
}

fun protect-check( resumed : ref<h,bool>,  k : (() -> b) -> e r, res : a ) : e a {
  val did-resume : bool = cast-ev1(get)(resumed)
  if (!did-resume)
   then finalize(k,res)
   else res
}

fun protect( x : a, clause : (x:a, k: b -> e r) -> e r, k : (() -> b) -> e r ) : e r {
  val resumed = cast-ev1(ref)(False)
  fun kprotect(ret) {
    val keep = cast-ev2(set)(resumed,True)
    k({ret})
  }
  val res = clause(x,kprotect)
  if (yielding()) return yield-extend( fn(xres){ protect-check(resumed,k,xres) } )
  protect-check(resumed,k,res)
}

fun clause-control( clause : (x:a, k: b -> e r) -> e r ) : clause<a,b,e,r> {
  Clause(fn(m,w,x){ yield-to(m, fn(k){ protect(x,clause,k) }) })
}

fun clause-tail(op : a -> e b) : clause<a,b,e,r> {
  Clause(fn(_m,w,x){ under1(w,op,x) })
}

fun clause-tail-noyield(op : a -> e b) : clause<a,b,e,r> {
  Clause(fn(_m,_w,x){ op(x) })
}

fun clause-value(v : b) : clause<(),b,e,r> {
  Clause(fn(_m,_w,_x){ v })
}

fun clause-never( op : a -> e r ) : clause<a,b,e,r> {
  Clause(fn(m,w,x){ yield-final(m, fn(_k){ op(x) }) })
}

//----------------------------------------------------------------
// 0 arguments; reuse 1 argument Clauses
//----------------------------------------------------------------

pub fun iperform0( ev : ev<h>, op : (forall<e1,r> h<e1,r> -> clause<(),b,e1,r>) ) : b {
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
  if (yielding()) return yield-extend(fn(yy){ evv-set(w0); yy })
  evv-set(w0)
  y
}

fun clause-control0( op : resume-context<b,e,r> -> e r ) : clause<(),b,e,r> {
  Clause(fn(m,w,_x){ yield-to(m, fn(k){ op(Resume-context(k)) } ) })
}

fun clause-tail0(op : () -> e b) : clause<(),b,e,r> {
  Clause(fn(_m,w,_x){ under0(w,op) })
}

fun clause-tail-noyield0(op : () -> e b) : clause<(),b,e,r> {
  Clause(fn(_m,_w,_x){ op() })
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
  if (yielding()) return yield-extend(fn(zz){ evv-set(w0); zz })
  evv-set(w0)
  z
}

fun clause-control2( op : (x1:a1, x2:a2, r: resume-context<b,e,r>) -> e r ) : op2<a1,a2,b,e,r> {
  Clause2(fn(m,w,x1,x2){ yield-to(m, fn(k){ op(x1,x2,Resume-context(k)) } ) })
}

fun clause-tail2(op : (a1,a2) -> e b) : op2<a1,a2,b,e,r> {
  Clause2(fn(m,w,x1,x2){ under2(w,op,x1,x2) })
}

fun clause-tail-noyield2(op : (a1,a2) -> e b) : op2<a1,a2,b,e,r> {
  Clause2(fn(m,w,x1,x2){ op(x1,x2) })
}

pub fun iperform2( evx : ev<h>, op : (forall<e1,r> h<e1,r> -> op2<a,b,c,e1,r>), x : a, y : b ) : c {
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

fun clause-control-raw3( op : (x1:a1, x2:a2, x3:a3, r: resume-context<b,e,r>) -> e r ) : clause<(a1,a2,a3),b,e,r> {
  clause-control-raw( fn(x:(_,_,_),r){ op(x.fst,x.snd,x.thd,r) } )
}

fun clause-control3( op : (x1:a1, x2:a2, x3:a3, k: b -> e r) -> e r ) : clause<(a1,a2,a3),b,e,r> {
  clause-control( fn(x:(_,_,_),k){ op(x.fst,x.snd,x.thd,k) } )
}

fun clause-tail3(op : (a1,a2,a3) -> e b) : clause<(a1,a2,a3),b,e,r> {
  clause-tail( fn( x:(_,_,_) ){ op(x.fst,x.snd,x.thd) } )
}

fun clause-tail-noyield3(op : (a1,a2,a3) -> e b) : clause<(a1,a2,a3),b,e,r> {
  clause-tail-noyield( fn( x:(_,_,_) ){ op(x.fst,x.snd,x.thd) } )
}

// -------------------------------------------
// Open
// -------------------------------------------

fun open-at1( i: int, f : a -> e1 b, x : a ) : e2 b {
  val ofs = evv-set-ofs(i)
  val res = cast-ev1(f)(x)
  if (yielding()) return yield-extend( fn(xres){ val j = evv-set-ofs(ofs); xres } );
  val j = evv-set-ofs(ofs)
  res
}

fun open1( indices : vector<int>, f : a -> e1 b, x : a ) : e2 b {
  val w = evv-get()
  evv-set( evv-create(w,indices) )
  val res = cast-ev1(f)(x)
  if (yielding()) return yield-extend( fn(xres){ evv-set(w); xres } );
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

fun amb-handler( h : amb-hnd<e,r>, ret : a -> e r ) : ((action:() -> <amb|e> a) -> e r) {
  ihandler(amb-tag,h,ret)
}

fun amb-handle( h : amb-hnd<e,r>, ret : a -> e r, action : () -> <amb|e> a ) : e r {
  ihandle(amb-tag,h,ret,action)
}

fun amb-flip( i : int ) : <amb|e> bool {
  iperform(evv-select(i,amb-tag),op-flip,())
}

val h = amb-handler( Amb-hnd( clause-control( fn(x,k) {
            with xs : list<_> = yield-bind(k(False))
            with ys = yield-bind( k(True) )
            xs + ys
         })), fn(x){ [x] } )

pub fun amb(action) {
  h(action)
}

pub fun amb2(action) {
  amb-handle( Amb-hnd( clause-control( fn(x,k) {
                with xs : list<_> = yield-bind(k(False))
                with ys = yield-bind( k(True) )
                xs + ys
              })),
              fn(x){ [x] },
              action )
}

fun xor() : amb bool {
  with p:bool = yield-bind(amb-flip(0))
  with q = yield-bind(amb-flip(0))
  (p!=q)
}

fun test-amb() : list<bool> {
  amb(xor)
}

// Reader with control
type reader<a> :: V -> HX;
val reader-tag : forall<a> htag<reader-hnd<a>> = Unsafe-htag("reader")

struct reader-hnd<a,e,r>( op-read : clause<(),a,e,r> )

fun read-handle( h : reader-hnd<a,e,r>, ret : b -> e r, action : () -> <reader<a>|e> b) : e r {
  ihandle(reader-tag,h,ret,action)
}

fun read(i:int) {
  iperform(evv-select(i,reader-tag), op-read, ())
}

fun reader(init,action) {
  read-handle( Reader-hnd( clause-control( fn(x,k){ k(init) } )), id, action)
}

// State
type state<a> :: V -> HX;

val state-tag : forall<a> htag<state-hnd<a>> = Unsafe-htag("state")

type state-hnd<a,e,r> {
  con State-hnd( op-get : clause<(),a,e,r>, op-set : clause<a,(),e,r> )
}

fun state-handle( h : state-hnd<a,e,r>, ret : b -> e r, action : () -> <state<a>|e> b ) : e r {
  ihandle(state-tag,h,ret,action)
}

fun state-get( i : int ) : <state<a>|e> a {
  iperform( evv-select(i,state-tag), op-get, () )
}
fun state-set( i : int, x : a ) : <state<a>|e> () {
  iperform( evv-select(i,state-tag), op-set, x )
}

fun state(init : s, action : () -> <state<s>|e> a) : e a {
  with s = ilocal-var(init)
  state-handle(State-hnd(
    clause-tail-noyield( fn(x){ s } ),
    clause-tail-noyield( fn(x){ s := x }) ),
    id, { mask<local>(action) } )
}

pub fun add21() : state<int> int {
  with i  = yield-bind(state-get(0))
  with _x = yield-bind(state-set(0,i+21))
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
  xorst()
}

fun test-amb-state() : list<int> {  // 1,1,1,1
  with reader(1)
  with amb
  with state(0)
  xorst()
}


// Tests
fun state-x(init,action) {
  with s = ilocal-var(init)
  state-handle(State-hnd(
    clause-tail( fn(x){ read(-1) } ),
    clause-tail-noyield( fn(x){ s := x }) ),
    id, action)
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
