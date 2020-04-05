public module std/core/hnd

extern include {
  js file "hnd-inline.js"
}


// -------------------------------------------
// Internal types
// -------------------------------------------

private struct marker<e::E,a>(m:int32)

abstract type htag<h::(E,V)->V> {
  Htag(:string)
}

public fun ".htag"( tag : string ) {
  Htag(tag)
}

abstract type ev<h> {
  con Ev<e,r>(:htag<h>, :marker<e,r>, :h<e,r>, :evv<e>)
}

private type evv<e::E>;

public alias ev-index = int32

// -------------------------------------------
// Internal Markers
// -------------------------------------------

private extern fresh-marker-int() : int32 {
  js inline "$marker_unique++"
}

private fun fresh-marker() : marker<e,r> {
  Marker(fresh-marker-int())
}


// -------------------------------------------
// Internal Evidence vectors
// -------------------------------------------

private extern inline evv-total() : evv<<>> {
  js inline "{ ofs: 0, evv: [] }"
}

public extern inline ".evv-at"( i : ev-index ) : e ev<h> {
  js inline "$std_core_hnd._evv[$std_core_hnd._evv_ofs + #1]"
}

public extern ".evv-index"<e,h>( htag : htag<h> ) : e ev-index {
  js inline "_evv_index($std_core_hnd._evv,$std_core_hnd._evv_ofs,#1)"
}


public extern ".evv-lookup"( htag : htag<h> ) : e ev<h> {
  js inline "_evv_lookup($std_core_hnd._evv,$std_core_hnd._evv_ofs,#1)"
}

public extern inline ".evv-select"( i : ev-index, htag : htag<h> ) : e ev<h> {
  js inline "(#1 >= 0 ? $std_core_hnd._evv[#1 + $std_core_hnd._evv_ofs] : _evv_lookup($std_core_hnd._evv,$std_core_hnd._evv_ofs,#2))"
}

private extern inline evv-get() : e evv<e> {
  js inline "{ evv: $std_core_hnd._evv, ofs: $std_core_hnd._evv_ofs }"
}

private extern evv-insert( evv : evv<e1>, ev : ev<h> ) : evv<e2> {
  js "_evv_insert"
}

private extern evv-delete( w : evv<e1>, i : ev-index ) : evv<e2> {
  js "_evv_delete"
}

private extern evv-create( evv : evv<e1>, indices : vector<ev-index> ) : evv<e2> {
  js "_evv_create"
}

private extern inline evv-set( w : evv<e1> ) : e2 () {
  js inline "(function(){ $std_core_hnd._evv = (#1).evv; $std_core_hnd._evv_ofs = (#1).ofs; }())"
}

private extern inline evv-set-ofs( ofs : ev-index ) : e ev-index {
  js inline "(function(){ const _ofs = $std_core_hnd._evv_ofs; $std_core_hnd._evv_ofs = #1; return _ofs; }())"
}

private extern evv-equals( w : evv<e> ) : bool {
  js inline "($std_core_hnd._evv === w.evv && $std_core_hnd._evv_ofs === w.ofs)"
}

private extern evv-show( evv : evv<e> ) : string {
  js "_evv_show"
}

private extern evv-expect( m: marker<e,r>, expected : evv<e0> ) : e () {
  js "_evv_expect"
}

// -------------------------------------------
// Internal Yielding
// -------------------------------------------

public extern inline yielding() : bool {
  js inline "($std_core_hnd._yield !== null)"
}

public extern inline yielding-non-final() : bool {
  js inline "($std_core_hnd._yield !== null && !$std_core_hnd._yield.final)"
}

public extern yield-extend(next : a -> e b ) : e b {
  js "_yield_extend"
}

public fun yield-bind( x : a, next : a -> e b ) : e b {
  if (yielding()) then yield-extend(next) else next(x)
}

private extern yield-cont(f : (a -> e1 b, a) -> e0 r ) : e0 r {  // make hidden public?
  js "_yield_cont"
}

private extern inline yielding-final() : e r {
  js inline "undefined"
}

private extern yield-prompt( m: marker<e,r> ) : yld<e,a,r> {
  js "_yield_prompt"
}

private extern inline yield-to-prim( m : marker<e1,r>, clause : (b -> e1 r) -> e1 r ) : e (() -> b) {
  js "_yield_to"
}

private extern inline yield-to-final( m : marker<e1,r>, clause : ((() -> b) -> e1 r) -> e1 r ) : e b {
  js "_yield_final"
}

private fun yield-to( m : marker<e1,r>, clause : ((() -> b) -> e1 r) -> e1 r ) : e1 b {
  val w0 = evv-get()
  val keep0 = yield-to-prim(m, clause)
  yield-extend( fun(f){ val keep1 = evv-set(w0); f() })
}

// -------------------------------------------
//
// -------------------------------------------

private extern inline unsafe-coerce(x:a) : b { inline "#1" }
private extern inline cast-ev0( f:() -> e1 b) : (() -> e0 b) { inline "#1" }
private extern inline cast-ev1( f:(a1) -> e1 b) : ((a1) -> e0 b) { inline "#1" }
private extern inline cast-ev2( f:(a1,a2) -> e1 b) : ((a1,a2) -> e0 b) { inline "#1" }


private type yld<e,a,r> {
  Pure
  YieldingFinal
  Yielding
  Yield<b>(clause : ((() -> b) -> e r) -> e r, cont : maybe<(() -> b) -> e a>)
}

private fun guard(w : evv<e> ) : e () {
  if (evv-equals(w)) then () else cast-ev1(error)("non-scoped resumption")
}

private fun prompt( w0:evv<e0>, w1:evv<e1>, m : marker<e0,r>, ret: a -> e0 r, result : a ) : e0 r
{
  //evv-expect(m,w1)
  match(yield-prompt(m)) {
    Pure {
      evv-set(w0)
      ret(result)
    }
    YieldingFinal {
      yielding-final()
    }
    Yielding {
      // evv-set(w0)  // only needed if we do evidence expected check
      yield-cont(fun(cont,res){ prompt(w0,w1,unsafe-decreasing(m),ret,cont(res)); })
    }
    Yield(clause,mbcont) {
      fun deep(f) {
        guard(w0)
        val g = match(mbcont) {
                  Just(cont) -> cont //prompt(w0,w1,unsafe-decreasing(m),ret,cont(f))
                  Nothing    -> cast-ev1(error)("trying to resume a non-resumable resumption")
                }
        prompt(w0,w1,unsafe-decreasing(m),ret,g(f))
      }
      evv-set(w0)
      clause(deep)
    }
  }
}

public fun ".handle"( tag:htag<h>, h : h<e,r>, ret: a -> e r, action : () -> e1 a ) : e r {
  val m  = fresh-marker()
  val w0 = evv-get()
  val ev = Ev(tag,m,h,w0)
  val w1 = evv-insert(w0,ev)
  evv-set(w1)
  prompt(w0,w1,m,ret,cast-ev0(action)())
}

//public fun hidden-handler( tag:htag<h>, h : h<e,r>, ret: a -> e r ) : ((action : () -> e1 a ) -> e r) {
//  (fun(action){ hidden-handle(tag,h,ret,action) })
//}

// -------------------------------------------
// named handler
// -------------------------------------------

abstract struct hname<h>( ev : ev<h> )

public fun ".named-handle"( tag:htag<h>, h : h<e,r>, ret: a -> e r, action : ev<h> -> e a ) : e r {
  val m  = fresh-marker()
  val w0 = evv-get()
  val ev = Ev(tag,m,h,w0)
  prompt(w0,w0,m,ret,action(ev))
}

// -------------------------------------------
// mask
// -------------------------------------------

public fun ".mask"( i : ev-index, action : () -> e1 a ) : e2 a {
  val w0 = evv-get()
  val w1 = evv-delete(w0,i)
  under0(w1,cast-ev0(action))
}


// -------------------------------------------
// Local variables
// -------------------------------------------

private fun prompt-local-var(r:ref<h,a>, res : b  ) : <div,st<h>,local<h>|e> b {
  if (!yielding()) return res;
  val v = !r
  yield-cont(fun(cont,x){ r := v; prompt-local-var(r,cont(x)) } )  // restore state early before the resume
}

public fun local-var(init:a, action: (l:local-var<s,a>) -> <local<s>|e> b ) : <local<s>|e> b {
  val r   = cast-ev1(ref)(init)
  val res = cast-ev1(action)(unsafe-coerce(r))
  cast-ev2(prompt-local-var)(r,res)
}

//private extern inline ilocal-varx(init:a, action: (l:local-var<s,a>) -> <local<s>|e> b ) : <local<s>|e> b {
//  js inline "(function(){ const _r = { value: (#1) }; const _res = (#2)(_r); if (!_yielding()) return _res; return prompt_local_var(_r,_res); })()"
//}


// -------------------------------------------
// Finally/Initially
// -------------------------------------------

public fun finally( fin : () -> e (), action : () -> e a ) : e a {
  finally-prompt(fin, action());
}

private fun finally-prompt(fin : () -> e (), res : a ) : e a {
  if (yielding-non-final()) return yield-cont(fun(cont,x){ finally-prompt(unsafe-decreasing(fin),cont(x)) })
  fin()
  if (yielding()) return yield-extend( fun(_x){ res } )
  res
}

private fun finalize(cont : (() -> b) -> e r, res : a) : e a {
  val m : marker<_e,_r> = fresh-marker()
  val w = evv-get()
  prompt(w,w,m,id, yield-bind( cont( { yield-to-final(m,fun(_k){ res }) } ), fun(_x) { res } ))
}

public fun initially(init : (int) -> e (), action : () -> e a ) : e a {
  init(0)
  if (yielding()) return yield-extend(fun(_ret){ initially-prompt(init,action()) })
  initially-prompt(init, action() )
}

private fun initially-prompt( init : (int) -> e (), res : a ) : e a {
  if (yielding-non-final()) then return unsafe-total {
    val count = ref(0);
    return yield-cont(fun(cont,x){
      val cnt = !count                   // increase counter on every resumption
      count := cnt + 1
      if (cnt == 0) then cont(x) else {  // for every resume after the first, run the initializer
        val r = inject-st{init(cnt)}
        if (yielding()) return yield-extend( fun(_ret){ cont(x) })
        cont(x)
      }
    })
  }
  res
}

// -------------------------------------------
// Resume context
// -------------------------------------------

abstract struct resume-context<b,e::E,r>( k : (() -> b) -> e r )

public fun resume( r : resume-context<b,e,r>, x : b ) : e r {
  (r.k)({x})
}

public fun finalize( r : resume-context<b,e,r>, x : a ) : e a {
  finalize(r.k,x)
}

// -------------------------------------------
// Clauses
// -------------------------------------------

abstract type clause1<a,b,e,r> {
  Clause1( clause: (marker<e,r>, evv<e>, a) -> e b )
}

private extern inline cast-ev0( f : (marker<e1,r>,evv<e1>) -> e1 b) : e ((marker<e1,r>,evv<e1>) -> e b) { inline "#1" }
private extern inline cast-ev1( f : (marker<e1,r>,evv<e1>,a) -> e1 b) : e ((marker<e1,r>,evv<e1>,a) -> e b) { inline "#1" }
private extern inline cast-ev2( f : (marker<e1,r>,evv<e1>,a1,a2) -> e1 b) : e ((marker<e1,r>,evv<e1>,a1,a2) -> e b) { inline "#1" }

public fun ".perform1"( ev : ev<h>, op : (forall<e1,r> h<e1,r> -> clause1<a,b,e1,r>), x : a ) : e b {
  match(ev) {
    Ev(_tag,m,h,w) -> match(h.op) {
      Clause1(f) -> cast-ev1(f)(m,w,x)
    }
  }
}

private fun under1( w : evv<e>, op : a -> e b, x : a ) : e b {
  val w0 = evv-get()
  evv-set(w)
  val y = op(x)
  // evv-set(w0)   // only needed before yielding for evidence expected check in prompt
  if (yielding()) return yield-extend(fun(yy){ evv-set(w0); yy })
  evv-set(w0)
  y
}


public fun clause-control-raw1( op : (x:a, r: resume-context<b,e,r>) -> e r ) : clause1<a,b,e,r> {
  Clause1(fun(m,w,x){ yield-to(m, fun(k){ op(x,Resume-context(k)) } ) } )
}

private fun get( ref: ref<h,a>) : <read<h>,div> a {
  !ref
}

private fun protect-check( resumed : ref<h,bool>,  k : (() -> b) -> e r, res : a ) : e a {
  val did-resume : bool = cast-ev1(get)(resumed)
  if (!did-resume)
   then finalize(k,res)
   else res
}

private fun protect( x : a, clause : (x:a, k: b -> e r) -> e r, k : (() -> b) -> e r ) : e r {
  val resumed = cast-ev1(ref)(False)
  fun kprotect(ret) {
    val keep = cast-ev2(set)(resumed,True)
    k({ret})
  }
  val res = clause(x,kprotect)
  if (yielding()) return yield-extend( fun(xres){ protect-check(resumed,k,xres) } )
  protect-check(resumed,k,res)
}

public fun clause-control1( clause : (x:a, k: b -> e r) -> e r ) : clause1<a,b,e,r> {
  Clause1(fun(m,w,x){ yield-to(m, fun(k){ protect(x,clause,k) }) })
}

public fun clause-tail1(op : a -> e b) : clause1<a,b,e,r> {
  Clause1(fun(_m,w,x){ under1(w,op,x) })
}

public fun clause-tail-noyield1(op : a -> e b) : clause1<a,b,e,r> {
  Clause1(fun(_m,_w,x){ op(x) })
}

public fun clause-never1( op : a -> e r ) : clause1<a,b,e,r> {
  Clause1(fun(m,w,x){ yield-to-final(m, fun(_k){ op(x) }) })
}

//----------------------------------------------------------------
// 0 arguments; reuse 1 argument Clauses
//----------------------------------------------------------------

abstract type clause0<b,e,r> {
  Clause0( clause: (marker<e,r>, evv<e>) -> e b )
}

public fun ".perform0"( ev : ev<h>, op : (forall<e1,r> h<e1,r> -> clause0<b,e1,r>) ) : e b {
  match(ev) {
    Ev(_tag,m,h,w) -> match(h.op) {
      Clause0(f) -> cast-ev0(f)(m,w)
    }
  }
}

private fun under0( w : evv<e>, op : () -> e b) : e b {
  val w0 = evv-get()
  evv-set(w)
  val y = op()
  // evv-set(w0)   // only needed before yielding for evidence expected check in prompt
  if (yielding()) return yield-extend(fun(yy){ evv-set(w0); yy })
  evv-set(w0)
  y
}

public fun clause-control-raw0( op : resume-context<b,e,r> -> e r ) : clause0<b,e,r> {
  Clause0(fun(m,w){ yield-to(m, fun(k){ op(Resume-context(k)) } ) })
}

public fun clause-control0( op : (b -> e r) -> e r ) : clause0<b,e,r> {
  Clause0(fun(m,w){ yield-to(m, fun(k){ protect((),fun(_x,r){ op(r) }, k) }) })
}


public fun clause-tail0(op : () -> e b) : clause0<b,e,r> {
  Clause0(fun(_m,w){ under0(w,op) })
}

public fun clause-tail-noyield0(op : () -> e b) : clause0<b,e,r> {
  Clause0(fun(_m,_w){ op() })
}

public fun clause-value(v : b) : clause0<b,e,r> {
  Clause0(fun(_m,_w){ v })
}

public fun clause-never0( op : () -> e r ) : clause0<b,e,r> {
  Clause0(fun(m,w){ yield-to-final(m, fun(_k){ op() }) })
}

//----------------------------------------------------------------
// 2 arguments
//----------------------------------------------------------------

abstract type clause2<a1,a2,b,e,r> {
  Clause2( clause: (marker<e,r>, evv<e>, a1, a2) -> e b )
}

private fun under2( w : evv<e>, op : (a1,a2) -> e b, x1 : a1, x2 : a2 ) : e b {
  val w0 = evv-get()
  evv-set(w)
  val z = op(x1,x2)
  if (yielding()) return yield-extend(fun(zz){ evv-set(w0); zz })
  evv-set(w0)
  z
}

public fun clause-control-raw2( op : (x1:a1, x2:a2, r: resume-context<b,e,r>) -> e r ) : clause2<a1,a2,b,e,r> {
  Clause2(fun(m,w,x1,x2){ yield-to(m, fun(k){ op(x1,x2,Resume-context(k)) } ) })
}

public fun clause-tail2(op : (a1,a2) -> e b) : clause2<a1,a2,b,e,r> {
  Clause2(fun(m,w,x1,x2){ under2(w,op,x1,x2) })
}

public fun clause-tail-noyield2(op : (a1,a2) -> e b) : clause2<a1,a2,b,e,r> {
  Clause2(fun(m,w,x1,x2){ op(x1,x2) })
}

public fun ".perform2"( evx : ev<h>, op : (forall<e1,r> h<e1,r> -> clause2<a,b,c,e1,r>), x : a, y : b ) : e c {
  match(evx) {
    Ev(_tag,m,h,w) -> match(h.op) {
      Clause2(f) -> cast-ev2(f)(m,w,x,y)
    }
  }
}

public fun clause-never2( op : (a1,a2) -> e r ) : clause2<a1,a2,b,e,r> {
  Clause2(fun(m,w,x1,x2){ yield-to-final(m, fun(_k){ op(x1,x2) }) })
}


//----------------------------------------------------------------
// 3 arguments: reuse 1 argument clause.
// Or should the compiler do tupling/untupling?
//----------------------------------------------------------------

// For interal use
private fun xperform1( ev : ev<h>, op : (forall<e1,r> h<e1,r> -> clause1<a,b,e1,r>), x : a ) : e b {
  match(ev) {
    Ev(_tag,m,h,w) -> match(h.op) {
      Clause1(f) -> cast-ev1(f)(m,w,x)
    }
  }
}

public fun clause-control-raw3( op : (x1:a1, x2:a2, x3:a3, r: resume-context<b,e,r>) -> e r ) : clause1<(a1,a2,a3),b,e,r> {
  clause-control-raw1( fun(x:(_,_,_),r){ op(x.fst,x.snd,x.thd,r) } )
}

public fun clause-control3( op : (x1:a1, x2:a2, x3:a3, k: b -> e r) -> e r ) : clause1<(a1,a2,a3),b,e,r> {
  clause-control1( fun(x:(_,_,_),k){ op(x.fst,x.snd,x.thd,k) } )
}

public fun clause-tail3(op : (a1,a2,a3) -> e b) : clause1<(a1,a2,a3),b,e,r> {
  clause-tail1( fun( x:(_,_,_) ){ op(x.fst,x.snd,x.thd) } )
}

public fun clause-tail-noyield3(op : (a1,a2,a3) -> e b) : clause1<(a1,a2,a3),b,e,r> {
  clause-tail-noyield1( fun( x:(_,_,_) ){ op(x.fst,x.snd,x.thd) } )
}

public fun clause-never3( op : (a1,a2,a3) -> e r ) : clause1<(a1,a2,a3),b,e,r> {
  clause-never1(fun(x:(_,_,_)){ op(x.fst,x.snd,x.thd) } )
}

public fun ".perform3"( ev : ev<h>, op : (forall<e1,r> h<e1,r> -> clause1<(a1,a2,a3),b,e1,r>), x1 : a1, x2 : a2, x3 : a3 ) : e b {
  xperform1(ev,op,(x1,x2,x3))
}


// -------------------------------------------
// Open
// -------------------------------------------
public extern inline ".open"<e1,e2,a,b>( x : a ) : e2 b {
  inline "(#1)"
}

public fun ".open-none0"( f : () -> e1 b ) : e2 b {
  cast-ev0(f)()
}

public fun ".open-none1"( f : a -> e1 b, x : a ) : e2 b {
  cast-ev1(f)(x)
}

public fun ".open-none2"( f : (a1,a2) -> e1 b, x1 : a1, x2 : a2 ) : e2 b {
  cast-ev2(f)(x1,x2)
}

public fun ".open-at0"( i: ev-index, f : () -> e1 b ) : e2 b {
  val ofs = evv-set-ofs(i)
  val res = cast-ev0(f)()
  if (yielding()) return yield-extend( fun(xres){ val j = evv-set-ofs(ofs); xres } );
  val j = evv-set-ofs(ofs)
  res
}

public fun ".open-at1"( i: ev-index, f : a -> e1 b, x : a ) : e2 b {
  val ofs = evv-set-ofs(i)
  val res = cast-ev1(f)(x)
  if (yielding()) return yield-extend( fun(xres){ val j = evv-set-ofs(ofs); xres } );
  val j = evv-set-ofs(ofs)
  res
}

public fun ".open0"( indices : vector<ev-index>, f : () -> e1 b ) : e2 b {
  val w = evv-get()
  evv-set( evv-create(w,indices) )
  val res = cast-ev0(f)()
  if (yielding()) return yield-extend( fun(xres){ evv-set(w); xres } );
  evv-set(w)
  res
}
public fun ".open1"( indices : vector<ev-index>, f : a -> e1 b, x : a ) : e2 b {
  val w = evv-get()
  evv-set( evv-create(w,indices) )
  val res = cast-ev1(f)(x)
  if (yielding()) return yield-extend( fun(xres){ evv-set(w); xres } );
  evv-set(w)
  res
}
