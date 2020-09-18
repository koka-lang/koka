public module std/core/hnd

import std/core/types

include extern {
  c  file "hnd-inline"
  js file "hnd-inline.js"
}

// -------------------------------------------
// Internal types
// -------------------------------------------

private struct marker<e::E,a>(m:int32)

abstract type htag<h::(E,V)->V> {
  Htag(:string)
}

public fun ".new-htag"( tag : string ) {
  Htag(tag)
}

public fun hidden-htag( tag : string ) {
  Htag(tag)
}

abstract type ev<h> {
  con Ev<e,r>(:htag<h>, :marker<e,r>, :h<e,r>, :evv<e>)
}

private type evv<e::E>;

public alias ev-index = size_t

// -------------------------------------------
// Internal Markers
// -------------------------------------------

private extern fresh-marker-int() : int32 {
  c inline "kk_marker_unique(kk_context())"
  js inline "$marker_unique++"
}

private fun fresh-marker() : marker<e,r> {
  Marker(fresh-marker-int())
}

private fun marker-none() : marker<e,r> {
  Marker(zero32())
}

// -------------------------------------------
// Internal Evidence vectors
// -------------------------------------------

private inline extern evv-total() : evv<<>> {
  c  "kk_evv_total"
  js inline "[]"
}


public inline extern ".evv-at"<e,h> ( i : ev-index ) : ev<h> {  // pretend total; don't simplify
  c inline "kk_evv_at(#1,kk_context())"
  js inline "$std_core_hnd._evv[#1]"
}

// (dynamically) find evidence insertion/deletion index in the evidence vector
public extern ".evv-index"<e::E,h>( htag : htag<h> ) : e ev-index {
  c inline "kk_evv_index(#1,kk_context())"
  js inline "__evv_index($std_core_hnd._evv,#1)"
}


public extern ".evv-lookup"( htag : htag<h> ) : ev<h> {
  c inline "kk_evv_lookup(#1,kk_context())"
  js inline "__evv_lookup($std_core_hnd._evv,#1)"
}

public inline extern ".evv-select"( i : ev-index, htag : htag<h> ) : ev<h> {
  c inline "(#1 >= 0 ? kk_evv_at(#1,kk_context()) : kk_evv_lookup(#2,kk_context()))"
  js inline "(#1 >= 0 ? $std_core_hnd._evv[#1] : __evv_lookup($std_core_hnd._evv,#2))"
}

private extern evv-show( evv : evv<e> ) : string {
  c  inline "kk_evv_show(#1,kk_context())"
  js "_evv_show"
}

private extern evv-eq(evv0 : evv<e>, evv1 : evv<e> ) : bool {
  c inline "kk_evv_eq(#1,#2,kk_context())"
  js inline "(#1) === (#2)"
}


private extern evv-get() : e evv<e> {
  c  inline "kk_evv_get(kk_context())"
  js inline "$std_core_hnd._evv"
}

private inline extern evv-set<e1,e>( w : evv<e1> ) : e () {
  c inline "kk_evv_set(#1,kk_context())"
  js inline "(function(){ $std_core_hnd._evv = (#1); })()"
}

private inline extern evv-swap<e1,e2>( w : evv<e1> ) : e evv<e2> {
  c inline "kk_evv_swap(#1,kk_context())"
  js inline "(function(){ const evv = $std_core_hnd._evv; $std_core_hnd._evv = (#1); return evv; })()"
}

private extern evv-insert( evv : evv<e1>, ev : ev<h> ) : e1 evv<e2> {
  c  inline "kk_evv_insert(#1,#2,kk_context())"
  js "_evv_insert"
}

private extern evv-swap-delete( i : ev-index, behind : bool ) : e1 evv<e> {
  c  inline "kk_evv_swap_delete(#1,#2,kk_context())"
  js "_evv_swap_delete"
}

private inline extern evv-swap-create0() : evv<<>> {
  c inline "kk_evv_swap_create0(kk_context())"
  js inline "(function(){ const evv = $std_core_hnd._evv; $std_core_hnd._evv = $std_core_hnd._evv_empty; return evv; })()"
}

private inline extern evv-swap-create1( i : ev-index ) : e evv<e> {   //not quite the right effect type but avoids unbound effect types
  c inline "kk_evv_swap_create1(#1,kk_context())"
  js inline "(function(){ const evv = $std_core_hnd._evv; if (evv.length !== 1) { $std_core_hnd._evv = [evv[#1]]; }; return evv; })()"
}

private extern evv-swap-create( indices : vector<ev-index> ) : e evv<e> {
  c inline "kk_evv_swap_create(#1,kk_context())"
  js "_evv_swap_create"
}

private inline extern ev-none<e,h>() : e ev<h> {
  c  "kk_ev_none"
  js "ev_none"
}

// -------------------------------------------
// Internal Yielding
// -------------------------------------------

public inline extern yielding() : bool {
  c  inline "kk_yielding(kk_context())"
  js inline "($std_core_hnd._yield !== null)"
}

public inline extern yielding-non-final() : bool {
  c  inline "kk_yielding_non_final(kk_context())"
  js inline "($std_core_hnd._yield !== null && !$std_core_hnd._yield.final)"
}

public noinline extern yield-extend(next : a -> e b ) : e b {
  c  inline "kk_yield_extend(#1,kk_context())"
  js "_yield_extend"
}

public inline fun yield-bind( x : a, next : a -> e b ) : e b {
  if (yielding()) then yield-extend(next) else next(x)
}

public inline fun yield-bind2( x : a, extend : a -> e b, next : a -> e b ) : e b {
  if (yielding()) then yield-extend(extend) else next(x)
}

private extern yield-cont(f : (a -> e a, a) -> e r ) : e r {  // make hidden public?
  c  inline "kk_yield_cont(#1,kk_context())"
  js "_yield_cont"
}

private inline extern keep-yielding-final() : e r {
  c inline "kk_box_any(kk_context())"
  js inline "undefined"
}

private extern yield-prompt( m: marker<e,r> ) : yld<e,a,r> {
  c  inline "kk_yield_prompt(#1,kk_context())"
  js "_yield_prompt"
}

private extern yield-to-prim( m : marker<e1,r>, clause : ((() -> b) -> e1 r) -> e1 r ) : e (() -> b) {
  c  inline "kk_yield_to(#1,#2,kk_context())"
  js "$std_core_hnd._yield_to"
}

private extern yield-to-final( m : marker<e1,r>, clause : ((() -> b) -> e1 r) -> e1 r ) : e b {
  c  inline "kk_yield_final(#1,#2,kk_context())"
  js "$std_core_hnd._yield_final"
}

private noinline fun yield-to( m : marker<e1,r>, clause : ((() -> b) -> e1 r) -> e1 r ) : e1 b {
  val w0 = evv-get()
  val g : () -> _b = yield-to-prim(m, clause)
  yield-extend fun(f){ 
    // val keep1 = guard(w0)  // check the evidence is correctly restored
    f() 
  }
}

public type yield-info

private extern yield-capture() : e yield-info {
  c "kk_yield_capture"
  js "$std_core_hnd._yield_capture"
}

public extern unsafe-reyield(yld : yield-info) : e a {
  c "kk_yield_reyield"
  js "$std_core_hnd._reyield"
}

// -------------------------------------------
//
// -------------------------------------------

private inline extern unsafe-coerce(x:a) : b { inline "#1" }
private inline extern cast-ev0( f:() -> e1 b) : (() -> e0 b) { inline "#1" }
private inline extern cast-ev1( f:(a1) -> e1 b) : ((a1) -> e0 b) { inline "#1" }
private inline extern cast-ev2( f:(a1,a2) -> e1 b) : ((a1,a2) -> e0 b) { inline "#1" }
private inline extern cast-ev3( f:(a1,a2,a3) -> e1 b) : ((a1,a2,a3) -> e0 b) { inline "#1" }
private inline extern cast-ev4( f:(a1,a2,a3,a4) -> e1 b) : ((a1,a2,a3,a4) -> e0 b) { inline "#1" }
private inline extern cast-ev5( f:(a1,a2,a3,a4,a5) -> e1 b) : ((a1,a2,a3,a4,a5) -> e0 b) { inline "#1" }

private type yld<e,a,r> {
  Pure
  YieldingFinal
  Yielding
  Yield<b>(clause : ((() -> b) -> e r) -> e r, cont : (() -> b) -> e a)
}

private extern guard(w : evv<e> ) : e () {
  c  inline "kk_evv_guard(#1,kk_context())"
  js "_guard"
  // if (evv-equals(w)) then () else unsafe-total{ error("non-scoped resumption") }
}

private extern resume-final() : a {
  c  inline "kk_fatal_resume_final(kk_context())"
  js "_throw_resume_final"
}

private fun prompt( w0 : evv<e0>,  w1 : evv<e0>, ev : ev<h>, m : marker<e0,r>, ret: a -> e0 r, result : a ) : e0 r
{
  guard(w1)
  evv-set(w0)  // restore the previous evidence vector  
  match(yield-prompt(m)) {
    Pure {
      // returning
      ret(result)
    }
    YieldingFinal {
      // yielding final (exception), keep yielding
      keep-yielding-final()
    }
    Yielding {
      // regular yield, install a continuation
      yield-cont(fun(cont,res){ 
        // we resume, continue under a fresh a prompt again
        val w0' = evv-get()  // if not using scoped resumptions, w0' may be different from w0
        val w1' = if (evv-eq(w0,w0')) then w1 else evv-insert(w0',ev)
        evv-set(w1')
        prompt(w0',w1',ev,unsafe-decreasing(m),ret,cont(res)); 
      })
    }
    Yield(clause,cont) {
      // yielded to the operation `clause` in our handler
      fun deep(f) {
        // deep resume
        val w0' = evv-get()  // if not using scoped resumptions, w0' may be different from w0
        val w1' = if (evv-eq(w0,w0')) then w1 else evv-insert(w0',ev)
        evv-set(w1')
        prompt(w0',w1',ev,unsafe-decreasing(m),ret,cont(f))
      }
      clause(deep) // TODO: we should exit prompt first, and then execute clause to use constant stack space when resuming
    }
  }
}

public noinline fun ".hhandle"( tag:htag<h>, h : h<e,r>, ret: a -> e r, action : () -> e1 a ) : e r {
  // insert new evidence for our handler
  val w0 = evv-get()        
  val m  = fresh-marker()
  val ev = Ev(tag,m,h,w0)
  val w1 = evv-insert(w0,ev)
  evv-set(w1)
  // call action first (this may be yielding), then check the result
  prompt(w0,w1,ev,m,ret,cast-ev0(action)())
}

//public fun ".hhandle"( tag:htag<h>, h : h<e,r>, ret: a -> e r, action : () -> e1 a ) : e r {
//  return hidden-handle(tag,h,ret,action);
//}

//public fun hidden-handler( tag:htag<h>, h : h<e,r>, ret: a -> e r ) : ((action : () -> e1 a ) -> e r) {
//  (fun(action){ hidden-handle(tag,h,ret,action) })
//}

// -------------------------------------------
// named handler
// -------------------------------------------

abstract struct hname<h>( ev : ev<h> )

public fun ".named-handle"( tag:htag<h>, h : h<e,r>, ret: a -> e r, action : ev<h> -> e a ) : e r {
  val m = marker-none()
  val w0 = evv-get()
  val ev = Ev(tag,m,h,w0)
  prompt(w0,w0,ev,m,ret,action(ev)) // TODO: needs special prompt that does not insert on resume (but does update the context in ev)
}

// -------------------------------------------
// mask
// -------------------------------------------

private fun mask-at1( i : ev-index, behind : bool, action : (a) -> e1 b, x : a ) : e2 b {
  val w0 = evv-swap-delete(i,behind)
  val y = cast-ev1(action)(x)
  evv-set(w0)
  if (yielding()) return yield-cont(fun(cont,res){ mask-at1(unsafe-decreasing(i),behind,cont,res) })
  y
}

public fun ".mask-at"<a,e1,e2>( i : ev-index, behind : bool, action : () -> e1 a ) : e2 a {
  val w0 = evv-swap-delete(i,behind)
  val x = cast-ev0(action)()
  evv-set(w0)
  if (yielding()) return yield-cont(fun(cont,res){ mask-at1(i,behind,cont,res) })
  x
}

// mask for builtin effects without a handler or evidence
public fun ".mask-builtin"<a,e1,e2>( action : () -> e1 a ) : e2 a {
  cast-ev0(action)()
}


// -------------------------------------------
// Local variables
// -------------------------------------------

private fun prompt-local-var(loc:local-var<s,a>, res : b  ) : <div,local<s>|e> b {
  if (!yielding()) return res;
  val v = loc
  yield-cont(fun(cont,x){ loc := v; prompt-local-var(std/core/types/byref(loc),cont(x)) } )  // restore state early before the resume
}

public fun local-var(init:a, action: (l:local-var<s,a>) -> <local<s>|e> b ) : <local<s>|e> b {
  unsafe-no-div {
    val loc : local-var<_,_> = local-new(init)
    val res = cast-ev1(action)(std/core/types/byref(loc))
    prompt-local-var(std/core/types/byref(loc),res)
  }
}


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
  prompt(w,w,ev-none(),m,id, yield-bind( cont( { yield-to-final(m,fun(_k){ res }) } ), fun(_x){ res } ))  // TODO: special prompt that does not insert on resume?
}
/*
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
*/

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

abstract type clause1<a,b,h,e,r> {
  Clause1( clause: (marker<e,r>, ev<h>, a) -> e b )
}

private inline extern cast-ev0( f : (marker<e1,r>,ev<h>) -> e1 b) : e ((marker<e1,r>,ev<h>) -> e b) { inline "#1" }
private inline extern cast-ev1( f : (marker<e1,r>,ev<h>,a) -> e1 b) : e ((marker<e1,r>,ev<h>,a) -> e b) { inline "#1" }
private inline extern cast-ev2( f : (marker<e1,r>,ev<h>,a1,a2) -> e1 b) : e ((marker<e1,r>,ev<h>,a1,a2) -> e b) { inline "#1" }

public fun ".perform1"( ev : ev<h>, op : (forall<e1,r> h<e1,r> -> clause1<a,b,h,e1,r>), x : a ) : e b {
  match(ev) {
    Ev(_tag,m,h,_w) -> match(h.op) {
      Clause1(f) -> cast-ev1(f)(m,ev,x)
    }
  }
}

private fun evv-swap-with(ev : ev<h>) {
  match(ev) {
    Ev(_tag,_m,_h,w) -> evv-swap(w)
  }
} 

private fun under1( ev : ev<h>, op : a -> e b, x : a ) : e b {
  val w0 = evv-swap-with(ev)
  val y = op(x)
  // evv-set(w0)   // only needed before yielding for evidence expected check in prompt
  if (yielding()) return yield-cont( fun(cont,res){ under1(unsafe-decreasing(ev),cont,res)} )
  evv-set(w0)
  y
}


public fun clause-control-raw1( op : (x:a, r: resume-context<b,e,r>) -> e r ) : clause1<a,b,h,e,r> {
  Clause1(fun(m,_ev,x){ yield-to(m, fun(k){ op(x,Resume-context(k)) } ) } )
}

private fun get( ref: ref<h,a>) : <read<h>,div> a {
  !ref
}

inline extern unsafe-st(f : () -> <st<global>|e> a ) : (() -> e a) { inline "#1" }

private fun protect-check( resumed : ref<global,bool>,  k : (() -> b) -> e r, res : a ) : e a {
  val did-resume : bool = (unsafe-st{ !resumed })()
  if (!did-resume)
   then finalize(k,res)
   else res
}

private fun protect( x : a, clause : (x:a, k: b -> e r) -> e r, k : (() -> b) -> e r ) : e r {
  val resumed = (unsafe-st{ref(False)})()
  fun kprotect(ret) {
    (unsafe-st{resumed := True})()
    k({ret})
  }
  val res = clause(x,kprotect)
  if (yielding()) return yield-extend( fun(xres){ protect-check(resumed,k,xres) } )
  protect-check(resumed,k,res)
}

/*
public fun clause-control1( clause : (x:a, k: b -> e r) -> e r ) : clause1<a,b,e,r> {
  Clause1(fun(m,w,x){ yield-to(m, fun(k){ clause(x, fun(r){ k({r}) } ) }) })
}
*/

public fun clause-control1( clause : (x:a, k: b -> e r) -> e r ) : clause1<a,b,h,e,r> {
  Clause1(fun(m,_ev,x){ yield-to(m, fun(k){ protect(x,clause,k) }) })
}

public fun clause-tail1<e,r,a,b>(op : a -> e b) : clause1<a,b,h,e,r> {
  Clause1(fun(_m,ev,x){ under1(ev,op,x) })
}

public fun clause-tail-noyield1<e,r,a,b>(op : a -> e b) : clause1<a,b,h,e,r> {
  Clause1(fun(_m,_ev,x){ op(x) })
}

public fun clause-never1( op : a -> e r ) : clause1<a,b,h,e,r> {
  Clause1(fun(m,_ev,x){ yield-to-final(m, fun(_k){ op(x) }) })
}

//----------------------------------------------------------------
// 0 arguments; reuse 1 argument Clauses
//----------------------------------------------------------------

abstract type clause0<b,h,e,r> {
  Clause0( clause: (marker<e,r>, ev<h>) -> e b )
}

public fun ".perform0"( ev : ev<h>, op : (forall<e1,r> h<e1,r> -> clause0<b,h,e1,r>) ) : e b {
  match(ev) {
    Ev(_tag,m,h,_w) -> match(h.op) {
      Clause0(f) -> cast-ev0(f)(m,ev)
    }
  }
}

private fun under0( ev : ev<i>, op : () -> e b) : e b {
  val w0 = evv-swap-with(ev)
  val y = op()
  // evv-set(w0)   // only needed before yielding for evidence expected check in prompt
  evv-set(w0)
  if (yielding()) return yield-cont( fun(cont,res){ under1(ev,cont,res)} )
  y
}

public fun clause-control-raw0( op : resume-context<b,e,r> -> e r ) : clause0<b,h,e,r> {
  Clause0(fun(m,_ev){ yield-to(m, fun(k){ op(Resume-context(k)) } ) })
}

/*
public fun clause-control0( op : (b -> e r) -> e r ) : clause0<b,e,r> {
  Clause0(fun(m,w){ yield-to(m, fun(k){ op(fun(r){ k({r} )}) }) })
}
*/

public fun clause-control0( op : (b -> e r) -> e r ) : clause0<b,h,e,r> {
  Clause0(fun(m,_ev){ yield-to(m, fun(k){ protect((),fun(_x,r){ op(r) }, k) }) })
}


public fun clause-tail0<e,r,b>(op : () -> e b) : clause0<b,h,e,r> {
  Clause0(fun(_m,ev){ under0(ev,op) })
}

public fun clause-tail-noyield0<e,r,b>(op : () -> e b) : clause0<b,h,e,r> {
  Clause0(fun(_m,_ev){ op() })
}

public fun clause-value(v : b) : clause0<b,h,e,r> {
  Clause0(fun(_m,_ev){ v })
}

public fun clause-never0( op : () -> e r ) : clause0<b,h,e,r> {
  Clause0(fun(m,_ev){ yield-to-final(m, fun(_k){ op() }) })
}

//----------------------------------------------------------------
// 2 arguments
//----------------------------------------------------------------

abstract type clause2<a1,a2,b,h,e,r> {
  Clause2( clause: (marker<e,r>, ev<h>, a1, a2) -> e b )
}

private fun under2( ev : ev<h>, op : (a1,a2) -> e b, x1 : a1, x2 : a2 ) : e b {
  val w0 = evv-swap-with(ev)
  val z = op(x1,x2)
  evv-set(w0)
  if (yielding()) return yield-cont( fun(cont,res){ under1(ev,cont,res)} )
  z
}

private fun protect( x1 : a1, x2:a2, clause : (x:a1,x:a2, k: b -> e r) -> e r, k : (() -> b) -> e r ) : e r {
  val resumed = (unsafe-st{ref(False)})()
  fun kprotect(ret) {
    (unsafe-st{ resumed := True })()
    k({ret})
  }
  val res = clause(x1,x2,kprotect)
  if (yielding()) return yield-extend( fun(xres){ protect-check(resumed,k,xres) } )
  protect-check(resumed,k,res)
}

public fun clause-control2( clause : (x1:a1, x2:a2, k: b -> e r) -> e r ) : clause2<a1,a2,b,h,e,r> {
  Clause2(fun(m,_ev,x1,x2){ yield-to(m, fun(k){ protect(x1,x2,clause,k) }) })
}


public fun clause-control-raw2( op : (x1:a1, x2:a2, r: resume-context<b,e,r>) -> e r ) : clause2<a1,a2,b,h,e,r> {
  Clause2(fun(m,_ev,x1,x2){ yield-to(m, fun(k){ op(x1,x2,Resume-context(k)) } ) })
}

public fun clause-tail2<e,r,a1,a2,b>(op : (a1,a2) -> e b) : clause2<a1,a2,b,h,e,r> {
  Clause2(fun(m,ev,x1,x2){ under2(ev,op,x1,x2) })
}

public fun clause-tail-noyield2<e,r,a1,a2,b>(op : (a1,a2) -> e b) : clause2<a1,a2,b,h,e,r> {
  Clause2(fun(_m,_ev,x1,x2){ op(x1,x2) })
}

public fun ".perform2"( evx : ev<h>, op : (forall<e1,r> h<e1,r> -> clause2<a,b,c,h,e1,r>), x : a, y : b ) : e c {
  match(evx) {
    Ev(_tag,m,h,_w) -> match(h.op) {
      Clause2(f) -> cast-ev2(f)(m,evx,x,y)
    }
  }
}

public fun clause-never2( op : (a1,a2) -> e r ) : clause2<a1,a2,b,h,e,r> {
  Clause2(fun(m,_ev,x1,x2){ yield-to-final(m, fun(_k){ op(x1,x2) }) })
}

//----------------------------------------------------------------
// 3 arguments: reuse 1 argument clause.
// Or should the compiler do tupling/untupling?
//----------------------------------------------------------------

// For interal use
private fun xperform1( ev : ev<h>, op : (forall<e1,r> h<e1,r> -> clause1<a,b,h,e1,r>), x : a ) : e b {
  match(ev) {
    Ev(_tag,m,h,_w) -> match(h.op) {
      Clause1(f) -> cast-ev1(f)(m,ev,x)
    }
  }
}

public fun clause-control-raw3( op : (x1:a1, x2:a2, x3:a3, r: resume-context<b,e,r>) -> e r ) : clause1<(a1,a2,a3),b,h,e,r> {
  clause-control-raw1( fun(x:(_,_,_),r){ op(x.fst,x.snd,x.thd,r) } )
}

public fun clause-control3( op : (x1:a1, x2:a2, x3:a3, k: b -> e r) -> e r ) : clause1<(a1,a2,a3),b,h,e,r> {
  clause-control1( fun(x:(_,_,_),k){ op(x.fst,x.snd,x.thd,k) } )
}

public fun clause-tail3(op : (a1,a2,a3) -> e b) : clause1<(a1,a2,a3),b,h,e,r> {
  clause-tail1( fun( x:(_,_,_) ){ op(x.fst,x.snd,x.thd) } )
}

public fun clause-tail-noyield3(op : (a1,a2,a3) -> e b) : clause1<(a1,a2,a3),b,h,e,r> {
  clause-tail-noyield1( fun( x:(_,_,_) ){ op(x.fst,x.snd,x.thd) } )
}

public fun clause-never3( op : (a1,a2,a3) -> e r ) : clause1<(a1,a2,a3),b,h,e,r> {
  clause-never1(fun(x:(_,_,_)){ op(x.fst,x.snd,x.thd) } )
}

public fun ".perform3"( ev : ev<h>, op : (forall<e1,r> h<e1,r> -> clause1<(a1,a2,a3),b,h,e1,r>), x1 : a1, x2 : a2, x3 : a3 ) : e b {
  xperform1(ev,op,(x1,x2,x3))
}

private fun under3( ev : ev<h>, op : (a1,a2,a3) -> e b, x1 : a1, x2 : a2, x3 : a3 ) : e b {
  val w0 = evv-swap-with(ev)
  val z = op(x1,x2,x3)
  evv-set(w0)
  if (yielding()) return yield-cont( fun(cont,res){ under1(ev,cont,res)} )
  z
}



public fun clause-control4( op : (x1:a1, x2:a2, x3:a3, x4:a4, k: b -> e r) -> e r ) : clause1<(a1,a2,a3,a4),b,h,e,r> {
  clause-control1( fun(x:(_,_,_,_),k){ op(x.fst,x.snd,x.thd,x.field4,k) } )
}

public fun clause-tail4(op : (a1,a2,a3,a4) -> e b) : clause1<(a1,a2,a3,a4),b,h,e,r> {
  clause-tail1( fun( x:(_,_,_,_) ){ op(x.fst,x.snd,x.thd,x.field4) } )
}

public fun ".perform4"( ev : ev<h>, op : (forall<e1,r> h<e1,r> -> clause1<(a1,a2,a3,a4),b,h,e1,r>), x1 : a1, x2 : a2, x3 : a3, x4 : a4 ) : e b {
  xperform1(ev,op,(x1,x2,x3,x4))
}

private fun under4( ev : ev<h>, op : (a1,a2,a3,a4) -> e b, x1 : a1, x2 : a2, x3 : a3, x4 : a4 ) : e b {
  val w0 = evv-swap-with(ev)
  val z = op(x1,x2,x3,x4)
  evv-set(w0)
  if (yielding()) return yield-cont( fun(cont,res){ under1(ev,cont,res)} )
  z
}


// -------------------------------------------
// Open
// -------------------------------------------
// Generated by type inference and later refined into one of the below variants
public inline extern ".open"<e1,e2,a,b>( x : a ) : e2 b {
  inline "(#1)"
}

public fun ".open-none0"<b,e1,e2>( f : () -> e1 b ) : e2 b {
  val w = evv-swap-create0()
  val x = cast-ev0(f)()
  val keep = evv-set(w)
  x
}

public fun ".open-none1"<a,b,e1,e2>( f : a -> e1 b, x1 : a ) : e2 b {
  val w = evv-swap-create0()
  val x = cast-ev1(f)(x1)
  val keep = evv-set(w)
  x
}

public fun ".open-none2"<a1,a2,b,e1,e2>( f : (a1,a2) -> e1 b, x1 : a1, x2 : a2 ) : e2 b {
  val w = evv-swap-create0()
  val x = cast-ev2(f)(x1,x2)
  val keep = evv-set(w)
  x
}

public fun ".open-none3"<a1,a2,a3,b,e1,e2>( f : (a1,a2,a3) -> e1 b, x1 : a1, x2 : a2, x3 : a3 ) : e2 b {
  val w = evv-swap-create0()
  val x = cast-ev3(f)(x1,x2,x3)
  val keep = evv-set(w)
  x
}

public fun ".open-none4"<a1,a2,a3,a4,b,e1,e2>( f : (a1,a2,a3,a4) -> e1 b, x1 : a1, x2 : a2, x3 : a3, x4 : a4 ) : e2 b {
  val w = evv-swap-create0()
  val x = cast-ev4(f)(x1,x2,x3,x4)
  val keep = evv-set(w)
  x
}


private fun open-at1<a,b,e1,e2>( i: ev-index, f : a -> e1 b, x : a ) : e2 b {
  val w = evv-swap-create1(i)
  val y = cast-ev1(f)(x)
  evv-set(w)
  if (yielding()) return yield-cont(fun(cont,res){ open-at1(unsafe-decreasing(i),cont,res) })
  y
}

public fun ".open-at0"<b,e1,e2>( i: ev-index, f : () -> e1 b ) : e2 b {
  val w = evv-swap-create1(i)
  val y = cast-ev0(f)()
  evv-set(w)
  if (yielding()) return yield-cont(fun(cont,res){ open-at1(i,cont,res) })
  y
}

public fun ".open-at1"<a,b,e1,e2>( i: ev-index, f : a -> e1 b, x : a ) : e2 b {
  val w = evv-swap-create1(i)
  val y = cast-ev1(f)(x)
  evv-set(w)
  if (yielding()) return yield-cont(fun(cont,res){ open-at1(i,cont,res) })
  y
}

public fun ".open-at2"<a1,a2,b,e1,e2> ( i: ev-index, f : (a1,a2) -> e1 b, x1 : a1, x2 : a2 ) : e2 b {
  val w = evv-swap-create1(i)
  val y = cast-ev2(f)(x1,x2)
  evv-set(w)
  if (yielding()) return yield-cont(fun(cont,res){ open-at1(i,cont,res) })
  y
}

public fun ".open-at3"<a1,a2,a3,b,e1,e2> ( i: ev-index, f : (a1,a2,a3) -> e1 b, x1 : a1, x2 : a2, x3 : a3 ) : e2 b {
  val w = evv-swap-create1(i)
  val y = cast-ev3(f)(x1,x2,x3)
  evv-set(w)
  if (yielding()) return yield-cont(fun(cont,res){ open-at1(i,cont,res) })
  y
}

public fun ".open-at4"<a1,a2,a3,a4,b,e1,e2> ( i: ev-index, f : (a1,a2,a3,a4) -> e1 b, x1 : a1, x2 : a2, x3 : a3, x4 : a4 ) : e2 b {
  val w = evv-swap-create1(i)
  val y = cast-ev4(f)(x1,x2,x3,x4)
  evv-set(w)
  if (yielding()) return yield-cont(fun(cont,res){ open-at1(i,cont,res) })
  y
}


private fun open1<a,b,e1,e2>( indices : vector<ev-index>, f : a -> e1 b, x : a ) : e2 b {
  val w = evv-swap-create(indices)
  val y = cast-ev1(f)(x)
  evv-set(w)
  if (yielding()) return yield-cont(fun(cont,res){ open1(unsafe-decreasing(indices),cont,res) })
  y
}


public noinline fun ".open0"<b,e1,e2>( indices : vector<ev-index>, f : () -> e1 b ) : e2 b {
  val w = evv-swap-create(indices)
  val y = cast-ev0(f)()
  evv-set(w)
  if (yielding()) return yield-cont(fun(cont,res){ open1(indices,cont,res) })
  y
}

public noinline fun ".open1"<a,b,e1,e2>( indices : vector<ev-index>, f : a -> e1 b, x : a ) : e2 b {
  val w = evv-swap-create(indices)
  val y = cast-ev1(f)(x)
  evv-set(w)
  if (yielding()) return yield-cont(fun(cont,res){ open1(indices,cont,res) })
  y
}

public noinline fun ".open2"<a1,a2,b,e1,e2>( indices : vector<ev-index>, f : (a1,a2) -> e1 b, x1 : a1, x2 : a2 ) : e2 b {
  val w = evv-swap-create(indices)
  val y = cast-ev2(f)(x1,x2)
  evv-set(w)
  if (yielding()) return yield-cont(fun(cont,res){ open1(indices,cont,res) })
  y
}

public noinline fun ".open3"<a1,a2,a3,b,e1,e2>( indices : vector<ev-index>, f : (a1,a2,a3) -> e1 b, x1 : a1, x2 : a2, x3 : a3 ) : e2 b {
  val w = evv-swap-create(indices)
  val y = cast-ev3(f)(x1,x2,x3)
  evv-set(w)
  if (yielding()) return yield-cont(fun(cont,res){ open1(indices,cont,res) })
  y
}

public noinline fun ".open4"<a1,a2,a3,a4,b,e1,e2>( indices : vector<ev-index>, f : (a1,a2,a3,a4) -> e1 b, x1 : a1, x2 : a2, x3 : a3, x4 : a4 ) : e2 b {
  val w = evv-swap-create(indices)
  val y = cast-ev4(f)(x1,x2,x3,x4)
  evv-set(w)
  if (yielding()) return yield-cont(fun(cont,res){ open1(indices,cont,res) })
  y
}

// -------------------------------------------
// capture yields
// -------------------------------------------

public fun unsafe-try-finalize( action : () -> e a ) : e either<yield-info,a> {
  try-finalize-prompt(action());
}

private fun try-finalize-prompt( res : a ) : e either<yield-info,a> {
  if (yielding-non-final()) return yield-cont(fun(cont,x){ try-finalize-prompt(unsafe-decreasing(cont(x))) })
  if (!yielding()) then Right(res) else Left(yield-capture())
}
