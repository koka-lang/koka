/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Internal effect handler primitives.

   Internal primitives to implement evidence based algebraic
   effect handlers. These are emitted by the compiler during evidence
   translation and this module is always implicitly imported.
*/
public module std/core/hnd

import std/core/types

extern import {
  c  file "hnd-inline"
  js file "hnd-inline.js"
}


// -------------------------------------------
// Internal types
// -------------------------------------------

private struct marker<e::E,a>(m:int32)

abstract type htag<h::(E,V)->V> {
  Htag(tagname:string)
}

public fun ".new-htag"( tag : string ) {
  Htag(tag)
}

public fun hidden-htag( tag : string ) {
  Htag(tag)
}

// control flow context:
//                 -1: none: bottom
//                   /          \
// 0: except: never resumes   1: linear: resumes exactly once
//                   \          /
//           2: affine: resumes never or once
//                        |
//     3: multi: resumes never, once, or multiple times
public alias cfc = int32


abstract type ev<h> {
  con Ev<e,r>(htag:htag<h>, marker:marker<e,r>, hnd:h<e,r>, cfc:cfc, hevv:evv<e>)
}

private type evv<e::E>;

public alias ev-index = ssize_t

// -------------------------------------------
// Internal Markers
// -------------------------------------------

private extern fresh-marker-int() : int32 {
  c inline "kk_marker_unique(kk_context())"
  js inline "$marker_unique++"
}

private extern fresh-marker-named-int() : int32 {
  c inline "-kk_marker_unique(kk_context())"
  js inline "-($marker_unique++)"
}

private fun fresh-marker() : marker<e,r> {
  Marker(fresh-marker-int())
}

private fun fresh-marker-named() : marker<e,r> {
  Marker(fresh-marker-named-int())
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
  js inline "$std_core_hnd._evv_at(#1)"
}

// (dynamically) find evidence insertion/deletion index in the evidence vector
public extern ".evv-index"<e::E,h>( htag : htag<h> ) : e ev-index {
  c inline "kk_evv_index(#1,kk_context())"
  js inline "__evv_index(#1)"
}


public extern ".evv-lookup"( htag : htag<h> ) : ev<h> {
  c inline "kk_evv_lookup(#1,kk_context())"
  js "__evv_lookup"
}

public inline extern ".evv-select"( i : ev-index, htag : htag<h> ) : ev<h> {
  c inline "(#1 >= 0 ? kk_evv_at(#1,kk_context()) : kk_evv_lookup(#2,kk_context()))"
  js inline "(#1 >= 0 ? $std_core_hnd._evv_at(#1) : __evv_lookup(#2))"
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
  js "$std_core_hnd._evv_get"
}

private inline extern evv-set<e1,e>( w : evv<e1> ) : e () {
  c inline "kk_evv_set(#1,kk_context())"
  js "$std_core_hnd._evv_set"
}

private inline extern evv-swap<e1,e2>( w : evv<e1> ) : e evv<e2> {
  c inline "kk_evv_swap(#1,kk_context())"
  js "$std_core_hnd._evv_swap"
}

private extern evv-insert( evv : evv<e1>, ev : ev<h> ) : e1 evv<e2> {
  c  inline "kk_evv_insert(#1,#2,kk_context())"
  js "_evv_insert"
}

private extern evv-swap-delete( i : ev-index, behind : bool ) : e1 evv<e> {
  c  inline "kk_evv_swap_delete(#1,#2,kk_context())"
  js "_evv_swap_delete"
}

private inline extern evv-swap-create0() : e evv<e> { //not quite the right effect type but avoids unbound effect types
  c inline "kk_evv_swap_create0(kk_context())"
  js "$std_core_hnd._evv_swap_create0"
}

private inline extern evv-swap-create1( i : ev-index ) : e evv<e> {   //not quite the right effect type but avoids unbound effect types
  c inline "kk_evv_swap_create1(#1,kk_context())"
  js "$std_core_hnd._evv_swap_create1"
}

private extern evv-swap-create( indices : vector<ev-index> ) : e evv<e> { //not quite the right effect type but avoids unbound effect types
  c inline "kk_evv_swap_create(#1,kk_context())"
  js "_evv_swap_create"
}

private inline extern ev-none<e,h>() : e ev<h> {
  c  "kk_ev_none"
  js "ev_none"
}

public extern ".evv-is-affine"() : bool {
  c inline "kk_evv_cfc(kk_context())<=2"
  js inline "$std_core_hnd._evv_cfc()<=2"
}


// -------------------------------------------
// Internal Yielding
// -------------------------------------------

public inline extern yielding() : bool {
  c  inline "kk_yielding(kk_context())"
  js inline "$std_core_hnd._yielding()"
}

public inline extern yielding-non-final() : bool {
  c  inline "kk_yielding_non_final(kk_context())"
  js inline "$std_core_hnd._yielding_non_final()"
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

private extern yield-cont(f : forall<b> (b -> e a, b) -> e r ) : e r {  // make hidden public?
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

private extern yield-to-prim( m : marker<e1,r>, clause : (resume-result<b,r> -> e1 r) -> e1 r ) : e (() -> b) {
  c  inline "kk_yield_to(#1,#2,kk_context())"
  js "$std_core_hnd._yield_to"
}

private extern yield-to-final( m : marker<e1,r>, clause : (resume-result<b,r> -> e1 r) -> e1 r ) : e b {
  c  inline "kk_yield_final(#1,#2,kk_context())"
  js "$std_core_hnd._yield_final"
}

private noinline fun yield-to( m : marker<e1,r>, clause : (resume-result<b,r> -> e1 r) -> e1 r ) : e1 b {
  //val w0 = evv-get()
  val g : () -> _b = yield-to-prim(m, clause)
  yield-extend fn(f){
    // val keep1 = guard(w0)  // check the evidence is correctly restored
    f()
  }
}

public type yield-info

private extern yield-capture() : e yield-info {
  c "kk_yield_capture"
  js "_yield_capture"
}

public extern unsafe-reyield(yld : yield-info) : e a {
  c "kk_yield_reyield"
  js "_reyield"
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

private value type resume-result<b,r> {
  Deep( result: b )
  Shallow( result: b )
  Finalize( result : r )
}

private type yld<e,a,r> {
  Pure
  YieldingFinal
  Yielding
  Yield<b>(clause : (resume-result<b,r> -> e r) -> e r, cont : (() -> b) -> e a)
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
    Pure -> {
      // returning
      ret(result)
    }
    YieldingFinal -> {
      // yielding final (exception), keep yielding
      keep-yielding-final()
    }
    Yielding -> {
      // regular yield, install a continuation
      yield-cont(fn(cont,res){
        // we resume, continue under a fresh a prompt again
        val w0' = evv-get()  // if not using scoped resumptions, w0' may be different from w0
        val w1' = if (evv-eq(w0,w0')) then w1 else evv-insert(w0',ev)
        evv-set(w1')
        prompt(w0',w1',ev,unsafe-decreasing(m),ret,cont(res));
      })
    }
    Yield(clause,cont) -> {
      // yielded to the operation `clause` in our handler
      fun resume(r) {
        match(r) {
          Deep(x) -> {
            val w0' = evv-get()  // if not using scoped resumptions, w0' may be different from w0
            val w1' = if (evv-eq(w0,w0')) then w1 else evv-insert(w0',ev)
            evv-set(w1')
            prompt(w0',w1',ev,unsafe-decreasing(m),ret,cont({x}))
          }
          Shallow(x) -> {
            yield-bind( cont({x}), fn(y){ ret(y) } )
          }
          Finalize(x) -> {
            val w0' = evv-get()  // if not using scoped resumptions, w0' may be different from w0
            val w1' = if (evv-eq(w0,w0')) then w1 else evv-insert(w0',ev)
            evv-set(w1')
            prompt(w0',w1',ev,unsafe-decreasing(m),ret,cont({ yield-to-final(m,fn(_k){x}) }))            
          }
        }
      }
      clause(resume) // TODO: we should exit prompt first, and then execute clause to use constant stack space when resuming
    }    
  }
}

public noinline fun ".hhandle"( tag:htag<h>, cfc: cfc, h : h<e,r>, ret: a -> e r, action : () -> e1 a ) : e r {
  // insert new evidence for our handler
  val w0 = evv-get()
  val m  = fresh-marker()
  val ev = Ev(tag,m,h,cfc,w0)
  val w1 = evv-insert(w0,ev)
  evv-set(w1)
  // call action first (this may be yielding), then check the result
  prompt(w0,w1,ev,m,ret,cast-ev0(action)())
}

// -------------------------------------------
// named handler
// -------------------------------------------

public noinline fun ".named-handle"( tag:htag<h>, cfc: cfc, h : h<e,r>, ret: a -> e r, action : ev<h> -> e1 a ) : e r {
  val m = fresh-marker-named()            // unique (negative) marker, but never gets inserted into the evidence vector
  val w0 = evv-get()
  val ev = Ev(tag,m,h,cfc,w0)
  prompt(w0,w0,ev,m,ret,cast-ev1(action)(ev))
}

// -------------------------------------------
// mask
// -------------------------------------------

private fun mask-at1( i : ev-index, behind : bool, action : (a) -> e1 b, x : a ) : e2 b {
  val w0 = evv-swap-delete(i,behind)
  val y = cast-ev1(action)(x)
  evv-set(w0)
  if (yielding()) return yield-cont(fn(cont,res){ mask-at1(unsafe-decreasing(i),behind,cont,res) })
  y
}

public fun ".mask-at"<a,e1,e2>( i : ev-index, behind : bool, action : () -> e1 a ) : e2 a {
  val w0 = evv-swap-delete(i,behind)
  val x = cast-ev0(action)()
  evv-set(w0)
  if (yielding()) return yield-cont(fn(cont,res){ mask-at1(i,behind,cont,res) })
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
  yield-cont(fn(cont,x){ loc := v; prompt-local-var(std/core/types/byref(loc),cont(x)) } )  // restore state early before the resume
}

public inline fun local-var(init:a, action: (l:local-var<s,a>) -> <local<s>|e> b ) : <local<s>|e> b {
  unsafe-no-div {
    val loc : local-var<_,_> = local-new(init)
    val res = cast-ev1(action)(std/core/types/byref(loc))
    prompt-local-var(std/core/types/byref(loc),res)
  }
}


// -------------------------------------------
// Finally
// -------------------------------------------

public fun finally( fin : () -> e (), action : () -> e a ) : e a {
  finally-prompt(fin, action());
}

private fun finally-prompt(fin : () -> e (), res : a ) : e a 
  if !yielding() then 
    fin()
    res
  elif yielding-non-final() then 
    yield-cont(fn(cont,x){ finally-prompt(unsafe-decreasing(fin),cont(x)) })
  else 
    val yld = yield-capture()
    fin()
    if yielding() return yield-extend( fn(_x){ unsafe-reyield(yld) } )
    unsafe-reyield(yld)

private fun finalize(cont : (() -> b) -> e r, res : a) : e a {
  val m : marker<_e,_r> = fresh-marker()
  val w = evv-get()
  prompt(w,w,ev-none(),m,id, yield-bind( cont( { yield-to-final(m,fn(_k){ res }) }), fn(_x){ res } ))  // TODO: special prompt that does not insert on resume?
}

// -------------------------------------------
// Initially
// -------------------------------------------

// add integers
private inline extern add(i : int, j : int) : int {
  c  "kk_integer_add"
  cs inline "(#1 + #2)"
  js inline "(#1 + #2)" // "$std_core._int_add"
}

// are two integers equal?
private inline extern eq( x : int, y : int) : bool  {
  c  "kk_integer_eq"
  cs inline "(#1 == #2)"
  js inline "(#1 == #2)" // $std_core._int_eq"
}


public fun initially(init : (int) -> e (), action : () -> e a ) : e a {
  init(0)
  if (yielding()) return yield-extend(fn(_ret:()){ initially-prompt(init,action()) })
  initially-prompt(init, action() )
}

private fun initially-prompt( init : (int) -> e (), res : a ) : e a {
  if (yielding-non-final()) then {
    val count = unsafe-st{ref(0)}()
    yield-cont(fn(cont,x){
      val cnt = unsafe-st{ !count }()    // increase counter on every resumption
      unsafe-st{ count := add(cnt,1) }()
      if (eq(cnt,0)) then () else {  // for every resume after the first, run the initializer
        val r = init(cnt)
        if (yielding()) return yield-extend( fn(_ret){ initially-prompt(unsafe-decreasing(init), cont(x)) })
      }
      initially-prompt(unsafe-decreasing(init), cont(x))
    })
  }
  else res
}


// -------------------------------------------
// Resume context
// -------------------------------------------



abstract struct resume-context<b,e::E,e0::E,r>( k : resume-result<b,r> -> e r )

public fun resume( r : resume-context<b,e,e0,r>, x : b ) : e r {
  (r.k)(Deep(x))
}

public fun resume-shallow( r : resume-context<b,e,e0,r>, x : b ) : e0 r {
  cast-ev1(r.k)(Shallow(x))
}


public fun finalize( r : resume-context<b,e,e0,r>, x : r ) : e r {
  //finalize(r.k,x)
  (r.k)(Finalize(x))
}

// -------------------------------------------
// Clauses
// -------------------------------------------

abstract type clause1<a,b,h,e::E,r> {
  Clause1( clause: (marker<e,r>, ev<h>, a) -> e b )
}

private inline extern cast-ev0( f : (marker<e1,r>,ev<h>) -> e1 b) : e ((marker<e1,r>,ev<h>) -> e b) { inline "#1" }
private inline extern cast-ev1( f : (marker<e1,r>,ev<h>,a) -> e1 b) : e ((marker<e1,r>,ev<h>,a) -> e b) { inline "#1" }
private inline extern cast-ev2( f : (marker<e1,r>,ev<h>,a1,a2) -> e1 b) : e ((marker<e1,r>,ev<h>,a1,a2) -> e b) { inline "#1" }

public inline fun ".perform1"( ev : ev<h>, op : (forall<e1,r> h<e1,r> -> clause1<a,b,h,e1,r>), x : a ) : e b {
  match(ev) {
    Ev(_tag,m,h,_c,_w) -> match(h.op) {
      Clause1(f) -> cast-ev1(f)(m,ev,x)
    }
  }
}

private fun evv-swap-with(ev : ev<h>) {
  match(ev) {
    Ev(_tag,_m,_h,_c,w) -> evv-swap(w)
  }
}

private inline fun under1( ev : ev<h>, op : a -> e b, x : a ) : e b {
  val w0 = evv-swap-with(ev)
  val y = op(x)
  // evv-set(w0)   // only needed before yielding for evidence expected check in prompt
  if (yielding()) return yield-cont( fn(cont,res){ under1x(unsafe-decreasing(ev),cont,res)} )
  evv-set(w0)
  y
}

// extra under1x to make under1 inlineable
private noinline fun under1x( ev : ev<h>, op : a -> e b, x : a ) : e b {
  val w0 = evv-swap-with(ev)
  val y = op(x)
  // evv-set(w0)   // only needed before yielding for evidence expected check in prompt
  if (yielding()) return yield-cont( fn(cont,res){ under1x(unsafe-decreasing(ev),cont,res)} )
  evv-set(w0)
  y
}

public fun clause-control-raw1( op : (x:a, r: resume-context<b,e,e0,r>) -> e r ) : clause1<a,b,h,e,r> {
  Clause1(fn(m,_ev,x){ yield-to(m, fn(k){ op(x,Resume-context(k)) } ) } )
}

private fun get( ref: ref<h,a>) : <read<h>,div> a {
  !ref
}

inline extern unsafe-st(f : () -> <st<global>|e> a ) : (() -> e a) { inline "#1" }

private fun protect-check( resumed : ref<global,bool>,  k : resume-result<b,r> -> e r, res : r ) : e r {
  val did-resume : bool = (unsafe-st{ !resumed })()
  if (!did-resume)
   then k(Finalize(res)) //finalize(k,res)
   else res
}

private fun protect( x : a, clause : (x:a, k: b -> e r) -> e r, k : resume-result<b,r> -> e r ) : e r {
  val resumed = (unsafe-st{ref(False)})()
  fun kprotect(ret) {
    (unsafe-st{resumed := True})()
    k(Deep(ret))
  }
  val res = clause(x,kprotect)
  if (yielding()) return yield-extend( fn(xres){ protect-check(resumed,k,xres) } )
  protect-check(resumed,k,res)
}

/*
public fun clause-control1( clause : (x:a, k: b -> e r) -> e r ) : clause1<a,b,e,r> {
  Clause1(fn(m,w,x){ yield-to(m, fn(k){ clause(x, fn(r){ k({r}) } ) }) })
}
*/

public fun clause-control1( clause : (x:a, k: b -> e r) -> e r ) : clause1<a,b,h,e,r> {
  Clause1(fn(m,_ev,x){ yield-to(m, fn(k){ protect(x,clause,k) }) })
}

public fun clause-tail1<e,r,a,b>(op : a -> e b) : clause1<a,b,h,e,r> {
  Clause1(fn(_m,ev,x){ under1(ev,op,x) })
}

public fun clause-tail-noyield1<e,r,a,b>(op : a -> e b) : clause1<a,b,h,e,r> {
  Clause1(fn(_m,_ev,x){ op(x) })
}

public fun clause-never1( op : a -> e r ) : clause1<a,b,h,e,r> {
  Clause1(fn(m,_ev,x){ yield-to-final(m, fn(_k){ op(x) }) })
}

//----------------------------------------------------------------
// 0 arguments; reuse 1 argument Clauses
//----------------------------------------------------------------

abstract type clause0<b,h,e,r> {
  Clause0( clause: (marker<e,r>, ev<h>) -> e b )
}


//private inline extern cast-hnd( h : h<e1,r> ) : e h<e,r> { inline "#1" }
//private inline extern cast-marker( m : marker<e1,r> ) : e marker<e,r> { inline "#1" }

public inline fun ".perform0"( ev : ev<h>, op : (forall<e1,r> h<e1,r> -> clause0<b,h,e1,r>) ) : e b {
  match(ev) {
    Ev(_tag,m,h,_c,_w) -> match(h.op) {
      Clause0(f) -> cast-ev0(f)(m,ev)
    }
  }
}

private inline fun under0( ev : ev<i>, op : () -> e b) : e b {
  val w0 = evv-swap-with(ev)
  val y = op()
  // evv-set(w0)   // only needed before yielding for evidence expected check in prompt
  evv-set(w0)
  if (yielding()) return yield-cont( fn(cont,res){ under1(ev,cont,res)} )
  y
}

public fun clause-control-raw0( op : resume-context<b,e,e0,r> -> e r ) : clause0<b,h,e,r> {
  Clause0(fn(m,_ev){ yield-to(m, fn(k){ op(Resume-context(k)) } ) })
}

/*
public fun clause-control0( op : (b -> e r) -> e r ) : clause0<b,e,r> {
  Clause0(fn(m,w){ yield-to(m, fn(k){ op(fn(r){ k({r} )}) }) })
}
*/

public fun clause-control0( op : (b -> e r) -> e r ) : clause0<b,h,e,r> {
  Clause0(fn(m,_ev){ yield-to(m, fn(k){ protect((),fn(_x,r){ op(r) }, k) }) })
}


public fun clause-tail0<e,r,b>(op : () -> e b) : clause0<b,h,e,r> {
  Clause0(fn(_m,ev){ under0(ev,op) })
}

public fun clause-tail-noyield0<e,r,b>(op : () -> e b) : clause0<b,h,e,r> {
  Clause0(fn(_m,_ev){ op() })
}

public fun clause-value(v : b) : clause0<b,h,e,r> {
  Clause0(fn(_m,_ev){ v })
}

public fun clause-never0( op : () -> e r ) : clause0<b,h,e,r> {
  Clause0(fn(m,_ev){ yield-to-final(m, fn(_k){ op() }) })
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
  if (yielding()) return yield-cont( fn(cont,res){ under1(ev,cont,res)} )
  z
}

private fun protect( x1 : a1, x2:a2, clause : (x:a1,x:a2, k: b -> e r) -> e r, k : resume-result<b,r> -> e r ) : e r {
  val resumed = (unsafe-st{ref(False)})()
  fun kprotect(ret) {
    (unsafe-st{ resumed := True })()
    k(Deep(ret))
  }
  val res = clause(x1,x2,kprotect)
  if (yielding()) return yield-extend( fn(xres){ protect-check(resumed,k,xres) } )
  protect-check(resumed,k,res)
}

public fun clause-control2( clause : (x1:a1, x2:a2, k: b -> e r) -> e r ) : clause2<a1,a2,b,h,e,r> {
  Clause2(fn(m,_ev,x1,x2){ yield-to(m, fn(k){ protect(x1,x2,clause,k) }) })
}


public fun clause-control-raw2( op : (x1:a1, x2:a2, r: resume-context<b,e,e0,r>) -> e r ) : clause2<a1,a2,b,h,e,r> {
  Clause2(fn(m,_ev,x1,x2){ yield-to(m, fn(k){ op(x1,x2,Resume-context(k)) } ) })
}

public fun clause-tail2<e,r,a1,a2,b>(op : (a1,a2) -> e b) : clause2<a1,a2,b,h,e,r> {
  Clause2(fn(m,ev,x1,x2){ under2(ev,op,x1,x2) })
}

public fun clause-tail-noyield2<e,r,a1,a2,b>(op : (a1,a2) -> e b) : clause2<a1,a2,b,h,e,r> {
  Clause2(fn(_m,_ev,x1,x2){ op(x1,x2) })
}

public inline fun ".perform2"( evx : ev<h>, op : (forall<e1,r> h<e1,r> -> clause2<a,b,c,h,e1,r>), x : a, y : b ) : e c {
  match(evx) {
    Ev(_tag,m,h,_c,_w) -> match(h.op) {
      Clause2(f) -> cast-ev2(f)(m,evx,x,y)
    }
  }
}

public fun clause-never2( op : (a1,a2) -> e r ) : clause2<a1,a2,b,h,e,r> {
  Clause2(fn(m,_ev,x1,x2){ yield-to-final(m, fn(_k){ op(x1,x2) }) })
}

//----------------------------------------------------------------
// 3 arguments: reuse 1 argument clause.
// Or should the compiler do tupling/untupling?
//----------------------------------------------------------------

// For interal use
private fun xperform1( ev : ev<h>, op : (forall<e1,r> h<e1,r> -> clause1<a,b,h,e1,r>), x : a ) : e b {
  match(ev) {
    Ev(_tag,m,h,_c,_w) -> match(h.op) {
      Clause1(f) -> cast-ev1(f)(m,ev,x)
    }
  }
}

public fun clause-control-raw3( op : (x1:a1, x2:a2, x3:a3, r: resume-context<b,e,e0,r>) -> e r ) : clause1<(a1,a2,a3),b,h,e,r> {
  clause-control-raw1( fn(x:(_,_,_),r){ op(x.fst,x.snd,x.thd,r) } )
}

public fun clause-control3( op : (x1:a1, x2:a2, x3:a3, k: b -> e r) -> e r ) : clause1<(a1,a2,a3),b,h,e,r> {
  clause-control1( fn(x:(_,_,_),k){ op(x.fst,x.snd,x.thd,k) } )
}

public fun clause-tail3(op : (a1,a2,a3) -> e b) : clause1<(a1,a2,a3),b,h,e,r> {
  clause-tail1( fn( x:(_,_,_) ){ op(x.fst,x.snd,x.thd) } )
}

public fun clause-tail-noyield3(op : (a1,a2,a3) -> e b) : clause1<(a1,a2,a3),b,h,e,r> {
  clause-tail-noyield1( fn( x:(_,_,_) ){ op(x.fst,x.snd,x.thd) } )
}

public fun clause-never3( op : (a1,a2,a3) -> e r ) : clause1<(a1,a2,a3),b,h,e,r> {
  clause-never1(fn(x:(_,_,_)){ op(x.fst,x.snd,x.thd) } )
}

public fun ".perform3"( ev : ev<h>, op : (forall<e1,r> h<e1,r> -> clause1<(a1,a2,a3),b,h,e1,r>), x1 : a1, x2 : a2, x3 : a3 ) : e b {
  xperform1(ev,op,(x1,x2,x3))
}

private fun under3( ev : ev<h>, op : (a1,a2,a3) -> e b, x1 : a1, x2 : a2, x3 : a3 ) : e b {
  val w0 = evv-swap-with(ev)
  val z = op(x1,x2,x3)
  evv-set(w0)
  if (yielding()) return yield-cont( fn(cont,res){ under1(ev,cont,res)} )
  z
}



public fun clause-control4( op : (x1:a1, x2:a2, x3:a3, x4:a4, k: b -> e r) -> e r ) : clause1<(a1,a2,a3,a4),b,h,e,r> {
  clause-control1( fn(x:(_,_,_,_),k){ op(x.fst,x.snd,x.thd,x.field4,k) } )
}

public fun clause-tail4(op : (a1,a2,a3,a4) -> e b) : clause1<(a1,a2,a3,a4),b,h,e,r> {
  clause-tail1( fn( x:(_,_,_,_) ){ op(x.fst,x.snd,x.thd,x.field4) } )
}

public fun ".perform4"( ev : ev<h>, op : (forall<e1,r> h<e1,r> -> clause1<(a1,a2,a3,a4),b,h,e1,r>), x1 : a1, x2 : a2, x3 : a3, x4 : a4 ) : e b {
  xperform1(ev,op,(x1,x2,x3,x4))
}

private fun under4( ev : ev<h>, op : (a1,a2,a3,a4) -> e b, x1 : a1, x2 : a2, x3 : a3, x4 : a4 ) : e b {
  val w0 = evv-swap-with(ev)
  val z = op(x1,x2,x3,x4)
  evv-set(w0)
  if (yielding()) return yield-cont( fn(cont,res){ under1(ev,cont,res)} )
  z
}


// -------------------------------------------
// Open
// -------------------------------------------

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


private noinline fun open-at1<a,b,e1,e2>( i: ev-index, f : a -> e1 b, x : a ) : e2 b {
  val w = evv-swap-create1(i)
  val y = cast-ev1(f)(x)
  evv-set(w)
  if (yielding()) return yield-cont(fn(cont,res){ open-at1(unsafe-decreasing(i),cont,res) })
  y
}

public fun ".open-at0"<b,e1,e2>( i: ev-index, f : () -> e1 b ) : e2 b {
  val w = evv-swap-create1(i)
  val y = cast-ev0(f)()
  evv-set(w)
  if (yielding()) return yield-cont(fn(cont,res){ open-at1(i,cont,res) })
  y
}

public fun ".open-at1"<a,b,e1,e2>( i: ev-index, f : a -> e1 b, x : a ) : e2 b {
  val w = evv-swap-create1(i)
  val y = cast-ev1(f)(x)
  evv-set(w)
  if (yielding()) return yield-cont(fn(cont,res){ open-at1(i,cont,res) })
  y
}

public fun ".open-at2"<a1,a2,b,e1,e2> ( i: ev-index, f : (a1,a2) -> e1 b, x1 : a1, x2 : a2 ) : e2 b {
  val w = evv-swap-create1(i)
  val y = cast-ev2(f)(x1,x2)
  evv-set(w)
  if (yielding()) return yield-cont(fn(cont,res){ open-at1(i,cont,res) })
  y
}

public fun ".open-at3"<a1,a2,a3,b,e1,e2> ( i: ev-index, f : (a1,a2,a3) -> e1 b, x1 : a1, x2 : a2, x3 : a3 ) : e2 b {
  val w = evv-swap-create1(i)
  val y = cast-ev3(f)(x1,x2,x3)
  evv-set(w)
  if (yielding()) return yield-cont(fn(cont,res){ open-at1(i,cont,res) })
  y
}

public fun ".open-at4"<a1,a2,a3,a4,b,e1,e2> ( i: ev-index, f : (a1,a2,a3,a4) -> e1 b, x1 : a1, x2 : a2, x3 : a3, x4 : a4 ) : e2 b {
  val w = evv-swap-create1(i)
  val y = cast-ev4(f)(x1,x2,x3,x4)
  evv-set(w)
  if (yielding()) return yield-cont(fn(cont,res){ open-at1(i,cont,res) })
  y
}


private noinline fun open1<a,b,e1,e2>( indices : vector<ev-index>, f : a -> e1 b, x : a ) : e2 b {
  val w = evv-swap-create(indices)
  val y = cast-ev1(f)(x)
  evv-set(w)
  if (yielding()) return yield-cont(fn(cont,res){ open1(unsafe-decreasing(indices),cont,res) })
  y
}


public fun ".open0"<b,e1,e2>( indices : vector<ev-index>, f : () -> e1 b ) : e2 b {
  val w = evv-swap-create(indices)
  val y = cast-ev0(f)()
  evv-set(w)
  if (yielding()) return yield-cont(fn(cont,res){ open1(indices,cont,res) })
  y
}

public fun ".open1"<a,b,e1,e2>( indices : vector<ev-index>, f : a -> e1 b, x : a ) : e2 b {
  val w = evv-swap-create(indices)
  val y = cast-ev1(f)(x)
  evv-set(w)
  if (yielding()) return yield-cont(fn(cont,res){ open1(indices,cont,res) })
  y
}

public fun ".open2"<a1,a2,b,e1,e2>( indices : vector<ev-index>, f : (a1,a2) -> e1 b, x1 : a1, x2 : a2 ) : e2 b {
  val w = evv-swap-create(indices)
  val y = cast-ev2(f)(x1,x2)
  evv-set(w)
  if (yielding()) return yield-cont(fn(cont,res){ open1(indices,cont,res) })
  y
}

public fun ".open3"<a1,a2,a3,b,e1,e2>( indices : vector<ev-index>, f : (a1,a2,a3) -> e1 b, x1 : a1, x2 : a2, x3 : a3 ) : e2 b {
  val w = evv-swap-create(indices)
  val y = cast-ev3(f)(x1,x2,x3)
  evv-set(w)
  if (yielding()) return yield-cont(fn(cont,res){ open1(indices,cont,res) })
  y
}

public fun ".open4"<a1,a2,a3,a4,b,e1,e2>( indices : vector<ev-index>, f : (a1,a2,a3,a4) -> e1 b, x1 : a1, x2 : a2, x3 : a3, x4 : a4 ) : e2 b {
  val w = evv-swap-create(indices)
  val y = cast-ev4(f)(x1,x2,x3,x4)
  evv-set(w)
  if (yielding()) return yield-cont(fn(cont,res){ open1(indices,cont,res) })
  y
}

// -------------------------------------------
// capture yields
// -------------------------------------------

public fun unsafe-try-finalize( action : () -> e a ) : e either<yield-info,a> {
  try-finalize-prompt(action());
}

private fun try-finalize-prompt( res : a ) : e either<yield-info,a> {
  if (yielding-non-final()) return yield-cont(fn(cont,x){ try-finalize-prompt(unsafe-decreasing(cont(x))) })
  if (!yielding()) then Right(res) else Left(yield-capture())
}
