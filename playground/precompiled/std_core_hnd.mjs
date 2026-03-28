// Koka generated module: std/core/hnd, koka version: 3.2.4
"use strict";
 
// imports
import * as $std_core_types from './std_core_types.mjs';
import * as $std_core_undiv from './std_core_undiv.mjs';
 
// externals
/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
var $marker_unique = 1;  // must be > 0
function _assert(cond,msg) {
  if (!cond) console.error(msg);
}
// for internal references
const $std_core_hnd = { "_evv_get"      : _evv_get,
                  "_evv_set"            : _evv_set,
                  "_evv_at"             : _evv_at,
                  "_evv_swap"           : _evv_swap,
                  "_evv_swap_create0"   : _evv_swap_create0,
                  "_evv_swap_create1"   : _evv_swap_create1,
                  "_yielding"           : _yielding,
                  "_yielding_non_final" : _yielding_non_final,
                  "_evv_is_affine_"     : _evv_is_affine_,
                  "_yield_to"           : _yield_to,
                  "_yield_final"        : _yield_final,
                };
var _evv = [];
var _yield = null; // { marker: 0, clause: null, conts: [], conts_count: 0, final: bool };
export function _evv_get() {
  return _evv;
}
export function _evv_at(i) {
  return _evv[i];
}
export function _evv_set(evv) {
  _evv = evv;
}
export function _evv_swap(evv) {
  const evv0 = _evv;
  _evv = evv;
  return evv0;
}
const _evv_empty = [];
export function _evv_swap_create0() {
  const evv = _evv;
  if (evv.length!==0) {
    _evv = _evv_empty;
  }
  return evv;
}
export function _evv_swap_create1( i ) {
  const evv = _evv;
  if (evv.length !== 1) {
    const ev = evv[i];
    _evv = [ev];
    _evv._cfc = ev.hnd._cfc;
  }
  return evv;
}
export function _yielding() {
  return (_yield !== null);
}
export function _yielding_non_final() {
  return (_yield !== null && !_yield.final);
}
//--------------------------------------------------
// evidence: { evv: [forall h. ev<h>], ofs : int }
//--------------------------------------------------
// function ev_none() {
//   var evv = []
//   evv._cfc = -1
//   return Ev(null,0,null,/*-1,*/evv);
// }
function _cfc_lub(x,y) {
  _assert(x!=null && y!=null);
  if (x < 0) return y;
  if (x + y === 1) return 2;
  if (x > y) return x;
  return y;
}
function _evv_get_cfc( evv ) {
  const cfc = evv._cfc;
  return (cfc==null ? -1 : cfc);
}
function _evv_is_affine_() {
  return (_evv_get_cfc(_evv) <= 2);
}
function _evv_insert(evv,ev) {
  // update ev
  if (ev.marker===0) return; // marker zero is ev-none
  ev.hevv = evv;
  const cfc = _cfc_lub(_evv_get_cfc(evv), ev.hnd._cfc);
  if (ev.marker < 0) { // negative marker is used for named evidence; this means this evidence should not be inserted into the evidence vector
    evv._cfc = cfc;  // update control flow context
    return; // a negative (named) marker is not in the evidence vector
  }
  // insert in the vector
  const n    = evv.length;
  const evv2 = new Array(n+1);
  evv2._cfc = cfc;
  var i;
  for(i = 0; i < n; i++) {
    const ev2 = evv[i];
    if (ev.htag <= ev2.htag) break;
    evv2[i] = ev2;
  }
  evv2[i] = ev;
  for(; i < n; i++) {
    evv2[i+1] = evv[i];
  }
  return evv2;
}
function _evv_delete(evv,i,behind) {
  // delete from the vector
  if (behind) i++;
  const n = evv.length;
  const evv2 = new Array(n-1);
  if (evv._cfc != null) { evv2._cfc = evv._cfc; }
  if (n==1) return evv2;  // empty
  // copy without i
  var j;
  for(j = 0; j < i; j++) {
    evv2[j] = evv[j];
  }
  for(; j < n-1; j++) {
    evv2[j] = evv[j + 1];
  }
  // update cfc?  no, as named handlers may still exist with a higher cfc (and not present in the vector)
  // if (evv[i].cfc >= _evv_get_cfc(evv)) {
  //   var cfc = evv2[0].hnd._cfc;
  //   for(j = 1; j < n-1; j++) {
  //     cfc = _cfc_lub(evv2[j].hnd._cfc, cfc);
  //   }
  //   evv2._cfc = cfc;
  // }
  return evv2;
}
function _evv_swap_delete(i,behind) {
  const w0 = _evv;
  _evv = _evv_delete(w0,i,behind);
  return w0;
}
// Find insertion/deletion point for an effect label
function __evv_index(tag) {
  const evv = _evv;
  const n = evv.length
  for(var i = 0; i < n; i++) {
    if (tag <= evv[i].htag) return i;  // string compare
  }
  return n;
}
function _evv_show(evv) {
  evv.sort(function(ev1,ev2){ return (ev1.marker - ev2.marker); });
  var out = "";
  for( var i = 0; i < evv.length; i++) {
    const evvi = evv[i].hevv;
    out += ((i==0 ? "{ " : "  ") + evv[i].htag.padEnd(8," ") + ": marker " + evv[i].marker + ", under <" +
             evvi.map(function(ev){ return ev.marker.toString(); }).join(",") + ">" + (i==evv.length-1 ? "}" : "") + "\n");
  }
  return out;
}
function _yield_show() {
  if (_yielding()) {
    return "yielding to " + _yield.marker + ", final: " + _yield.final;
  }
  else {
    return "pure"
  }
}
function _evv_expect(m,expected) {
  if ((_yield===null || _yield.marker === m) && (_evv !== expected.evv)) {
    console.error("expected evidence: \n" + _evv_show(expected) + "\nbut found:\n" + _evv_show(_evv));
  }
}
function _guard(evv) {
  if (_evv !== evv) {
    if (_evv.length == evv.length) {
      var equal = true;
      for(var i = 0; i < evv.length; i++) {
        if (_evv[i].marker != evv[i].marker) {
          equal = false;
          break;
        }
      }
      if (equal) return;
    }
    console.error("trying to resume outside the (handler) scope of the original handler. \n captured under:\n" + _evv_show(evv) + "\n but resumed under:\n" + _evv_show(_evv));
    throw "trying to resume outside the (handler) scope of the original handler";
  }
}
function _throw_resume_final(f) {
  throw "trying to resume an unresumable resumption (from finalization)";
}
function _evv_create( evv, indices ) {
  const n = indices.length;
  const evv2 = new Array(n);
  if (evv._cfc != null) { evv2._cfc = evv._cfc; }
  for(var i = 0; i < n; i++) {
    evv2[i] = evv[indices[i]];
  }
  return evv2;
}
function _evv_swap_create(indices) {
  const evv = _evv;
  _evv = _evv_create(evv,indices);
  return evv;
}
//--------------------------------------------------
// Yielding
//--------------------------------------------------
function _kcompose( to, conts ) {
  return function(x) {
    var acc = x;
    for(var i = 0; i < to; i++) {
      acc = conts[i](acc);
      if (_yielding()) {
        //return ((function(i){ return _yield_extend(_kcompose(i+1,to,conts)); })(i));
        while(++i < to) {
          _yield_extend(conts[i]);
        }
        return; // undefined
      }
    }
    return acc;
  }
}
function _yield_extend(next) {
  _assert(_yielding(), "yield extension while not yielding!");
  if (_yield.final) return;
  _yield.conts[_yield.conts_count++] = next;  // index is ~80% faster as push
}
function _yield_cont(f) {
  _assert(_yielding(), "yield extension while not yielding!");
  if (_yield.final) return;
  const cont   = _kcompose(_yield.conts_count,_yield.conts);
  _yield.conts = new Array(8);
  _yield.conts_count = 1;
  _yield.conts[0] = function(x){ return f(cont,x); };
}
function _yield_prompt(m) {
  if (_yield === null) {
    return Pure;
  }
  else if (_yield.marker !== m) {
    return (_yield.final ? YieldingFinal : Yielding);
  }
  else { // _yield.marker === m
    const cont   = (_yield.final ? _throw_resume_final : _kcompose(_yield.conts_count,_yield.conts));
    const clause = _yield.clause;
    _yield = null;
    return Yield(clause,cont);
  }
}
export function _yield_final(m,clause) {
  _assert(!_yielding(),"yielding final while yielding!");
  _yield = { marker: m, clause: clause, conts: null, conts_count: 0, final: true };
}
export function _yield_to(m,clause) {
  _assert(!_yielding(),"yielding while yielding!");
  _yield = { marker: m, clause: clause, conts: new Array(8), conts_count: 0, final: false };
}
function _yield_capture() {
  _assert(_yielding(),"can only capture a yield when yielding!");
  const yld = _yield;
  _yield = null;
  return yld;
}
function _reyield( yld ) {
  _assert(!_yielding(),"can only reyield a yield when not yielding!");
  _yield = yld;
}
 
// type declarations
// type evv
// type htag
export function Htag(tagname) /* forall<a> (tagname : string) -> htag<a> */  {
  return tagname;
}
// type marker
// type ev
export function Ev(htag, marker, hnd, hevv) /* forall<a,e,b> (htag : htag<a>, marker : marker<e,b>, hnd : a<e,b>, hevv : evv<e>) -> ev<a> */  {
  return { htag: htag, marker: marker, hnd: hnd, hevv: hevv };
}
// type clause0
export function Clause0(clause) /* forall<a,b,e,c> (clause : (marker<e,c>, ev<b>) -> e a) -> clause0<a,b,e,c> */  {
  return clause;
}
// type clause1
export function Clause1(clause) /* forall<a,b,c,e,d> (clause : (marker<e,d>, ev<c>, a) -> e b) -> clause1<a,b,c,e,d> */  {
  return clause;
}
// type clause2
export function Clause2(clause) /* forall<a,b,c,d,e,a1> (clause : (marker<e,a1>, ev<d>, a, b) -> e c) -> clause2<a,b,c,d,e,a1> */  {
  return clause;
}
// type resume-result
export function Deep(result) /* forall<a,b> (result : a) -> resume-result<a,b> */  {
  return { _tag: 1, result: result };
}
export function Shallow(result) /* forall<a,b> (result : a) -> resume-result<a,b> */  {
  return { _tag: 2, result: result };
}
export function Finalize(result) /* forall<a,b> (result : b) -> resume-result<a,b> */  {
  return { _tag: 3, result: result };
}
// type resume-context
export function Resume_context(k) /* forall<a,e,e1,b> (k : (resume-result<a,b>) -> e b) -> resume-context<a,e,e1,b> */  {
  return k;
}
// type yield-info
// type yld
export const Pure = { _tag: 1 }; // forall<e,a,b> yld<e,a,b>
export const YieldingFinal = { _tag: 2 }; // forall<e,a,b> yld<e,a,b>
export const Yielding = { _tag: 3 }; // forall<e,a,b> yld<e,a,b>
export function Yield(clause, cont) /* forall<e,a,b,c> (clause : ((resume-result<c,b>) -> e b) -> e b, cont : (() -> c) -> e a) -> yld<e,a,b> */  {
  return { _tag: 4, clause: clause, cont: cont };
}
 
// declarations
 
 
// Automatically generated. Retrieves the `tagname` constructor field of the `:htag` type.
export function htag_fs_tagname(htag) /* forall<a> (htag : htag<a>) -> string */  {
  return htag;
}
 
export function htag_fs__copy(_this, tagname) /* forall<a> (htag<a>, tagname : ? string) -> htag<a> */  {
  if (tagname !== undefined) {
    var _x0 = tagname;
  }
  else {
    var _x0 = _this;
  }
  return _x0;
}
 
 
// _Internal_ hidden constructor for creating handler tags
export function _new_htag(tag) /* forall<a> (tag : string) -> htag<a> */  {
  return tag;
}
 
 
// Show a handler tag.
export function htag_fs_show(_pat_x126__20) /* forall<a> (htag<a>) -> string */  {
  return _pat_x126__20;
}
 
 
// Automatically generated. Retrieves the `htag` constructor field of the `:ev` type.
export function ev_fs_htag(ev) /* forall<a> (ev : ev<a>) -> htag<a> */  {
  return ev.htag;
}
 
export function ev_fs__copy(_this, htag, marker, hnd, hevv) /* forall<a,e,b> (ev<a>, htag : ? (htag<a>), marker : marker<e,b>, hnd : a<e,b>, hevv : evv<e>) -> ev<a> */  {
  if (htag !== undefined) {
    var _x1 = htag;
  }
  else {
    var _x1 = _this.htag;
  }
  return Ev(_x1, marker, hnd, hevv);
}
 
 
// Are two markers equal?
export function eq_marker(x, y) /* forall<a,b,e,e1> (x : marker<e,a>, y : marker<e1,b>) -> bool */  {
  return x==y;
}
 
 
// Evidence equality compares the markers.
export function ev_fs__lp__eq__eq__rp_(_pat_x141__18, _pat_x141__37) /* forall<a> (ev<a>, ev<a>) -> bool */  {
  return eq_marker(_pat_x141__18.marker, _pat_x141__37.marker);
}
 
export function fresh_marker() /* forall<a,e> () -> marker<e,a> */  {
  return $marker_unique++;
}
 
export function fresh_marker_named() /* forall<a,e> () -> marker<e,a> */  {
  return -($marker_unique++);
}
 
 
// Insert new evidence into the given evidence vector.
export function evv_insert(evv, ev) /* forall<e,e1,a> (evv : evv<e>, ev : ev<a>) -> e evv<e1> */  {
  return _evv_insert(evv,ev);
}
 
 
// show evidence for debug purposes
export function evv_show(evv) /* forall<e> (evv : evv<e>) -> string */  {
  return _evv_show(evv);
}
 
 
// Is an evidence vector unchanged? (i.e. as pointer equality).
// This is used to avoid copying in common cases.
export function evv_eq(evv0, evv1) /* forall<e> (evv0 : evv<e>, evv1 : evv<e>) -> bool */  {
  return (evv0) === (evv1);
}
 
 
// (dynamically) find evidence insertion/deletion index in the evidence vector
// The compiler optimizes `@evv-index` to a static index when apparent from the effect type.
export function _evv_index(htag) /* forall<e,a> (htag : htag<a>) -> e ev-index */  {
  return __evv_index(htag);
}
 
 
// Get the current evidence vector.
export function evv_get() /* forall<e> () -> e evv<e> */  {
  return $std_core_hnd._evv_get();
}
 
 
// Does the current evidence vector consist solely of affine handlers?
// This is called in backends that do not have context paths (like javascript)
// to optimize TRMC (where we can use faster update-in-place TRMC if we know the
// operations are all affine). As such, it is always safe to return `false`.
//
// control flow context:
//                 -1: none: bottom
//                   /          \
// 0: except: never resumes   1: linear: resumes exactly once
//                   \          /
//           2: affine: resumes never or once
//                        |
//     3: multi: resumes never, once, or multiple times
//
export function _evv_is_affine() /* () -> bool */  {
  return $std_core_hnd._evv_is_affine_();
}
 
 
// Remove evidence at index `i` of the current evidence vector, and return the old one.
// (used by `mask`)
export function evv_swap_delete(i, behind) /* forall<e,e1> (i : ev-index, behind : bool) -> e1 evv<e> */  {
  return _evv_swap_delete(i,behind);
}
 
 
// Swap the current evidence vector with a new vector consisting of evidence
// at indices `indices` in the current vector.
export function evv_swap_create(indices) /* forall<e> (indices : vector<ev-index>) -> e evv<e> */  {
  return _evv_swap_create(indices);
}
 
export function yield_extend(next) /* forall<a,b,e> (next : (a) -> e b) -> e b */  {
  return _yield_extend(next);
}
 
export function yield_bind(x, next) /* forall<a,b,e> (x : a, next : (a) -> e b) -> e b */  {
  if ($std_core_hnd._yielding()) {
    return yield_extend(next);
  }
  else {
    return next(x);
  }
}
 
export function yield_bind2(x, extend, next) /* forall<a,b,e> (x : a, extend : (a) -> e b, next : (a) -> e b) -> e b */  {
  if ($std_core_hnd._yielding()) {
    return yield_extend(extend);
  }
  else {
    return next(x);
  }
}
 
export function yield_cont(f) /* forall<a,e,b> (f : forall<c> ((c) -> e a, c) -> e b) -> e b */  {
  return _yield_cont(f);
}
 
export function yield_prompt(m) /* forall<a,e,b> (m : marker<e,b>) -> yld<e,a,b> */  {
  return _yield_prompt(m);
}
 
export function yield_to_prim(m, clause) /* forall<a,e,e1,b> (m : marker<e1,b>, clause : ((resume-result<a,b>) -> e1 b) -> e1 b) -> e (() -> a) */  {
  return $std_core_hnd._yield_to(m,clause);
}
 
export function yield_to_final(m, clause) /* forall<a,e,e1,b> (m : marker<e1,b>, clause : ((resume-result<a,b>) -> e1 b) -> e1 b) -> e a */  {
  return $std_core_hnd._yield_final(m,clause);
}
 
export function yield_to(m, clause) /* forall<a,e,b> (m : marker<e,b>, clause : ((resume-result<a,b>) -> e b) -> e b) -> e a */  {
   
  var g = yield_to_prim(m, clause);
  return yield_extend(function(f /* () -> 2786 2785 */ ) {
    return f();
  });
}
 
export function yield_capture() /* forall<e> () -> e yield-info */  {
  return _yield_capture();
}
 
export function unsafe_reyield(yld) /* forall<a,e> (yld : yield-info) -> e a */  {
  return _reyield(yld);
}
 
 
// Automatically generated. Tests for the `Deep` constructor of the `:resume-result` type.
export function is_deep(resume_result) /* forall<a,b> (resume-result : resume-result<a,b>) -> bool */  {
  return (resume_result._tag === 1);
}
 
 
// Automatically generated. Tests for the `Shallow` constructor of the `:resume-result` type.
export function is_shallow(resume_result) /* forall<a,b> (resume-result : resume-result<a,b>) -> bool */  {
  return (resume_result._tag === 2);
}
 
 
// Automatically generated. Tests for the `Finalize` constructor of the `:resume-result` type.
export function is_finalize(resume_result) /* forall<a,b> (resume-result : resume-result<a,b>) -> bool */  {
  return (resume_result._tag === 3);
}
 
 
// Automatically generated. Tests for the `Pure` constructor of the `:yld` type.
export function is_pure(yld) /* forall<a,b,e> (yld : yld<e,a,b>) -> bool */  {
  return (yld._tag === 1);
}
 
 
// Automatically generated. Tests for the `YieldingFinal` constructor of the `:yld` type.
export function is_yieldingFinal(yld) /* forall<a,b,e> (yld : yld<e,a,b>) -> bool */  {
  return (yld._tag === 2);
}
 
 
// Automatically generated. Tests for the `Yielding` constructor of the `:yld` type.
export function is_yielding(yld) /* forall<a,b,e> (yld : yld<e,a,b>) -> bool */  {
  return (yld._tag === 3);
}
 
 
// Automatically generated. Tests for the `Yield` constructor of the `:yld` type.
export function is_yield(yld) /* forall<a,b,e> (yld : yld<e,a,b>) -> bool */  {
  return (yld._tag === 4);
}
 
export function guard(w) /* forall<e> (w : evv<e>) -> e () */  {
  return _guard(w);
}
 
export function resume_final() /* forall<a> () -> a */  {
  return _throw_resume_final();
}
 
export function prompt(w0, w1, ev, m, ret, result) /* forall<a,e,b,c> (w0 : evv<e>, w1 : evv<e>, ev : ev<b>, m : marker<e,c>, ret : (a) -> e c, result : a) -> e c */  {
   
  guard(w1);
   
  $std_core_hnd._evv_set(w0);
  var _x2 = yield_prompt(m);
  if (_x2._tag === 1) {
    return ret(result);
  }
  else if (_x2._tag === 2) {
    return undefined;
  }
  else if (_x2._tag === 3) {
    return yield_cont(function(cont /* (3242) -> 3483 3482 */ , res /* 3242 */ ) {
       
      var w0_sq_ = evv_get();
       
      var _x3 = evv_eq(w0, w0_sq_);
      if (_x3) {
        var w1_sq_ = w1;
      }
      else {
        var w1_sq_ = evv_insert(w0_sq_, ev);
      }
       
      $std_core_hnd._evv_set(w1_sq_);
      return prompt(w0_sq_, w1_sq_, ev, m, ret, cont(res));
    });
  }
  else {
    return _x2.clause(function(r /* resume-result<3463,3485> */ ) {
      if (r._tag === 1) {
         
        var w0_0_sq_ = evv_get();
         
        var _x3 = evv_eq(w0, w0_0_sq_);
        if (_x3) {
          var w1_0_sq_ = w1;
        }
        else {
          var w1_0_sq_ = evv_insert(w0_0_sq_, ev);
        }
         
        $std_core_hnd._evv_set(w1_0_sq_);
        return prompt(w0_0_sq_, w1_0_sq_, ev, m, ret, _x2.cont(function() {
            return r.result;
          }));
      }
      else if (r._tag === 2) {
         
        var x_1_10002 = _x2.cont(function() {
          return r.result;
        });
        if ($std_core_hnd._yielding()) {
          return yield_extend(ret);
        }
        else {
          return ret(x_1_10002);
        }
      }
      else {
         
        var w0_1_sq_ = evv_get();
         
        var _x3 = evv_eq(w0, w0_1_sq_);
        if (_x3) {
          var w1_1_sq_ = w1;
        }
        else {
          var w1_1_sq_ = evv_insert(w0_1_sq_, ev);
        }
         
        $std_core_hnd._evv_set(w1_1_sq_);
        return prompt(w0_1_sq_, w1_1_sq_, ev, m, ret, _x2.cont(function() {
            return yield_to_final(m, function(___wildcard_x409__85 /* (resume-result<3463,3485>) -> 3483 3485 */ ) {
                return r.result;
              });
          }));
      }
    });
  }
}
 
export function _hhandle(tag, h, ret, action) /* forall<a,e,e1,b,c> (tag : htag<b>, h : b<e,c>, ret : (a) -> e c, action : () -> e1 a) -> e c */  {
   
  var w0 = evv_get();
   
  var m = fresh_marker();
   
  var ev = Ev(tag, m, h, w0);
   
  var w1 = evv_insert(w0, ev);
   
  $std_core_hnd._evv_set(w1);
  return prompt(w0, w1, ev, m, ret, action());
}
 
export function _named_handle(tag, h, ret, action) /* forall<a,e,e1,b,c> (tag : htag<b>, h : b<e,c>, ret : (a) -> e c, action : (ev<b>) -> e1 a) -> e c */  {
   
  var m = fresh_marker_named();
   
  var w0 = evv_get();
   
  var ev = Ev(tag, m, h, w0);
  return prompt(w0, w0, ev, m, ret, action(ev));
}
 
export function mask_at1(i, behind, action, x) /* forall<a,b,e,e1> (i : ev-index, behind : bool, action : (a) -> e b, x : a) -> e1 b */  {
   
  var w0 = evv_swap_delete(i, behind);
   
  var y = action(x);
   
  $std_core_hnd._evv_set(w0);
  if ($std_core_hnd._yielding()) {
    return yield_cont(function(cont /* (3838) -> 3864 3862 */ , res /* 3838 */ ) {
      return mask_at1(i, behind, cont, res);
    });
  }
  else {
    return y;
  }
}
 
export function _mask_at(i, behind, action) /* forall<a,e,e1> (i : ev-index, behind : bool, action : () -> e a) -> e1 a */  {
   
  var w0 = evv_swap_delete(i, behind);
   
  var x = action();
   
  $std_core_hnd._evv_set(w0);
  if ($std_core_hnd._yielding()) {
    return yield_cont(function(cont /* (3955) -> 3976 3974 */ , res /* 3955 */ ) {
      return mask_at1(i, behind, cont, res);
    });
  }
  else {
    return x;
  }
}
 
 
// mask for builtin effects without a handler or evidence (like `:st` or `:local`)
export function _mask_builtin(action) /* forall<a,e,e1> (action : () -> e a) -> e1 a */  {
  return action();
}
 
export function prompt_local_var(loc, res) /* forall<a,b,h> (loc : local-var<h,a>, res : b) -> <div,local<h>> b */  {
   
  var b_10004 = $std_core_hnd._yielding();
  if (b_10004) {
     
    var v = ((loc).value);
    return yield_cont(function(cont /* (4118) -> <div,local<4163>> 4162 */ , x /* 4118 */ ) {
       
      ((loc).value = v);
      return prompt_local_var(loc, cont(x));
    });
  }
  else {
    return res;
  }
}
 
export function local_var(init, action) /* forall<a,b,e,h> (init : a, action : (local-var<h,a>) -> <local<h>|e> b) -> <local<h>|e> b */  {
   
  var loc = { value: init };
   
  var res = action(loc);
  return prompt_local_var(loc, res);
}
 
export function finally_prompt(fin, res) /* forall<a,e> (fin : () -> e (), res : a) -> e a */  {
   
  var b = $std_core_hnd._yielding();
  if (b) {
    if ($std_core_hnd._yielding_non_final()) {
      return yield_cont(function(cont /* (4354) -> 4415 4414 */ , x /* 4354 */ ) {
        return finally_prompt(fin, cont(x));
      });
    }
    else {
       
      var yld = yield_capture();
       
      fin();
      if ($std_core_hnd._yielding()) {
        return yield_extend(function(___wildcard_x490__43 /* _4375 */ ) {
          return unsafe_reyield(yld);
        });
      }
      else {
        return unsafe_reyield(yld);
      }
    }
  }
  else {
     
    fin();
    if ($std_core_hnd._yielding()) {
      return yield_extend(function(___wildcard_x483__40 /* _4302 */ ) {
        return res;
      });
    }
    else {
      return res;
    }
  }
}
 
export function $finally(fin, action) /* forall<a,e> (fin : () -> e (), action : () -> e a) -> e a */  {
  return finally_prompt(fin, action());
}
 
export function initially_prompt(init, res) /* forall<a,e> (init : (int) -> e (), res : a) -> e a */  {
  if ($std_core_hnd._yielding_non_final()) {
     
    var count = { value: 0 };
    return yield_cont(function(cont /* (4636) -> 4650 4649 */ , x /* 4636 */ ) {
       
      var cnt = count.value;
       
      ((count).value = ((cnt + 1)));
       
      if ((cnt == 0)) {
        $std_core_types.Unit;
      }
      else {
         
        var r = init(cnt);
        if ($std_core_hnd._yielding()) {
           
          yield_extend(function(___wildcard_x531__35 /* _4583 */ ) {
            return initially_prompt(init, cont(x));
          });
          $std_core_types.Unit;
        }
        else {
          $std_core_types.Unit;
        }
      }
      return initially_prompt(init, cont(x));
    });
  }
  else {
    return res;
  }
}
 
export function initially(init, action) /* forall<a,e> (init : (int) -> e (), action : () -> e a) -> e a */  {
   
  init(0);
  if ($std_core_hnd._yielding()) {
    return yield_extend(function(___wildcard_x519__40 /* () */ ) {
      return initially_prompt(init, action());
    });
  }
  else {
    return initially_prompt(init, action());
  }
}
 
 
// Automatically generated. Retrieves the `k` constructor field of the `:resume-context` type.
export function resume_context_fs_k(_this) /* forall<a,e,e1,b> (resume-context<a,e,e1,b>) -> ((resume-result<a,b>) -> e b) */  {
  return _this;
}
 
export function resume_context_fs__copy(_this, k) /* forall<a,e,e1,b> (resume-context<a,e,e1,b>, k : ? ((resume-result<a,b>) -> e b)) -> resume-context<a,e,e1,b> */  {
  if (k !== undefined) {
    var _x3 = k;
  }
  else {
    var _x3 = _this;
  }
  return _x3;
}
 
export function resume(r, x) /* forall<a,e,e1,b> (r : resume-context<a,e,e1,b>, x : a) -> e b */  {
  return r(Deep(x));
}
 
export function resume_shallow(r, x) /* forall<a,e,e1,b> (r : resume-context<a,e,e1,b>, x : a) -> e1 b */  {
  return r(Shallow(x));
}
 
export function finalize(r, x) /* forall<a,e,e1,b> (r : resume-context<a,e,e1,b>, x : b) -> e b */  {
  return r(Finalize(x));
}
 
 
// Automatically generated. Retrieves the `clause` constructor field of the `:clause1` type.
export function clause1_fs_clause(clause1) /* forall<a,b,c,e,d> (clause1 : clause1<a,b,c,e,d>) -> ((marker<e,d>, ev<c>, a) -> e b) */  {
  return clause1;
}
 
export function clause1_fs__copy(_this, clause) /* forall<a,b,c,e,d> (clause1<a,b,c,e,d>, clause : ? ((marker<e,d>, ev<c>, a) -> e b)) -> clause1<a,b,c,e,d> */  {
  if (clause !== undefined) {
    var _x4 = clause;
  }
  else {
    var _x4 = _this;
  }
  return _x4;
}
 
export function _perform1(ev, op, x) /* forall<a,b,c,e> (ev : ev<c>, op : forall<e1,d> (c<e1,d>) -> clause1<a,b,c,e1,d>, x : a) -> e b */  {
  var _x5 = op(ev.hnd);
  return _x5(ev.marker, ev, x);
}
 
export function evv_swap_with(ev) /* forall<a,e> (ev : ev<a>) -> evv<e> */  {
  return $std_core_hnd._evv_swap((ev.hevv));
}
 
 
// extra under1x to make under1 inlineable
export function under1x(ev, op, x) /* forall<a,b,e,c> (ev : ev<c>, op : (a) -> e b, x : a) -> e b */  {
   
  var w0 = $std_core_hnd._evv_swap((ev.hevv));
   
  var y = op(x);
  if ($std_core_hnd._yielding()) {
    return yield_cont(function(cont /* (5438) -> 5472 5471 */ , res /* 5438 */ ) {
      return under1x(ev, cont, res);
    });
  }
  else {
     
    $std_core_hnd._evv_set(w0);
    return y;
  }
}
 
export function under1(ev, op, x) /* forall<a,b,e,c> (ev : ev<c>, op : (a) -> e b, x : a) -> e b */  {
   
  var w0 = $std_core_hnd._evv_swap((ev.hevv));
   
  var y = op(x);
  if ($std_core_hnd._yielding()) {
    return yield_cont(function(cont /* (5551) -> 5585 5584 */ , res /* 5551 */ ) {
      return under1x(ev, cont, res);
    });
  }
  else {
     
    $std_core_hnd._evv_set(w0);
    return y;
  }
}
 
export function clause_control_raw1(op) /* forall<a,b,e,e1,c,d> (op : (x : a, r : resume-context<b,e,e1,d>) -> e d) -> clause1<a,b,c,e,d> */  {
  return function(m /* marker<5686,5689> */ , ___wildcard_x598__16 /* ev<5688> */ , x /* 5684 */ ) {
    return yield_to(m, function(k /* (resume-result<5685,5689>) -> 5686 5689 */ ) {
        return op(x, k);
      });
  };
}
 
export function get(ref) /* forall<a,h> (ref : ref<h,a>) -> <read<h>,div> a */  {
  return ref.value;
}
 
export function protect_prompt(resumed, k, res) /* forall<a,e,b> (resumed : ref<global,bool>, k : (resume-result<a,b>) -> e b, res : b) -> e b */  {
   
  var did_resume = resumed.value;
  if (did_resume) {
    return res;
  }
  else {
     
    var b = $std_core_hnd._yielding();
    if (b) {
      if ($std_core_hnd._yielding_non_final()) {
        return yield_cont(function(cont /* (5860) -> 5937 5938 */ , x /* 5860 */ ) {
          return protect_prompt(resumed, k, cont(x));
        });
      }
      else {
         
        var yld = yield_capture();
         
        k(Finalize(res));
        if ($std_core_hnd._yielding()) {
          return yield_extend(function(___wildcard_x621__43 /* _5892 */ ) {
            return unsafe_reyield(yld);
          });
        }
        else {
          return unsafe_reyield(yld);
        }
      }
    }
    else {
      return k(Finalize(res));
    }
  }
}
 
export function protect(x, clause, k) /* forall<a,b,e,c> (x : a, clause : (x : a, k : (b) -> e c) -> e c, k : (resume-result<b,c>) -> e c) -> e c */  {
   
  var resumed = { value: false };
   
  var res = clause(x, function(ret /* 6056 */ ) {
       
      ((resumed).value = true);
      return k(Deep(ret));
    });
  return protect_prompt(resumed, k, res);
}
 
 
// generic control clause
export function clause_control1(clause) /* forall<a,b,e,c,d> (clause : (x : a, k : (b) -> e d) -> e d) -> clause1<a,b,c,e,d> */  {
  return function(m /* marker<6150,6152> */ , ___wildcard_x639__16 /* ev<6151> */ , x /* 6148 */ ) {
    return yield_to(m, function(k /* (resume-result<6149,6152>) -> 6150 6152 */ ) {
        return protect(x, clause, k);
      });
  };
}
 
 
// tail-resumptive clause: resumes exactly once at the end
// (these can be executed 'in-place' without capturing a resumption)
export function clause_tail1(op) /* forall<e,a,b,c,d> (op : (c) -> e d) -> clause1<c,d,b,e,a> */  {
  return function(___wildcard_x644__14 /* marker<6233,6234> */ , ev /* ev<6235> */ , x /* 6236 */ ) {
     
    var w0 = $std_core_hnd._evv_swap((ev.hevv));
     
    var y = op(x);
    if ($std_core_hnd._yielding()) {
      return yield_cont(function(cont /* (5551) -> 6233 6237 */ , res /* 5551 */ ) {
        return under1x(ev, cont, res);
      });
    }
    else {
       
      $std_core_hnd._evv_set(w0);
      return y;
    }
  };
}
 
 
// tail-resumptive clause that does not itself invoke operations
// (these can be executed 'in-place' without setting the correct evidence vector)
export function clause_tail_noop1(op) /* forall<e,a,b,c,d> (op : (c) -> e d) -> clause1<c,d,b,e,a> */  {
  return function(___wildcard_x649__14 /* marker<6304,6305> */ , ___wildcard_x649__17 /* ev<6306> */ , x /* 6307 */ ) {
    return op(x);
  };
}
 
 
// clause that never resumes (e.g. an exception handler)
// (these do not need to capture a resumption and execute finally clauses upfront)
export function clause_never1(op) /* forall<a,b,e,c,d> (op : (a) -> e d) -> clause1<a,b,c,e,d> */  {
  return function(m /* marker<6393,6395> */ , ___wildcard_x654__16 /* ev<6394> */ , x /* 6391 */ ) {
    return yield_to_final(m, function(___wildcard_x654__45 /* (resume-result<6392,6395>) -> 6393 6395 */ ) {
        return op(x);
      });
  };
}
 
 
// Automatically generated. Retrieves the `clause` constructor field of the `:clause0` type.
export function clause0_fs_clause(clause0) /* forall<a,b,e,c> (clause0 : clause0<a,b,e,c>) -> ((marker<e,c>, ev<b>) -> e a) */  {
  return clause0;
}
 
export function clause0_fs__copy(_this, clause) /* forall<a,b,e,c> (clause0<a,b,e,c>, clause : ? ((marker<e,c>, ev<b>) -> e a)) -> clause0<a,b,e,c> */  {
  if (clause !== undefined) {
    var _x6 = clause;
  }
  else {
    var _x6 = _this;
  }
  return _x6;
}
 
 
//inline extern cast-hnd( h : h<e1,r> ) : e h<e,r> { inline "#1"//inline extern cast-marker( m : marker<e1,r> ) : e marker<e,r> { inline "#1"
export function _perform0(ev, op) /* forall<a,e,b> (ev : ev<b>, op : forall<e1,c> (b<e1,c>) -> clause0<a,b,e1,c>) -> e a */  {
  var _x7 = op(ev.hnd);
  return _x7(ev.marker, ev);
}
 
export function under0(ev, op) /* forall<a,e,b> (ev : ev<b>, op : () -> e a) -> e a */  {
   
  var w0 = $std_core_hnd._evv_swap((ev.hevv));
   
  var y = op();
   
  $std_core_hnd._evv_set(w0);
  if ($std_core_hnd._yielding()) {
    return yield_cont(function(cont /* (6696) -> 6716 6715 */ , res /* 6696 */ ) {
       
      var w0_0 = $std_core_hnd._evv_swap((ev.hevv));
       
      var y_0 = cont(res);
      if ($std_core_hnd._yielding()) {
        return yield_cont(function(cont_0 /* (5551) -> 6716 6715 */ , res_0 /* 5551 */ ) {
          return under1x(ev, cont_0, res_0);
        });
      }
      else {
         
        $std_core_hnd._evv_set(w0_0);
        return y_0;
      }
    });
  }
  else {
    return y;
  }
}
 
export function clause_control_raw0(op) /* forall<a,e,e1,b,c> (op : (resume-context<a,e,e1,c>) -> e c) -> clause0<a,b,e,c> */  {
  return function(m /* marker<6804,6807> */ , ___wildcard_x680__16 /* ev<6806> */ ) {
    return yield_to(m, function(k /* (resume-result<6803,6807>) -> 6804 6807 */ ) {
        return op(k);
      });
  };
}
 
export function clause_control0(op) /* forall<a,e,b,c> (op : ((a) -> e c) -> e c) -> clause0<a,b,e,c> */  {
  return function(m /* marker<6897,6899> */ , ___wildcard_x688__16 /* ev<6898> */ ) {
    return yield_to(m, function(k /* (resume-result<6896,6899>) -> 6897 6899 */ ) {
        return protect($std_core_types.Unit, function(___wildcard_x688__55 /* () */ , r /* (6896) -> 6897 6899 */ ) {
            return op(r);
          }, k);
      });
  };
}
 
export function clause_tail0(op) /* forall<e,a,b,c> (op : () -> e c) -> clause0<c,b,e,a> */  {
  return function(___wildcard_x692__14 /* marker<6965,6966> */ , ev /* ev<6967> */ ) {
     
    var w0 = $std_core_hnd._evv_swap((ev.hevv));
     
    var y = op();
     
    $std_core_hnd._evv_set(w0);
    if ($std_core_hnd._yielding()) {
      return yield_cont(function(cont /* (6696) -> 6965 6968 */ , res /* 6696 */ ) {
         
        var w0_0 = $std_core_hnd._evv_swap((ev.hevv));
         
        var y_0 = cont(res);
        if ($std_core_hnd._yielding()) {
          return yield_cont(function(cont_0 /* (5551) -> 6965 6968 */ , res_0 /* 5551 */ ) {
            return under1x(ev, cont_0, res_0);
          });
        }
        else {
           
          $std_core_hnd._evv_set(w0_0);
          return y_0;
        }
      });
    }
    else {
      return y;
    }
  };
}
 
export function clause_tail_noop0(op) /* forall<e,a,b,c> (op : () -> e c) -> clause0<c,b,e,a> */  {
  return function(___wildcard_x695__14 /* marker<7023,7024> */ , ___wildcard_x695__17 /* ev<7025> */ ) {
    return op();
  };
}
 
export function clause_value(v) /* forall<a,e,b,c> (v : a) -> clause0<a,b,e,c> */  {
  return function(___wildcard_x698__14 /* marker<7081,7083> */ , ___wildcard_x698__17 /* ev<7082> */ ) {
    return v;
  };
}
 
export function clause_never0(op) /* forall<a,e,b,c> (op : () -> e c) -> clause0<a,b,e,c> */  {
  return function(m /* marker<7155,7157> */ , ___wildcard_x701__16 /* ev<7156> */ ) {
    return yield_to_final(m, function(___wildcard_x701__43 /* (resume-result<7154,7157>) -> 7155 7157 */ ) {
        return op();
      });
  };
}
 
 
// Automatically generated. Retrieves the `clause` constructor field of the `:clause2` type.
export function clause2_fs_clause(clause2) /* forall<a,b,c,d,e,a1> (clause2 : clause2<a,b,c,d,e,a1>) -> ((marker<e,a1>, ev<d>, a, b) -> e c) */  {
  return clause2;
}
 
export function clause2_fs__copy(_this, clause) /* forall<a,b,c,d,e,a1> (clause2<a,b,c,d,e,a1>, clause : ? ((marker<e,a1>, ev<d>, a, b) -> e c)) -> clause2<a,b,c,d,e,a1> */  {
  if (clause !== undefined) {
    var _x8 = clause;
  }
  else {
    var _x8 = _this;
  }
  return _x8;
}
 
export function under2(ev, op, x1, x2) /* forall<a,b,c,e,d> (ev : ev<d>, op : (a, b) -> e c, x1 : a, x2 : b) -> e c */  {
   
  var w0 = $std_core_hnd._evv_swap((ev.hevv));
   
  var z = op(x1, x2);
   
  $std_core_hnd._evv_set(w0);
  if ($std_core_hnd._yielding()) {
    return yield_cont(function(cont /* (7420) -> 7450 7449 */ , res /* 7420 */ ) {
       
      var w0_0 = $std_core_hnd._evv_swap((ev.hevv));
       
      var y = cont(res);
      if ($std_core_hnd._yielding()) {
        return yield_cont(function(cont_0 /* (5551) -> 7450 7449 */ , res_0 /* 5551 */ ) {
          return under1x(ev, cont_0, res_0);
        });
      }
      else {
         
        $std_core_hnd._evv_set(w0_0);
        return y;
      }
    });
  }
  else {
    return z;
  }
}
 
export function protect2(x1, x2, clause, k) /* forall<a,b,c,e,d> (x1 : a, x2 : b, clause : (x : a, x : b, k : (c) -> e d) -> e d, k : (resume-result<c,d>) -> e d) -> e d */  {
   
  var resumed = { value: false };
   
  var res = clause(x1, x2, function(ret /* 7582 */ ) {
       
      ((resumed).value = true);
      return k(Deep(ret));
    });
  return protect_prompt(resumed, k, res);
}
 
export function clause_control2(clause) /* forall<a,b,c,e,d,a1> (clause : (x1 : a, x2 : b, k : (c) -> e a1) -> e a1) -> clause2<a,b,c,d,e,a1> */  {
  return function(m /* marker<7692,7694> */ , ___wildcard_x726__16 /* ev<7693> */ , x1 /* 7689 */ , x2 /* 7690 */ ) {
    return yield_to(m, function(k /* (resume-result<7691,7694>) -> 7692 7694 */ ) {
        return protect2(x1, x2, clause, k);
      });
  };
}
 
export function clause_control_raw2(op) /* forall<a,b,c,e,e1,d,a1> (op : (x1 : a, x2 : b, r : resume-context<c,e,e1,a1>) -> e a1) -> clause2<a,b,c,d,e,a1> */  {
  return function(m /* marker<7811,7814> */ , ___wildcard_x730__16 /* ev<7813> */ , x1 /* 7808 */ , x2 /* 7809 */ ) {
    return yield_to(m, function(k /* (resume-result<7810,7814>) -> 7811 7814 */ ) {
        return op(x1, x2, k);
      });
  };
}
 
export function clause_tail2(op) /* forall<e,a,b,c,d,a1> (op : (c, d) -> e a1) -> clause2<c,d,a1,b,e,a> */  {
  return function(m /* marker<7913,7914> */ , ev /* ev<7915> */ , x1 /* 7916 */ , x2 /* 7917 */ ) {
    return under2(ev, op, x1, x2);
  };
}
 
export function clause_tail_noop2(op) /* forall<e,a,b,c,d,a1> (op : (c, d) -> e a1) -> clause2<c,d,a1,b,e,a> */  {
  return function(___wildcard_x736__14 /* marker<7997,7998> */ , ___wildcard_x736__17 /* ev<7999> */ , x1 /* 8000 */ , x2 /* 8001 */ ) {
    return op(x1, x2);
  };
}
 
export function _perform2(evx, op, x, y) /* forall<a,b,c,e,d> (evx : ev<d>, op : forall<e1,a1> (d<e1,a1>) -> clause2<a,b,c,d,e1,a1>, x : a, y : b) -> e c */  {
  var _x9 = op(evx.hnd);
  return _x9(evx.marker, evx, x, y);
}
 
export function clause_never2(op) /* forall<a,b,c,e,d,a1> (op : (a, b) -> e a1) -> clause2<a,b,c,d,e,a1> */  {
  return function(m /* marker<8221,8223> */ , ___wildcard_x744__16 /* ev<8222> */ , x1 /* 8218 */ , x2 /* 8219 */ ) {
    return yield_to_final(m, function(___wildcard_x744__49 /* (resume-result<8220,8223>) -> 8221 8223 */ ) {
        return op(x1, x2);
      });
  };
}
 
 
// For internal use
export function xperform1(ev, op, x) /* forall<a,b,e,c> (ev : ev<c>, op : forall<e1,d> (c<e1,d>) -> clause1<a,b,c,e1,d>, x : a) -> e b */  {
  var _x10 = op(ev.hnd);
  return _x10(ev.marker, ev, x);
}
 
export function clause_control_raw3(op) /* forall<a,b,c,d,e,e1,a1,b1> (op : (x1 : a, x2 : b, x3 : c, r : resume-context<d,e,e1,b1>) -> e b1) -> clause1<(a, b, c),d,a1,e,b1> */  {
  return function(m /* marker<8434,8437> */ , ___wildcard_x598__16 /* ev<8436> */ , x /* (8430, 8431, 8432) */ ) {
    return yield_to(m, function(k /* (resume-result<8433,8437>) -> 8434 8437 */ ) {
        return op(x.fst, x.snd, x.thd, k);
      });
  };
}
 
export function clause_control3(op) /* forall<a,b,c,d,e,a1,b1> (op : (x1 : a, x2 : b, x3 : c, k : (d) -> e b1) -> e b1) -> clause1<(a, b, c),d,a1,e,b1> */  {
  return function(m /* marker<8540,8542> */ , ___wildcard_x639__16 /* ev<8541> */ , x /* (8536, 8537, 8538) */ ) {
    return yield_to(m, function(k /* (resume-result<8539,8542>) -> 8540 8542 */ ) {
        return protect(x, function(_pat_x762__23 /* (8536, 8537, 8538) */ , k_0 /* (8539) -> 8540 8542 */ ) {
            return op(_pat_x762__23.fst, _pat_x762__23.snd, _pat_x762__23.thd, k_0);
          }, k);
      });
  };
}
 
export function clause_tail3(op) /* forall<e,a,b,c,d,a1,b1> (op : (c, d, a1) -> e b1) -> clause1<(c, d, a1),b1,b,e,a> */  {
  return clause_tail1(function(_pat_x765__20 /* (8640, 8641, 8642) */ ) {
    return op(_pat_x765__20.fst, _pat_x765__20.snd, _pat_x765__20.thd);
  });
}
 
export function clause_tail_noop3(op) /* forall<e,a,b,c,d,a1,b1> (op : (c, d, a1) -> e b1) -> clause1<(c, d, a1),b1,b,e,a> */  {
  return function(___wildcard_x649__14 /* marker<8738,8739> */ , ___wildcard_x649__17 /* ev<8740> */ , x /* (8741, 8742, 8743) */ ) {
    return op(x.fst, x.snd, x.thd);
  };
}
 
export function clause_never3(op) /* forall<a,b,c,d,e,a1,b1> (op : (a, b, c) -> e b1) -> clause1<(a, b, c),d,a1,e,b1> */  {
  return function(m /* marker<8843,8845> */ , ___wildcard_x654__16 /* ev<8844> */ , x /* (8839, 8840, 8841) */ ) {
    return yield_to_final(m, function(___wildcard_x654__45 /* (resume-result<8842,8845>) -> 8843 8845 */ ) {
        return op(x.fst, x.snd, x.thd);
      });
  };
}
 
export function _perform3(ev, op, x1, x2, x3) /* forall<a,b,c,d,e,a1> (ev : ev<a1>, op : forall<e1,b1> (a1<e1,b1>) -> clause1<(a, b, c),d,a1,e1,b1>, x1 : a, x2 : b, x3 : c) -> e d */  {
  var _x11 = op(ev.hnd);
  return _x11(ev.marker, ev, $std_core_types.Tuple3(x1, x2, x3));
}
 
export function under3(ev, op, x1, x2, x3) /* forall<a,b,c,d,e,a1> (ev : ev<a1>, op : (a, b, c) -> e d, x1 : a, x2 : b, x3 : c) -> e d */  {
   
  var w0 = $std_core_hnd._evv_swap((ev.hevv));
   
  var z = op(x1, x2, x3);
   
  $std_core_hnd._evv_set(w0);
  if ($std_core_hnd._yielding()) {
    return yield_cont(function(cont /* (9046) -> 9081 9080 */ , res /* 9046 */ ) {
       
      var w0_0 = $std_core_hnd._evv_swap((ev.hevv));
       
      var y = cont(res);
      if ($std_core_hnd._yielding()) {
        return yield_cont(function(cont_0 /* (5551) -> 9081 9080 */ , res_0 /* 5551 */ ) {
          return under1x(ev, cont_0, res_0);
        });
      }
      else {
         
        $std_core_hnd._evv_set(w0_0);
        return y;
      }
    });
  }
  else {
    return z;
  }
}
 
export function clause_control4(op) /* forall<a,b,c,d,a1,e,b1,c1> (op : (x1 : a, x2 : b, x3 : c, x4 : d, k : (a1) -> e c1) -> e c1) -> clause1<(a, b, c, d),a1,b1,e,c1> */  {
  return function(m /* marker<9187,9189> */ , ___wildcard_x639__16 /* ev<9188> */ , x /* (9182, 9183, 9184, 9185) */ ) {
    return yield_to(m, function(k /* (resume-result<9186,9189>) -> 9187 9189 */ ) {
        return protect(x, function(_pat_x786__23 /* (9182, 9183, 9184, 9185) */ , k_0 /* (9186) -> 9187 9189 */ ) {
            return op(_pat_x786__23.fst, _pat_x786__23.snd, _pat_x786__23.thd, _pat_x786__23.field4, k_0);
          }, k);
      });
  };
}
 
export function clause_tail4(op) /* forall<e,a,b,c,d,a1,b1,c1> (op : (c, d, a1, b1) -> e c1) -> clause1<(c, d, a1, b1),c1,b,e,a> */  {
  return clause_tail1(function(_pat_x789__20 /* (9298, 9299, 9300, 9301) */ ) {
    return op(_pat_x789__20.fst, _pat_x789__20.snd, _pat_x789__20.thd, _pat_x789__20.field4);
  });
}
 
export function clause_tail_noop4(op) /* forall<e,a,b,c,d,a1,b1,c1> (op : (c, d, a1, b1) -> e c1) -> clause1<(c, d, a1, b1),c1,b,e,a> */  {
  return function(___wildcard_x649__14 /* marker<9408,9409> */ , ___wildcard_x649__17 /* ev<9410> */ , x /* (9411, 9412, 9413, 9414) */ ) {
    return op(x.fst, x.snd, x.thd, x.field4);
  };
}
 
export function clause_never4(op) /* forall<a,b,c,d,a1,e,b1,c1> (op : (a, b, c, d) -> e c1) -> clause1<(a, b, c, d),a1,b1,e,c1> */  {
  return function(m /* marker<9526,9528> */ , ___wildcard_x654__16 /* ev<9527> */ , x /* (9521, 9522, 9523, 9524) */ ) {
    return yield_to_final(m, function(___wildcard_x654__45 /* (resume-result<9525,9528>) -> 9526 9528 */ ) {
        return op(x.fst, x.snd, x.thd, x.field4);
      });
  };
}
 
export function _perform4(ev, op, x1, x2, x3, x4) /* forall<a,b,c,d,a1,e,b1> (ev : ev<b1>, op : forall<e1,c1> (b1<e1,c1>) -> clause1<(a, b, c, d),a1,b1,e1,c1>, x1 : a, x2 : b, x3 : c, x4 : d) -> e a1 */  {
  var _x12 = op(ev.hnd);
  return _x12(ev.marker, ev, $std_core_types.Tuple4(x1, x2, x3, x4));
}
 
export function under4(ev, op, x1, x2, x3, x4) /* forall<a,b,c,d,a1,e,b1> (ev : ev<b1>, op : (a, b, c, d) -> e a1, x1 : a, x2 : b, x3 : c, x4 : d) -> e a1 */  {
   
  var w0 = $std_core_hnd._evv_swap((ev.hevv));
   
  var z = op(x1, x2, x3, x4);
   
  $std_core_hnd._evv_set(w0);
  if ($std_core_hnd._yielding()) {
    return yield_cont(function(cont /* (9747) -> 9787 9786 */ , res /* 9747 */ ) {
       
      var w0_0 = $std_core_hnd._evv_swap((ev.hevv));
       
      var y = cont(res);
      if ($std_core_hnd._yielding()) {
        return yield_cont(function(cont_0 /* (5551) -> 9787 9786 */ , res_0 /* 5551 */ ) {
          return under1x(ev, cont_0, res_0);
        });
      }
      else {
         
        $std_core_hnd._evv_set(w0_0);
        return y;
      }
    });
  }
  else {
    return z;
  }
}
 
export function _open_none0(f) /* forall<a,e,e1> (f : () -> e a) -> e1 a */  {
   
  var w = $std_core_hnd._evv_swap_create0();
   
  var x = f();
   
  var keep = $std_core_hnd._evv_set(w);
  return x;
}
 
export function _open_none1(f, x1) /* forall<a,b,e,e1> (f : (a) -> e b, x1 : a) -> e1 b */  {
   
  var w = $std_core_hnd._evv_swap_create0();
   
  var x = f(x1);
   
  var keep = $std_core_hnd._evv_set(w);
  return x;
}
 
export function _open_none2(f, x1, x2) /* forall<a,b,c,e,e1> (f : (a, b) -> e c, x1 : a, x2 : b) -> e1 c */  {
   
  var w = $std_core_hnd._evv_swap_create0();
   
  var x = f(x1, x2);
   
  var keep = $std_core_hnd._evv_set(w);
  return x;
}
 
export function _open_none3(f, x1, x2, x3) /* forall<a,b,c,d,e,e1> (f : (a, b, c) -> e d, x1 : a, x2 : b, x3 : c) -> e1 d */  {
   
  var w = $std_core_hnd._evv_swap_create0();
   
  var x = f(x1, x2, x3);
   
  var keep = $std_core_hnd._evv_set(w);
  return x;
}
 
export function _open_none4(f, x1, x2, x3, x4) /* forall<a,b,c,d,a1,e,e1> (f : (a, b, c, d) -> e a1, x1 : a, x2 : b, x3 : c, x4 : d) -> e1 a1 */  {
   
  var w = $std_core_hnd._evv_swap_create0();
   
  var x = f(x1, x2, x3, x4);
   
  var keep = $std_core_hnd._evv_set(w);
  return x;
}
 
export function open_at1(i, f, x) /* forall<a,b,e,e1> (i : ev-index, f : (a) -> e b, x : a) -> e1 b */  {
   
  var w = $std_core_hnd._evv_swap_create1(i);
   
  var y = f(x);
   
  $std_core_hnd._evv_set(w);
  if ($std_core_hnd._yielding()) {
    return yield_cont(function(cont /* (10315) -> 10341 10339 */ , res /* 10315 */ ) {
      return open_at1(i, cont, res);
    });
  }
  else {
    return y;
  }
}
 
export function _open_at0(i, f) /* forall<a,e,e1> (i : ev-index, f : () -> e a) -> e1 a */  {
   
  var w = $std_core_hnd._evv_swap_create1(i);
   
  var y = f();
   
  $std_core_hnd._evv_set(w);
  if ($std_core_hnd._yielding()) {
    return yield_cont(function(cont /* (10427) -> 10448 10446 */ , res /* 10427 */ ) {
      return open_at1(i, cont, res);
    });
  }
  else {
    return y;
  }
}
 
export function _open_at1(i, f, x) /* forall<a,b,e,e1> (i : ev-index, f : (a) -> e b, x : a) -> e1 b */  {
   
  var w = $std_core_hnd._evv_swap_create1(i);
   
  var y = f(x);
   
  $std_core_hnd._evv_set(w);
  if ($std_core_hnd._yielding()) {
    return yield_cont(function(cont /* (10536) -> 10562 10560 */ , res /* 10536 */ ) {
      return open_at1(i, cont, res);
    });
  }
  else {
    return y;
  }
}
 
export function _open_at2(i, f, x1, x2) /* forall<a,b,c,e,e1> (i : ev-index, f : (a, b) -> e c, x1 : a, x2 : b) -> e1 c */  {
   
  var w = $std_core_hnd._evv_swap_create1(i);
   
  var y = f(x1, x2);
   
  $std_core_hnd._evv_set(w);
  if ($std_core_hnd._yielding()) {
    return yield_cont(function(cont /* (10658) -> 10689 10687 */ , res /* 10658 */ ) {
      return open_at1(i, cont, res);
    });
  }
  else {
    return y;
  }
}
 
export function _open_at3(i, f, x1, x2, x3) /* forall<a,b,c,d,e,e1> (i : ev-index, f : (a, b, c) -> e d, x1 : a, x2 : b, x3 : c) -> e1 d */  {
   
  var w = $std_core_hnd._evv_swap_create1(i);
   
  var y = f(x1, x2, x3);
   
  $std_core_hnd._evv_set(w);
  if ($std_core_hnd._yielding()) {
    return yield_cont(function(cont /* (10793) -> 10829 10827 */ , res /* 10793 */ ) {
      return open_at1(i, cont, res);
    });
  }
  else {
    return y;
  }
}
 
export function _open_at4(i, f, x1, x2, x3, x4) /* forall<a,b,c,d,a1,e,e1> (i : ev-index, f : (a, b, c, d) -> e a1, x1 : a, x2 : b, x3 : c, x4 : d) -> e1 a1 */  {
   
  var w = $std_core_hnd._evv_swap_create1(i);
   
  var y = f(x1, x2, x3, x4);
   
  $std_core_hnd._evv_set(w);
  if ($std_core_hnd._yielding()) {
    return yield_cont(function(cont /* (10941) -> 10982 10980 */ , res /* 10941 */ ) {
      return open_at1(i, cont, res);
    });
  }
  else {
    return y;
  }
}
 
export function open1(indices, f, x) /* forall<a,b,e,e1> (indices : vector<ev-index>, f : (a) -> e b, x : a) -> e1 b */  {
   
  var w = evv_swap_create(indices);
   
  var y = f(x);
   
  $std_core_hnd._evv_set(w);
  if ($std_core_hnd._yielding()) {
    return yield_cont(function(cont /* (11089) -> 11115 11113 */ , res /* 11089 */ ) {
      return open1(indices, cont, res);
    });
  }
  else {
    return y;
  }
}
 
export function _open0(indices, f) /* forall<a,e,e1> (indices : vector<ev-index>, f : () -> e a) -> e1 a */  {
   
  var w = evv_swap_create(indices);
   
  var y = f();
   
  $std_core_hnd._evv_set(w);
  if ($std_core_hnd._yielding()) {
    return yield_cont(function(cont /* (11201) -> 11222 11220 */ , res /* 11201 */ ) {
      return open1(indices, cont, res);
    });
  }
  else {
    return y;
  }
}
 
export function _open1(indices, f, x) /* forall<a,b,e,e1> (indices : vector<ev-index>, f : (a) -> e b, x : a) -> e1 b */  {
   
  var w = evv_swap_create(indices);
   
  var y = f(x);
   
  $std_core_hnd._evv_set(w);
  if ($std_core_hnd._yielding()) {
    return yield_cont(function(cont /* (11310) -> 11336 11334 */ , res /* 11310 */ ) {
      return open1(indices, cont, res);
    });
  }
  else {
    return y;
  }
}
 
export function _open2(indices, f, x1, x2) /* forall<a,b,c,e,e1> (indices : vector<ev-index>, f : (a, b) -> e c, x1 : a, x2 : b) -> e1 c */  {
   
  var w = evv_swap_create(indices);
   
  var y = f(x1, x2);
   
  $std_core_hnd._evv_set(w);
  if ($std_core_hnd._yielding()) {
    return yield_cont(function(cont /* (11432) -> 11463 11461 */ , res /* 11432 */ ) {
      return open1(indices, cont, res);
    });
  }
  else {
    return y;
  }
}
 
export function _open3(indices, f, x1, x2, x3) /* forall<a,b,c,d,e,e1> (indices : vector<ev-index>, f : (a, b, c) -> e d, x1 : a, x2 : b, x3 : c) -> e1 d */  {
   
  var w = evv_swap_create(indices);
   
  var y = f(x1, x2, x3);
   
  $std_core_hnd._evv_set(w);
  if ($std_core_hnd._yielding()) {
    return yield_cont(function(cont /* (11567) -> 11603 11601 */ , res /* 11567 */ ) {
      return open1(indices, cont, res);
    });
  }
  else {
    return y;
  }
}
 
export function _open4(indices, f, x1, x2, x3, x4) /* forall<a,b,c,d,a1,e,e1> (indices : vector<ev-index>, f : (a, b, c, d) -> e a1, x1 : a, x2 : b, x3 : c, x4 : d) -> e1 a1 */  {
   
  var w = evv_swap_create(indices);
   
  var y = f(x1, x2, x3, x4);
   
  $std_core_hnd._evv_set(w);
  if ($std_core_hnd._yielding()) {
    return yield_cont(function(cont /* (11715) -> 11756 11754 */ , res /* 11715 */ ) {
      return open1(indices, cont, res);
    });
  }
  else {
    return y;
  }
}
 
export function try_finalize_prompt(res) /* forall<a,e> (res : a) -> e either<yield-info,a> */  {
  if ($std_core_hnd._yielding_non_final()) {
    return yield_cont(function(cont /* (11821) -> 11881 11880 */ , x /* 11821 */ ) {
      return try_finalize_prompt(cont(x));
    });
  }
  else {
     
    var b = $std_core_hnd._yielding();
    if (b) {
      return $std_core_types.Left(yield_capture());
    }
    else {
      return $std_core_types.Right(res);
    }
  }
}
 
export function unsafe_try_finalize(action) /* forall<a,e> (action : () -> e a) -> e either<yield-info,a> */  {
  return try_finalize_prompt(action());
}