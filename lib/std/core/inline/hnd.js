/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

//var $std_core_hnd._evv_ofs = 0;
//var _evv     = [];
//var _yield   = null; // { marker: 0, clause: null, conts: [], conts_count: 0, final: bool };

var $marker_unique = 1;  // must be > 0

function _assert(cond,msg) {
  if (!cond) console.error(msg);
}

// for internal references
const $std_core_hnd = { "_evv_get"    : _evv_get,
                  "_evv_set"    : _evv_set,
                  "_evv_at"     : _evv_at,
                  "_evv_swap"   : _evv_swap,
                  "_evv_swap_create0"   : _evv_swap_create0,
                  "_evv_swap_create1"   : _evv_swap_create1,
                  "_yielding"   : _yielding,
                  "_yielding_non_final": _yielding_non_final,     
                  "_evv_cfc"    : _evv_cfc,
                  "_yield_to"   : _yield_to,
                  "_yield_final": _yield_final,
                };


var _evv = [];
var _yield = null;

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
    _evv._cfc = ev.cfc;
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

function ev_none() {
  return Ev(null,0,null,-1,[]);
}

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

export function _evv_cfc() {
  return _evv_get_cfc(_evv);
}


function _evv_insert(evv,ev) {
  // update ev
  if (ev.marker===0) return; // marker zero is ev-none
  ev.hevv = evv;
  const cfc = _cfc_lub(_evv_get_cfc(evv), ev.cfc);
  if (ev.marker < 0) { // negative marker is used for named evidence; this means this evidence should not be inserted into the evidence vector
    evv._cfc = cfc;  // control flow context
    return; // a negative (named) marker is not in the evidence vector
  }
  // insert in the vector
  const n    = evv.length;
  const evv2 = new Array(n+1);  
  evv2._cfc = cfc;
  ev.cfc = cfc; // update in-place
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
  // update cfc?
  if (evv[i].cfc >= _evv_get_cfc(evv)) {
    var cfc = evv2[0].cfc;
    for(j = 1; j < n-1; j++) {
      cfc = _cfc_lub(evv2[j].cfc, cfc);
    }
    evv2._cfc = cfc;
  }
  return evv2;
}

function _evv_swap_delete(i,behind) {
  const w0 = _evv;
  _evv = _evv_delete(w0,i,behind);
  return w0;
}

function __evv_lookup(tag) {
  const evv = _evv;
  for(var i = 0; i < evv.length; i++) {
    if (tag === evv[i].htag) return evv[i];
  }
  console.error("cannot find " + tag + " in " + _evv_show(evv));
  return null;
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
