/*---------------------------------------------------------------------------
  Copyright 2020 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

//var $std_core_hnd._evv_ofs = 0;
//var $std_core_hnd._evv     = [];
//var $std_core_hnd._yield   = null; // { marker: 0, clause: null, conts: [], conts_count: 0, final: bool };

var $marker_unique = 1;

function _assert(cond,msg) {
  if (!cond) console.error(msg);
}

$std_core_hnd = $std_core_types._export($std_core_hnd,{
                  "_evv"        : [],
                  "_evv_empty"  : [],
                  "_yield"      : null,
                  "_yield_to"   : _yield_to,
                  "_yield_final": _yield_final,
                });

//--------------------------------------------------
// evidence: { evv: [forall h. ev<h>], ofs : int }
//--------------------------------------------------

function _evv_insert(evv,ev) {
  const n    = evv.length;
  const evv2 = new Array(n+1);
  var i;
  for(i = 0; i < n; i++) {
    const ev2 = evv[i];
    if (ev._field1 <= ev2._field1) break;
    evv2[i] = ev2;
  }
  evv2[i] = ev;
  for(; i < n; i++) {
    evv2[i+1] = evv[i];
  }
  return evv2;
}

function _evv_delete(evv,i,behind) {
  if (behind) i++;
  const n = evv.length;
  const evv2 = new Array(n-1);
  var j;
  for(j = 0; j < i; j++) {
    evv2[j] = evv[j];
  }
  for(; j < n-1; j++) {
    evv2[j] = evv[j + 1];
  }
  return evv2;
}

function __evv_lookup(evv,tag) {
  for(var i = 0; i < evv.length; i++) {
    if (tag === evv[i]._field1) return evv[i];
  }
  console.error("cannot find " + tag + " in " + _evv_show(evv));
  return null;
}

function __evv_index(evv,tag) {
  for(var i = 0; i < evv.length; i++) {
    if (tag === evv[i]._field1) return (i);
  }
  console.error("cannot find " + tag + " in " + _evv_show(evv));
  return null;
}

function _evv_show(evv) {
  evv.sort(function(ev1,ev2){ return (ev1._field2 - ev2._field2); });
  var out = "";
  for( var i = 0; i < evv.length; i++) {
    const evvi = evv[i]._field4;
    out += ((i==0 ? "{ " : "  ") + evv[i]._field1.padEnd(8," ") + ": marker " + evv[i]._field2 + ", under <" +
             evvi.map(function(ev){ return ev._field2.toString(); }).join(",") + ">" + (i==evv.length-1 ? "}" : "") + "\n");
  }
  return out;
}

function _yield_show() {
  if (_yielding()) {
    return "yielding to " + $std_core_hnd._yield.marker + ", final: " + $std_core_hnd._yield.final;
  }
  else {
    return "pure"
  }
}


function _evv_expect(m,expected) {
  if (($std_core_hnd._yield===null || $std_core_hnd._yield.marker === m) && ($std_core_hnd._evv !== expected.evv)) {
    console.error("expected evidence: \n" + _evv_show(expected) + "\nbut found:\n" + _evv_show($std_core_hnd._evv));
  }
}

function _guard(evv) {
  if ($std_core_hnd._evv !== evv) {
    console.error("trying to resume outside the (handler) scope of the original handler. \n captured under:\n" + _evv_show(evv) + "\n but resumed under:\n" + _evv_show($std_core_hnd._evv));
    throw "trying to resume outside the (handler) scope of the original handler";
  }
}

function _throw_resume_final(f) {
  throw "trying to resume an unresumable resumption (from finalization)";
}


function _evv_swap(w) {
  const w0 = $std_core_hnd._evv;
  $std_core_hnd._evv = w;
  return w0;
}

const _evv_empty = [];

function _evv_swap_create0() {
  if (_evv.length===0) return;
  const evv = $std_core_hnd._evv;
  $std_core_hnd._evv = evv_empty;
  return evv;
}

function _evv_swap_create1(i) {
  const evv = _evv;
  if (evv.length!==1) $std_core_hnd._evv = [evv[i]];
  return evv;
}

function _evv_create( evv, indices ) {
  const n = indices.length;
  const evv2 = new Array(n);
  for(var i = 0; i < n; i++) {
    evv2[i] = evv[indices[i]];
  }
  return evv2;
}

function _evv_swap_create(indices) {
  const evv = $std_core_hnd._evv;
  $std_core_hnd._evv = _evv_create(evv,indices);
  return evv;
}



//--------------------------------------------------
// Yielding
//--------------------------------------------------
function _yielding() {
  return ($std_core_hnd._yield !== null);
}

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
  if ($std_core_hnd._yield.final) return;
  $std_core_hnd._yield.conts[$std_core_hnd._yield.conts_count++] = next;  // index is ~80% faster as push
}

function _yield_cont(f) {
  _assert(_yielding(), "yield extension while not yielding!");
  if ($std_core_hnd._yield.final) return;
  const cont   = _kcompose($std_core_hnd._yield.conts_count,$std_core_hnd._yield.conts);
  $std_core_hnd._yield.conts = new Array(8);
  $std_core_hnd._yield.conts_count = 1;
  $std_core_hnd._yield.conts[0] = function(x){ return f(cont,x); };
}

function _yield_prompt(m) {
  if ($std_core_hnd._yield === null) {
    return Pure;
  }
  else if ($std_core_hnd._yield.marker !== m) {
    return ($std_core_hnd._yield.final ? YieldingFinal : Yielding);
  }
  else { // $std_core_hnd._yield.marker === m
    const cont   = ($std_core_hnd._yield.final ? _throw_resume_final : _kcompose($std_core_hnd._yield.conts_count,$std_core_hnd._yield.conts));
    const clause = $std_core_hnd._yield.clause;
    $std_core_hnd._yield = null;
    return Yield(clause,cont);
  }
}

function _yield_final(m,clause) {
  _assert(!_yielding(),"yielding final while yielding!");
  $std_core_hnd._yield = { marker: m, clause: clause, conts: null, conts_count: 0, final: true };
}

function _yield_to(m,clause) {
  _assert(!_yielding(),"yielding while yielding!");
  $std_core_hnd._yield = { marker: m, clause: clause, conts: new Array(8), conts_count: 0, final: false };
}

function _yield_capture() {
  _assert(_yielding(),"can only capture a yield when yielding!");
  const yld = $std_core_hnd._yield;
  $std_core_hnd._yield = null;
  return yld;
}

function _reyield( yld ) {
  _assert(!_yielding(),"can only reyield a yield when not yielding!");
  $std_core_hnd._yield = yld;
}
