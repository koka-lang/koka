/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

var $evv = [];
var $yield = null; // { marker: 0, clause: null, conts: [], conts_count: 0, final: bool };
var $marker_unique = 1;

//--------------------------------------------------
// evidence
//--------------------------------------------------

function _evv_insert(evv,ev) {
  const n = evv.length;
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

function _evv_lookup(evv,tag) {
  for(var i = 0; i < evv.length; i++) {
    if (tag === evv[i]._field1) return evv[i];
  }
  console.error("cannot find " + tag + " in " + JSON.stringify(evv,2));
  return null;
}

function _evv_show(evv0) {
  const evv = evv0.slice(0);
  evv.sort(function(ev1,ev2){ return (ev1._field2 - ev2._field2); });
  var out = "";
  for( var i = 0; i < evv.length; i++) {
    out += ("" + evv[i]._field1.padEnd(8," ") + ": marker " + evv[i]._field2 + ", under <" + evv[i]._field4.map(function(ev){ return ev._field2.toString(); }).join(",") + ">\n");
  }
  return out;
}

function _yield_show() {
  if (_yielding()) {
    return "yielding to " + $yield.marker + ", final: " + $yield.final;
  }
  else {
    return "pure"
  }
}

function _evv_expect(m,expected) {
  if (($yield===null || $yield.marker === m) && ($evv !== expected)) {
    console.error("expected evidence: \n" + _evv_show(expected) + "\nbut found:\n" + _evv_show($evv));
  }
}

//--------------------------------------------------
// Yielding
//--------------------------------------------------
function _yielding() {
  return ($yield !== null);
}

function _kcompose( from, to, conts ) {
  return function(x) {
    var acc = x;
    for(var i = from; i < to; i++) {
      acc = conts[i](acc);
      if (_yielding()) return ((function(i){ return _yield_extend(_kcompose(i+1,to,conts)); })(i));
    }
    return acc;
  }
}

function _yield_extend(next) {
  if (!_yielding()) console.error("yield extension while not yielding!");
  if ($yield.final) return;
  $yield.conts[$yield.conts_count++] = next;  // index is ~80% faster as push
}

function _yield_cont(f) {
  if (!_yielding()) console.error("yield extension while not yielding!");
  if ($yield.final) return;
  const cont   = _kcompose(0,$yield.conts_count,$yield.conts);
  $yield.conts = new Array(8);
  $yield.conts_count = 1;
  $yield.conts[0] = function(x){ return f(cont,x); };
}

function _yield_prompt(m, res) {
  if ($yield === null) {
    return Pure(res);
  }
  else if ($yield.marker !== m) {
    return Yielding($yield.final);
  }
  else {
    const cont   = ($yield.final ? $std_core.Nothing : $std_core.Just(_kcompose(0,$yield.conts_count,$yield.conts)));
    const clause = $yield.clause;
    $yield = null;
    return Yield(clause,cont);
  }
}

function _yield_to(m,clause,final) {
  if (_yielding()) console.error("yielding while yielding!");
  const evv = $evv;
  $yield = { marker: m, clause: clause, conts: new Array(8), conts_count: 1, final: final };
  $yield.conts[0] = function(f){ $evv = evv; return f(); };  // restore evidence when resuming
}
