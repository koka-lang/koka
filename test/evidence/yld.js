/*---------------------------------------------------------------------------
  Copyright 2020 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

const _context = { yield: 0, evv: [], yield_conts : [], yield_clause : null };

var _marker_unique = 1;

function evv_insert_ev(evv,ev) {
  const n = evv.length;
  const evv2 = new Array(n+1);
  var i;
  for(i = 0; i < n; i++) {
    const ev2 = evv[i];
    if (ev.tag <= ev2.tag) break;
    evv2[i] = ev2;
  }
  evv2[i] = ev;
  for(; i < n; i++) {
    evv2[i+1] = evv[i];
  }
  _context.evv = evv2;
  return evv2;
}

function evv_lookup(evv,tag) {
  for(var i = 0; i < evv.length; i++) {
    if (tag === evv[i].tag) return evv[i];
  }
  console.error("cannot find " + tag + " in " + evv.toString());
  return null;
}

function _kcompose( start, conts ) {
  return function(x) {
    var y = x;
    for(var i = start; i < conts.length; i++) {
      y = conts[i](y);
      if (_context.yield !== 0) {
        (function(i){ return _yield_extend(_kcompose(i+1,conts)); })(i);
      }
    }
    return y;
  }
}

function _yield_extend(_x, next) {
  if (_context.yield===0) console.error("yield extension while not yielding!");
  _context.yield_conts.push(next);
  return undefined;
}

function _yield_prompt(evv, evv_expected, m, res) {
  if (_context.evv !== evv_expected) console.log("non-matching evidence");
  _context.evv = evv;
  if (_context.yield === 0) return Pure(res);
  if (_context.yield !== m) return Yielding;
  const conts = _context.yield_conts;
  const clause = _context.yield_clause;
  _context.yield = 0;
  _context.yield_conts = null;
  _context.yield_clause = null;
  return Yielded(clause, _kcompose(0,conts));
}

function _yield_to(m,clause) {
  if (_context.yield!==0) console.error("yielding while yielding!");
  const evv = _context.evv;
  _context.yield = m;
  _context.yield_clause = clause;
  _context.yield_conts = new Array();
  _context.yield_conts.push(function(x){ _context.evv = evv; return x; });
  return undefined;
}
