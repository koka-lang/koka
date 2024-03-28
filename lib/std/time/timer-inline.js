/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

var _ticks;
var _ticks_resolution = 0.001; // milli seconds

// only used if we don't have a performance or process counter
var _ticks_delta = 0.0;
var _ticks_last  = 0.0;
var _ticks_get;

if (typeof process !== 'undefined' && typeof process.hrtime === 'function') {
  _ticks_resolution = 1.0e-9; // nano seconds
  _ticks = function() {
    var t = process.hrtime();
    return $std_core_types.Tuple2(t[0], t[1] * _ticks_resolution); // to seconds
  }
}
else if (typeof performance !== 'undefined' && typeof performance.now === 'function') {
  _ticks_resolution = 1.0e-6; // micro seconds
  _ticks = function() {
    var secs = performance.now() * 0.001; // performance.now is in fractional milli seconds
    return $std_core_types.Tuple2(secs, 0.0);
  };
}
else {
  // need to use Date; ensure monotonicity even if the clock is set back
  _ticks_resolution = 1.0e-3; // milli seconds
  _ticks_get    = (typeof Date.now == "function" ? function() { return Date.now(); } : function(){ return new Date().getTime(); });
  _ticks_last   = _ticks_get();
  _ticks_delta  = - _ticks_last;
  _ticks = function() {
    var t = _ticks_get();
    if (t <= _ticks_last) { // ouch, not monotonic; increase by a little and remember a new delta
      _ticks_delta = _ticks_last - t + 1;
    }
    _ticks_last = t;
    return $std_core_types.Tuple2( (t + _ticks_delta) * _ticks_resolution, 0.0);
  };
}

function _init_timer(){
  return {};
}

function _start_timer(timer, ms, repeat, fcn){
  const rp = Number(repeat)
  const msx = Number(ms)
  if (rp != 0) {
    timer.id = setInterval(fcn, rp);
    timer.repeat = rp;
  } else {
    timer.id = setTimeout(fcn, msx);
  } 
}

function _stop_timer(timer){
  if (timer.id) {
    if (timer.repeat != 0) {
      clearInterval(timer.id);
    } else {
      clearTimeout(timer.id);
    }
    timer.id = null;
  }
}