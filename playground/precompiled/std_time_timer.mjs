// Koka generated module: std/time/timer, koka version: 3.2.4
"use strict";
 
// imports
import * as $std_core_types from './std_core_types.mjs';
import * as $std_core_hnd from './std_core_hnd.mjs';
import * as $std_core_exn from './std_core_exn.mjs';
import * as $std_core_bool from './std_core_bool.mjs';
import * as $std_core_order from './std_core_order.mjs';
import * as $std_core_char from './std_core_char.mjs';
import * as $std_core_int from './std_core_int.mjs';
import * as $std_core_vector from './std_core_vector.mjs';
import * as $std_core_string from './std_core_string.mjs';
import * as $std_core_sslice from './std_core_sslice.mjs';
import * as $std_core_list from './std_core_list.mjs';
import * as $std_core_maybe from './std_core_maybe.mjs';
import * as $std_core_maybe2 from './std_core_maybe2.mjs';
import * as $std_core_either from './std_core_either.mjs';
import * as $std_core_result from './std_core_result.mjs';
import * as $std_core_tuple from './std_core_tuple.mjs';
import * as $std_core_lazy from './std_core_lazy.mjs';
import * as $std_core_show from './std_core_show.mjs';
import * as $std_core_debug from './std_core_debug.mjs';
import * as $std_core_delayed from './std_core_delayed.mjs';
import * as $std_core_console from './std_core_console.mjs';
import * as $std_core from './std_core.mjs';
import * as $std_num_float64 from './std_num_float64.mjs';
import * as $std_num_ddouble from './std_num_ddouble.mjs';
import * as $std_time_duration from './std_time_duration.mjs';
import * as $std_time_instant from './std_time_instant.mjs';
 
// externals
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
 
// type declarations
 
// declarations
 
export function xticks() /* () -> ndet (float64, float64) */  {
  return _ticks();
}
 
 
// Return a high-resolution time stamp in fractional SI seconds.
// The duration is guaranteed to be monotonically increasing
// and have at least millisecond resolution.
export function ticks() /* () -> ndet std/time/duration/duration */  {
  var _x0 = xticks();
  return $std_time_duration.float64frac_fs_duration(_x0.fst, _x0.snd);
}
 
 
// Return the smallest time difference in seconds that `ticks` can measure.
export function xticks_resolution() /* () -> ndet float64 */  {
  return _ticks_resolution();
}
 
 
// Return the smallest time difference in seconds that `ticks` can measure.
export function ticks_resolution() /* () -> ndet std/time/duration/duration */  {
   
  var secs_10000 = xticks_resolution();
  return $std_num_ddouble.Ddouble(secs_10000, 0.0);
}
 
 
// monadic lift
export function _mlift_elapsed_10004(t0, x) /* forall<a,e> (t0 : std/time/duration/duration, x : a) -> <ndet|e> (std/time/duration/duration, a) */  {
   
  var _x1 = xticks();
  var t1 = $std_time_duration.float64frac_fs_duration(_x1.fst, _x1.snd);
  return $std_core_types.Tuple2($std_time_duration._lp__dash__rp_(t1, t0), x);
}
 
 
// Return the number of fractional seconds that it takes to evaluate `action`.
export function elapsed(action) /* forall<a,e> (action : () -> <ndet|e> a) -> <ndet|e> (std/time/duration/duration, a) */  {
   
  var _x1 = xticks();
  var t0 = $std_time_duration.float64frac_fs_duration(_x1.fst, _x1.snd);
   
  var x_10006 = action();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(x_0 /* 126 */ ) {
       
      var _x1 = xticks();
      var t1 = $std_time_duration.float64frac_fs_duration(_x1.fst, _x1.snd);
      return $std_core_types.Tuple2($std_time_duration._lp__dash__rp_(t1, t0), x_0);
    });
  }
  else {
     
    var _x1 = xticks();
    var t1_0 = $std_time_duration.float64frac_fs_duration(_x1.fst, _x1.snd);
    return $std_core_types.Tuple2($std_time_duration._lp__dash__rp_(t1_0, t0), x_10006);
  }
}
 
 
// monadic lift
export function _mlift_print_elapsed_10005(msg, _y_x10002) /* forall<a,e> (msg : ? string, (std/time/duration/duration, a)) -> <ndet,console/console|e> a */  {
   
  var _x1 = (msg !== undefined) ? msg : "elapsed";
  $std_core_console.printsln($std_core_types._lp__plus__plus__rp_(_x1, $std_core_types._lp__plus__plus__rp_(" ", $std_time_duration.show(_y_x10002.fst, 3))));
  return _y_x10002.snd;
}
 
 
// Measure the number of fractional seconds that it takes to evaluate `action`, and print `msg` postfixed with the
// measured time in millisecond resolution.
export function print_elapsed(action, msg) /* forall<a,e> (action : () -> <ndet,console/console|e> a, msg : ? string) -> <ndet,console/console|e> a */  {
   
  var x_10011 = elapsed(action);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10002 /* (std/time/duration/duration, 310) */ ) {
      return _mlift_print_elapsed_10005(msg, _y_x10002);
    });
  }
  else {
     
    var _x1 = (msg !== undefined) ? msg : "elapsed";
    $std_core_console.printsln($std_core_types._lp__plus__plus__rp_(_x1, $std_core_types._lp__plus__plus__rp_(" ", $std_time_duration.show(x_10011.fst, 3))));
    return x_10011.snd;
  }
}