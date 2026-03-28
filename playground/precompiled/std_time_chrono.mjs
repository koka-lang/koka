// Koka generated module: std/time/chrono, koka version: 3.2.4
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
import * as $std_time_timestamp from './std_time_timestamp.mjs';
import * as $std_time_duration from './std_time_duration.mjs';
import * as $std_time_instant from './std_time_instant.mjs';
import * as $std_time_utc from './std_time_utc.mjs';
import * as $std_num_ddouble from './std_num_ddouble.mjs';
import * as $std_num_float64 from './std_num_float64.mjs';
 
// externals
/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
var prev = 0;

function _unix_now() {
  var now  = Date.now() * 1e-3; // from milliseconds to seconds
  var diff = now - prev;
  if (prev != 0 && diff <= 0 && diff >= -1) {
    // negative timestep of less than 1 second.
    // in this case we assume this is a leap second on an OS that
    // jumps back (instead of smearing).
    // keep increasing by 1 nano second until the clock catches up to
    // maintain monotonicity.
    now = prev + 1e-9;
  }
  prev = now;
  var secs = Math.floor(now);
  var frac = now - secs;
  return $std_core_types.Tuple2(secs, frac);
}

function _now_precision() {
  return 0.001; // millisecond precision
}
 
// type declarations
 
// declarations
 
 
// Returns a unix time stamp as seconds and fraction of seconds;
// The fraction of seconds is for added precision if necessary,
// and can be larger than one to indicate leap seconds.
// This still needs to be adjusted to our epoch and taking account of leap seconds.
export function unix_now() /* () -> ndet (float64, float64) */  {
  return _unix_now();
}
 
 
// The current `:instant` in time as returned by the system clock
// in an optional time scale `ts` (`= ts-ti`).
//
// This uses the best available system clock for the requested
// timescale. For example
// it uses [``CLOCK_UTC``](https://www.madore.org/~david/computers/unix-leap-seconds.html)
// when available to get proper UTC time, or ``CLOCK_TAI`` for TAI time.
//
// Otherwise, it usually uses Unix (POSIX) time (``CLOCK_REALTIME``).
// Unfortunately, most operating systems cannot not report time in leap
// seconds accurately. The `now` function is limited by the OS in this case.
//
// To guard against inaccurate clocks and increase monotonicity,
// the `now` function guarantees that if the current measurement is
// upto 1 second in the past with regard to the previous call to `now`,
// that the returned instant is monotonic by adding nano seconds to the
// previous measurement until the system clock catches up again.
//
// This is effective in particular on older OS's where the time sometimes jumps
// back one second after a leap second. By limiting the adjustment to at most
// one second it ensures the clock catches up soon and does not affect the user
// setting a new time in the past.
export function now_in(ts) /* (ts : ? std/time/instant/timescale) -> ndet std/time/instant/instant */  {
  var _x0 = unix_now();
  if (((_x0.snd) === (0.0))) {
    var _x1 = $std_num_ddouble.Ddouble(_x0.fst, 0.0);
  }
  else {
    var _x1 = $std_num_ddouble.dsum(_x0.fst, _x0.snd);
  }
  var _x2 = (ts !== undefined) ? ts : $std_time_utc.ts_ti;
  return $std_time_utc.timespan_fs_unix_instant(_x1, 0, _x2);
}
 
 
// monadic lift
export function _mlift_now_10006(_y_x10003) /* (std/time/instant/timescale) -> std/time/utc/utc std/time/instant/instant */  {
  return $std_core_hnd._open_none1(now_in, _y_x10003);
}
 
 
// The current `:instant` in time as returned by the system clock
// in the UTC timescale. Equivalent to `now-in(utc())`, see
// `now-in` for further information about the system clock.
export function now() /* () -> <ndet,std/time/utc/utc> std/time/instant/instant */  {
   
  var ev_10010 = $std_core_hnd._evv_at(0);
   
  var _x3 = $std_time_utc.utc_fs__select(ev_10010.hnd);
  var x_10007 = _x3(ev_10010.marker, ev_10010);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_now_10006);
  }
  else {
    return $std_core_hnd._open_none1(now_in, x_10007);
  }
}
 
export function xnow_resolution() /* () -> ndet float64 */  {
  return _now_resolution();
}
 
 
// Return the smallest time difference that the system clock can measure.
export function now_resolution() /* () -> ndet std/time/duration/duration */  {
   
  var secs_10002 = xnow_resolution();
  return $std_num_ddouble.Ddouble(secs_10002, 0.0);
}