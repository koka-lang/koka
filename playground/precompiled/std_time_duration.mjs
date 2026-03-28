// Koka generated module: std/time/duration, koka version: 3.2.4
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
import * as $std_num_ddouble from './std_num_ddouble.mjs';
import * as $std_time_timestamp from './std_time_timestamp.mjs';
import * as $std_num_float64 from './std_num_float64.mjs';
import * as $std_num_int32 from './std_num_int32.mjs';
 
// externals
 
// type declarations
// type duration
export function Duration(secs) /* (secs : std/time/timestamp/timespan) -> duration */  {
  return secs;
}
 
// declarations
 
 
// Automatically generated. Retrieves the `secs` constructor field of the `:duration` type.
export function duration_fs_secs(duration_0) /* (duration : duration) -> std/time/timestamp/timespan */  {
  return duration_0;
}
 
export function duration_fs__copy(_this, secs) /* (duration, secs : ? std/time/timestamp/timespan) -> duration */  {
  if (secs !== undefined) {
    var _x0 = secs;
  }
  else {
    var _x0 = _this;
  }
  return _x0;
}
 
 
// A zero duration.
export var zero;
var zero = $std_num_ddouble.zero;
 
export var duration0;
var duration0 = $std_num_ddouble.zero;
 
 
// Convert a `:timespan` to a `:duration`. Be careful to only use
// use this on timespan's that are in TAI SI seconds!
export function duration(t) /* (t : std/time/timestamp/timespan) -> duration */  {
  return t;
}
 
 
// Convert a `:timestamp` to a `:duration`. Be careful to only use
// use this on timestamp's that are in TAI SI seconds and do not
// contain leap seconds!
export function unsafe_duration(t) /* (t : std/time/timestamp/timestamp) -> duration */  {
  return $std_time_timestamp.unsafe_timespan_withleap(t);
}
 
 
// Return the duration in SI seconds.
export function seconds(d) /* (d : duration) -> std/time/timestamp/timespan */  {
  return d;
}
 
 
// Create a duration from seconds as a `:float64`.
export function float64_fs_duration(secs) /* (secs : float64) -> duration */  {
  return $std_num_ddouble.Ddouble(secs, 0.0);
}
 
 
// Create a duration from seconds and a fraction as a `:float64`'s.
export function float64frac_fs_duration(secs, frac) /* (secs : float64, frac : float64) -> duration */  {
  if ((frac === (0.0))) {
    var _x1 = $std_num_ddouble.Ddouble(secs, 0.0);
  }
  else {
    var _x1 = $std_num_ddouble.dsum(secs, frac);
  }
  return _x1;
}
 
 
// Create a duration from whole seconds `secs` and a fraction of seconds `frac`.
export function int_fs_duration(secs, frac) /* (secs : int, frac : ? float64) -> duration */  {
  var _x2 = (frac !== undefined) ? frac : 0.0;
  return $std_time_timestamp.int_fs_timespan(secs, _x2);
}
 
 
// Create a `:duration` of `n` seconds.
export function int_fs_seconds(n) /* (n : int) -> duration */  {
  var _x4 = undefined;
  var _x3 = (_x4 !== undefined) ? _x4 : 0.0;
  return $std_time_timestamp.int_fs_timespan(n, _x3);
}
 
 
// Convert a duration to a `:timespan`.
export function timespan(d) /* (d : duration) -> std/time/timestamp/timespan */  {
  return d;
}
 
 
// Convert a duration to a `:timespan`.
export function timestamp(d) /* (d : duration) -> std/time/timestamp/timestamp */  {
  var _x5 = d;
  var _x7 = undefined;
  var _x6 = (_x7 !== undefined) ? _x7 : 0;
  return $std_time_timestamp.Timestamp(_x5, $std_core_types._int_clamp32(_x6));
}
 
 
// Return the duration in rounded SI milli-seconds.
export function milli_seconds(d) /* (d : duration) -> int */  {
  var _x8 = d;
  return $std_num_ddouble.int($std_num_ddouble._lp__star__rp_(_x8, $std_time_timestamp.int_fs_timespan(1000)));
}
 
 
// Return the duration in rounded SI nano-seconds.
export function nano_seconds(d) /* (d : duration) -> int */  {
  var _x9 = d;
  return $std_num_ddouble.int($std_num_ddouble._lp__star__rp_(_x9, $std_time_timestamp.int_fs_timespan(1000000000)));
}
 
 
// The whole seconds (in some time unit) of the duration as `:int``. Rounds towards zero.
export function truncate(d) /* (d : duration) -> int */  {
  var _x12 = d.hi;
  var _x11 = (_x12 < (0.0));
  if (_x11) {
    var _x13 = d;
    var _x10 = $std_num_ddouble.ceiling(_x13);
  }
  else {
    var _x14 = d;
    var _x10 = $std_num_ddouble.floor(_x14);
  }
  return $std_num_ddouble.int(_x10);
}
 
 
// The fractional seconds of a duration as a `:float64`.
// `d.seconds == d.truncate.fixed + d.fraction.fixed
export function fraction(d) /* (d : duration) -> float64 */  {
   
  var _x15 = d;
  var x_10017 = $std_num_ddouble.fraction(_x15);
  return x_10017.hi;
}
 
 
// Is this a negative duration?
export function is_neg(d) /* (d : duration) -> bool */  {
  var _x15 = d.hi;
  return (_x15 < (0.0));
}
 
 
// Is this a zero duration?
export function is_zero(d) /* (d : duration) -> bool */  {
  var _x16 = d.hi;
  return (_x16 === (0.0));
}
 
 
// Is this a positive duration?
export function is_pos(d) /* (d : duration) -> bool */  {
  var _x17 = d.hi;
  return (_x17 > (0.0));
}
 
 
// Compare two `:duration`s.
export function cmp(i, j) /* (i : duration, j : duration) -> order */  {
  var _x19 = i.hi;
  var _x20 = j.hi;
  var _x18 = $std_num_float64.cmp(_x19, _x20);
  if (_x18 === 2) {
    var _x21 = i.lo;
    var _x22 = j.lo;
    return $std_num_float64.cmp(_x21, _x22);
  }
  else {
    return _x18;
  }
}
 
export function _lp__eq__eq__rp_(i, j) /* (i : duration, j : duration) -> bool */  {
  var _x25 = i.hi;
  var _x26 = j.hi;
  var _x24 = $std_num_float64.cmp(_x25, _x26);
  if (_x24 === 2) {
    var _x27 = i.lo;
    var _x28 = j.lo;
    var _x23 = $std_num_float64.cmp(_x27, _x28);
  }
  else {
    var _x23 = _x24;
  }
  return $std_core_order._lp__eq__eq__rp_(_x23, $std_core_types.Eq);
}
 
export function _lp__lt__rp_(i, j) /* (i : duration, j : duration) -> bool */  {
  var _x31 = i.hi;
  var _x32 = j.hi;
  var _x30 = $std_num_float64.cmp(_x31, _x32);
  if (_x30 === 2) {
    var _x33 = i.lo;
    var _x34 = j.lo;
    var _x29 = $std_num_float64.cmp(_x33, _x34);
  }
  else {
    var _x29 = _x30;
  }
  return $std_core_order._lp__eq__eq__rp_(_x29, $std_core_types.Lt);
}
 
export function _lp__excl__eq__rp_(i, j) /* (i : duration, j : duration) -> bool */  {
  var _x37 = i.hi;
  var _x38 = j.hi;
  var _x36 = $std_num_float64.cmp(_x37, _x38);
  if (_x36 === 2) {
    var _x39 = i.lo;
    var _x40 = j.lo;
    var _x35 = $std_num_float64.cmp(_x39, _x40);
  }
  else {
    var _x35 = _x36;
  }
  return $std_core_order._lp__excl__eq__rp_(_x35, $std_core_types.Eq);
}
 
export function _lp__lt__eq__rp_(i, j) /* (i : duration, j : duration) -> bool */  {
  var _x43 = i.hi;
  var _x44 = j.hi;
  var _x42 = $std_num_float64.cmp(_x43, _x44);
  if (_x42 === 2) {
    var _x45 = i.lo;
    var _x46 = j.lo;
    var _x41 = $std_num_float64.cmp(_x45, _x46);
  }
  else {
    var _x41 = _x42;
  }
  return $std_core_order._lp__excl__eq__rp_(_x41, $std_core_types.Gt);
}
 
export function _lp__gt__rp_(i, j) /* (i : duration, j : duration) -> bool */  {
  var _x49 = i.hi;
  var _x50 = j.hi;
  var _x48 = $std_num_float64.cmp(_x49, _x50);
  if (_x48 === 2) {
    var _x51 = i.lo;
    var _x52 = j.lo;
    var _x47 = $std_num_float64.cmp(_x51, _x52);
  }
  else {
    var _x47 = _x48;
  }
  return $std_core_order._lp__eq__eq__rp_(_x47, $std_core_types.Gt);
}
 
export function _lp__gt__eq__rp_(i, j) /* (i : duration, j : duration) -> bool */  {
  var _x55 = i.hi;
  var _x56 = j.hi;
  var _x54 = $std_num_float64.cmp(_x55, _x56);
  if (_x54 === 2) {
    var _x57 = i.lo;
    var _x58 = j.lo;
    var _x53 = $std_num_float64.cmp(_x57, _x58);
  }
  else {
    var _x53 = _x54;
  }
  return $std_core_order._lp__excl__eq__rp_(_x53, $std_core_types.Lt);
}
 
 
// The minimum of two durations.
export function min(i, j) /* (i : duration, j : duration) -> duration */  {
  var _x62 = i.hi;
  var _x63 = j.hi;
  var _x61 = $std_num_float64.cmp(_x62, _x63);
  if (_x61 === 2) {
    var _x64 = i.lo;
    var _x65 = j.lo;
    var _x60 = $std_num_float64.cmp(_x64, _x65);
  }
  else {
    var _x60 = _x61;
  }
  var _x59 = $std_core_order._lp__excl__eq__rp_(_x60, $std_core_types.Gt);
  return (_x59) ? i : j;
}
 
 
// The maximum of two durations.
export function max(i, j) /* (i : duration, j : duration) -> duration */  {
  var _x69 = i.hi;
  var _x70 = j.hi;
  var _x68 = $std_num_float64.cmp(_x69, _x70);
  if (_x68 === 2) {
    var _x71 = i.lo;
    var _x72 = j.lo;
    var _x67 = $std_num_float64.cmp(_x71, _x72);
  }
  else {
    var _x67 = _x68;
  }
  var _x66 = $std_core_order._lp__excl__eq__rp_(_x67, $std_core_types.Lt);
  return (_x66) ? i : j;
}
 
 
// Add two durations.
export function _lp__plus__rp_(d, e) /* (d : duration, e : duration) -> duration */  {
  var _x73 = d;
  var _x74 = e;
  return $std_num_ddouble._lp__plus__rp_(_x73, _x74);
}
 
 
// Negate a duration.
export function _lp__tilde__rp_(d) /* (d : duration) -> duration */  {
  var _x75 = d.hi;
  var _x76 = d.lo;
  return $std_num_ddouble.Ddouble((-_x75), (-_x76));
}
 
 
// Subtract a duration from a duration.
export function _lp__dash__rp_(d, e) /* (d : duration, e : duration) -> duration */  {
   
  var _x77 = e.hi;
  var _x78 = e.lo;
  var e_0_10038 = $std_num_ddouble.Ddouble((-_x77), (-_x78));
  var _x77 = d;
  var _x78 = e_0_10038;
  return $std_num_ddouble._lp__plus__rp_(_x77, _x78);
}
 
 
// Show a duration in SI seconds.
export function show(d, max_prec) /* (d : duration, max-prec : ? int) -> string */  {
  var _x79 = d;
  var _x80 = (max_prec !== undefined) ? max_prec : 9;
  return $std_core_types._lp__plus__plus__rp_($std_num_ddouble.show_fixed(_x79, $std_core_types._int_negate(($std_core_types._int_abs(_x80)))), "s");
}
 
 
// Create a `:duration` of `n` milli-seconds.
export function int_fs_milli_seconds(n) /* (n : int) -> duration */  {
  return $std_num_ddouble._lp__fs__rp_($std_time_timestamp.int_fs_timespan(n), $std_time_timestamp.int_fs_timespan(1000));
}
 
 
// Create a `:duration` of `n` minutes.
export function minutes(n) /* (n : int) -> duration */  {
   
  var secs_10042 = $std_core_types._int_mul(n,60);
  var _x82 = undefined;
  var _x81 = (_x82 !== undefined) ? _x82 : 0.0;
  return $std_time_timestamp.int_fs_timespan(secs_10042, _x81);
}
 
 
// Create a `:duration` of `n` hours.
export function hours(n) /* (n : int) -> duration */  {
   
  var secs_10044 = $std_core_types._int_mul(n,3600);
  var _x84 = undefined;
  var _x83 = (_x84 !== undefined) ? _x84 : 0.0;
  return $std_time_timestamp.int_fs_timespan(secs_10044, _x83);
}
 
 
// Create a `:duration` of `n` "days" (assuming 86400s in a day).
export function days(n) /* (n : int) -> duration */  {
   
  var secs_10046 = $std_core_types._int_mul(n,86400);
  var _x86 = undefined;
  var _x85 = (_x86 !== undefined) ? _x86 : 0.0;
  return $std_time_timestamp.int_fs_timespan(secs_10046, _x85);
}
 
 
// Create a `:duration` of `n` "weeks" (assuming 7 days of 86400s).
export function weeks(n) /* (n : int) -> duration */  {
   
  var secs_10048 = $std_core_types._int_mul(($std_core_types._int_mul(n,7)),86400);
  var _x88 = undefined;
  var _x87 = (_x88 !== undefined) ? _x88 : 0.0;
  return $std_time_timestamp.int_fs_timespan(secs_10048, _x87);
}
 
 
// Create a `:duration` of `n` "years" (assuming 365 days of 86400s).
export function years(n) /* (n : int) -> duration */  {
   
  var secs_10050 = $std_core_types._int_mul(($std_core_types._int_mul(n,365)),86400);
  var _x90 = undefined;
  var _x89 = (_x90 !== undefined) ? _x90 : 0.0;
  return $std_time_timestamp.int_fs_timespan(secs_10050, _x89);
}