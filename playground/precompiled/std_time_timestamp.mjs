// Koka generated module: std/time/timestamp, koka version: 3.2.4
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
import * as $std_num_int32 from './std_num_int32.mjs';
import * as $std_time_date from './std_time_date.mjs';
 
// externals
 
// type declarations
// type timestamp
export function Timestamp(since, leap32) /* (since : timespan, leap32 : int32) -> timestamp */  {
  return { since: since, leap32: leap32 };
}
 
// declarations
 
 
// A zero-valued timespan.
export var timespan0;
var timespan0 = $std_num_ddouble.zero;
 
export function float64_fs_timespan(secs) /* (secs : float64) -> timespan */  {
  return $std_num_ddouble.Ddouble(secs, 0.0);
}
 
 
// Timespan from a `:ddouble`. Just for convenience as `:timespan` is an alias
export function ddouble_fs_timespan(secs) /* (secs : std/num/ddouble/ddouble) -> timespan */  {
  return secs;
}
 
 
// Seconds in a solar day, 86400.
export var isolar_secs_per_day;
var isolar_secs_per_day = 86400;
 
 
// Automatically generated. Retrieves the `since` constructor field of the `:timestamp` type.
export function timestamp_fs_since(timestamp_0) /* (timestamp : timestamp) -> timespan */  {
  return timestamp_0.since;
}
 
 
// Automatically generated. Retrieves the `leap32` constructor field of the `:timestamp` type.
export function timestamp_fs_leap32(timestamp_0) /* (timestamp : timestamp) -> int32 */  {
  return timestamp_0.leap32;
}
 
export function timestamp_fs__copy(_this, since, leap32) /* (timestamp, since : ? timespan, leap32 : ? int32) -> timestamp */  {
  if (since !== undefined) {
    var _x0 = since;
  }
  else {
    var _x0 = _this.since;
  }
  if (leap32 !== undefined) {
    var _x1 = leap32;
  }
  else {
    var _x1 = _this.leap32;
  }
  return Timestamp(_x0, _x1);
}
 
export function leap(t) /* (t : timestamp) -> int */  {
  var _x2 = t.leap32;
  return $std_core_types._int_from_int32(_x2);
}
 
 
// Compare two `:timestamp`s.
export function cmp(i, j) /* (i : timestamp, j : timestamp) -> order */  {
  var _x5 = i.since.hi;
  var _x6 = j.since.hi;
  var _x4 = $std_num_float64.cmp(_x5, _x6);
  if (_x4 === 2) {
    var _x7 = i.since.lo;
    var _x8 = j.since.lo;
    var _x3 = $std_num_float64.cmp(_x7, _x8);
  }
  else {
    var _x3 = _x4;
  }
  if (_x3 === 2) {
     
    var _x9 = i.leap32;
    var x_0_10008 = $std_core_types._int_from_int32(_x9);
     
    var _x10 = j.leap32;
    var y_0_10009 = $std_core_types._int_from_int32(_x10);
    if ($std_core_types._int_eq(x_0_10008,y_0_10009)) {
      return $std_core_types.Eq;
    }
    else {
      return ($std_core_types._int_gt(x_0_10008,y_0_10009)) ? $std_core_types.Gt : $std_core_types.Lt;
    }
  }
  else {
    return _x3;
  }
}
 
export function _lp__eq__eq__rp_(i, j) /* (i : timestamp, j : timestamp) -> bool */  {
  return $std_core_order._lp__eq__eq__rp_(cmp(i, j), $std_core_types.Eq);
}
 
export function _lp__lt__rp_(i, j) /* (i : timestamp, j : timestamp) -> bool */  {
  return $std_core_order._lp__eq__eq__rp_(cmp(i, j), $std_core_types.Lt);
}
 
export function _lp__excl__eq__rp_(i, j) /* (i : timestamp, j : timestamp) -> bool */  {
  return $std_core_order._lp__excl__eq__rp_(cmp(i, j), $std_core_types.Eq);
}
 
export function _lp__lt__eq__rp_(i, j) /* (i : timestamp, j : timestamp) -> bool */  {
  return $std_core_order._lp__excl__eq__rp_(cmp(i, j), $std_core_types.Gt);
}
 
export function _lp__gt__rp_(i, j) /* (i : timestamp, j : timestamp) -> bool */  {
  return $std_core_order._lp__eq__eq__rp_(cmp(i, j), $std_core_types.Gt);
}
 
export function _lp__gt__eq__rp_(i, j) /* (i : timestamp, j : timestamp) -> bool */  {
  return $std_core_order._lp__excl__eq__rp_(cmp(i, j), $std_core_types.Lt);
}
 
 
// Add a time span to a time stamp.
export function _lp__plus__rp_(ts, t) /* (ts : timestamp, t : timespan) -> timestamp */  {
  var _x9 = ts.since;
  var _x10 = ts.leap32;
  return Timestamp($std_num_ddouble._lp__plus__rp_(_x9, t), _x10);
}
 
export function int_fs_timespan(seconds, frac) /* (seconds : int, frac : ? float64) -> timespan */  {
  var _x12 = (frac !== undefined) ? frac : 0.0;
  var _x11 = (_x12 === (0.0));
  if (_x11) {
    return $std_num_ddouble.ddouble_int_exp(seconds, 0);
  }
  else {
    var _x13 = (frac !== undefined) ? frac : 0.0;
    return $std_num_ddouble._lp__plus__rp_($std_num_ddouble.ddouble_int_exp(seconds, 0), $std_num_ddouble.Ddouble(_x13, 0.0));
  }
}
 
export function tuple64_fs_timespan(secs, frac) /* (secs : float64, frac : float64) -> timespan */  {
  if ((frac === (0.0))) {
    return $std_num_ddouble.Ddouble(secs, 0.0);
  }
  else {
    return $std_num_ddouble.dsum(secs, frac);
  }
}
 
 
// Divide using `div` to allow for different timespan representations
export function div(x, y, prec) /* (x : timespan, y : timespan, prec : ? int) -> timespan */  {
  return $std_num_ddouble._lp__fs__rp_(x, y);
}
 
export var solar_secs_per_day;
var _x16 = undefined;
var _x15 = (_x16 !== undefined) ? _x16 : 0.0;
var _x14 = (_x15 === (0.0));
if (_x14) {
  var solar_secs_per_day = $std_num_ddouble.ddouble_int_exp(86400, 0);
}
else {
  var _x18 = undefined;
  var _x17 = (_x18 !== undefined) ? _x18 : 0.0;
  var solar_secs_per_day = $std_num_ddouble._lp__plus__rp_($std_num_ddouble.ddouble_int_exp(86400, 0), $std_num_ddouble.Ddouble(_x17, 0.0));
}
 
 
// The time stamp at 2000-01-01
export var timestamp0;
var timestamp0 = Timestamp($std_num_ddouble.zero, $std_num_int32.zero);
 
 
// Create a time stamp from a `:timespan` since 2000-01-01 and possible leap seconds.
export function timestamp(t, leap_0) /* (t : timespan, leap : ? int) -> timestamp */  {
  var _x19 = (leap_0 !== undefined) ? leap_0 : 0;
  return Timestamp(t, $std_core_types._int_clamp32(_x19));
}
 
 
// Create a time stamp from an integer timespan since 2000-01-01 and possible leap seconds.
export function int_fs_timestamp(t, frac, leap_0) /* (t : int, frac : ? float64, leap : ? int) -> timestamp */  {
   
  var _x20 = (frac !== undefined) ? frac : 0.0;
  var t_0_10022 = $std_num_ddouble._lp__plus__rp_($std_num_ddouble.ddouble_int_exp(t, 0), $std_num_ddouble.Ddouble(_x20, 0.0));
  var _x20 = (leap_0 !== undefined) ? leap_0 : 0;
  return Timestamp(t_0_10022, $std_core_types._int_clamp32(_x20));
}
 
 
// Timestamp from days, seconds into the day and possible leap second.
export function timestamp_days(days_0, secs, leap_0) /* (days : int, secs : ? timespan, leap : ? int) -> timestamp */  {
   
  var seconds_10084 = $std_core_types._int_mul(days_0,86400);
   
  var _x24 = undefined;
  var _x23 = (_x24 !== undefined) ? _x24 : 0.0;
  var _x22 = (_x23 === (0.0));
  if (_x22) {
    var _x21 = $std_num_ddouble.ddouble_int_exp(seconds_10084, 0);
  }
  else {
    var _x26 = undefined;
    var _x25 = (_x26 !== undefined) ? _x26 : 0.0;
    var _x21 = $std_num_ddouble._lp__plus__rp_($std_num_ddouble.ddouble_int_exp(seconds_10084, 0), $std_num_ddouble.Ddouble(_x25, 0.0));
  }
  var _x27 = (secs !== undefined) ? secs : $std_num_ddouble.zero;
  var t_10026 = $std_num_ddouble._lp__plus__rp_(_x21, _x27);
  var _x21 = (leap_0 !== undefined) ? leap_0 : 0;
  return Timestamp(t_10026, $std_core_types._int_clamp32(_x21));
}
 
 
// Add `days` days to a timestamp
export function add_days(ts, days_0) /* (ts : timestamp, days : int) -> timestamp */  {
   
  var seconds_10086 = $std_core_types._int_mul(days_0,86400);
   
  var _x24 = undefined;
  var _x23 = (_x24 !== undefined) ? _x24 : 0.0;
  var _x22 = (_x23 === (0.0));
  if (_x22) {
    var t_10029 = $std_num_ddouble.ddouble_int_exp(seconds_10086, 0);
  }
  else {
    var _x26 = undefined;
    var _x25 = (_x26 !== undefined) ? _x26 : 0.0;
    var t_10029 = $std_num_ddouble._lp__plus__rp_($std_num_ddouble.ddouble_int_exp(seconds_10086, 0), $std_num_ddouble.Ddouble(_x25, 0.0));
  }
  var _x22 = ts.since;
  var _x23 = ts.leap32;
  return Timestamp($std_num_ddouble._lp__plus__rp_(_x22, t_10029), _x23);
}
 
 
// Subtract a time span from a time stamp.
export function _lp__dash__rp_(ts, t) /* (ts : timestamp, t : timespan) -> timestamp */  {
  var _x24 = ts.since;
  var _x25 = t.hi;
  var _x26 = t.lo;
  var _x27 = ts.leap32;
  return Timestamp($std_num_ddouble._lp__plus__rp_(_x24, $std_num_ddouble.Ddouble((-_x25), (-_x26))), _x27);
}
 
 
// Return days and seconds into the day, disregarding leap seconds.
export function days_seconds(ts) /* (ts : timestamp) -> (int, std/num/ddouble/ddouble) */  {
   
  var _x28 = ts.since;
  var secs = $std_num_ddouble.floor(_x28);
   
  var _x29 = ts.since;
  var _x30 = secs.hi;
  var _x31 = secs.lo;
  var frac = $std_num_ddouble._lp__plus__rp_(_x29, $std_num_ddouble.Ddouble((-_x30), (-_x31)));
  var _x28 = $std_core_int.divmod($std_num_ddouble.int(secs), 86400);
  return $std_core_types.Tuple2(_x28.fst, $std_num_ddouble._lp__plus__rp_($std_num_ddouble.ddouble_int_exp(_x28.snd, 0), frac));
}
 
 
// Return days and clock into the day, handling possible leap seconds.
export function days_clock(ts) /* (ts : timestamp) -> (int, std/time/date/clock) */  {
  var _x29 = days_seconds(ts);
  var _x30 = ts.leap32;
  return $std_core_types.Tuple2(_x29.fst, $std_time_date.leap_fs_clock(_x29.snd, $std_core_types._int_from_int32(_x30)));
}
 
export function days(ts) /* (ts : timestamp) -> int */  {
   
  var tuple2_10043 = days_seconds(ts);
  return tuple2_10043.fst;
}
 
export function seconds_into_day(ts) /* (ts : timestamp) -> std/num/ddouble/ddouble */  {
   
  var tuple2_10044 = days_seconds(ts);
   
  var _x31 = ts.leap32;
  var i_10045 = $std_core_types._int_from_int32(_x31);
  var _x31 = tuple2_10044.snd;
  return $std_num_ddouble._lp__plus__rp_(_x31, $std_num_ddouble.ddouble_int_exp(i_10045, 0));
}
 
 
// The time span since 2000-01-01 including time inside a possible leap second.
export function unsafe_timespan_withleap(ts) /* (ts : timestamp) -> timespan */  {
   
  var _x32 = ts.leap32;
  var seconds_10088 = $std_core_types._int_from_int32(_x32);
  var _x32 = ts.since;
  var _x36 = undefined;
  var _x35 = (_x36 !== undefined) ? _x36 : 0.0;
  var _x34 = (_x35 === (0.0));
  if (_x34) {
    var _x33 = $std_num_ddouble.ddouble_int_exp(seconds_10088, 0);
  }
  else {
    var _x38 = undefined;
    var _x37 = (_x38 !== undefined) ? _x38 : 0.0;
    var _x33 = $std_num_ddouble._lp__plus__rp_($std_num_ddouble.ddouble_int_exp(seconds_10088, 0), $std_num_ddouble.Ddouble(_x37, 0.0));
  }
  return $std_num_ddouble._lp__plus__rp_(_x32, _x33);
}
 
 
// The time span since 2000-01-01 for time scales that do not have
// leap seconds and where every day is 86400s. For time scales
// with leap seconds, this effectively ignores any leap seconds.
export function timespan_noleap(ts) /* (ts : timestamp) -> timespan */  {
  return ts.since;
}
 
 
// Round a time stamp to a certain precision (`prec` is number of digits of the fraction of the second).
export function round_to_prec(t, prec) /* (t : timestamp, prec : int) -> timestamp */  {
  var _x39 = t.since;
  var _x40 = t.leap32;
  return Timestamp($std_num_ddouble.round_to_prec(_x39, prec), _x40);
}
 
 
// Add `leaps` leap seconds to the timestamp.
export function add_leap_seconds(ts, leaps) /* (ts : timestamp, leaps : timespan) -> timestamp */  {
   
  var _x41 = leaps.hi;
  var b_10054 = (_x41 > (0.0));
  if (b_10054) {
     
    var _x43 = undefined;
    var _x42 = (_x43 !== undefined) ? _x43 : 0.0;
    var _x41 = (_x42 === (0.0));
    if (_x41) {
      var y_10057 = $std_num_ddouble.ddouble_int_exp(1, 0);
    }
    else {
      var _x45 = undefined;
      var _x44 = (_x45 !== undefined) ? _x45 : 0.0;
      var y_10057 = $std_num_ddouble._lp__plus__rp_($std_num_ddouble.ddouble_int_exp(1, 0), $std_num_ddouble.Ddouble(_x44, 0.0));
    }
    var _x44 = leaps.hi;
    var _x45 = y_10057.hi;
    var _x43 = $std_num_float64.cmp(_x44, _x45);
    if (_x43 === 2) {
      var _x46 = leaps.lo;
      var _x47 = y_10057.lo;
      var _x42 = $std_num_float64.cmp(_x46, _x47);
    }
    else {
      var _x42 = _x43;
    }
    var _x41 = $std_core_order._lp__eq__eq__rp_(_x42, $std_core_types.Lt);
    if (_x41) {
      var _x49 = ts.leap32;
      var _x48 = $std_core_types._int_iszero(($std_core_types._int_from_int32(_x49)));
      if (_x48) {
         
        var _x52 = undefined;
        var _x51 = (_x52 !== undefined) ? _x52 : 0.0;
        var _x50 = (_x51 === (0.0));
        if (_x50) {
          var y_0_10061 = $std_num_ddouble.ddouble_int_exp(1, 0);
        }
        else {
          var _x54 = undefined;
          var _x53 = (_x54 !== undefined) ? _x54 : 0.0;
          var y_0_10061 = $std_num_ddouble._lp__plus__rp_($std_num_ddouble.ddouble_int_exp(1, 0), $std_num_ddouble.Ddouble(_x53, 0.0));
        }
        var _x50 = ts.since;
        var _x51 = y_0_10061.hi;
        var _x52 = y_0_10061.lo;
        return Timestamp($std_num_ddouble._lp__plus__rp_($std_num_ddouble._lp__plus__rp_(_x50, $std_num_ddouble.Ddouble((-_x51), (-_x52))), leaps), 1);
      }
      else {
        var _x53 = ts.since;
        var _x54 = ts.leap32;
        var _x57 = leaps.hi;
        var _x56 = (_x57 < (0.0));
        if (_x56) {
          var _x55 = $std_num_ddouble.ceiling(leaps);
        }
        else {
          var _x55 = $std_num_ddouble.floor(leaps);
        }
        return Timestamp($std_num_ddouble._lp__plus__rp_(_x53, $std_num_ddouble.fraction(leaps)), ((_x54 + ($std_core_types._int_clamp32(($std_num_ddouble.int(_x55)))))|0));
      }
    }
    else {
      var _x58 = ts.since;
      var _x59 = ts.leap32;
      var _x62 = leaps.hi;
      var _x61 = (_x62 < (0.0));
      if (_x61) {
        var _x60 = $std_num_ddouble.ceiling(leaps);
      }
      else {
        var _x60 = $std_num_ddouble.floor(leaps);
      }
      return Timestamp($std_num_ddouble._lp__plus__rp_(_x58, $std_num_ddouble.fraction(leaps)), ((_x59 + ($std_core_types._int_clamp32(($std_num_ddouble.int(_x60)))))|0));
    }
  }
  else {
    return ts;
  }
}
 
 
// The minimum of two timestamps.
export function min(i, j) /* (i : timestamp, j : timestamp) -> timestamp */  {
  var _x63 = $std_core_order._lp__excl__eq__rp_(cmp(i, j), $std_core_types.Gt);
  return (_x63) ? i : j;
}
 
 
// The maximum of two timestamps.
export function max(i, j) /* (i : timestamp, j : timestamp) -> timestamp */  {
  var _x64 = $std_core_order._lp__excl__eq__rp_(cmp(i, j), $std_core_types.Lt);
  return (_x64) ? i : j;
}
 
 
// Show a time stamp.
export function ts_show(ts, max_prec, secs_width, unit) /* (ts : timestamp, max-prec : ? int, secs-width : ? int, unit : ? string) -> string */  {
   
  var _x66 = ts.leap32;
  var _x65 = $std_core_types._int_iszero(($std_core_types._int_from_int32(_x66)));
  if (_x65) {
    var l = "";
  }
  else {
    var _x67 = ts.leap32;
    var l = $std_core_types._lp__plus__plus__rp_(" (+", $std_core_types._lp__plus__plus__rp_($std_core_int.show($std_core_types._int_from_int32(_x67)), " leap)"));
  }
  var _x65 = ts.since;
  var _x66 = (max_prec !== undefined) ? max_prec : 9;
  var _x67 = (secs_width !== undefined) ? secs_width : 1;
  var _x68 = (unit !== undefined) ? unit : "";
  return $std_core_types._lp__plus__plus__rp_($std_time_date.show_seconds(_x65, _x66, _x67, _x68), l);
}
 
 
// Show a day stamp
export function ts_show_days(ts, prec) /* (ts : timestamp, prec : ? int) -> string */  {
  var _x69 = days_seconds(ts);
   
  var _x70 = ts.leap32;
  var leap_0_10079 = $std_core_types._int_from_int32(_x70);
   
  var _x71 = (leap_0_10079 !== undefined) ? leap_0_10079 : 0;
  var _arg_x2306 = Timestamp(_x69.snd, $std_core_types._int_clamp32(_x71));
  var _x70 = (prec !== undefined) ? prec : 9;
  return $std_core_types._lp__plus__plus__rp_($std_core_int.show(_x69.fst), $std_core_types._lp__plus__plus__rp_("d ", ts_show(_arg_x2306, _x70, undefined, "s")));
}