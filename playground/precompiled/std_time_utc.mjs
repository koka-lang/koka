// Koka generated module: std/time/utc, koka version: 3.2.4
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
import * as $std_text_parse from './std_text_parse.mjs';
import * as $std_time_timestamp from './std_time_timestamp.mjs';
import * as $std_time_duration from './std_time_duration.mjs';
import * as $std_time_instant from './std_time_instant.mjs';
import * as $std_num_int32 from './std_num_int32.mjs';
 
// externals
 
// type declarations
 
 
// runtime tag for the effect `:utc`
export var utc_fs__tag;
var utc_fs__tag = "utc@utc";
// type leap-adjust
export function Leap_adjust(utc_start, offset, drift_start, drift) /* (utc-start : utc-timestamp, offset : std/time/timestamp/timespan, drift-start : utc-timestamp, drift : std/num/ddouble/ddouble) -> leap-adjust */  {
  return { utc_start: utc_start, offset: offset, drift_start: drift_start, drift: drift };
}
// type leaps-table
export function Leaps_table(expire, adjusts) /* (expire : std/time/instant/instant, adjusts : list<leap-adjust>) -> leaps-table */  {
  return { expire: expire, adjusts: adjusts };
}
// type utc
export function _Hnd_utc(_cfc, _fun_utc) /* forall<e,a> (int, hnd/clause0<std/time/instant/timescale,utc,e,a>) -> utc<e,a> */  {
  return { _cfc: _cfc, _fun_utc: _fun_utc };
}
 
// declarations
 
 
// Automatically generated. Retrieves the `@cfc` constructor field of the `:utc` type.
export function utc_fs__cfc(utc_0) /* forall<e,a> (utc : utc<e,a>) -> int */  {
  return utc_0._cfc;
}
 
 
// select `utc` operation out of effect `:utc`
export function utc_fs__select(hnd) /* forall<e,a> (hnd : utc<e,a>) -> hnd/clause0<std/time/instant/timescale,utc,e,a> */  {
  return hnd._fun_utc;
}
 
 
// Call the `fun utc` operation of the effect `:utc`
export function utc() /* () -> utc std/time/instant/timescale */  {
   
  var ev_10345 = $std_core_hnd._evv_at(0);
  return ev_10345.hnd._fun_utc(ev_10345.marker, ev_10345);
}
 
 
// Automatically generated. Retrieves the `@fun-utc` constructor field of the `:utc` type.
export function utc_fs__fun_utc(utc_0) /* forall<e,a> (utc : utc<e,a>) -> hnd/clause0<std/time/instant/timescale,utc,e,a> */  {
  return utc_0._fun_utc;
}
 
 
// handler for the effect `:utc`
export function utc_fs__handle(hnd, ret, action) /* forall<a,e,b> (hnd : utc<e,b>, ret : (res : a) -> e b, action : () -> <utc|e> a) -> e b */  {
  return $std_core_hnd._hhandle(utc_fs__tag, hnd, ret, action);
}
 
 
// Automatically generated. Retrieves the `expire` constructor field of the `:leaps-table` type.
export function leaps_table_fs_expire(_this) /* (leaps-table) -> std/time/instant/instant */  {
  return _this.expire;
}
 
 
// Automatically generated. Retrieves the `adjusts` constructor field of the `:leaps-table` type.
export function leaps_table_fs_adjusts(_this) /* (leaps-table) -> list<leap-adjust> */  {
  return _this.adjusts;
}
 
export function leaps_table_fs__copy(_this, expire, adjusts) /* (leaps-table, expire : ? std/time/instant/instant, adjusts : ? (list<leap-adjust>)) -> leaps-table */  {
  if (expire !== undefined) {
    var _x0 = expire;
  }
  else {
    var _x0 = _this.expire;
  }
  if (adjusts !== undefined) {
    var _x1 = adjusts;
  }
  else {
    var _x1 = _this.adjusts;
  }
  return Leaps_table(_x0, _x1);
}
 
 
// Leap second adjustments. For an instant `i` after `start`:\
// ``TAI-offset = offset + (drift * days(i - drift-start))``
export function _create_Leap_adjust(utc_start, offset, drift_start, drift) /* (utc-start : utc-timestamp, offset : std/time/timestamp/timespan, drift-start : ? utc-timestamp, drift : ? std/num/ddouble/ddouble) -> leap-adjust */  {
  var _x2 = (drift_start !== undefined) ? drift_start : $std_time_timestamp.timestamp0;
  var _x3 = (drift !== undefined) ? drift : $std_num_ddouble.zero;
  return Leap_adjust(utc_start, offset, _x2, _x3);
}
 
export var zero;
var zero = Leap_adjust($std_time_timestamp.timestamp0, $std_num_ddouble.zero, $std_time_timestamp.timestamp0, $std_num_ddouble.zero);
 
 
// Automatically generated. Retrieves the `utc-start` constructor field of the `:leap-adjust` type.
export function leap_adjust_fs_utc_start(_this) /* (leap-adjust) -> utc-timestamp */  {
  return _this.utc_start;
}
 
 
// Automatically generated. Retrieves the `offset` constructor field of the `:leap-adjust` type.
export function leap_adjust_fs_offset(_this) /* (leap-adjust) -> std/time/timestamp/timespan */  {
  return _this.offset;
}
 
 
// Automatically generated. Retrieves the `drift-start` constructor field of the `:leap-adjust` type.
export function leap_adjust_fs_drift_start(_this) /* (leap-adjust) -> utc-timestamp */  {
  return _this.drift_start;
}
 
 
// Automatically generated. Retrieves the `drift` constructor field of the `:leap-adjust` type.
export function leap_adjust_fs_drift(_this) /* (leap-adjust) -> std/num/ddouble/ddouble */  {
  return _this.drift;
}
 
export function leap_adjust_fs__copy(_this, utc_start, offset, drift_start, drift) /* (leap-adjust, utc-start : ? utc-timestamp, offset : ? std/time/timestamp/timespan, drift-start : ? utc-timestamp, drift : ? std/num/ddouble/ddouble) -> leap-adjust */  {
  if (utc_start !== undefined) {
    var _x4 = utc_start;
  }
  else {
    var _x4 = _this.utc_start;
  }
  if (offset !== undefined) {
    var _x5 = offset;
  }
  else {
    var _x5 = _this.offset;
  }
  if (drift_start !== undefined) {
    var _x6 = drift_start;
  }
  else {
    var _x6 = _this.drift_start;
  }
  if (drift !== undefined) {
    var _x7 = drift;
  }
  else {
    var _x7 = _this.drift;
  }
  return Leap_adjust(_x4, _x5, _x6, _x7);
}
 
export function is_zero(la) /* (la : leap-adjust) -> bool */  {
  var _x9 = la.offset.hi;
  var _x8 = (_x9 === (0.0));
  if (_x8) {
    var _x10 = la.drift.hi;
    return (_x10 === (0.0));
  }
  else {
    return false;
  }
}
 
export var ntp2000;
var ntp2000 = $std_time_timestamp.int_fs_timespan(3155673600);
 
export var utc1958;
var utc1958 = $std_time_timestamp.int_fs_timestamp(-1325376000);
 
export function find_leap_adjust(utc_0, leaps) /* (utc : utc-timestamp, leaps : list<leap-adjust>) -> leap-adjust */  { tailcall: while(1)
{
  if (leaps === null) {
    return zero;
  }
  else {
    var _x12 = leaps.head.utc_start;
    var _x11 = $std_core_order._lp__eq__eq__rp_($std_time_timestamp.cmp(_x12, utc_0), $std_core_types.Gt);
    if (_x11) {
      {
        // tail call
        leaps = leaps.tail;
        continue tailcall;
      }
    }
    else {
      return leaps.head;
    }
  }
}}
 
export function utc_to_leap_adjust(leaps, utc_0) /* (leaps : leaps-table, utc : utc-timestamp) -> leap-adjust */  {
  var _x13 = $std_core_order._lp__eq__eq__rp_($std_time_timestamp.cmp(utc_0, utc1958), $std_core_types.Lt);
  if (_x13) {
    return zero;
  }
  else {
    var _x14 = leaps.adjusts;
    return find_leap_adjust(utc_0, _x14);
  }
}
 
 
// Get the leap-second adjustment _delta-tai_ (= TAI - UTC).
// Needs the timestamp to handle _drift_.
export function delta_tai(la, utc_0) /* (la : leap-adjust, utc : utc-timestamp) -> std/time/timestamp/timespan */  {
  var _x16 = la.drift.hi;
  var _x15 = (_x16 === (0.0));
  if (_x15) {
    return la.offset;
  }
  else {
     
    var _x17 = utc_0.since;
    var _x18 = la.drift_start.since.hi;
    var _x19 = la.drift_start.since.lo;
    var days = $std_num_ddouble._lp__fs__rp_($std_num_ddouble._lp__plus__rp_(_x17, $std_num_ddouble.Ddouble((-_x18), (-_x19))), $std_time_timestamp.solar_secs_per_day);
    var _x17 = la.offset;
    var _x18 = la.drift;
    return $std_num_ddouble._lp__plus__rp_(_x17, $std_num_ddouble._lp__star__rp_(_x18, days));
  }
}
 
export function utc_to_delta_tai(leaps, utc_0) /* (leaps : leaps-table, utc : utc-timestamp) -> std/time/timestamp/timespan */  {
  var _x20 = $std_core_order._lp__eq__eq__rp_($std_time_timestamp.cmp(utc_0, utc1958), $std_core_types.Lt);
  if (_x20) {
    var _x19 = zero;
  }
  else {
    var _x21 = leaps.adjusts;
    var _x19 = find_leap_adjust(utc_0, _x21);
  }
  return delta_tai(_x19, utc_0);
}
 
export function utc_to_tai(leaps, utc_0) /* (leaps : leaps-table, utc : std/time/timestamp/timestamp) -> std/time/duration/duration */  {
   
  var _x23 = $std_core_order._lp__eq__eq__rp_($std_time_timestamp.cmp(utc_0, utc1958), $std_core_types.Lt);
  if (_x23) {
    var _x22 = zero;
  }
  else {
    var _x24 = leaps.adjusts;
    var _x22 = find_leap_adjust(utc_0, _x24);
  }
  var dtai = delta_tai(_x22, utc_0);
   
  var _x25 = utc_0.since;
  var _x26 = utc_0.leap32;
  var t_10032 = $std_time_timestamp.Timestamp($std_num_ddouble._lp__plus__rp_(_x25, dtai), _x26);
  return $std_time_timestamp.unsafe_timespan_withleap(t_10032);
}
 
 
/* Converting TAI time back to UTC is not straightforward as
we need to estimate UTC at first as the leap table goes from UTC-to-TAI.
Moreover, we need to detect if we crossed over a leap second, or are
inside a leap step. Take for example the leap second of 2006-01-01
to +33. This looks like:
UTC-to-TAI-delta:                        ... +32   |   +33 ...
UTC timestamp          189388799   189388799+1  189388800
UTC 2005-12-31T23:59:     59          60   leap    00
                 ---------|-----------|xxxxxxxxxxxx|-------------
                          |           |            |
TAI 2006-01-01T00:00:     31          32           33
TAI timestamp:         189388831   189388832    189388833
In the code below, suppose `tai` is `189388832.5`.
Then we estimate at first the delta `dtai0` to +33, so our
estimate `utc0` is `189388799.5` (just before the leap step!).
We then use `utc0` to get delta-TAI at that time, +32 and
set the difference `diff` to `(33-32) == 1` -- the time of the
leap second we crossed. (usually, `diff` is zero of course).
If the difference is positive, we then check if `utc0` is in the
leap period itself (instead of before it): that is the case if
the delta-TAI at `utc0+diff` equals `dtai0` again.
If we are not in a leap second, the final utc time is the
estimate plus the time of the leap period if we crossed over it, `utc0+diff`.
Otherwise, the same holds but we need to add `diff` as leap seconds,
in the example ending up as `189388799.5+1`.
*/
export function utc_from_tai(leaps, tai_since) /* (leaps : leaps-table, tai-since : std/time/duration/duration) -> std/time/timestamp/timestamp */  {
   
  var _x22 = tai_since;
  var _x24 = undefined;
  var _x23 = (_x24 !== undefined) ? _x24 : 0;
  var tai = $std_time_timestamp.Timestamp(_x22, $std_core_types._int_clamp32(_x23));
   
  var _x26 = $std_core_order._lp__eq__eq__rp_($std_time_timestamp.cmp(tai, utc1958), $std_core_types.Lt);
  if (_x26) {
    var _x25 = zero;
  }
  else {
    var _x27 = leaps.adjusts;
    var _x25 = find_leap_adjust(tai, _x27);
  }
  var dtai0 = delta_tai(_x25, tai);
   
  var utc0 = $std_time_timestamp._lp__dash__rp_(tai, dtai0);
   
  var _x29 = $std_core_order._lp__eq__eq__rp_($std_time_timestamp.cmp(utc0, utc1958), $std_core_types.Lt);
  if (_x29) {
    var _x28 = zero;
  }
  else {
    var _x30 = leaps.adjusts;
    var _x28 = find_leap_adjust(utc0, _x30);
  }
  var dtai1 = delta_tai(_x28, utc0);
   
  var _x31 = dtai1.hi;
  var _x32 = dtai1.lo;
  var diff = $std_num_ddouble._lp__plus__rp_(dtai0, $std_num_ddouble.Ddouble((-_x31), (-_x32)));
   
  var x_0_10042 = $std_num_ddouble.round_to_prec(diff, 3);
   
  var _x33 = x_0_10042.hi;
  var hasgap = (_x33 > (0.0));
   
  if (hasgap) {
     
    var _x34 = utc0.since;
    var _x35 = utc0.leap32;
    var utc_0_1_10046 = $std_time_timestamp.Timestamp($std_num_ddouble._lp__plus__rp_(_x34, diff), _x35);
     
    var _x37 = $std_core_order._lp__eq__eq__rp_($std_time_timestamp.cmp(utc_0_1_10046, utc1958), $std_core_types.Lt);
    if (_x37) {
      var _x36 = zero;
    }
    else {
      var _x38 = leaps.adjusts;
      var _x36 = find_leap_adjust(utc_0_1_10046, _x38);
    }
    var x_1_10043 = delta_tai(_x36, utc_0_1_10046);
    var _x36 = x_1_10043.hi;
    var _x37 = dtai1.hi;
    var _x35 = $std_num_float64.cmp(_x36, _x37);
    if (_x35 === 2) {
      var _x38 = x_1_10043.lo;
      var _x39 = dtai1.lo;
      var _x34 = $std_num_float64.cmp(_x38, _x39);
    }
    else {
      var _x34 = _x35;
    }
    var inleap = $std_core_order._lp__excl__eq__rp_(_x34, $std_core_types.Eq);
  }
  else {
    var inleap = false;
  }
  if (inleap) {
    return $std_time_timestamp.add_leap_seconds(utc0, diff);
  }
  else {
    var _x22 = utc0.since;
    var _x23 = utc0.leap32;
    return $std_time_timestamp.Timestamp($std_num_ddouble._lp__plus__rp_(_x22, diff), _x23);
  }
}
 
 
// Return `Just(start,diff)` if a leap second occurred in the given day
// (`days` after 2000-01-01) with the start time and leap second gap (`diff`)
export function utc_leap_in_day(leaps, days) /* (leaps : leaps-table, days : int) -> maybe<(std/time/timestamp/timestamp, std/time/timestamp/timespan)> */  {
   
  var utc0 = $std_time_timestamp.timestamp_days(days);
   
  var utc1 = $std_time_timestamp.timestamp_days($std_core_types._int_add(days,1));
   
  var _x24 = $std_core_order._lp__eq__eq__rp_($std_time_timestamp.cmp(utc1, utc1958), $std_core_types.Lt);
  if (_x24) {
    var la1 = zero;
  }
  else {
    var _x25 = leaps.adjusts;
    var la1 = find_leap_adjust(utc1, _x25);
  }
  var _x25 = la1.utc_start;
  var _x24 = $std_core_order._lp__eq__eq__rp_($std_time_timestamp.cmp(_x25, utc0), $std_core_types.Lt);
  if (_x24) {
    return $std_core_types.Nothing;
  }
  else {
     
    var _x27 = $std_core_order._lp__eq__eq__rp_($std_time_timestamp.cmp(utc0, utc1958), $std_core_types.Lt);
    if (_x27) {
      var _x26 = zero;
    }
    else {
      var _x28 = leaps.adjusts;
      var _x26 = find_leap_adjust(utc0, _x28);
    }
    var dtai0 = delta_tai(_x26, utc0);
     
    var dtai1 = delta_tai(la1, utc1);
     
    var _x29 = dtai0.hi;
    var _x30 = dtai0.lo;
    var diff = $std_num_ddouble.round_to_prec($std_num_ddouble._lp__plus__rp_(dtai1, $std_num_ddouble.Ddouble((-_x29), (-_x30))), 3);
    var _x26 = la1.utc_start;
    return $std_core_types.Just($std_core_types.Tuple2(_x26, diff));
  }
}
 
 
// The UTC seconds in a day
export function utc_seconds_in_day(leaps, utc_0) /* (leaps : leaps-table, utc : std/time/timestamp/timestamp) -> std/time/timestamp/timespan */  {
   
  var tuple2_10043 = $std_time_timestamp.days_seconds(utc_0);
  var _x28 = tuple2_10043.fst;
  var _x27 = utc_leap_in_day(leaps, _x28);
  if (_x27 === null) {
    return $std_time_timestamp.solar_secs_per_day;
  }
  else {
    return $std_num_ddouble._lp__plus__rp_($std_time_timestamp.solar_secs_per_day, _x27.value.snd);
  }
}
 
 
// Return the modified julian day since 2000-01-01 taking leap seconds into
// account that happen any time during the day
export function utc_to_mjd(leaps, utc_0, tzdelta) /* (leaps : leaps-table, utc : utc-timestamp, tzdelta : std/time/timestamp/timespan) -> std/num/ddouble/ddouble */  {
  var _x30 = utc_0.since;
  var _x31 = utc_0.leap32;
  var _x29 = $std_time_timestamp.days_seconds($std_time_timestamp.Timestamp($std_num_ddouble._lp__plus__rp_(_x30, tzdelta), _x31));
   
  var tuple2_10043 = $std_time_timestamp.days_seconds(utc_0);
   
  var _x33 = tuple2_10043.fst;
  var _x32 = utc_leap_in_day(leaps, _x33);
  if (_x32 === null) {
     
    var _x34 = utc_0.leap32;
    var i_10066 = $std_core_types._int_from_int32(_x34);
    var frac = $std_num_ddouble._lp__fs__rp_($std_num_ddouble._lp__plus__rp_(_x29.snd, $std_num_ddouble.ddouble_int_exp(i_10066, 0)), $std_time_timestamp.solar_secs_per_day);
  }
  else {
     
    var secs_in_day = $std_num_ddouble._lp__plus__rp_($std_time_timestamp.solar_secs_per_day, _x32.value.snd);
     
    var _x34 = $std_core_order._lp__eq__eq__rp_($std_time_timestamp.cmp(utc_0, _x32.value.fst), $std_core_types.Lt);
    if (_x34) {
       
      var _x35 = utc_0.leap32;
      var i_1_10070 = $std_core_types._int_from_int32(_x35);
      var secs = $std_num_ddouble._lp__plus__rp_(_x29.snd, $std_num_ddouble.ddouble_int_exp(i_1_10070, 0));
    }
    else {
      var secs = $std_num_ddouble._lp__plus__rp_(_x29.snd, _x32.value.snd);
    }
    var frac = $std_num_ddouble._lp__fs__rp_(secs, secs_in_day);
  }
  return $std_num_ddouble._lp__plus__rp_($std_num_ddouble.ddouble_int_exp(_x29.fst, 0), frac);
}
 
 
// Return UTC from the modified julian day since 2000-01-01 taking leap seconds into
// account that happen any time during the day
export function utc_from_mjd(leaps, days, frac) /* (leaps : leaps-table, days : int, frac : std/num/ddouble/ddouble) -> utc-timestamp */  {
  var _x32 = utc_leap_in_day(leaps, days);
  if (_x32 === null) {
    return $std_time_timestamp.timestamp_days(days, $std_num_ddouble._lp__star__rp_(frac, $std_time_timestamp.solar_secs_per_day));
  }
  else {
     
    var secs = $std_num_ddouble._lp__star__rp_(frac, $std_num_ddouble._lp__plus__rp_($std_time_timestamp.solar_secs_per_day, _x32.value.snd));
     
    var utc_0 = $std_time_timestamp.timestamp_days(days, secs);
    var _x33 = $std_core_order._lp__eq__eq__rp_($std_time_timestamp.cmp(utc_0, _x32.value.fst), $std_core_types.Lt);
    if (_x33) {
      return utc_0;
    }
    else {
       
      var _x34 = _x32.value.fst.since;
      var _x35 = _x32.value.fst.leap32;
      var j_0_10076 = $std_time_timestamp.Timestamp($std_num_ddouble._lp__plus__rp_(_x34, _x32.value.snd), _x35);
      var _x34 = $std_core_order._lp__eq__eq__rp_($std_time_timestamp.cmp(utc_0, j_0_10076), $std_core_types.Gt);
      if (_x34) {
        return $std_time_timestamp._lp__dash__rp_(utc_0, _x32.value.snd);
      }
      else {
        return $std_time_timestamp.add_leap_seconds($std_time_timestamp._lp__dash__rp_(utc_0, _x32.value.snd), _x32.value.snd);
      }
    }
  }
}
 
 
// Create a new time scale based on UTC seconds with a given `name`
// and a leap second table.
export function utc_timescale(name, leaps) /* (name : string, leaps : leaps-table) -> std/time/instant/timescale */  {
  return $std_time_instant.timescale(name, function(tai /* std/time/duration/duration */ ) {
      return utc_from_tai(leaps, tai);
    }, function(utc_0 /* std/time/timestamp/timestamp */ ) {
      return utc_to_tai(leaps, utc_0);
    }, "UTC", $std_core_types.Just(function(utc_0_0 /* std/time/timestamp/timestamp */ ) {
       
      var tuple2_10043 = $std_time_timestamp.days_seconds(utc_0_0);
      var _x36 = tuple2_10043.fst;
      var _x35 = utc_leap_in_day(leaps, _x36);
      if (_x35 === null) {
        return $std_time_timestamp.solar_secs_per_day;
      }
      else {
        return $std_num_ddouble._lp__plus__rp_($std_time_timestamp.solar_secs_per_day, _x35.value.snd);
      }
    }), $std_core_types.Just(function(utc_1 /* std/time/timestamp/timestamp */ , tzdelta /* std/time/timestamp/timespan */ ) {
      return utc_to_mjd(leaps, utc_1, tzdelta);
    }), $std_core_types.Just(function(days /* int */ , frac /* std/num/ddouble/ddouble */ ) {
      return utc_from_mjd(leaps, days, frac);
    }));
}
 
export function upto(lt, end) /* (lt : leaps-table, end : utc-timestamp) -> leaps-table */  {
   
  var _x37 = lt.adjusts;
  var _arg_x1670 = $std_core_list.drop_while(_x37, function(la /* leap-adjust */ ) {
      var _x38 = la.utc_start;
      return $std_core_order._lp__eq__eq__rp_($std_time_timestamp.cmp(_x38, end), $std_core_types.Gt);
    });
  var _x38 = undefined;
  if (_x38 !== undefined) {
    var _x37 = _x38;
  }
  else {
    var _x37 = lt.expire;
  }
  if (_arg_x1670 !== undefined) {
    var _x39 = _arg_x1670;
  }
  else {
    var _x39 = lt.adjusts;
  }
  return Leaps_table(_x37, _x39);
}
 
export function extend(leap1, leap2) /* (leap1 : leaps-table, leap2 : leaps-table) -> leaps-table */  {
  var _x41 = leap1.adjusts;
  var _x40 = $std_core_list.reverse_acc($std_core_types.Nil, _x41);
  if (_x40 === null) {
    return leap2;
  }
  else {
     
    var _x42 = _x40.head.utc_start;
    var end_10222 = $std_time_timestamp._lp__dash__rp_(_x42, $std_time_timestamp.int_fs_timespan(1));
     
    var _x43 = leap2.adjusts;
    var _arg_x1670 = $std_core_list.drop_while(_x43, function(la_0 /* leap-adjust */ ) {
        var _x44 = la_0.utc_start;
        return $std_core_order._lp__eq__eq__rp_($std_time_timestamp.cmp(_x44, end_10222), $std_core_types.Gt);
      });
     
    var _x45 = leap1.adjusts;
    var _x48 = undefined;
    if (_x48 !== undefined) {
      var _x47 = _x48;
    }
    else {
      var _x47 = leap2.expire;
    }
    if (_arg_x1670 !== undefined) {
      var _x49 = _arg_x1670;
    }
    else {
      var _x49 = leap2.adjusts;
    }
    var _x46 = _x49;
    var _arg_x1793 = $std_core_list.append(_x45, _x46);
    var _x43 = undefined;
    if (_x43 !== undefined) {
      var _x42 = _x43;
    }
    else {
      var _x42 = leap1.expire;
    }
    if (_arg_x1793 !== undefined) {
      var _x44 = _arg_x1793;
    }
    else {
      var _x44 = leap1.adjusts;
    }
    return Leaps_table(_x42, _x44);
  }
}
 
 
// IERS leap second data valid until 2024-12-28
export var default_iers_leap_seconds;
var default_iers_leap_seconds = "\n  # From: https://hpiers.obspm.fr/iers/bul/bulc/ntp/leap-seconds.list\n  #\tUpdated through IERS Bulletin C (https://hpiers.obspm.fr/iers/bul/bulc/bulletinc.dat)\n  # File expires on:  28 December 2024\n  #\n  #@\t3944332800\n  #\n  2272060800  10  # 1 Jan 1972\n  2287785600  11  # 1 Jul 1972\n  2303683200  12  # 1 Jan 1973\n  2335219200  13  # 1 Jan 1974\n  2366755200  14  # 1 Jan 1975\n  2398291200  15  # 1 Jan 1976\n  2429913600  16  # 1 Jan 1977\n  2461449600  17  # 1 Jan 1978\n  2492985600  18  # 1 Jan 1979\n  2524521600  19  # 1 Jan 1980\n  2571782400  20  # 1 Jul 1981\n  2603318400  21  # 1 Jul 1982\n  2634854400  22  # 1 Jul 1983\n  2698012800  23  # 1 Jul 1985\n  2776982400  24  # 1 Jan 1988\n  2840140800  25  # 1 Jan 1990\n  2871676800  26  # 1 Jan 1991\n  2918937600  27  # 1 Jul 1992\n  2950473600  28  # 1 Jul 1993\n  2982009600  29  # 1 Jul 1994\n  3029443200  30  # 1 Jan 1996\n  3076704000  31  # 1 Jul 1997\n  3124137600  32  # 1 Jan 1999\n  3345062400  33  # 1 Jan 2006\n  3439756800  34  # 1 Jan 2009\n  3550089600  35  # 1 Jul 2012\n  3644697600  36  # 1 Jul 2015\n  3692217600  37  # 1 Jan 2017\n  // 3723753600  35  # 1 Jan 2018";
 
export var jd_epoch_shift;
var jd_epoch_shift = $std_num_ddouble.Ddouble(2400000.5, 0.0);
 
 
// JD to MJD
export var mjd_epoch_shift;
var mjd_epoch_shift = $std_time_timestamp.int_fs_timespan(51544);
 
 
// monadic lift
export function _mlift_pexpire_10316(ntpex) /* (ntpex : std/num/ddouble/ddouble) -> std/text/parse/parse std/time/timestamp/timestamp */  {
   
  var _x_x1_10280 = $std_core_hnd._open_none2(function(x /* std/num/ddouble/ddouble */ , y /* std/num/ddouble/ddouble */ ) {
      var _x45 = y.hi;
      var _x46 = y.lo;
      return $std_num_ddouble._lp__plus__rp_(x, $std_num_ddouble.Ddouble((-_x45), (-_x46)));
    }, ntpex, ntp2000);
  return $std_core_hnd._open_none2(function(t /* std/time/timestamp/timespan */ , leap_0 /* ? int */ ) {
      var _x45 = (leap_0 !== undefined) ? leap_0 : 0;
      return $std_time_timestamp.Timestamp(t, $std_core_types._int_clamp32(_x45));
    }, _x_x1_10280);
}
 
 
// monadic lift
export function _mlift_pexpire_10317(wild___1) /* (wild_@1 : string) -> std/text/parse/parse std/time/timestamp/timestamp */  {
   
  var x_10348 = $std_text_parse._lp__bar__bar__rp_($std_num_ddouble.pddouble_sum, $std_num_ddouble.pddouble_normal);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_pexpire_10316);
  }
  else {
    return _mlift_pexpire_10316(x_10348);
  }
}
 
 
// monadic lift
export function _mlift_pexpire_10318(wild___0) /* (wild_@0 : string) -> std/text/parse/parse std/time/timestamp/timestamp */  {
   
  var x_10350 = $std_text_parse.whitespace0();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_pexpire_10317);
  }
  else {
    return _mlift_pexpire_10317(x_10350);
  }
}
 
 
// monadic lift
export function _mlift_pexpire_10319(wild__) /* (wild_ : string) -> std/text/parse/parse std/time/timestamp/timestamp */  {
   
  var x_10352 = $std_text_parse.pstring("#@");
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_pexpire_10318);
  }
  else {
    return _mlift_pexpire_10318(x_10352);
  }
}
 
export function pexpire() /* () -> std/text/parse/parse std/time/timestamp/timestamp */  {
   
  var x_10354 = $std_text_parse.whitespace0();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_pexpire_10319);
  }
  else {
     
    var x_0_10357 = $std_text_parse.pstring("#@");
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(_mlift_pexpire_10318);
    }
    else {
       
      var x_1_10360 = $std_text_parse.whitespace0();
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(_mlift_pexpire_10317);
      }
      else {
         
        var x_2_10363 = $std_text_parse._lp__bar__bar__rp_($std_num_ddouble.pddouble_sum, $std_num_ddouble.pddouble_normal);
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(_mlift_pexpire_10316);
        }
        else {
           
          var _x_x1_10280 = $std_core_hnd._open_none2(function(x_3 /* std/num/ddouble/ddouble */ , y /* std/num/ddouble/ddouble */ ) {
              var _x46 = y.hi;
              var _x47 = y.lo;
              return $std_num_ddouble._lp__plus__rp_(x_3, $std_num_ddouble.Ddouble((-_x46), (-_x47)));
            }, x_2_10363, ntp2000);
          return $std_core_hnd._open_none2(function(t /* std/time/timestamp/timespan */ , leap_0 /* ? int */ ) {
              var _x46 = (leap_0 !== undefined) ? leap_0 : 0;
              return $std_time_timestamp.Timestamp(t, $std_core_types._int_clamp32(_x46));
            }, _x_x1_10280);
        }
      }
    }
  }
}
 
 
// val rxexpire = regex(r"^[ \t]*#@[ \t]*(\d+)[ \t]*(?:#.*)?$", multiLine=True)
export function parse_expire(line) /* (line : string) -> maybe<std/time/timestamp/timestamp> */  {
   
  var maybe_10101 = $std_core_sslice.starts_with($std_core_sslice.trim_left(line, " "), "#@");
  if (maybe_10101 !== null) {
     
    var perr_10102 = $std_text_parse.parse($std_core_sslice.Sslice(line, 0, line.length), pexpire);
    if (perr_10102._tag === 1) {
      return $std_core_types.Just(perr_10102.result);
    }
    else {
      return $std_core_types.Nothing;
    }
  }
  else {
    return $std_core_types.Nothing;
  }
}
 
export function parse_leap_expire(leaps, adjusts) /* (leaps : string, adjusts : list<leap-adjust>) -> std/time/instant/instant */  {
   
  var v_10011 = ((leaps).split(("\n")));
   
  var utc_expires = $std_core_list.flatmap_maybe($std_core_vector.vlist(v_10011), parse_expire);
   
  var t_10112 = $std_time_timestamp.int_fs_timespan($std_core_types._int_mul(182,86400));
   
  if (adjusts !== null) {
    var _x48 = adjusts.head;
  }
  else {
    var _x49 = $std_core_types.Nothing;
    var _x48 = (_x49 === null) ? zero : _x49.value;
  }
  var _x47 = _x48.utc_start.since;
  if (adjusts !== null) {
    var _x51 = adjusts.head;
  }
  else {
    var _x52 = $std_core_types.Nothing;
    var _x51 = (_x52 === null) ? zero : _x52.value;
  }
  var _x50 = _x51.utc_start.leap32;
  var nothing_0_10109 = $std_time_timestamp.Timestamp($std_num_ddouble._lp__plus__rp_(_x47, t_10112), _x50);
   
  if (utc_expires !== null) {
    var _x53 = utc_expires.head;
  }
  else {
    var _x54 = $std_core_types.Nothing;
    var _x53 = (_x54 === null) ? nothing_0_10109 : _x54.value;
  }
  var ts_1_10116 = $std_time_timestamp._lp__dash__rp_(_x53, ntp2000);
   
  var _x55 = ts_1_10116.since;
  if (adjusts !== null) {
    var _x57 = adjusts.head;
  }
  else {
    var _x58 = $std_core_types.Nothing;
    var _x57 = (_x58 === null) ? zero : _x58.value;
  }
  var _x56 = _x57.offset;
  var _x59 = ts_1_10116.leap32;
  var t_0_10115 = $std_time_timestamp.Timestamp($std_num_ddouble._lp__plus__rp_(_x55, _x56), _x59);
  return $std_time_instant.Instant(t_0_10115, $std_time_instant.ts_tai);
}
 
 
// monadic lift
export function _mlift_ptaiadjust_10320(dmjd, drift, mjd, ofs, wild___10) /* (dmjd : std/num/ddouble/ddouble, drift : std/num/ddouble/ddouble, mjd : std/num/ddouble/ddouble, ofs : std/num/ddouble/ddouble, wild_@10 : string) -> std/text/parse/parse leap-adjust */  {
   
  var _x_x1_2_10289 = $std_core_hnd._open_none2(function(x_0 /* std/num/ddouble/ddouble */ , y_0 /* std/num/ddouble/ddouble */ ) {
      var _x47 = y_0.hi;
      var _x48 = y_0.lo;
      return $std_num_ddouble._lp__plus__rp_(x_0, $std_num_ddouble.Ddouble((-_x47), (-_x48)));
    }, mjd, mjd_epoch_shift);
   
  var _x_x1_1_10288 = $std_core_hnd._open_none2($std_num_ddouble._lp__star__rp_, _x_x1_2_10289, $std_time_timestamp.solar_secs_per_day);
   
  var _x_x1_0_10286 = $std_core_hnd._open_none1($std_num_ddouble.round, _x_x1_1_10288);
   
  var start = $std_core_hnd._open_none2(function(t /* std/time/timestamp/timespan */ , leap_0 /* ? int */ ) {
      var _x49 = (leap_0 !== undefined) ? leap_0 : 0;
      return $std_time_timestamp.Timestamp(t, $std_core_types._int_clamp32(_x49));
    }, _x_x1_0_10286);
   
  var _x_x1_6_10296 = $std_core_hnd._open_none2(function(x_1 /* std/num/ddouble/ddouble */ , y_1 /* std/num/ddouble/ddouble */ ) {
      var _x50 = y_1.hi;
      var _x51 = y_1.lo;
      return $std_num_ddouble._lp__plus__rp_(x_1, $std_num_ddouble.Ddouble((-_x50), (-_x51)));
    }, dmjd, mjd_epoch_shift);
   
  var _x_x1_5_10295 = $std_core_hnd._open_none2($std_num_ddouble._lp__star__rp_, _x_x1_6_10296, $std_time_timestamp.solar_secs_per_day);
   
  var _x_x1_4_10293 = $std_core_hnd._open_none1($std_num_ddouble.round, _x_x1_5_10295);
   
  var dstart = $std_core_hnd._open_none2(function(t_0 /* std/time/timestamp/timespan */ , leap_0_0 /* ? int */ ) {
      var _x52 = (leap_0_0 !== undefined) ? leap_0_0 : 0;
      return $std_time_timestamp.Timestamp(t_0, $std_core_types._int_clamp32(_x52));
    }, _x_x1_4_10293);
  return $std_core_hnd._open_none4(function(utc_start /* utc-timestamp */ , offset /* std/time/timestamp/timespan */ , drift_start /* ? utc-timestamp */ , drift_0 /* ? std/num/ddouble/ddouble */ ) {
      var _x47 = (drift_start !== undefined) ? drift_start : $std_time_timestamp.timestamp0;
      var _x48 = (drift_0 !== undefined) ? drift_0 : $std_num_ddouble.zero;
      return Leap_adjust(utc_start, offset, _x47, _x48);
    }, start, ofs, dstart, drift);
}
 
 
// monadic lift
export function _mlift_ptaiadjust_10321(dmjd, drift, mjd, ofs, wild___9) /* (dmjd : std/num/ddouble/ddouble, drift : std/num/ddouble/ddouble, mjd : std/num/ddouble/ddouble, ofs : std/num/ddouble/ddouble, wild_@9 : string) -> std/text/parse/parse leap-adjust */  {
   
  var x_10366 = $std_text_parse.whitespace0();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild___10 /* string */ ) {
      return _mlift_ptaiadjust_10320(dmjd, drift, mjd, ofs, wild___10);
    });
  }
  else {
    return _mlift_ptaiadjust_10320(dmjd, drift, mjd, ofs, x_10366);
  }
}
 
 
// monadic lift
export function _mlift_ptaiadjust_10322(dmjd, drift, mjd, ofs, wild___8) /* (dmjd : std/num/ddouble/ddouble, drift : std/num/ddouble/ddouble, mjd : std/num/ddouble/ddouble, ofs : std/num/ddouble/ddouble, wild_@8 : string) -> std/text/parse/parse leap-adjust */  {
   
  var x_10368 = $std_text_parse.pstring("S");
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild___9 /* string */ ) {
      return _mlift_ptaiadjust_10321(dmjd, drift, mjd, ofs, wild___9);
    });
  }
  else {
    return _mlift_ptaiadjust_10321(dmjd, drift, mjd, ofs, x_10368);
  }
}
 
 
// monadic lift
export function _mlift_ptaiadjust_10323(dmjd, mjd, ofs, drift) /* (dmjd : std/num/ddouble/ddouble, mjd : std/num/ddouble/ddouble, ofs : std/num/ddouble/ddouble, drift : std/num/ddouble/ddouble) -> std/text/parse/parse leap-adjust */  {
   
  var x_10370 = $std_text_parse.whitespace0();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild___8 /* string */ ) {
      return _mlift_ptaiadjust_10322(dmjd, drift, mjd, ofs, wild___8);
    });
  }
  else {
    return _mlift_ptaiadjust_10322(dmjd, drift, mjd, ofs, x_10370);
  }
}
 
 
// monadic lift
export function _mlift_ptaiadjust_10324(dmjd, mjd, ofs, _y_x10249) /* (dmjd : std/num/ddouble/ddouble, mjd : std/num/ddouble/ddouble, ofs : std/num/ddouble/ddouble, list<char>) -> std/text/parse/parse leap-adjust */  {
   
  var x_10372 = $std_text_parse._lp__bar__bar__rp_($std_num_ddouble.pddouble_sum, $std_num_ddouble.pddouble_normal);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(drift /* std/num/ddouble/ddouble */ ) {
      return _mlift_ptaiadjust_10323(dmjd, mjd, ofs, drift);
    });
  }
  else {
    return _mlift_ptaiadjust_10323(dmjd, mjd, ofs, x_10372);
  }
}
 
 
// monadic lift
export function _mlift_ptaiadjust_10325(dmjd, mjd, ofs, _y_x10248) /* (dmjd : std/num/ddouble/ddouble, mjd : std/num/ddouble/ddouble, ofs : std/num/ddouble/ddouble, char) -> std/text/parse/parse leap-adjust */  {
   
  var x_10374 = $std_text_parse.many_acc($std_text_parse.no_digit, $std_core_types.Nil);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10249 /* list<char> */ ) {
      return _mlift_ptaiadjust_10324(dmjd, mjd, ofs, _y_x10249);
    });
  }
  else {
    return _mlift_ptaiadjust_10324(dmjd, mjd, ofs, x_10374);
  }
}
 
 
// monadic lift
export function _mlift_ptaiadjust_10326(mjd, ofs, dmjd) /* (mjd : std/num/ddouble/ddouble, ofs : std/num/ddouble/ddouble, dmjd : std/num/ddouble/ddouble) -> std/text/parse/parse leap-adjust */  {
   
  var x_10376 = $std_text_parse.no_digit();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10248 /* char */ ) {
      return _mlift_ptaiadjust_10325(dmjd, mjd, ofs, _y_x10248);
    });
  }
  else {
    return _mlift_ptaiadjust_10325(dmjd, mjd, ofs, x_10376);
  }
}
 
 
// monadic lift
export function _mlift_ptaiadjust_10327(mjd, ofs, _y_x10246) /* (mjd : std/num/ddouble/ddouble, ofs : std/num/ddouble/ddouble, list<char>) -> std/text/parse/parse leap-adjust */  {
   
  var x_10378 = $std_text_parse._lp__bar__bar__rp_($std_num_ddouble.pddouble_sum, $std_num_ddouble.pddouble_normal);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(dmjd /* std/num/ddouble/ddouble */ ) {
      return _mlift_ptaiadjust_10326(mjd, ofs, dmjd);
    });
  }
  else {
    return _mlift_ptaiadjust_10326(mjd, ofs, x_10378);
  }
}
 
 
// monadic lift
export function _mlift_ptaiadjust_10328(mjd, ofs, _y_x10245) /* (mjd : std/num/ddouble/ddouble, ofs : std/num/ddouble/ddouble, char) -> std/text/parse/parse leap-adjust */  {
   
  var x_10380 = $std_text_parse.many_acc($std_text_parse.no_digit, $std_core_types.Nil);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10246 /* list<char> */ ) {
      return _mlift_ptaiadjust_10327(mjd, ofs, _y_x10246);
    });
  }
  else {
    return _mlift_ptaiadjust_10327(mjd, ofs, x_10380);
  }
}
 
 
// monadic lift
export function _mlift_ptaiadjust_10329(mjd, ofs) /* (mjd : std/num/ddouble/ddouble, ofs : std/num/ddouble/ddouble) -> std/text/parse/parse leap-adjust */  {
   
  var x_10382 = $std_text_parse.no_digit();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10245 /* char */ ) {
      return _mlift_ptaiadjust_10328(mjd, ofs, _y_x10245);
    });
  }
  else {
    return _mlift_ptaiadjust_10328(mjd, ofs, x_10382);
  }
}
 
 
// monadic lift
export function _mlift_ptaiadjust_10330(mjd, wild___5) /* (mjd : std/num/ddouble/ddouble, wild_@5 : string) -> std/text/parse/parse leap-adjust */  {
   
  var x_10384 = $std_text_parse._lp__bar__bar__rp_($std_num_ddouble.pddouble_sum, $std_num_ddouble.pddouble_normal);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(ofs /* std/num/ddouble/ddouble */ ) {
      return _mlift_ptaiadjust_10329(mjd, ofs);
    });
  }
  else {
    return _mlift_ptaiadjust_10329(mjd, x_10384);
  }
}
 
 
// monadic lift
export function _mlift_ptaiadjust_10331(mjd, wild___4) /* (mjd : std/num/ddouble/ddouble, wild_@4 : string) -> std/text/parse/parse leap-adjust */  {
   
  var x_10386 = $std_text_parse.whitespace0();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild___5 /* string */ ) {
      return _mlift_ptaiadjust_10330(mjd, wild___5);
    });
  }
  else {
    return _mlift_ptaiadjust_10330(mjd, x_10386);
  }
}
 
 
// monadic lift
export function _mlift_ptaiadjust_10332(mjd, wild___3) /* (mjd : std/num/ddouble/ddouble, wild_@3 : string) -> std/text/parse/parse leap-adjust */  {
   
  var x_10388 = $std_text_parse.pstring("TAI-UTC=");
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild___4 /* string */ ) {
      return _mlift_ptaiadjust_10331(mjd, wild___4);
    });
  }
  else {
    return _mlift_ptaiadjust_10331(mjd, x_10388);
  }
}
 
 
// monadic lift
export function _mlift_ptaiadjust_10333(_y_x10240) /* (std/num/ddouble/ddouble) -> std/text/parse/parse leap-adjust */  {
   
  var mjd = $std_core_hnd._open_none2(function(x /* std/num/ddouble/ddouble */ , y /* std/num/ddouble/ddouble */ ) {
      var _x49 = y.hi;
      var _x50 = y.lo;
      return $std_num_ddouble._lp__plus__rp_(x, $std_num_ddouble.Ddouble((-_x49), (-_x50)));
    }, _y_x10240, jd_epoch_shift);
   
  var x_0_10390 = $std_text_parse.whitespace0();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild___3 /* string */ ) {
      return _mlift_ptaiadjust_10332(mjd, wild___3);
    });
  }
  else {
    return _mlift_ptaiadjust_10332(mjd, x_0_10390);
  }
}
 
 
// monadic lift
export function _mlift_ptaiadjust_10334(wild___2) /* (wild_@2 : string) -> std/text/parse/parse leap-adjust */  {
   
  var x_10392 = $std_text_parse._lp__bar__bar__rp_($std_num_ddouble.pddouble_sum, $std_num_ddouble.pddouble_normal);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_ptaiadjust_10333);
  }
  else {
    return _mlift_ptaiadjust_10333(x_10392);
  }
}
 
 
// monadic lift
export function _mlift_ptaiadjust_10335(wild___1) /* (wild_@1 : string) -> std/text/parse/parse leap-adjust */  {
   
  var x_10394 = $std_text_parse.whitespace0();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_ptaiadjust_10334);
  }
  else {
    return _mlift_ptaiadjust_10334(x_10394);
  }
}
 
 
// monadic lift
export function _mlift_ptaiadjust_10336(_y_x10237) /* (list<char>) -> std/text/parse/parse leap-adjust */  {
   
  var x_10396 = $std_text_parse.pstring("=JD");
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_ptaiadjust_10335);
  }
  else {
    return _mlift_ptaiadjust_10335(x_10396);
  }
}
 
 
// monadic lift
export function _mlift_ptaiadjust_10337(_y_x10235) /* (char) -> std/text/parse/parse leap-adjust */  {
   
  var x_10398 = $std_text_parse.many_acc(function() {
      return $std_text_parse.none_of("=");
    }, $std_core_types.Nil);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_ptaiadjust_10336);
  }
  else {
    return _mlift_ptaiadjust_10336(x_10398);
  }
}
 
 
// monadic lift
export function _mlift_ptaiadjust_10338(wild__) /* (wild_ : string) -> std/text/parse/parse leap-adjust */  {
   
  var x_10400 = $std_text_parse.none_of("=");
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_ptaiadjust_10337);
  }
  else {
    return _mlift_ptaiadjust_10337(x_10400);
  }
}
 
export function ptaiadjust() /* () -> std/text/parse/parse leap-adjust */  {
   
  var x_10402 = $std_text_parse.whitespace0();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_ptaiadjust_10338);
  }
  else {
     
    var x_0_10405 = $std_text_parse.none_of("=");
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(_mlift_ptaiadjust_10337);
    }
    else {
       
      var x_1_10408 = $std_text_parse.many_acc(function() {
          return $std_text_parse.none_of("=");
        }, $std_core_types.Nil);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(_mlift_ptaiadjust_10336);
      }
      else {
         
        var x_2_10411 = $std_text_parse.pstring("=JD");
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(_mlift_ptaiadjust_10335);
        }
        else {
           
          var x_3_10414 = $std_text_parse.whitespace0();
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(_mlift_ptaiadjust_10334);
          }
          else {
             
            var x_4_10417 = $std_text_parse._lp__bar__bar__rp_($std_num_ddouble.pddouble_sum, $std_num_ddouble.pddouble_normal);
            if ($std_core_hnd._yielding()) {
              return $std_core_hnd.yield_extend(_mlift_ptaiadjust_10333);
            }
            else {
               
              var mjd = $std_core_hnd._open_none2(function(x_5 /* std/num/ddouble/ddouble */ , y /* std/num/ddouble/ddouble */ ) {
                  var _x49 = y.hi;
                  var _x50 = y.lo;
                  return $std_num_ddouble._lp__plus__rp_(x_5, $std_num_ddouble.Ddouble((-_x49), (-_x50)));
                }, x_4_10417, jd_epoch_shift);
               
              var x_6_10420 = $std_text_parse.whitespace0();
              if ($std_core_hnd._yielding()) {
                return $std_core_hnd.yield_extend(function(wild___3 /* string */ ) {
                  return _mlift_ptaiadjust_10332(mjd, wild___3);
                });
              }
              else {
                 
                var x_7_10423 = $std_text_parse.pstring("TAI-UTC=");
                if ($std_core_hnd._yielding()) {
                  return $std_core_hnd.yield_extend(function(wild___4 /* string */ ) {
                    return _mlift_ptaiadjust_10331(mjd, wild___4);
                  });
                }
                else {
                   
                  var x_8_10426 = $std_text_parse.whitespace0();
                  if ($std_core_hnd._yielding()) {
                    return $std_core_hnd.yield_extend(function(wild___5 /* string */ ) {
                      return _mlift_ptaiadjust_10330(mjd, wild___5);
                    });
                  }
                  else {
                     
                    var x_9_10429 = $std_text_parse._lp__bar__bar__rp_($std_num_ddouble.pddouble_sum, $std_num_ddouble.pddouble_normal);
                    if ($std_core_hnd._yielding()) {
                      return $std_core_hnd.yield_extend(function(ofs /* std/num/ddouble/ddouble */ ) {
                        return _mlift_ptaiadjust_10329(mjd, ofs);
                      });
                    }
                    else {
                       
                      var x_10_10432 = $std_text_parse.no_digit();
                      if ($std_core_hnd._yielding()) {
                        return $std_core_hnd.yield_extend(function(_y_x10245 /* char */ ) {
                          return _mlift_ptaiadjust_10328(mjd, x_9_10429, _y_x10245);
                        });
                      }
                      else {
                         
                        var x_11_10435 = $std_text_parse.many_acc($std_text_parse.no_digit, $std_core_types.Nil);
                        if ($std_core_hnd._yielding()) {
                          return $std_core_hnd.yield_extend(function(_y_x10246 /* list<char> */ ) {
                            return _mlift_ptaiadjust_10327(mjd, x_9_10429, _y_x10246);
                          });
                        }
                        else {
                           
                          var x_12_10438 = $std_text_parse._lp__bar__bar__rp_($std_num_ddouble.pddouble_sum, $std_num_ddouble.pddouble_normal);
                          if ($std_core_hnd._yielding()) {
                            return $std_core_hnd.yield_extend(function(dmjd /* std/num/ddouble/ddouble */ ) {
                              return _mlift_ptaiadjust_10326(mjd, x_9_10429, dmjd);
                            });
                          }
                          else {
                             
                            var x_13_10441 = $std_text_parse.no_digit();
                            if ($std_core_hnd._yielding()) {
                              return $std_core_hnd.yield_extend(function(_y_x10248 /* char */ ) {
                                return _mlift_ptaiadjust_10325(x_12_10438, mjd, x_9_10429, _y_x10248);
                              });
                            }
                            else {
                               
                              var x_14_10444 = $std_text_parse.many_acc($std_text_parse.no_digit, $std_core_types.Nil);
                              if ($std_core_hnd._yielding()) {
                                return $std_core_hnd.yield_extend(function(_y_x10249 /* list<char> */ ) {
                                  return _mlift_ptaiadjust_10324(x_12_10438, mjd, x_9_10429, _y_x10249);
                                });
                              }
                              else {
                                 
                                var x_15_10447 = $std_text_parse._lp__bar__bar__rp_($std_num_ddouble.pddouble_sum, $std_num_ddouble.pddouble_normal);
                                if ($std_core_hnd._yielding()) {
                                  return $std_core_hnd.yield_extend(function(drift /* std/num/ddouble/ddouble */ ) {
                                    return _mlift_ptaiadjust_10323(x_12_10438, mjd, x_9_10429, drift);
                                  });
                                }
                                else {
                                   
                                  var x_16_10450 = $std_text_parse.whitespace0();
                                  if ($std_core_hnd._yielding()) {
                                    return $std_core_hnd.yield_extend(function(wild___8 /* string */ ) {
                                      return _mlift_ptaiadjust_10322(x_12_10438, x_15_10447, mjd, x_9_10429, wild___8);
                                    });
                                  }
                                  else {
                                     
                                    var x_17_10453 = $std_text_parse.pstring("S");
                                    if ($std_core_hnd._yielding()) {
                                      return $std_core_hnd.yield_extend(function(wild___9 /* string */ ) {
                                        return _mlift_ptaiadjust_10321(x_12_10438, x_15_10447, mjd, x_9_10429, wild___9);
                                      });
                                    }
                                    else {
                                       
                                      var x_18_10456 = $std_text_parse.whitespace0();
                                      if ($std_core_hnd._yielding()) {
                                        return $std_core_hnd.yield_extend(function(wild___10 /* string */ ) {
                                          return _mlift_ptaiadjust_10320(x_12_10438, x_15_10447, mjd, x_9_10429, wild___10);
                                        });
                                      }
                                      else {
                                         
                                        var _x_x1_2_10289 = $std_core_hnd._open_none2(function(x_0_0 /* std/num/ddouble/ddouble */ , y_0 /* std/num/ddouble/ddouble */ ) {
                                            var _x49 = y_0.hi;
                                            var _x50 = y_0.lo;
                                            return $std_num_ddouble._lp__plus__rp_(x_0_0, $std_num_ddouble.Ddouble((-_x49), (-_x50)));
                                          }, mjd, mjd_epoch_shift);
                                         
                                        var _x_x1_1_10288 = $std_core_hnd._open_none2($std_num_ddouble._lp__star__rp_, _x_x1_2_10289, $std_time_timestamp.solar_secs_per_day);
                                         
                                        var _x_x1_0_10286 = $std_core_hnd._open_none1($std_num_ddouble.round, _x_x1_1_10288);
                                         
                                        var start = $std_core_hnd._open_none2(function(t /* std/time/timestamp/timespan */ , leap_0 /* ? int */ ) {
                                            var _x51 = (leap_0 !== undefined) ? leap_0 : 0;
                                            return $std_time_timestamp.Timestamp(t, $std_core_types._int_clamp32(_x51));
                                          }, _x_x1_0_10286);
                                         
                                        var _x_x1_6_10296 = $std_core_hnd._open_none2(function(x_1_0 /* std/num/ddouble/ddouble */ , y_1 /* std/num/ddouble/ddouble */ ) {
                                            var _x52 = y_1.hi;
                                            var _x53 = y_1.lo;
                                            return $std_num_ddouble._lp__plus__rp_(x_1_0, $std_num_ddouble.Ddouble((-_x52), (-_x53)));
                                          }, x_12_10438, mjd_epoch_shift);
                                         
                                        var _x_x1_5_10295 = $std_core_hnd._open_none2($std_num_ddouble._lp__star__rp_, _x_x1_6_10296, $std_time_timestamp.solar_secs_per_day);
                                         
                                        var _x_x1_4_10293 = $std_core_hnd._open_none1($std_num_ddouble.round, _x_x1_5_10295);
                                         
                                        var dstart = $std_core_hnd._open_none2(function(t_0 /* std/time/timestamp/timespan */ , leap_0_0 /* ? int */ ) {
                                            var _x54 = (leap_0_0 !== undefined) ? leap_0_0 : 0;
                                            return $std_time_timestamp.Timestamp(t_0, $std_core_types._int_clamp32(_x54));
                                          }, _x_x1_4_10293);
                                        return $std_core_hnd._open_none4(function(utc_start /* utc-timestamp */ , offset /* std/time/timestamp/timespan */ , drift_start /* ? utc-timestamp */ , drift_0_0 /* ? std/num/ddouble/ddouble */ ) {
                                            var _x49 = (drift_start !== undefined) ? drift_start : $std_time_timestamp.timestamp0;
                                            var _x50 = (drift_0_0 !== undefined) ? drift_0_0 : $std_num_ddouble.zero;
                                            return Leap_adjust(utc_start, offset, _x49, _x50);
                                          }, start, x_9_10429, dstart, x_15_10447);
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
 
 
// monadic lift
export function _mlift_parse_taiadjust_10339(x, wild__) /* (x : leap-adjust, wild_ : ()) -> std/text/parse/parse leap-adjust */  {
  return x;
}
 
 
// monadic lift
export function _mlift_parse_taiadjust_10340(x) /* (x : leap-adjust) -> std/text/parse/parse leap-adjust */  {
   
  var x_0_10459 = $std_text_parse.eof();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild__ /* () */ ) {
      return x;
    });
  }
  else {
    return x;
  }
}
 
export function parse_taiadjust(line) /* (line : string) -> maybe<leap-adjust> */  {
   
  var input_10123 = $std_core_sslice.Sslice(line, 0, line.length);
   
  var perr_10122 = $std_text_parse.parse(input_10123, function() {
       
      var x_10463 = ptaiadjust();
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(_mlift_parse_taiadjust_10340);
      }
      else {
        return _mlift_parse_taiadjust_10340(x_10463);
      }
    });
  if (perr_10122._tag === 1) {
    return $std_core_types.Just(perr_10122.result);
  }
  else {
    return $std_core_types.Nothing;
  }
}
 
 
// Parse the standard UTC leap second adjustment file in the "old" .dat format as
// in <https://maia.usno.navy.mil/ser7/tai-utc.dat>, where entries have the shape
// ````
// 1961 JAN  1 =JD 2437300.5  TAI-UTC=   1.4228180 S + (MJD - 37300.) X 0.001296 S
// ````
// which specifies the start time (`JD 2437300.5`), new TAI-UTC offset
// (`1.4228180`s), and the drift, starting at `37300` MJD of 0.001296s per day.
// Lines that start with ``#`` are comments. As an extension you can have an
// expiration date on a line that starts with ``#@`` followed by seconds since
// the NTP epoch (1900-01-01). Just as in a standard IERS leap second file.
export function parse_leap_seconds_dat(leaps) /* (leaps : string) -> leaps-table */  {
   
  var v_10011 = ((leaps).split(("\n")));
   
  var xs_10126 = $std_core_list.flatmap_maybe($std_core_vector.vlist(v_10011), parse_taiadjust);
   
  var adjusts = $std_core_list.reverse_acc($std_core_types.Nil, xs_10126);
   
  var expire = parse_leap_expire(leaps, adjusts);
  return Leaps_table(expire, adjusts);
}
 
 
// TAI leap second adjustments for dates before 1972-01-01Z are linear interpolations.
// TAI started in 1958-01-01Z. The initial official UTC time step in 1961-01-01Z was 1.422818s and before that there
// were small steps of 20ms. See Explanatory Supplement to the Astronomical Almanac, 1992 edition, pages 86--87.
// In 1958, the supplement remarks that WWC operated at an offset of _about_ -100e-10, we
// change it to -85e-10 to end up with TAI-UTC == 0 at 1958-01-01.
// (without a rate change it is a negative -0.0472380s).
// Note the JD dates are at 0.29167 as the time steps were usually at 19:00h instead of midnight.
export var default_leap_seconds_pre72;
var default_leap_seconds_pre72 = "\n  # from: Explanatory Supplement to the Astronomical Almanac, 1992 edition, pages 86--87.\n  1958 JAN  1 =JD 2436204.5     TAI-UTC= 0.0  S + (MJD - 36204.) X 0.00073458 S\n  1958 JAN 15 =JD 2436219.29167 TAI-UTC= 0.02 S + (MJD - 36204.) X 0.00073458 S\n  1958 FEB  5 =JD 2436240.29167 TAI-UTC= 0.04 S + (MJD - 36204.) X 0.00073458 S\n  1958 FEB 19 =JD 2436254.29167 TAI-UTC= 0.06 S + (MJD - 36204.) X 0.00073458 S\n  1958 APR  9 =JD 2436303.29167 TAI-UTC= 0.08 S + (MJD - 36204.) X 0.00073458 S\n  1958 JUN 11 =JD 2436366.29167 TAI-UTC= 0.10 S + (MJD - 36204.) X 0.00073458 S\n  1958 JUL  2 =JD 2436387.29167 TAI-UTC= 0.12 S + (MJD - 36204.) X 0.00073458 S\n  1958 JUL 16 =JD 2436401.29167 TAI-UTC= 0.14 S + (MJD - 36204.) X 0.00073458 S\n  1958 OCT 22 =JD 2436499.29167 TAI-UTC= 0.16 S + (MJD - 36204.) X 0.00073458 S\n  1958 NOV 26 =JD 2436534.29167 TAI-UTC= 0.18 S + (MJD - 36204.) X 0.00073458 S\n  1958 DEC 24 =JD 2436562.29167 TAI-UTC= 0.20 S + (MJD - 36204.) X 0.00073458 S\n\n  1959 JAN  1 =JD 2436569.5     TAI-UTC= 0.4681220 S + (MJD - 36569.) X 0.000864 S\n  1959 JAN 28 =JD 2436597.29167 TAI-UTC= 0.4881220 S + (MJD - 36569.) X 0.000864 S\n  1959 FEB 25 =JD 2436625.29167 TAI-UTC= 0.5081220 S + (MJD - 36569.) X 0.000864 S\n  1959 APR  5 =JD 2436664.29167 TAI-UTC= 0.5281220 S + (MJD - 36569.) X 0.000864 S\n  1959 AUG 26 =JD 2436807.29167 TAI-UTC= 0.5481220 S + (MJD - 36569.) X 0.000864 S\n  1959 SEP 30 =JD 2436842.29167 TAI-UTC= 0.5681220 S + (MJD - 36569.) X 0.000864 S\n  1959 NOV  4 =JD 2436877.29167 TAI-UTC= 0.5881220 S + (MJD - 36569.) X 0.000864 S\n  1959 NOV 18 =JD 2436891.29167 TAI-UTC= 0.6081220 S + (MJD - 36569.) X 0.000864 S\n  1959 DEC 16 =JD 2436919.29167 TAI-UTC= 0.6281220 S + (MJD - 36569.) X 0.000864 S\n  1960 JAN  1 =JD 2436934.5     TAI-UTC= 0.9434820 S + (MJD - 36934.) X 0.001296 S\n\n  # from: https://maia.usno.navy.mil/ser7/tai-utc.dat\n  1961 JAN  1 =JD 2437300.5  TAI-UTC=   1.4228180 S + (MJD - 37300.) X 0.001296 S\n  1961 AUG  1 =JD 2437512.5  TAI-UTC=   1.3728180 S + (MJD - 37300.) X 0.001296 S\n  1962 JAN  1 =JD 2437665.5  TAI-UTC=   1.8458580 S + (MJD - 37665.) X 0.0011232S\n  1963 NOV  1 =JD 2438334.5  TAI-UTC=   1.9458580 S + (MJD - 37665.) X 0.0011232S\n  1964 JAN  1 =JD 2438395.5  TAI-UTC=   3.2401300 S + (MJD - 38761.) X 0.001296 S\n  1964 APR  1 =JD 2438486.5  TAI-UTC=   3.3401300 S + (MJD - 38761.) X 0.001296 S\n  1964 SEP  1 =JD 2438639.5  TAI-UTC=   3.4401300 S + (MJD - 38761.) X 0.001296 S\n  1965 JAN  1 =JD 2438761.5  TAI-UTC=   3.5401300 S + (MJD - 38761.) X 0.001296 S\n  1965 MAR  1 =JD 2438820.5  TAI-UTC=   3.6401300 S + (MJD - 38761.) X 0.001296 S\n  1965 JUL  1 =JD 2438942.5  TAI-UTC=   3.7401300 S + (MJD - 38761.) X 0.001296 S\n  1965 SEP  1 =JD 2439004.5  TAI-UTC=   3.8401300 S + (MJD - 38761.) X 0.001296 S\n  1966 JAN  1 =JD 2439126.5  TAI-UTC=   4.3131700 S + (MJD - 39126.) X 0.002592 S\n  1968 FEB  1 =JD 2439887.5  TAI-UTC=   4.2131700 S + (MJD - 39126.) X 0.002592 S";
 
 
// Leap second table upto (but not including) 1972-01-01 UTC
export var leaps_table_pre1972;
 
var v_10011 = ((("\n  # from: Explanatory Supplement to the Astronomical Almanac, 1992 edition, pages 86--87.\n  1958 JAN  1 =JD 2436204.5     TAI-UTC= 0.0  S + (MJD - 36204.) X 0.00073458 S\n  1958 JAN 15 =JD 2436219.29167 TAI-UTC= 0.02 S + (MJD - 36204.) X 0.00073458 S\n  1958 FEB  5 =JD 2436240.29167 TAI-UTC= 0.04 S + (MJD - 36204.) X 0.00073458 S\n  1958 FEB 19 =JD 2436254.29167 TAI-UTC= 0.06 S + (MJD - 36204.) X 0.00073458 S\n  1958 APR  9 =JD 2436303.29167 TAI-UTC= 0.08 S + (MJD - 36204.) X 0.00073458 S\n  1958 JUN 11 =JD 2436366.29167 TAI-UTC= 0.10 S + (MJD - 36204.) X 0.00073458 S\n  1958 JUL  2 =JD 2436387.29167 TAI-UTC= 0.12 S + (MJD - 36204.) X 0.00073458 S\n  1958 JUL 16 =JD 2436401.29167 TAI-UTC= 0.14 S + (MJD - 36204.) X 0.00073458 S\n  1958 OCT 22 =JD 2436499.29167 TAI-UTC= 0.16 S + (MJD - 36204.) X 0.00073458 S\n  1958 NOV 26 =JD 2436534.29167 TAI-UTC= 0.18 S + (MJD - 36204.) X 0.00073458 S\n  1958 DEC 24 =JD 2436562.29167 TAI-UTC= 0.20 S + (MJD - 36204.) X 0.00073458 S\n\n  1959 JAN  1 =JD 2436569.5     TAI-UTC= 0.4681220 S + (MJD - 36569.) X 0.000864 S\n  1959 JAN 28 =JD 2436597.29167 TAI-UTC= 0.4881220 S + (MJD - 36569.) X 0.000864 S\n  1959 FEB 25 =JD 2436625.29167 TAI-UTC= 0.5081220 S + (MJD - 36569.) X 0.000864 S\n  1959 APR  5 =JD 2436664.29167 TAI-UTC= 0.5281220 S + (MJD - 36569.) X 0.000864 S\n  1959 AUG 26 =JD 2436807.29167 TAI-UTC= 0.5481220 S + (MJD - 36569.) X 0.000864 S\n  1959 SEP 30 =JD 2436842.29167 TAI-UTC= 0.5681220 S + (MJD - 36569.) X 0.000864 S\n  1959 NOV  4 =JD 2436877.29167 TAI-UTC= 0.5881220 S + (MJD - 36569.) X 0.000864 S\n  1959 NOV 18 =JD 2436891.29167 TAI-UTC= 0.6081220 S + (MJD - 36569.) X 0.000864 S\n  1959 DEC 16 =JD 2436919.29167 TAI-UTC= 0.6281220 S + (MJD - 36569.) X 0.000864 S\n  1960 JAN  1 =JD 2436934.5     TAI-UTC= 0.9434820 S + (MJD - 36934.) X 0.001296 S\n\n  # from: https://maia.usno.navy.mil/ser7/tai-utc.dat\n  1961 JAN  1 =JD 2437300.5  TAI-UTC=   1.4228180 S + (MJD - 37300.) X 0.001296 S\n  1961 AUG  1 =JD 2437512.5  TAI-UTC=   1.3728180 S + (MJD - 37300.) X 0.001296 S\n  1962 JAN  1 =JD 2437665.5  TAI-UTC=   1.8458580 S + (MJD - 37665.) X 0.0011232S\n  1963 NOV  1 =JD 2438334.5  TAI-UTC=   1.9458580 S + (MJD - 37665.) X 0.0011232S\n  1964 JAN  1 =JD 2438395.5  TAI-UTC=   3.2401300 S + (MJD - 38761.) X 0.001296 S\n  1964 APR  1 =JD 2438486.5  TAI-UTC=   3.3401300 S + (MJD - 38761.) X 0.001296 S\n  1964 SEP  1 =JD 2438639.5  TAI-UTC=   3.4401300 S + (MJD - 38761.) X 0.001296 S\n  1965 JAN  1 =JD 2438761.5  TAI-UTC=   3.5401300 S + (MJD - 38761.) X 0.001296 S\n  1965 MAR  1 =JD 2438820.5  TAI-UTC=   3.6401300 S + (MJD - 38761.) X 0.001296 S\n  1965 JUL  1 =JD 2438942.5  TAI-UTC=   3.7401300 S + (MJD - 38761.) X 0.001296 S\n  1965 SEP  1 =JD 2439004.5  TAI-UTC=   3.8401300 S + (MJD - 38761.) X 0.001296 S\n  1966 JAN  1 =JD 2439126.5  TAI-UTC=   4.3131700 S + (MJD - 39126.) X 0.002592 S\n  1968 FEB  1 =JD 2439887.5  TAI-UTC=   4.2131700 S + (MJD - 39126.) X 0.002592 S")).split(("\n")));
 
var xs_10126 = $std_core_list.flatmap_maybe($std_core_vector.vlist(v_10011), parse_taiadjust);
 
var adjusts = $std_core_list.reverse_acc($std_core_types.Nil, xs_10126);
 
var expire = parse_leap_expire("\n  # from: Explanatory Supplement to the Astronomical Almanac, 1992 edition, pages 86--87.\n  1958 JAN  1 =JD 2436204.5     TAI-UTC= 0.0  S + (MJD - 36204.) X 0.00073458 S\n  1958 JAN 15 =JD 2436219.29167 TAI-UTC= 0.02 S + (MJD - 36204.) X 0.00073458 S\n  1958 FEB  5 =JD 2436240.29167 TAI-UTC= 0.04 S + (MJD - 36204.) X 0.00073458 S\n  1958 FEB 19 =JD 2436254.29167 TAI-UTC= 0.06 S + (MJD - 36204.) X 0.00073458 S\n  1958 APR  9 =JD 2436303.29167 TAI-UTC= 0.08 S + (MJD - 36204.) X 0.00073458 S\n  1958 JUN 11 =JD 2436366.29167 TAI-UTC= 0.10 S + (MJD - 36204.) X 0.00073458 S\n  1958 JUL  2 =JD 2436387.29167 TAI-UTC= 0.12 S + (MJD - 36204.) X 0.00073458 S\n  1958 JUL 16 =JD 2436401.29167 TAI-UTC= 0.14 S + (MJD - 36204.) X 0.00073458 S\n  1958 OCT 22 =JD 2436499.29167 TAI-UTC= 0.16 S + (MJD - 36204.) X 0.00073458 S\n  1958 NOV 26 =JD 2436534.29167 TAI-UTC= 0.18 S + (MJD - 36204.) X 0.00073458 S\n  1958 DEC 24 =JD 2436562.29167 TAI-UTC= 0.20 S + (MJD - 36204.) X 0.00073458 S\n\n  1959 JAN  1 =JD 2436569.5     TAI-UTC= 0.4681220 S + (MJD - 36569.) X 0.000864 S\n  1959 JAN 28 =JD 2436597.29167 TAI-UTC= 0.4881220 S + (MJD - 36569.) X 0.000864 S\n  1959 FEB 25 =JD 2436625.29167 TAI-UTC= 0.5081220 S + (MJD - 36569.) X 0.000864 S\n  1959 APR  5 =JD 2436664.29167 TAI-UTC= 0.5281220 S + (MJD - 36569.) X 0.000864 S\n  1959 AUG 26 =JD 2436807.29167 TAI-UTC= 0.5481220 S + (MJD - 36569.) X 0.000864 S\n  1959 SEP 30 =JD 2436842.29167 TAI-UTC= 0.5681220 S + (MJD - 36569.) X 0.000864 S\n  1959 NOV  4 =JD 2436877.29167 TAI-UTC= 0.5881220 S + (MJD - 36569.) X 0.000864 S\n  1959 NOV 18 =JD 2436891.29167 TAI-UTC= 0.6081220 S + (MJD - 36569.) X 0.000864 S\n  1959 DEC 16 =JD 2436919.29167 TAI-UTC= 0.6281220 S + (MJD - 36569.) X 0.000864 S\n  1960 JAN  1 =JD 2436934.5     TAI-UTC= 0.9434820 S + (MJD - 36934.) X 0.001296 S\n\n  # from: https://maia.usno.navy.mil/ser7/tai-utc.dat\n  1961 JAN  1 =JD 2437300.5  TAI-UTC=   1.4228180 S + (MJD - 37300.) X 0.001296 S\n  1961 AUG  1 =JD 2437512.5  TAI-UTC=   1.3728180 S + (MJD - 37300.) X 0.001296 S\n  1962 JAN  1 =JD 2437665.5  TAI-UTC=   1.8458580 S + (MJD - 37665.) X 0.0011232S\n  1963 NOV  1 =JD 2438334.5  TAI-UTC=   1.9458580 S + (MJD - 37665.) X 0.0011232S\n  1964 JAN  1 =JD 2438395.5  TAI-UTC=   3.2401300 S + (MJD - 38761.) X 0.001296 S\n  1964 APR  1 =JD 2438486.5  TAI-UTC=   3.3401300 S + (MJD - 38761.) X 0.001296 S\n  1964 SEP  1 =JD 2438639.5  TAI-UTC=   3.4401300 S + (MJD - 38761.) X 0.001296 S\n  1965 JAN  1 =JD 2438761.5  TAI-UTC=   3.5401300 S + (MJD - 38761.) X 0.001296 S\n  1965 MAR  1 =JD 2438820.5  TAI-UTC=   3.6401300 S + (MJD - 38761.) X 0.001296 S\n  1965 JUL  1 =JD 2438942.5  TAI-UTC=   3.7401300 S + (MJD - 38761.) X 0.001296 S\n  1965 SEP  1 =JD 2439004.5  TAI-UTC=   3.8401300 S + (MJD - 38761.) X 0.001296 S\n  1966 JAN  1 =JD 2439126.5  TAI-UTC=   4.3131700 S + (MJD - 39126.) X 0.002592 S\n  1968 FEB  1 =JD 2439887.5  TAI-UTC=   4.2131700 S + (MJD - 39126.) X 0.002592 S", adjusts);
var leaps_table_pre1972 = Leaps_table(expire, adjusts);
 
 
// monadic lift
export function _mlift_pleap_10341(ntpsecs, adjust) /* (ntpsecs : int, adjust : int) -> std/text/parse/parse leap-adjust */  {
   
  var _x_x1_1_10310 = $std_core_hnd._open_none2($std_time_timestamp.int_fs_timespan, ntpsecs);
   
  var _x_x1_0_10308 = $std_core_hnd._open_none2(function(x /* std/num/ddouble/ddouble */ , y /* std/num/ddouble/ddouble */ ) {
      var _x51 = y.hi;
      var _x52 = y.lo;
      return $std_num_ddouble._lp__plus__rp_(x, $std_num_ddouble.Ddouble((-_x51), (-_x52)));
    }, _x_x1_1_10310, ntp2000);
   
  var _x_x1_10304 = $std_core_hnd._open_none2(function(t /* std/time/timestamp/timespan */ , leap_0 /* ? int */ ) {
      var _x53 = (leap_0 !== undefined) ? leap_0 : 0;
      return $std_time_timestamp.Timestamp(t, $std_core_types._int_clamp32(_x53));
    }, _x_x1_0_10308);
   
  var _x_x2_10305 = $std_core_hnd._open_none2($std_time_timestamp.int_fs_timespan, adjust);
  return $std_core_hnd._open_none4(function(utc_start /* utc-timestamp */ , offset /* std/time/timestamp/timespan */ , drift_start /* ? utc-timestamp */ , drift /* ? std/num/ddouble/ddouble */ ) {
      var _x51 = (drift_start !== undefined) ? drift_start : $std_time_timestamp.timestamp0;
      var _x52 = (drift !== undefined) ? drift : $std_num_ddouble.zero;
      return Leap_adjust(utc_start, offset, _x51, _x52);
    }, _x_x1_10304, _x_x2_10305, $std_time_timestamp.timestamp0, $std_num_ddouble.zero);
}
 
 
// monadic lift
export function _mlift_pleap_10342(ntpsecs, wild___0) /* (ntpsecs : int, wild_@0 : string) -> std/text/parse/parse leap-adjust */  {
   
  var x_10465 = $std_text_parse.pint();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(adjust /* int */ ) {
      return _mlift_pleap_10341(ntpsecs, adjust);
    });
  }
  else {
    return _mlift_pleap_10341(ntpsecs, x_10465);
  }
}
 
 
// monadic lift
export function _mlift_pleap_10343(ntpsecs) /* (ntpsecs : int) -> std/text/parse/parse leap-adjust */  {
   
  var x_10467 = $std_text_parse.whitespace();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild___0 /* string */ ) {
      return _mlift_pleap_10342(ntpsecs, wild___0);
    });
  }
  else {
    return _mlift_pleap_10342(ntpsecs, x_10467);
  }
}
 
 
// monadic lift
export function _mlift_pleap_10344(wild__) /* (wild_ : string) -> std/text/parse/parse leap-adjust */  {
   
  var x_10469 = $std_text_parse.pint();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_pleap_10343);
  }
  else {
    return _mlift_pleap_10343(x_10469);
  }
}
 
export function pleap() /* () -> std/text/parse/parse leap-adjust */  {
   
  var x_10471 = $std_text_parse.whitespace0();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_pleap_10344);
  }
  else {
     
    var x_0_10474 = $std_text_parse.pint();
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(_mlift_pleap_10343);
    }
    else {
       
      var x_1_10477 = $std_text_parse.whitespace();
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(wild___0 /* string */ ) {
          return _mlift_pleap_10342(x_0_10474, wild___0);
        });
      }
      else {
         
        var x_2_10480 = $std_text_parse.pint();
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(adjust /* int */ ) {
            return _mlift_pleap_10341(x_0_10474, adjust);
          });
        }
        else {
           
          var _x_x1_1_10310 = $std_core_hnd._open_none2($std_time_timestamp.int_fs_timespan, x_0_10474);
           
          var _x_x1_0_10308 = $std_core_hnd._open_none2(function(x_3 /* std/num/ddouble/ddouble */ , y /* std/num/ddouble/ddouble */ ) {
              var _x53 = y.hi;
              var _x54 = y.lo;
              return $std_num_ddouble._lp__plus__rp_(x_3, $std_num_ddouble.Ddouble((-_x53), (-_x54)));
            }, _x_x1_1_10310, ntp2000);
           
          var _x_x1_10304 = $std_core_hnd._open_none2(function(t /* std/time/timestamp/timespan */ , leap_0 /* ? int */ ) {
              var _x55 = (leap_0 !== undefined) ? leap_0 : 0;
              return $std_time_timestamp.Timestamp(t, $std_core_types._int_clamp32(_x55));
            }, _x_x1_0_10308);
           
          var _x_x2_10305 = $std_core_hnd._open_none2($std_time_timestamp.int_fs_timespan, x_2_10480);
          return $std_core_hnd._open_none4(function(utc_start /* utc-timestamp */ , offset /* std/time/timestamp/timespan */ , drift_start /* ? utc-timestamp */ , drift /* ? std/num/ddouble/ddouble */ ) {
              var _x53 = (drift_start !== undefined) ? drift_start : $std_time_timestamp.timestamp0;
              var _x54 = (drift !== undefined) ? drift : $std_num_ddouble.zero;
              return Leap_adjust(utc_start, offset, _x53, _x54);
            }, _x_x1_10304, _x_x2_10305, $std_time_timestamp.timestamp0, $std_num_ddouble.zero);
        }
      }
    }
  }
}
 
export function parse_leap(line) /* (line : string) -> maybe<leap-adjust> */  {
   
  var maybe_10128 = $std_core_sslice.starts_with($std_core_sslice.trim_left(line, " "), "#");
  if (maybe_10128 !== null) {
    return $std_core_types.Nothing;
  }
  else {
     
    var perr_10129 = $std_text_parse.parse($std_core_sslice.Sslice(line, 0, line.length), pleap);
    if (perr_10129._tag === 1) {
      return $std_core_types.Just(perr_10129.result);
    }
    else {
      return $std_core_types.Nothing;
    }
  }
}
 
export function parse_leap_seconds(leaps) /* (leaps : string) -> leaps-table */  {
   
  var v_10011 = ((leaps).split(("\n")));
   
  var xs_10131 = $std_core_list.flatmap_maybe($std_core_vector.vlist(v_10011), parse_leap);
   
  var adjusts = $std_core_list.reverse_acc($std_core_types.Nil, xs_10131);
   
  var expire = parse_leap_expire(leaps, adjusts);
  return Leaps_table(expire, adjusts);
}
 
 
// Default TI leaps table has leap second information up to the compiler release (currently `leaps-table-y2017`).
export var leaps_table_ti;
 
var v_10011 = ((("\n  # From: https://hpiers.obspm.fr/iers/bul/bulc/ntp/leap-seconds.list\n  #\tUpdated through IERS Bulletin C (https://hpiers.obspm.fr/iers/bul/bulc/bulletinc.dat)\n  # File expires on:  28 December 2024\n  #\n  #@\t3944332800\n  #\n  2272060800  10  # 1 Jan 1972\n  2287785600  11  # 1 Jul 1972\n  2303683200  12  # 1 Jan 1973\n  2335219200  13  # 1 Jan 1974\n  2366755200  14  # 1 Jan 1975\n  2398291200  15  # 1 Jan 1976\n  2429913600  16  # 1 Jan 1977\n  2461449600  17  # 1 Jan 1978\n  2492985600  18  # 1 Jan 1979\n  2524521600  19  # 1 Jan 1980\n  2571782400  20  # 1 Jul 1981\n  2603318400  21  # 1 Jul 1982\n  2634854400  22  # 1 Jul 1983\n  2698012800  23  # 1 Jul 1985\n  2776982400  24  # 1 Jan 1988\n  2840140800  25  # 1 Jan 1990\n  2871676800  26  # 1 Jan 1991\n  2918937600  27  # 1 Jul 1992\n  2950473600  28  # 1 Jul 1993\n  2982009600  29  # 1 Jul 1994\n  3029443200  30  # 1 Jan 1996\n  3076704000  31  # 1 Jul 1997\n  3124137600  32  # 1 Jan 1999\n  3345062400  33  # 1 Jan 2006\n  3439756800  34  # 1 Jan 2009\n  3550089600  35  # 1 Jul 2012\n  3644697600  36  # 1 Jul 2015\n  3692217600  37  # 1 Jan 2017\n  // 3723753600  35  # 1 Jan 2018")).split(("\n")));
 
var xs_10131 = $std_core_list.flatmap_maybe($std_core_vector.vlist(v_10011), parse_leap);
 
var adjusts = $std_core_list.reverse_acc($std_core_types.Nil, xs_10131);
 
var expire = parse_leap_expire("\n  # From: https://hpiers.obspm.fr/iers/bul/bulc/ntp/leap-seconds.list\n  #\tUpdated through IERS Bulletin C (https://hpiers.obspm.fr/iers/bul/bulc/bulletinc.dat)\n  # File expires on:  28 December 2024\n  #\n  #@\t3944332800\n  #\n  2272060800  10  # 1 Jan 1972\n  2287785600  11  # 1 Jul 1972\n  2303683200  12  # 1 Jan 1973\n  2335219200  13  # 1 Jan 1974\n  2366755200  14  # 1 Jan 1975\n  2398291200  15  # 1 Jan 1976\n  2429913600  16  # 1 Jan 1977\n  2461449600  17  # 1 Jan 1978\n  2492985600  18  # 1 Jan 1979\n  2524521600  19  # 1 Jan 1980\n  2571782400  20  # 1 Jul 1981\n  2603318400  21  # 1 Jul 1982\n  2634854400  22  # 1 Jul 1983\n  2698012800  23  # 1 Jul 1985\n  2776982400  24  # 1 Jan 1988\n  2840140800  25  # 1 Jan 1990\n  2871676800  26  # 1 Jan 1991\n  2918937600  27  # 1 Jul 1992\n  2950473600  28  # 1 Jul 1993\n  2982009600  29  # 1 Jul 1994\n  3029443200  30  # 1 Jan 1996\n  3076704000  31  # 1 Jul 1997\n  3124137600  32  # 1 Jan 1999\n  3345062400  33  # 1 Jan 2006\n  3439756800  34  # 1 Jan 2009\n  3550089600  35  # 1 Jul 2012\n  3644697600  36  # 1 Jul 2015\n  3692217600  37  # 1 Jan 2017\n  // 3723753600  35  # 1 Jan 2018", adjusts);
var leaps_table_ti = extend(Leaps_table(expire, adjusts), leaps_table_pre1972);
 
 
/* The [TI] (_International Time_) time scale with a 2000-01-01Z UTC epoch.
This is the default time scale used in this library. It was a
[proposed][TIpropose] time scale at the 2004 ITU-R meeting as a replacement of UTC
without future leap seconds. In this library, we define TI to match
exactly UTC up to the compiler release date (currently 2017) but ignore any
possible future leap seconds after that date. This is the preferred time scale
in this library as it guarantees deterministic time calculations for any
future date, i.e. before 2017-01-01Z, TI == UTC, while after that, TI == TAI - 37s.
*/
export var ts_ti;
var ts_ti = utc_timescale("", leaps_table_ti);
 
export function _default_utc(action) /* forall<a,e> (action : () -> <utc|e> a) -> e a */  {
  return utc_fs__handle(_Hnd_utc(1, $std_core_hnd.clause_tail0(function() {
        return ts_ti;
      })), function(_res /* 2574 */ ) {
      return _res;
    }, action);
}
 
export var leaps_table0;
var leaps_table0 = Leaps_table($std_time_instant.epoch, $std_core_types.Nil);
 
export function leap_adjust_fs_show(l) /* (l : leap-adjust) -> string */  {
  var _x55 = l.utc_start;
  var _x56 = l.offset;
  var _x57 = l.drift_start;
  var _x58 = l.drift;
  return $std_core_list.concat_fs_join($std_core_types.Cons($std_time_instant.timestamp_fs_show(_x55), $std_core_types.Cons(": offset: ", $std_core_types.Cons($std_num_ddouble.show(_x56), $std_core_types.Cons(", drift-start: ", $std_core_types.Cons($std_time_instant.timestamp_fs_show(_x57), $std_core_types.Cons(", drift: ", $std_core_types.Cons($std_num_ddouble.show(_x58), $std_core_types.Nil))))))));
}
 
export function leaps_table_fs_show(t) /* (t : leaps-table) -> string */  {
  var _x59 = t.adjusts;
  return $std_core_list.unlines($std_core_list.map(_x59, leap_adjust_fs_show));
}
 
export var unix2000;
var unix2000 = $std_time_timestamp.int_fs_timespan(946684800);
 
 
// Create an instant from raw unix seconds since the unix epoch (1970-01-01T00:00:10 TAI)
// and optional leap seconds to designate instants inside a leap seconds.
export function timespan_fs_unix_instant(t, leap, ts) /* (t : std/time/timestamp/timespan, leap : ? int, ts : ? std/time/instant/timescale) -> std/time/instant/instant */  {
   
  var _x60 = unix2000.hi;
  var _x61 = unix2000.lo;
  var t_1_10142 = $std_num_ddouble._lp__plus__rp_(t, $std_num_ddouble.Ddouble((-_x60), (-_x61)));
   
  var _x62 = (leap !== undefined) ? leap : 0;
  var t_0_10141 = $std_time_timestamp.Timestamp(t_1_10142, $std_core_types._int_clamp32(_x62));
   
  var i_10138 = $std_time_instant.Instant(t_0_10141, ts_ti);
  var _x61 = i_10138.ts.name;
  var _x63 = (ts !== undefined) ? ts : ts_ti;
  var _x62 = _x63.name;
  var _x60 = (_x61 === _x62);
  if (_x60) {
    return i_10138;
  }
  else {
    var _x64 = i_10138.since;
    var _x65 = i_10138.ts;
    var _x66 = (ts !== undefined) ? ts : ts_ti;
    var _x67 = (ts !== undefined) ? ts : ts_ti;
    return $std_time_instant.Instant($std_time_instant.convert(_x64, _x65, _x66), _x67);
  }
}
 
 
/* Given a Unix time stamp in (fractional) seconds (`secs`) and an optional separate
fraction of seconds `frac` (for increased precision for nanosecond timestamps), return an `:instant`.
that is `secs + frac` seconds after the Unix epoch (``1970-01-01Z``).
Unfortunately, Unix time stamps are [ambiguous](https://en.wikipedia.org/wiki/Unix_time).
The seconds `secs` are interpreted as: `val days = secs / 86400` and `val dsecs = secs % 86400`,
where `days` is the number of days since ``1970-01-01Z`` and `dsecs` is the SI seconds into
the day. This means that one cannot represent a possible extra leap second since it
will look as the first second of the next day. For example, here is how the time stamps look
around the leap second of ``1973-01-01Z``:
````
> instant(1972,12,31,23,59,59).unix-timestamp
94694399
> instant(1972,12,31,23,59,60).unix-timestamp
94694400
> instant(1973,1,1).unix-timestamp
94694400
````
Internally, this library uses proper `:timestamp`s that _can_ keep track of leap seconds.
To indicate a time in a leap second, you can use a fraction `frac` that is larger than `1.0`. For example:
````
> unix-instant(94694399.0).time
1972-12-31T23:59:59Z
> unix-instant(94694399.0,1.0).time
1972-12-31T23:59:60Z
> unix-instant(94694400.0).time
1973-01-01T00:00:00Z
````
This works well for systems that support [``CLOCK_UTC``](https://www.madore.org/~david/computers/unix-leap-seconds.html).
*/
export function float64_fs_unix_instant(u, frac, ts) /* (u : float64, frac : ? float64, ts : ? std/time/instant/timescale) -> std/time/instant/instant */  {
   
  var _x68 = (frac !== undefined) ? frac : 0.0;
  var frac_0_10147 = $std_num_float64.fraction(_x68);
   
  if ((frac_0_10147 === (0.0))) {
    var t = $std_num_ddouble.Ddouble(u, 0.0);
  }
  else {
    var t = $std_num_ddouble.dsum(u, frac_0_10147);
  }
   
  var _x71 = (frac !== undefined) ? frac : 0.0;
  var _x70 = (_x71 >= (0.0));
  if (_x70) {
    var _x72 = (frac !== undefined) ? frac : 0.0;
    var _x69 = Math.floor(_x72);
  }
  else {
    var _x73 = (frac !== undefined) ? frac : 0.0;
    var _x69 = Math.ceil(_x73);
  }
  var leap = $std_core_types._int_double(_x69);
  var _x68 = (ts !== undefined) ? ts : ts_ti;
  return timespan_fs_unix_instant(t, leap, _x68);
}
 
 
// Create an instant from raw unix seconds since the unix epoch (1970-01-01T00:00:10 TAI)
// Use a fraction `> 1` to indicate a time inside a leap second.
export function int_fs_unix_instant(u, frac, ts) /* (u : int, frac : ? float64, ts : ? std/time/instant/timescale) -> std/time/instant/instant */  {
   
  var _x69 = (frac !== undefined) ? frac : 0.0;
  var d_10151 = $std_num_float64.fraction(_x69);
   
  var secs_10149 = $std_num_ddouble._lp__plus__rp_($std_num_ddouble.ddouble_int_exp(u, 0), $std_num_ddouble.Ddouble(d_10151, 0.0));
   
  var _x72 = (frac !== undefined) ? frac : 0.0;
  var _x71 = (_x72 >= (0.0));
  if (_x71) {
    var _x73 = (frac !== undefined) ? frac : 0.0;
    var _x70 = Math.floor(_x73);
  }
  else {
    var _x74 = (frac !== undefined) ? frac : 0.0;
    var _x70 = Math.ceil(_x74);
  }
  var leap = $std_core_types._int_double(_x70);
  var _x69 = (ts !== undefined) ? ts : ts_ti;
  return timespan_fs_unix_instant(secs_10149, leap, _x69);
}
 
 
// Get a standard Unix timestamp in fractional seconds since the Unix epoch (1970-01-01Z).
// Since Unix time stamps are ambiguous,
// instants inside a leap seconds show as occurring in the second after that.
export function unix_timestamp(i) /* (i : std/time/instant/instant) -> std/num/ddouble/ddouble */  {
  var _x70 = i.since;
  var _x71 = i.ts;
  return $std_num_ddouble._lp__plus__rp_(unix2000, $std_time_timestamp.unsafe_timespan_withleap($std_time_instant.convert(_x70, _x71, ts_ti)));
}
 
 
// [NTP](https://en.wikipedia.org/wiki/Network_Time_Protocol) time scale.
// It equals the `ts-ti` time scale.
export var ts_ntp;
var ts_ntp = ts_ti;
 
 
// Convert an NTP time in seconds since the NTP epoch (1900-01-01Z) to an instant.
// Also takes an optional `leap` argument if the NTP time is inside a leap second.
export function ntp_instant(ntp, leap) /* (ntp : std/num/ddouble/ddouble, leap : ? int) -> std/time/instant/instant */  {
   
  var _x72 = (leap !== undefined) ? leap : 0;
  var t_10156 = $std_time_timestamp._lp__dash__rp_($std_time_timestamp.Timestamp(ntp, $std_core_types._int_clamp32(_x72)), ntp2000);
  return $std_time_instant.Instant(t_10156, ts_ti);
}
 
 
// Return the NTP time of an instant since the NTP epoch (1900-01-01)
// Since NTP time stamps are ambiguous, times inside a leap second show
// as occurring in the second after the leap second.
export function ntp_timestamp(i) /* (i : std/time/instant/instant) -> std/num/ddouble/ddouble */  {
  var _x72 = i.since;
  var _x73 = i.ts;
  return $std_num_ddouble._lp__plus__rp_(ntp2000, $std_time_timestamp.unsafe_timespan_withleap($std_time_instant.convert(_x72, _x73, ts_ti)));
}
 
 
// The UTC time scale.
export function ts_utc_create(leaps) /* (leaps : leaps-table) -> std/time/instant/timescale */  {
  return utc_timescale("UTC", leaps);
}
 
 
// [Unix](https://en.wikipedia.org/wiki/Unix_time) time scale is equal
// to the UTC time scale (`ts-utc`).
export function ts_unix_create(leaps) /* (leaps : leaps-table) -> std/time/instant/timescale */  {
  return utc_timescale("UTC", leaps);
}
 
 
// [NTP](https://en.wikipedia.org/wiki/Network_Time_Protocol) time scale is equal
// to the UTC time scale (`ts-utc`).
export function ts_ntp_create(leaps) /* (leaps : leaps-table) -> std/time/instant/timescale */  {
  return utc_timescale("UTC", leaps);
}
 
 
// [Unix](https://en.wikipedia.org/wiki/Unix_time) time scale based on Unix seconds.
// It equals the `ts-ti` time scale.
export var ts_unix;
var ts_unix = ts_ti;
 
 
/*----------------------------------------------------------------------------
  UTC-SLS to TAI conversion
----------------------------------------------------------------------------*/
export function utc_sls_leap_in_day(leaps, smooth, utc_0) /* (leaps : leaps-table, smooth : std/time/timestamp/timespan, utc : utc-timestamp) -> maybe<(std/time/timestamp/timestamp, std/time/timestamp/timestamp, std/time/timestamp/timespan, std/time/timestamp/timespan)> */  {
   
  var tuple2_10043 = $std_time_timestamp.days_seconds(utc_0);
  var _x75 = tuple2_10043.fst;
  var _x74 = utc_leap_in_day(leaps, _x75);
  if (_x74 === null) {
    return $std_core_types.Nothing;
  }
  else {
     
    var ts_0_10164 = $std_time_timestamp._lp__dash__rp_(_x74.value.fst, smooth);
     
    var _x76 = ts_0_10164.since;
    var _x77 = ts_0_10164.leap32;
    var smooth_start = $std_time_timestamp.Timestamp($std_num_ddouble._lp__plus__rp_(_x76, _x74.value.snd), _x77);
     
    var smooth_end = $std_time_timestamp.add_leap_seconds(_x74.value.fst, _x74.value.snd);
    var _x76 = $std_core_order._lp__excl__eq__rp_($std_time_timestamp.cmp(utc_0, smooth_start), $std_core_types.Gt);
    if (_x76) {
      return $std_core_types.Nothing;
    }
    else {
      var _x77 = $std_core_order._lp__eq__eq__rp_($std_time_timestamp.cmp(utc_0, smooth_end), $std_core_types.Gt);
      if (_x77) {
        return $std_core_types.Nothing;
      }
      else {
         
        var _x78 = smooth_end.since;
        var _x79 = smooth_start.since.hi;
        var _x80 = smooth_start.since.lo;
        var smooth_total = $std_num_ddouble._lp__plus__rp_(_x78, $std_num_ddouble.Ddouble((-_x79), (-_x80)));
         
        var x_0_10174 = $std_time_timestamp.unsafe_timespan_withleap(utc_0);
         
        var _x81 = smooth_start.since.hi;
        var _x82 = smooth_start.since.lo;
        var dt = $std_num_ddouble._lp__plus__rp_(x_0_10174, $std_num_ddouble.Ddouble((-_x81), (-_x82)));
        return $std_core_types.Just($std_core_types.Tuple4(_x74.value.fst, smooth_start, smooth_total, dt));
      }
    }
  }
}
 
export function utc_sls_from_tai(leaps, smooth, tai_since) /* (leaps : leaps-table, smooth : std/time/timestamp/timespan, tai-since : std/time/duration/duration) -> std/time/timestamp/timestamp */  {
   
  var utc_0 = utc_from_tai(leaps, tai_since);
  var _x78 = utc_sls_leap_in_day(leaps, smooth, utc_0);
  if (_x78 === null) {
    return utc_0;
  }
  else {
     
    var frac = $std_num_ddouble._lp__fs__rp_(_x78.value.field4, smooth);
     
    var t_10178 = $std_num_ddouble._lp__star__rp_(frac, _x78.value.thd);
    var _x79 = _x78.value.snd.since;
    var _x80 = _x78.value.snd.leap32;
    return $std_time_timestamp.Timestamp($std_num_ddouble._lp__plus__rp_(_x79, t_10178), _x80);
  }
}
 
export function utc_sls_to_tai(leaps, smooth, sls) /* (leaps : leaps-table, smooth : std/time/timestamp/timespan, sls : utc-timestamp) -> std/time/duration/duration */  {
   
  var _x81 = utc_sls_leap_in_day(leaps, smooth, sls);
  if (_x81 === null) {
    var utc_0 = sls;
  }
  else {
     
    var frac = $std_num_ddouble._lp__fs__rp_(_x81.value.field4, _x81.value.thd);
     
    var t_10180 = $std_num_ddouble._lp__star__rp_(frac, smooth);
     
    var _x82 = _x81.value.snd.since;
    var _x83 = _x81.value.snd.leap32;
    var utc0 = $std_time_timestamp.Timestamp($std_num_ddouble._lp__plus__rp_(_x82, t_10180), _x83);
    var _x82 = $std_core_order._lp__excl__eq__rp_($std_time_timestamp.cmp(utc0, _x81.value.fst), $std_core_types.Gt);
    if (_x82) {
      var utc_0 = utc0;
    }
    else {
       
      var _x83 = utc0.since;
      var _x84 = _x81.value.fst.since.hi;
      var _x85 = _x81.value.fst.since.lo;
      var ldiff = $std_num_ddouble._lp__plus__rp_(_x83, $std_num_ddouble.Ddouble((-_x84), (-_x85)));
      var utc_0 = $std_time_timestamp.add_leap_seconds(_x81.value.fst, ldiff);
    }
  }
  return utc_to_tai(leaps, utc_0);
}
 
 
// Create a new smoothed leap second time scale with an optional period during
// which smoothing takes place. This is 1000s for `ts-utc-sls`.
export function utc_sls_timescale(name, leaps, smooth) /* (name : string, leaps : leaps-table, smooth : ? std/time/timestamp/timespan) -> std/time/instant/timescale */  {
   
  if (smooth !== undefined) {
    var _uniq_smooth_3744 = smooth;
  }
  else {
    var _uniq_smooth_3744 = $std_time_timestamp.int_fs_timespan(1000);
  }
  return $std_time_instant.timescale(name, function(tai /* std/time/duration/duration */ ) {
      return utc_sls_from_tai(leaps, _uniq_smooth_3744, tai);
    }, function(utc_0 /* std/time/timestamp/timestamp */ ) {
      return utc_sls_to_tai(leaps, _uniq_smooth_3744, utc_0);
    }, "UTC-SLS");
}
 
 
// Create a new UTC-SLS time scale from a provided leap second table `leaps`.
// Implements a UTC time scale except without ever showing leap seconds.
// UTC-SLS is equivalent to UTC except for the last 1000 seconds of a day where
// a leap second occurs. On such day, the leap second time step (positive or negative)
// is distributed over the last 1000 seconds of the day. On the full hour, UTC
// and UTC-SLS are equal again.
//
// This is a recommended time scale to use for time stamps or communication
// with other services since it avoids any potential trouble
// with leap seconds while still being quite precise.
// See also: <https://www.cl.cam.ac.uk/~mgk25/time/utc-sls>.
//
// You can create a UTC-SLS time scale based on the latest IERS leap second
// data using [`cal-utc-sls-load`](std_time_download.html#cal_utc_sls_load).
export function ts_utc_sls_create(leaps) /* (leaps : leaps-table) -> std/time/instant/timescale */  {
   
  var _x81 = undefined;
  if (_x81 !== undefined) {
    var _uniq_smooth_3744 = _x81;
  }
  else {
    var _uniq_smooth_3744 = $std_time_timestamp.int_fs_timespan(1000);
  }
  return $std_time_instant.timescale("UTC-SLS", function(tai /* std/time/duration/duration */ ) {
      return utc_sls_from_tai(leaps, _uniq_smooth_3744, tai);
    }, function(utc_0 /* std/time/timestamp/timestamp */ ) {
      return utc_sls_to_tai(leaps, _uniq_smooth_3744, utc_0);
    }, "UTC-SLS");
}
 
 
// TI time scale with smoothed leap seconds.\
// Implements a TI time scale (`ts-ti`) except without ever showing leap seconds.
// TI-SLS is equivalent to TI except for the last 1000 seconds of a day where
// a leap second occurs. On such day, the leap second time step (positive or negative)
// is distributed over the last 1000 seconds of the day. On the full hour, TI
// and TI-SLS are equal again.
export var ts_ti_sls;
 
var _x81 = undefined;
if (_x81 !== undefined) {
  var _uniq_smooth_3744 = _x81;
}
else {
  var _uniq_smooth_3744 = $std_time_timestamp.int_fs_timespan(1000);
}
var ts_ti_sls = $std_time_instant.timescale("TI-SLS", function(tai /* std/time/duration/duration */ ) {
    return utc_sls_from_tai(leaps_table_ti, _uniq_smooth_3744, tai);
  }, function(utc_0 /* std/time/timestamp/timestamp */ ) {
    return utc_sls_to_tai(leaps_table_ti, _uniq_smooth_3744, utc_0);
  }, "UTC-SLS");
 
 
// Leap second table up to 2017-01-01Z.
export var leaps_table_y2017;
 
var end_10226 = $std_time_timestamp.int_fs_timestamp(536544000);
 
var _x81 = leaps_table_ti.adjusts;
var _arg_x1670 = $std_core_list.drop_while(_x81, function(la /* leap-adjust */ ) {
    var _x82 = la.utc_start;
    return $std_core_order._lp__eq__eq__rp_($std_time_timestamp.cmp(_x82, end_10226), $std_core_types.Gt);
  });
var _x82 = undefined;
if (_x82 !== undefined) {
  var _x81 = _x82;
}
else {
  var _x81 = leaps_table_ti.expire;
}
if (_arg_x1670 !== undefined) {
  var _x83 = _arg_x1670;
}
else {
  var _x83 = leaps_table_ti.adjusts;
}
var leaps_table_y2017 = Leaps_table(_x81, _x83);
 
 
// Get a list of leap second steps in a triple, NTP start time, offset just before, and the new offset at that time,
// the base offset, the drift start date and the drift rate.
export function get_leap_steps(table) /* (table : ? leaps-table) -> list<(utc-timestamp, std/time/timestamp/timespan, std/time/timestamp/timespan, (std/time/timestamp/timespan, utc-timestamp, std/num/ddouble/ddouble))> */  {
   
  var _x85 = (table !== undefined) ? table : leaps_table_ti;
  var _x84 = _x85.adjusts;
  var adjusts = $std_core_list.reverse_acc($std_core_types.Nil, _x84);
  return $std_core_list.map($std_core_list.zip($std_core_types.Cons(zero, adjusts), adjusts), function(las /* (leap-adjust, leap-adjust) */ ) {
       
      var _x84 = las.fst;
      var _x85 = las.snd.utc_start;
      var ofs1 = delta_tai(_x84, _x85);
       
      var _x86 = las.snd;
      var _x87 = las.snd.utc_start;
      var ofs2 = delta_tai(_x86, _x87);
      var _x84 = las.snd.utc_start;
      var _x85 = las.snd.offset;
      var _x86 = las.snd.drift_start;
      var _x87 = las.snd.drift;
      return $std_core_types.Tuple4(_x84, ofs1, ofs2, $std_core_types.Tuple3(_x85, _x86, _x87));
    });
}