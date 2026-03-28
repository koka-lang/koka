// Koka generated module: std/time/instant, koka version: 3.2.4
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
import * as $std_time_duration from './std_time_duration.mjs';
import * as $std_time_date from './std_time_date.mjs';
import * as $std_num_int32 from './std_num_int32.mjs';
import * as $std_num_float64 from './std_num_float64.mjs';
 
// externals
 
// type declarations
// type timescale
export function Timescale(name, unit, from_tai, to_tai, mb_seconds_in_day, mb_to_mjd2000, mb_from_mjd2000) /* (name : string, unit : string, from-tai : (std/time/duration/duration) -> std/time/timestamp/timestamp, to-tai : (std/time/timestamp/timestamp) -> std/time/duration/duration, mb-seconds-in-day : maybe<(t : std/time/timestamp/timestamp) -> std/time/timestamp/timespan>, mb-to-mjd2000 : maybe<(t : std/time/timestamp/timestamp, tzdelta : std/time/timestamp/timespan) -> std/num/ddouble/ddouble>, mb-from-mjd2000 : maybe<(days : int, frac : std/num/ddouble/ddouble) -> std/time/timestamp/timestamp>) -> timescale */  {
  return { name: name, unit: unit, from_tai: from_tai, to_tai: to_tai, mb_seconds_in_day: mb_seconds_in_day, mb_to_mjd2000: mb_to_mjd2000, mb_from_mjd2000: mb_from_mjd2000 };
}
// type instant
export function Instant(since, ts) /* (since : std/time/timestamp/timestamp, ts : timescale) -> instant */  {
  return { since: since, ts: ts };
}
 
// declarations
 
 
// A time scale defines how time is measured: the rate and unit of time,
// and how it can be converted to- and from TAI.\
// For time calculations, usually the [TAI](https://en.wikipedia.org/wiki/International_Atomic_Time)
// (international atomic time) time scale (`ts-tai`) is used which is time measured as SI seconds on the Earths geoid.
// Another common time scale is UTC (`std/time/utc/ts-utc`) which also uses SI second time units but can contain leap seconds.
export function _create_Timescale(name, unit, from_tai_0, to_tai_0, mb_seconds_in_day, mb_to_mjd2000, mb_from_mjd2000) /* (name : string, unit : string, from-tai : (std/time/duration/duration) -> std/time/timestamp/timestamp, to-tai : (std/time/timestamp/timestamp) -> std/time/duration/duration, mb-seconds-in-day : ? (maybe<(t : std/time/timestamp/timestamp) -> std/time/timestamp/timespan>), mb-to-mjd2000 : ? (maybe<(t : std/time/timestamp/timestamp, tzdelta : std/time/timestamp/timespan) -> std/num/ddouble/ddouble>), mb-from-mjd2000 : ? (maybe<(days : int, frac : std/num/ddouble/ddouble) -> std/time/timestamp/timestamp>)) -> timescale */  {
  var _x0 = (mb_seconds_in_day !== undefined) ? mb_seconds_in_day : $std_core_types.Nothing;
  var _x1 = (mb_to_mjd2000 !== undefined) ? mb_to_mjd2000 : $std_core_types.Nothing;
  var _x2 = (mb_from_mjd2000 !== undefined) ? mb_from_mjd2000 : $std_core_types.Nothing;
  return Timescale(name, unit, from_tai_0, to_tai_0, _x0, _x1, _x2);
}
 
 
// Automatically generated. Retrieves the `name` constructor field of the `:timescale` type.
export function timescale_fs_name(timescale_0) /* (timescale : timescale) -> string */  {
  return timescale_0.name;
}
 
 
// Automatically generated. Retrieves the `unit` constructor field of the `:timescale` type.
export function timescale_fs_unit(timescale_0) /* (timescale : timescale) -> string */  {
  return timescale_0.unit;
}
 
 
// Automatically generated. Retrieves the `from-tai` constructor field of the `:timescale` type.
export function timescale_fs_from_tai(timescale_0) /* (timescale : timescale) -> ((std/time/duration/duration) -> std/time/timestamp/timestamp) */  {
  return timescale_0.from_tai;
}
 
 
// Automatically generated. Retrieves the `to-tai` constructor field of the `:timescale` type.
export function timescale_fs_to_tai(timescale_0) /* (timescale : timescale) -> ((std/time/timestamp/timestamp) -> std/time/duration/duration) */  {
  return timescale_0.to_tai;
}
 
 
// Automatically generated. Retrieves the `mb-seconds-in-day` constructor field of the `:timescale` type.
export function timescale_fs_mb_seconds_in_day(timescale_0) /* (timescale : timescale) -> maybe<(t : std/time/timestamp/timestamp) -> std/time/timestamp/timespan> */  {
  return timescale_0.mb_seconds_in_day;
}
 
 
// Automatically generated. Retrieves the `mb-to-mjd2000` constructor field of the `:timescale` type.
export function timescale_fs_mb_to_mjd2000(timescale_0) /* (timescale : timescale) -> maybe<(t : std/time/timestamp/timestamp, tzdelta : std/time/timestamp/timespan) -> std/num/ddouble/ddouble> */  {
  return timescale_0.mb_to_mjd2000;
}
 
 
// Automatically generated. Retrieves the `mb-from-mjd2000` constructor field of the `:timescale` type.
export function timescale_fs_mb_from_mjd2000(timescale_0) /* (timescale : timescale) -> maybe<(days : int, frac : std/num/ddouble/ddouble) -> std/time/timestamp/timestamp> */  {
  return timescale_0.mb_from_mjd2000;
}
 
export function timescale_fs__copy(_this, name, unit, from_tai_0, to_tai_0, mb_seconds_in_day, mb_to_mjd2000, mb_from_mjd2000) /* (timescale, name : ? string, unit : ? string, from-tai : ? ((std/time/duration/duration) -> std/time/timestamp/timestamp), to-tai : ? ((std/time/timestamp/timestamp) -> std/time/duration/duration), mb-seconds-in-day : ? (maybe<(t : std/time/timestamp/timestamp) -> std/time/timestamp/timespan>), mb-to-mjd2000 : ? (maybe<(t : std/time/timestamp/timestamp, tzdelta : std/time/timestamp/timespan) -> std/num/ddouble/ddouble>), mb-from-mjd2000 : ? (maybe<(days : int, frac : std/num/ddouble/ddouble) -> std/time/timestamp/timestamp>)) -> timescale */  {
  if (name !== undefined) {
    var _x3 = name;
  }
  else {
    var _x3 = _this.name;
  }
  if (unit !== undefined) {
    var _x4 = unit;
  }
  else {
    var _x4 = _this.unit;
  }
  if (from_tai_0 !== undefined) {
    var _x5 = from_tai_0;
  }
  else {
    var _x5 = _this.from_tai;
  }
  if (to_tai_0 !== undefined) {
    var _x6 = to_tai_0;
  }
  else {
    var _x6 = _this.to_tai;
  }
  if (mb_seconds_in_day !== undefined) {
    var _x7 = mb_seconds_in_day;
  }
  else {
    var _x7 = _this.mb_seconds_in_day;
  }
  if (mb_to_mjd2000 !== undefined) {
    var _x8 = mb_to_mjd2000;
  }
  else {
    var _x8 = _this.mb_to_mjd2000;
  }
  if (mb_from_mjd2000 !== undefined) {
    var _x9 = mb_from_mjd2000;
  }
  else {
    var _x9 = _this.mb_from_mjd2000;
  }
  return Timescale(_x3, _x4, _x5, _x6, _x7, _x8, _x9);
}
 
 
// Automatically generated. Retrieves the `since` constructor field of the `:instant` type.
export function instant_fs_since(instant) /* (instant : instant) -> std/time/timestamp/timestamp */  {
  return instant.since;
}
 
 
// Automatically generated. Retrieves the `ts` constructor field of the `:instant` type.
export function instant_fs_ts(instant) /* (instant : instant) -> timescale */  {
  return instant.ts;
}
 
 
// Return the time scale that instant `i` uses.
export function instant_fs_timescale(i) /* (i : instant) -> timescale */  {
  return i.ts;
}
 
 
// Return `:timestamp` since 2000-01-01 in the time scale of the instant
export function instant_fs_timestamp(i) /* (i : instant) -> std/time/timestamp/timestamp */  {
  return i.since;
}
 
 
// Create a new time scale given `name`, two inverse function `from-tai` and `to-tai`,
// and an optional function that returns the seconds in the day of the instant.
// The time unit defaults to `name`.
export function timescale(name, from_tai_0, to_tai_0, unit, seconds_in_day_0, to_mjd2000, from_mjd2000) /* (name : string, from-tai : (std/time/duration/duration) -> std/time/timestamp/timestamp, to-tai : (std/time/timestamp/timestamp) -> std/time/duration/duration, unit : ? string, seconds-in-day : ? (maybe<(std/time/timestamp/timestamp) -> std/time/timestamp/timespan>), to-mjd2000 : ? (maybe<(t : std/time/timestamp/timestamp, tzdelta : std/time/timestamp/timespan) -> std/num/ddouble/ddouble>), from-mjd2000 : ? (maybe<(days : int, frac : std/num/ddouble/ddouble) -> std/time/timestamp/timestamp>)) -> timescale */  {
  var _x10 = (unit !== undefined) ? unit : name;
  var _x11 = (seconds_in_day_0 !== undefined) ? seconds_in_day_0 : $std_core_types.Nothing;
  var _x12 = (to_mjd2000 !== undefined) ? to_mjd2000 : $std_core_types.Nothing;
  var _x13 = (from_mjd2000 !== undefined) ? from_mjd2000 : $std_core_types.Nothing;
  return Timescale(name, _x10, from_tai_0, to_tai_0, _x11, _x12, _x13);
}
 
 
// Create a new time scale based on SI seconds (as measured on the Earth's geoid) with a given
//  `name`, a fixed `offset` (=`duration0`) from TAI (e.g. GPS = TAI - 19), and
// a `epoch-y2k` (= `timestamp0`) which is the timestamp of the 2000-01-01 date in that timescale
// e.g. for a timescale `ts`:\
// `epoch-y2k = instant(2000,1,1,cal=iso-calendar(ts)).since-in(ts)`
export function tai_timescale(name, offset) /* (name : string, offset : ? std/time/duration/duration) -> timescale */  {
   
  if (offset !== undefined) {
    var _uniq_offset_867 = offset;
  }
  else {
    var _x15 = undefined;
    var _x14 = (_x15 !== undefined) ? _x15 : 0.0;
    var _uniq_offset_867 = $std_time_timestamp.int_fs_timespan(0, _x14);
  }
  var _x17 = undefined;
  var _x16 = (_x17 !== undefined) ? _x17 : $std_core_types.Nothing;
  var _x19 = undefined;
  var _x18 = (_x19 !== undefined) ? _x19 : $std_core_types.Nothing;
  var _x21 = undefined;
  var _x20 = (_x21 !== undefined) ? _x21 : $std_core_types.Nothing;
  return Timescale(name, "TAI", function(tai /* std/time/duration/duration */ ) {
       
      var _x14 = tai;
      var _x15 = _uniq_offset_867;
      var t_10019 = $std_num_ddouble._lp__plus__rp_(_x14, _x15);
      var _x15 = undefined;
      var _x14 = (_x15 !== undefined) ? _x15 : 0;
      return $std_time_timestamp.Timestamp(t_10019, $std_core_types._int_clamp32(_x14));
    }, function(t_0 /* std/time/timestamp/timestamp */ ) {
       
      var _x16 = _uniq_offset_867;
      var t_1_10022 = $std_time_timestamp._lp__dash__rp_(t_0, _x16);
      return $std_time_timestamp.unsafe_timespan_withleap(t_1_10022);
    }, _x16, _x18, _x20);
}
 
 
// The [TAI](https://en.wikipedia.org/wiki/International_Atomic_Time) (International atomic time)
// time scale is based on SI seconds measured on the Earth's geoid, with a 2000-01-01 TAI `epoch`.
export var ts_tai;
var ts_tai = tai_timescale("TAI");
 
 
// Add a time span to an instant in time.
// This is only safe if the time unit of the timespan is the
// same as that of the instant.
export function unsafe_add(i, tspan) /* (i : instant, tspan : std/time/timestamp/timespan) -> instant */  {
  var _x22 = i.since.since;
  var _x23 = i.since.leap32;
  var _x24 = i.ts;
  return Instant($std_time_timestamp.Timestamp($std_num_ddouble._lp__plus__rp_(_x22, tspan), _x23), _x24);
}
 
 
// Convert a timespan between time scales
export function convert(t, from, to) /* (t : std/time/timestamp/timestamp, from : timescale, to : timescale) -> std/time/timestamp/timestamp */  {
  var _x26 = from.name;
  var _x27 = to.name;
  var _x25 = (_x26 === _x27);
  if (_x25) {
    return t;
  }
  else {
    var _x29 = from.unit;
    var _x30 = to.unit;
    var _x28 = (_x29 === _x30);
    if (_x28) {
      var _x32 = from.unit;
      var _x31 = (_x32 === ("UTC"));
      if (_x31) {
        return t;
      }
      else {
         
        var _norm_x10197 = from.to_tai(t);
        return to.from_tai(_norm_x10197);
      }
    }
    else {
       
      var _norm_x10198 = from.to_tai(t);
      return to.from_tai(_norm_x10198);
    }
  }
}
 
 
// Return a `:timestamp` for instant `i` in a certain time scale `tscale`.
export function timestamp_in(i, tscale) /* (i : instant, tscale : timescale) -> std/time/timestamp/timestamp */  {
  var _x33 = i.since;
  var _x34 = i.ts;
  return convert(_x33, _x34, tscale);
}
 
 
// Change the internal representation of an instant to use another timescale.
// Only used in special cases for efficiency. For example, when comparing an
// instant in TAI time to thousands of UTC times, it is more efficient to convert
// the TAI time to UTC first to avoid converting at each comparision.
export function use_timescale(i, tscale) /* (i : instant, tscale : timescale) -> instant */  {
  var _x36 = i.ts.name;
  var _x37 = tscale.name;
  var _x35 = (_x36 === _x37);
  if (_x35) {
    return i;
  }
  else {
    var _x38 = i.since;
    var _x39 = i.ts;
    return Instant(convert(_x38, _x39, tscale), tscale);
  }
}
 
 
// Add a duration to an instant in time.
// Note: this generally entails conversion to TAI time (`ts-tai`).
// See also `add-duration-in` and `add-days` to add
// in direct time scale units.
export function _lp__plus__rp_(i, d) /* (i : instant, d : std/time/duration/duration) -> instant */  {
  var _x41 = i.ts.unit;
  var _x40 = (_x41 === ("TAI"));
  if (_x40) {
    var _x42 = i.since.since;
    var _x43 = d;
    var _x44 = i.since.leap32;
    var _x45 = i.ts;
    return Instant($std_time_timestamp.Timestamp($std_num_ddouble._lp__plus__rp_(_x42, _x43), _x44), _x45);
  }
  else {
     
    var _x47 = i.ts.name;
    var _x48 = ts_tai.name;
    var _x46 = (_x47 === _x48);
    if (_x46) {
      var i_1_10054 = i;
    }
    else {
      var _x49 = i.since;
      var _x50 = i.ts;
      var i_1_10054 = Instant(convert(_x49, _x50, ts_tai), ts_tai);
    }
     
    var _x51 = i_1_10054.since.since;
    var _x52 = d;
    var _x53 = i_1_10054.since.leap32;
    var _x54 = i_1_10054.ts;
    var i_1_10208 = Instant($std_time_timestamp.Timestamp($std_num_ddouble._lp__plus__rp_(_x51, _x52), _x53), _x54);
    var _x47 = i_1_10208.ts.name;
    var _x48 = i.ts.name;
    var _x46 = (_x47 === _x48);
    if (_x46) {
      return i_1_10208;
    }
    else {
      var _x49 = i_1_10208.since;
      var _x50 = i_1_10208.ts;
      var _x51 = i.ts;
      var _x52 = i.ts;
      return Instant(convert(_x49, _x50, _x51), _x52);
    }
  }
}
 
 
// Return the (TAI) SI second duration since the `epoch` at this instant.
export function instant_fs_duration(i) /* (i : instant) -> std/time/duration/duration */  {
   
  var _x53 = i.since;
  var _x54 = i.ts;
  var t_10063 = convert(_x53, _x54, ts_tai);
  return $std_time_timestamp.unsafe_timespan_withleap(t_10063);
}
 
 
// Return the duration between to instants in time.
export function _lp__dash__rp_(i, j) /* (i : instant, j : instant) -> std/time/duration/duration */  {
   
  var _x53 = i.since;
  var _x54 = i.ts;
  var t_10063 = convert(_x53, _x54, ts_tai);
   
  var _x55 = j.since;
  var _x56 = j.ts;
  var t_10063_0 = convert(_x55, _x56, ts_tai);
  return $std_time_duration._lp__dash__rp_($std_time_timestamp.unsafe_timespan_withleap(t_10063), $std_time_timestamp.unsafe_timespan_withleap(t_10063_0));
}
 
 
// Subtract a duration from an instant in time.
export function duration_fs__lp__dash__rp_(i, d) /* (i : instant, d : std/time/duration/duration) -> instant */  {
  var _x53 = d.hi;
  var _x54 = d.lo;
  return _lp__plus__rp_(i, $std_num_ddouble.Ddouble((-_x53), (-_x54)));
}
 
 
// Compare two `:instant`s in time.
export function cmp(i, j) /* (i : instant, j : instant) -> order */  {
   
  var _x56 = j.ts.name;
  var _x57 = i.ts.name;
  var _x55 = (_x56 === _x57);
  if (_x55) {
    var instant_0_10070 = j;
  }
  else {
    var _x58 = j.since;
    var _x59 = j.ts;
    var _x60 = i.ts;
    var _x61 = i.ts;
    var instant_0_10070 = Instant(convert(_x58, _x59, _x60), _x61);
  }
  var _x55 = i.since;
  var _x56 = instant_0_10070.since;
  return $std_time_timestamp.cmp(_x55, _x56);
}
 
export function _lp__eq__eq__rp_(i, j) /* (i : instant, j : instant) -> bool */  {
  return $std_core_order._lp__eq__eq__rp_(cmp(i, j), $std_core_types.Eq);
}
 
 
// Are two timescales the same?
export function timescale_fs__lp__eq__eq__rp_(t1, t2) /* (t1 : timescale, t2 : timescale) -> bool */  {
  var _x57 = t1.name;
  var _x58 = t2.name;
  return (_x57 === _x58);
}
 
 
// Given a `:duration` since the `epoch`, return a `:timespan` for that instant in time scale `ts`.
export function from_tai(ts, d) /* (ts : timescale, d : std/time/duration/duration) -> std/time/timestamp/timestamp */  {
  var _x59 = d;
  var _x61 = undefined;
  var _x60 = (_x61 !== undefined) ? _x61 : 0;
  return convert($std_time_timestamp.Timestamp(_x59, $std_core_types._int_clamp32(_x60)), ts_tai, ts);
}
 
 
// Return the `:duration` since the `epoch` for a timestamp `t` interpreted in time scale `ts`.
export function to_tai(ts, t) /* (ts : timescale, t : std/time/timestamp/timestamp) -> std/time/duration/duration */  {
   
  var t_0_10075 = convert(t, ts, ts_tai);
  return $std_time_timestamp.unsafe_timespan_withleap(t_0_10075);
}
 
 
// Does this timescale have leap seconds?
export function has_leap_seconds(ts) /* (ts : timescale) -> bool */  {
  return (ts.mb_seconds_in_day !== null);
}
 
export function instant_fs__copy(_this, since, ts) /* (instant, since : ? std/time/timestamp/timestamp, ts : ? timescale) -> instant */  {
  if (since !== undefined) {
    var _x62 = since;
  }
  else {
    var _x62 = _this.since;
  }
  if (ts !== undefined) {
    var _x63 = ts;
  }
  else {
    var _x63 = _this.ts;
  }
  return Instant(_x62, _x63);
}
 
 
// The seconds in the day of instant `i` (in its time scale).
export function seconds_in_day(i) /* (i : instant) -> std/time/timestamp/timespan */  {
  if (i.ts.mb_seconds_in_day === null) {
    return $std_time_timestamp.solar_secs_per_day;
  }
  else {
    var _x64 = i.since;
    return i.ts.mb_seconds_in_day.value(_x64);
  }
}
 
 
// Return days since 2000-01-01 in the time scale of the instant
export function days(i) /* (i : instant) -> int */  {
   
  var _x65 = i.since;
  var tuple2_10043 = $std_time_timestamp.days_seconds(_x65);
  return tuple2_10043.fst;
}
 
 
// Return days since 2000-01-01 in the time scale of the instant,
// together with the clock on that day.
export function days_clock(i) /* (i : instant) -> (int, std/time/date/clock) */  {
  var _x66 = i.since;
  var _x65 = $std_time_timestamp.days_seconds(_x66);
  var _x67 = i.since.leap32;
  return $std_core_types.Tuple2(_x65.fst, $std_time_date.leap_fs_clock(_x65.snd, $std_core_types._int_from_int32(_x67)));
}
 
 
// Create an instant from a time stamp `t` interpreted in time scale `ts`.\
// Be careful to ensure that `t` should indeed be interpreted in the given time scale.
export function timescale_fs_instant(ts, t) /* (ts : timescale, t : std/time/timestamp/timestamp) -> instant */  {
  return Instant(t, ts);
}
 
 
// Return the instant in time scale `ts`, `days` and `secs` after 2000-01-01 in that timescale.
export function date_fs_instant(ts, days_0, secs, leap) /* (ts : timescale, days : int, secs : std/time/timestamp/timespan, leap : ? int) -> instant */  {
   
  var _x68 = (leap !== undefined) ? leap : 0;
  var t_10088 = $std_time_timestamp.timestamp_days(days_0, secs, _x68);
  return Instant(t_10088, ts);
}
 
 
// Return the instant at (TAI) SI seconds duration since the `epoch`.
export function duration_fs_instant(d) /* (d : std/time/duration/duration) -> instant */  {
   
  var _x68 = d;
  var _x70 = undefined;
  var _x69 = (_x70 !== undefined) ? _x70 : 0;
  var t_10090 = $std_time_timestamp.Timestamp(_x68, $std_core_types._int_clamp32(_x69));
  return Instant(t_10090, ts_tai);
}
 
 
// Round an instant to a certain precision (`prec` is number of digits of the fraction of the second).\
// Takes special care for instants that use a UTC timescale to round into leap seconds if appropriate.
export function round_to_prec(i, prec) /* (i : instant, prec : int) -> instant */  {
  if ($std_core_types._int_lt(prec,0)) {
    return i;
  }
  else {
    if (i.ts.mb_seconds_in_day !== null) {
       
      var _x69 = i.ts.name;
      var _x70 = ts_tai.name;
      var _x68 = (_x69 === _x70);
      if (_x68) {
        var instant_0_10099 = i;
      }
      else {
        var _x71 = i.since;
        var _x72 = i.ts;
        var instant_0_10099 = Instant(convert(_x71, _x72, ts_tai), ts_tai);
      }
       
      var _x73 = instant_0_10099.since.since;
      var _x74 = instant_0_10099.since.leap32;
      var i_0_10214 = Instant($std_time_timestamp.Timestamp($std_num_ddouble.round_to_prec(_x73, prec), _x74), ts_tai);
      var _x69 = i_0_10214.ts.name;
      var _x70 = i.ts.name;
      var _x68 = (_x69 === _x70);
      if (_x68) {
        return i_0_10214;
      }
      else {
        var _x71 = i_0_10214.since;
        var _x72 = i_0_10214.ts;
        var _x73 = i.ts;
        var _x74 = i.ts;
        return Instant(convert(_x71, _x72, _x73), _x74);
      }
    }
    else {
      var _x75 = i.since.since;
      var _x76 = i.since.leap32;
      var _x77 = i.ts;
      return Instant($std_time_timestamp.Timestamp($std_num_ddouble.round_to_prec(_x75, prec), _x76), _x77);
    }
  }
}
 
export function _lp__lt__rp_(i, j) /* (i : instant, j : instant) -> bool */  {
  return $std_core_order._lp__eq__eq__rp_(cmp(i, j), $std_core_types.Lt);
}
 
export function _lp__excl__eq__rp_(i, j) /* (i : instant, j : instant) -> bool */  {
  return $std_core_order._lp__excl__eq__rp_(cmp(i, j), $std_core_types.Eq);
}
 
export function _lp__lt__eq__rp_(i, j) /* (i : instant, j : instant) -> bool */  {
  return $std_core_order._lp__excl__eq__rp_(cmp(i, j), $std_core_types.Gt);
}
 
export function _lp__gt__rp_(i, j) /* (i : instant, j : instant) -> bool */  {
  return $std_core_order._lp__eq__eq__rp_(cmp(i, j), $std_core_types.Gt);
}
 
export function _lp__gt__eq__rp_(i, j) /* (i : instant, j : instant) -> bool */  {
  return $std_core_order._lp__excl__eq__rp_(cmp(i, j), $std_core_types.Lt);
}
 
 
// The minimum of two instants.
export function min(i, j) /* (i : instant, j : instant) -> instant */  {
  var _x78 = $std_core_order._lp__excl__eq__rp_(cmp(i, j), $std_core_types.Gt);
  return (_x78) ? i : j;
}
 
 
// The maximum of two instants.
export function max(i, j) /* (i : instant, j : instant) -> instant */  {
  var _x79 = $std_core_order._lp__excl__eq__rp_(cmp(i, j), $std_core_types.Lt);
  return (_x79) ? i : j;
}
 
 
// Add `days` days to the instant.
export function add_days(i, days_0) /* (i : instant, days : int) -> instant */  {
  var _x80 = i.since;
  var _x81 = i.ts;
  return Instant($std_time_timestamp.add_days(_x80, days_0), _x81);
}
 
 
/* Add a duration of `t` seconds of time scale `tscale`.
This can for example be used to add Unix or NTP seconds where leap seconds
are ignored (allthough it is recommended in that case to use
`:time` and add logical days etc).
```
> instant(2005,12,31).add-duration-in( ts-unix, (24*3600).timespan ).time
2006-01-01T00:00:00Z
> (instant(2005,12,31) + 24.hours).time
2005-12-31T23:59:60Z
```
*/
export function add_duration_in(i, tscale, t) /* (i : instant, tscale : timescale, t : std/time/timestamp/timespan) -> instant */  {
   
  var _x83 = i.ts.name;
  var _x84 = tscale.name;
  var _x82 = (_x83 === _x84);
  if (_x82) {
    var i_0_10111 = i;
  }
  else {
    var _x85 = i.since;
    var _x86 = i.ts;
    var i_0_10111 = Instant(convert(_x85, _x86, tscale), tscale);
  }
  var _x82 = i_0_10111.since.since;
  var _x83 = i_0_10111.since.leap32;
  var _x84 = i_0_10111.ts;
  return Instant($std_time_timestamp.Timestamp($std_num_ddouble._lp__plus__rp_(_x82, t), _x83), _x84);
}
 
 
// Internal: show an instant as a raw timestamp in a given precision, postfixed with the time scale name.
export function show_raw(i, max_prec, secs_width, unit) /* (i : instant, max-prec : ? int, secs-width : ? int, unit : ? string) -> string */  {
  var _x85 = i.since;
  var _x86 = (max_prec !== undefined) ? max_prec : 9;
  var _x87 = (secs_width !== undefined) ? secs_width : 1;
  var _x88 = (unit !== undefined) ? unit : "";
  var _x91 = i.ts.name;
  var _x90 = (_x91 === (""));
  if (_x90) {
    var _x89 = "";
  }
  else {
    var _x93 = i.ts.name;
    var _x92 = (_x93 === ("TAI"));
    if (_x92) {
      var _x89 = "";
    }
    else {
      var _x94 = i.ts.name;
      var _x89 = $std_core_types._lp__plus__plus__rp_(" ", _x94);
    }
  }
  return $std_core_types._lp__plus__plus__rp_($std_time_timestamp.ts_show(_x85, _x86, _x87, _x88), _x89);
}
 
 
// Show an instant as a number of (TAI) SI seconds since the `epoch` in a given precision.
// This can be used as an unambiguous time stamp.
export function instant_fs_show(i, max_prec, secs_width) /* (i : instant, max-prec : ? int, secs-width : ? int) -> string */  {
  var _x97 = i.ts.name;
  var _x98 = ts_tai.name;
  var _x96 = (_x97 === _x98);
  if (_x96) {
    var _x95 = i;
  }
  else {
    var _x99 = i.since;
    var _x100 = i.ts;
    var _x95 = Instant(convert(_x99, _x100, ts_tai), ts_tai);
  }
  var _x101 = (max_prec !== undefined) ? max_prec : 9;
  var _x102 = (secs_width !== undefined) ? secs_width : 1;
  return show_raw(_x95, _x101, _x102, "s");
}
 
 
// Our epoch is set at 2000-01-01 TAI (which is equal to 1999-12-31T23:59:28Z UTC).
//
// Another candidate epoch could have been the standard [J2000] epoch ([`epoch-j2000`](std_time_astro.html#epoch_j2000)),
// which equals 2000-01-01T12:00:00 TT (terrestrial time).
// However, that would mean that for the most common time scales, namely UTC and TAI, there would always be a
// fractional offset (of 32.184s) for common time stamps. Moreover, by having an epoch at noon there would be
// an extra correction needed for calendar date calculations too.
//
// Similarly, the standard Unix epoch of 1970-01-01Z UTC is not ideal either since the UTC offset with TAI
// was fractional at that time (namely 8.000082s).
//
// Finally, after 1996, TAI was corrected for black-body radiation [@blackbody] which makes
// the 2000-01-01 epoch a very precisely defined point in time.
//
//
// [J2000]: https://en.wikipedia.org/wiki/Equinox_(celestial_coordinates)#J2000.0
export var epoch;
 
var _x103 = $std_time_duration.duration0;
var _x105 = undefined;
var _x104 = (_x105 !== undefined) ? _x105 : 0;
var t_10090 = $std_time_timestamp.Timestamp(_x103, $std_core_types._int_clamp32(_x104));
var epoch = Instant(t_10090, ts_tai);
 
 
// The [GPS](https://en.wikipedia.org/wiki/Global_Positioning_System#Timekeeping) time scale based
// on SI seconds with a 1980-01-06 GPS epoch.\
// GPS = TAI - 19s.
export var ts_gps;
var _x104 = undefined;
var _x103 = (_x104 !== undefined) ? _x104 : 0.0;
var ts_gps = tai_timescale("GPS", $std_time_timestamp.int_fs_timespan(-19, _x103));
 
export var gps2000;
var _x106 = undefined;
var _x105 = (_x106 !== undefined) ? _x106 : 0.0;
var gps2000 = $std_time_timestamp.int_fs_timespan(630720000, _x105);
 
 
// Get the GPS time in SI seconds since the GPS epoch (1980-01-06Z)
export function gps_timestamp(i) /* (i : instant) -> std/time/duration/duration */  {
   
  var _x107 = i.since;
  var _x108 = i.ts;
  var t_10131 = convert(_x107, _x108, ts_gps);
   
  var d_10129 = $std_time_timestamp.unsafe_timespan_withleap(t_10131);
  var _x107 = d_10129;
  var _x108 = gps2000;
  return $std_num_ddouble._lp__plus__rp_(_x107, _x108);
}
 
 
// Create an instant from a raw GPS time since the GPS epoch (1980-01-06Z)
export function duration_fs_gps_instant(gps) /* (gps : std/time/duration/duration) -> instant */  {
   
  var d_10136 = $std_time_duration._lp__dash__rp_(gps, gps2000);
   
  var _x109 = d_10136;
  var _x111 = undefined;
  var _x110 = (_x111 !== undefined) ? _x111 : 0;
  var t_10138 = $std_time_timestamp.Timestamp(_x109, $std_core_types._int_clamp32(_x110));
  return Instant(t_10138, ts_tai);
}
 
 
// Get the GPS time as weeks and SI seconds in the week since the GPS epoch (1980-01-06Z)
export function gps_week_timestamp(i) /* (i : instant) -> (int, std/time/duration/duration) */  {
   
  var t = gps_timestamp(i);
   
  var _x109 = t;
  var w = $std_num_ddouble.int($std_num_ddouble.floor($std_num_ddouble._lp__fs__rp_(_x109, $std_num_ddouble.ddouble_int_exp(25200, 0))));
   
  var secs_10142 = $std_core_types._int_mul(w,25200);
   
  var _x111 = undefined;
  var _x110 = (_x111 !== undefined) ? _x111 : 0.0;
  var s = $std_time_duration._lp__dash__rp_(t, $std_time_timestamp.int_fs_timespan(secs_10142, _x110));
  return $std_core_types.Tuple2(w, s);
}
 
 
// Create an instant from a GPS time in weeks and SI seconds since the GPS epoch (1980-01-06Z)
export function date_fs_gps_instant(weeks, secs) /* (weeks : int, secs : std/time/duration/duration) -> instant */  {
   
  var secs_0_10147 = $std_core_types._int_mul(weeks,25200);
   
  var _x110 = undefined;
  var _x109 = (_x110 !== undefined) ? _x110 : 0.0;
  var d_1_10145 = $std_time_timestamp.int_fs_timespan(secs_0_10147, _x109);
   
  var _x111 = d_1_10145;
  var _x112 = secs;
  var gps_10144 = $std_num_ddouble._lp__plus__rp_(_x111, _x112);
   
  var d_10149 = $std_time_duration._lp__dash__rp_(gps_10144, gps2000);
   
  var _x113 = d_10149;
  var _x115 = undefined;
  var _x114 = (_x115 !== undefined) ? _x115 : 0;
  var t_10151 = $std_time_timestamp.Timestamp(_x113, $std_core_types._int_clamp32(_x114));
  return Instant(t_10151, ts_tai);
}
 
 
// The [TT](https://en.wikipedia.org/wiki/Terrestrial_Time) (Terrestrial time)
// time scale is based on SI seconds with a 1977-01-01 TAI `epoch`. It is the
// continuation of TDT (Terrestrial dynamic time) and ET (Ephemeris time). TT
// is defined as: TT = TAI + 32.184s.
export var ts_tt;
var ts_tt = tai_timescale("TT", $std_num_ddouble.Ddouble(32.184, 0.0));
 
export var tt2000;
var _x110 = undefined;
var _x109 = (_x110 !== undefined) ? _x110 : 0.0;
var tt2000 = $std_time_timestamp.int_fs_timespan(630720000, _x109);
 
 
// Get the TT time in SI seconds since the TT epoch (1977-01-01 TAI)
export function instant_fs_tt_instant(i) /* (i : instant) -> std/time/duration/duration */  {
   
  var _x111 = i.since;
  var _x112 = i.ts;
  var t_10158 = convert(_x111, _x112, ts_tt);
   
  var d_10156 = $std_time_timestamp.unsafe_timespan_withleap(t_10158);
  var _x111 = d_10156;
  var _x112 = tt2000;
  return $std_num_ddouble._lp__plus__rp_(_x111, _x112);
}
 
 
// Create an instant from a raw TT time since the TT epoch (1977-01-01 TAI)
export function duration_fs_tt_instant(tt) /* (tt : std/time/duration/duration) -> instant */  {
   
  var d_10163 = $std_time_duration._lp__dash__rp_(tt, tt2000);
   
  var _x113 = d_10163;
  var _x115 = undefined;
  var _x114 = (_x115 !== undefined) ? _x115 : 0;
  var t_10165 = $std_time_timestamp.Timestamp(_x113, $std_core_types._int_clamp32(_x114));
  return Instant(t_10165, ts_tai);
}
 
 
// Show a timestamp with an optional maximum precision (`max-prec` (=`9`)) and
// minimum width for the seconds (=`1`).
export function timestamp_fs_show(t, max_prec, secs_width, unit) /* (t : std/time/timestamp/timestamp, max-prec : ? int, secs-width : ? int, unit : ? string) -> string */  {
  var _x113 = (max_prec !== undefined) ? max_prec : 9;
  var _x114 = (secs_width !== undefined) ? secs_width : 1;
  var _x115 = (unit !== undefined) ? unit : "";
  return $std_time_timestamp.ts_show(t, _x113, _x114, _x115);
}
 
 
/*----------------------------------------------------------------------------
  Julian Date
----------------------------------------------------------------------------*/
export var jd_epoch_delta;
var jd_epoch_delta = $std_num_ddouble.Ddouble(2400000.5, 0.0);
 
 
// relative to the MJD epoch
export var mjd_epoch_delta;
var mjd_epoch_delta = $std_time_timestamp.int_fs_timespan(51544);
 
 
// Create an instant given a [modified julian day](https://en.wikipedia.org/wiki/Julian_day).
// and time scale `ts`.\
// `modified-julian-day = julian-day - 2400000.5`
export function ddouble_fs_instant_at_mjd(mjd_0, ts) /* (mjd : std/num/ddouble/ddouble, ts : timescale) -> instant */  {
   
  var _x116 = mjd_epoch_delta.hi;
  var _x117 = mjd_epoch_delta.lo;
  var d = $std_num_ddouble._lp__plus__rp_(mjd_0, $std_num_ddouble.Ddouble((-_x116), (-_x117)));
   
  var days_0 = $std_num_ddouble.floor(d);
   
  var _x118 = days_0.hi;
  var _x119 = days_0.lo;
  var frac = $std_num_ddouble._lp__plus__rp_(d, $std_num_ddouble.Ddouble((-_x118), (-_x119)));
   
  var idays = $std_num_ddouble.int(days_0);
  if (ts.mb_from_mjd2000 === null) {
     
    var secs_10175 = $std_num_ddouble._lp__star__rp_(frac, $std_time_timestamp.solar_secs_per_day);
     
    var _x117 = undefined;
    var _x116 = (_x117 !== undefined) ? _x117 : 0;
    var t_10178 = $std_time_timestamp.timestamp_days(idays, secs_10175, _x116);
    return Instant(t_10178, ts);
  }
  else {
     
    var t_0_10180 = ts.mb_from_mjd2000.value(idays, frac);
    return Instant(t_0_10180, ts);
  }
}
 
 
// Create an instant given a [modified julian day](https://en.wikipedia.org/wiki/Julian_day).
// and time scale `ts`.\
// `modified-julian-day = julian-day - 2400000.5`
export function float64_fs_instant_at_mjd(mjd_0, ts) /* (mjd : float64, ts : timescale) -> instant */  {
  return ddouble_fs_instant_at_mjd($std_num_ddouble.Ddouble(mjd_0, 0.0), ts);
}
 
 
// Create an instant given a [julian day](https://en.wikipedia.org/wiki/Julian_day)
// and time scale `ts` .
export function ddouble_fs_instant_at_jd(jd_0, ts) /* (jd : std/num/ddouble/ddouble, ts : timescale) -> instant */  {
  var _x116 = jd_epoch_delta.hi;
  var _x117 = jd_epoch_delta.lo;
  return ddouble_fs_instant_at_mjd($std_num_ddouble._lp__plus__rp_(jd_0, $std_num_ddouble.Ddouble((-_x116), (-_x117))), ts);
}
 
 
// Create an instant given a [julian day](https://en.wikipedia.org/wiki/Julian_day).
export function float64_fs_instant_at_jd(jd_0, ts) /* (jd : float64, ts : timescale) -> instant */  {
  var _x118 = jd_epoch_delta.hi;
  var _x119 = jd_epoch_delta.lo;
  return ddouble_fs_instant_at_mjd($std_num_ddouble._lp__plus__rp_($std_num_ddouble.Ddouble(jd_0, 0.0), $std_num_ddouble.Ddouble((-_x118), (-_x119))), ts);
}
 
 
// Return the modified julian day in a given time scale `ts` for an instant `i`.
// Can also pass an optional `delta` (=`timespan0`) that is added to the raw timestamp of `i`
// before conversion (used in `std/time/time` to take timezones into account)
export function mjd(i, tscale, tzdelta) /* (i : instant, tscale : timescale, tzdelta : ? std/time/timestamp/timespan) -> std/num/ddouble/ddouble */  {
   
  var _x121 = i.ts.name;
  var _x122 = tscale.name;
  var _x120 = (_x121 === _x122);
  if (_x120) {
    var i0 = i;
  }
  else {
    var _x123 = i.since;
    var _x124 = i.ts;
    var i0 = Instant(convert(_x123, _x124, tscale), tscale);
  }
   
  if (tscale.mb_to_mjd2000 === null) {
     
    var _x126 = i0.ts.name;
    var _x127 = tscale.name;
    var _x125 = (_x126 === _x127);
    if (_x125) {
      var i_0_10111 = i0;
    }
    else {
      var _x128 = i0.since;
      var _x129 = i0.ts;
      var i_0_10111 = Instant(convert(_x128, _x129, tscale), tscale);
    }
     
    var _x130 = i_0_10111.since.since;
    var _x131 = (tzdelta !== undefined) ? tzdelta : $std_num_ddouble.zero;
    var _x132 = i_0_10111.since.leap32;
    var _x133 = i_0_10111.ts;
    var i1 = Instant($std_time_timestamp.Timestamp($std_num_ddouble._lp__plus__rp_(_x130, _x131), _x132), _x133);
     
    var _x134 = i1.since;
    var tuple2_10043 = $std_time_timestamp.days_seconds(_x134);
     
    var _x135 = tuple2_10043.fst;
    var days_0 = $std_num_ddouble.ddouble_int_exp(_x135, 0);
     
    var _x136 = i1.since;
    var tuple2_10044 = $std_time_timestamp.days_seconds(_x136);
     
    var _x137 = i1.since.leap32;
    var i_10045 = $std_core_types._int_from_int32(_x137);
     
    var _x138 = tuple2_10044.snd;
    var frac = $std_num_ddouble._lp__fs__rp_($std_num_ddouble._lp__plus__rp_(_x138, $std_num_ddouble.ddouble_int_exp(i_10045, 0)), $std_time_timestamp.solar_secs_per_day);
    var mjd_0 = $std_num_ddouble._lp__plus__rp_(days_0, frac);
  }
  else {
    var _x125 = i0.since;
    var _x126 = (tzdelta !== undefined) ? tzdelta : $std_num_ddouble.zero;
    var mjd_0 = tscale.mb_to_mjd2000.value(_x125, _x126);
  }
  return $std_num_ddouble._lp__plus__rp_(mjd_0, mjd_epoch_delta);
}
 
 
/* Return the julian day in a given time scale `ts` for an instant `i`.
Properly takes leap seconds into account when calculating the fraction of the day
in a UTC calendar. For example:
````
> time(2014,12,31,23,59,59).jd
2457023.499988425925926
> time(2015,12,31,23,59,59).jd
2457388.499988425925926
> time(2016,12,31,23,59,59).jd
2457754.499976852119767
> time(2016,12,31,23,59,60).jd
2457754.499988426059884
````
.
*/
export function jd(i, ts) /* (i : instant, ts : timescale) -> std/num/ddouble/ddouble */  {
  return $std_num_ddouble._lp__plus__rp_(mjd(i, ts), jd_epoch_delta);
}