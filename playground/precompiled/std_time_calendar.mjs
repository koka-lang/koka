// Koka generated module: std/time/calendar, koka version: 3.2.4
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
import * as $std_time_instant from './std_time_instant.mjs';
import * as $std_time_utc from './std_time_utc.mjs';
import * as $std_time_date from './std_time_date.mjs';
import * as $std_num_float64 from './std_num_float64.mjs';
import * as $std_num_int32 from './std_num_int32.mjs';
 
// externals
/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

function _local_get_timezone() {
  return null; // not possible in javascript, it's a global :-(
}

var dtformat = undefined;

function _local_get_timezone_abbrv(d) {
  // try to get the timezone abbreviation; expensive to initialize so do that once only;
  // even with static initialization, getTimezoneOffset and format are pretty slow...
  if (typeof dtformat === "undefined") {
    try {
      dtformat = Intl.DateTimeFormat("en-US",{timeZoneName:"short"});
    }
    catch(exn) {
      dtformat = null;
    }
  }
  return (dtformat == null ? "" : dtformat.format(d).replace(/^.*\s([\w\+\-:]{3,6})$/,"$1"));
}

function _local_utc_delta(tz,i) {
  var d = new Date(i*1000);

  // if we have a datetime format, we get the timezone abbreviation
  var abbrv = _local_get_timezone_abbrv(d);

  // Return the timezone offset (switch sign!) and the timezone abbreviation
  return $std_core_types.Tuple2( (d.getTimezoneOffset())*-60, abbrv );
}
 
// type declarations
// type timezone
export function Timezone(name, utc_delta, utc_inverse) /* (name : string, utc-delta : (std/time/instant/instant) -> (std/time/duration/duration, string), utc-inverse : (std/time/instant/instant) -> maybe<std/time/instant/instant>) -> timezone */  {
  return { name: name, utc_delta: utc_delta, utc_inverse: utc_inverse };
}
// type calendar
export function Calendar(name, long_name, month_prefix, show_era, instant_to_dc, dc_to_instant, days_to_date, date_to_days) /* (name : string, long-name : string, month-prefix : string, show-era : (std/time/date/date) -> string, instant-to-dc : (i : std/time/instant/instant, tzdelta : std/time/duration/duration) -> (std/time/date/date, std/time/date/clock), dc-to-instant : (std/time/date/date, std/time/date/clock, timezone, std/time/instant/timescale) -> std/time/instant/instant, days-to-date : (days : int) -> std/time/date/date, date-to-days : (date : std/time/date/date) -> int) -> calendar */  {
  return { name: name, long_name: long_name, month_prefix: month_prefix, show_era: show_era, instant_to_dc: instant_to_dc, dc_to_instant: dc_to_instant, days_to_date: days_to_date, date_to_days: date_to_days };
}
// type local-timezone
 
// declarations
 
 
// Automatically generated. Retrieves the `name` constructor field of the `:calendar` type.
export function calendar_fs_name(calendar) /* (calendar : calendar) -> string */  {
  return calendar.name;
}
 
 
// Automatically generated. Retrieves the `long-name` constructor field of the `:calendar` type.
export function calendar_fs_long_name(calendar) /* (calendar : calendar) -> string */  {
  return calendar.long_name;
}
 
 
// Automatically generated. Retrieves the `month-prefix` constructor field of the `:calendar` type.
export function calendar_fs_month_prefix(calendar) /* (calendar : calendar) -> string */  {
  return calendar.month_prefix;
}
 
 
// Automatically generated. Retrieves the `show-era` constructor field of the `:calendar` type.
export function calendar_fs_show_era(calendar) /* (calendar : calendar) -> ((std/time/date/date) -> string) */  {
  return calendar.show_era;
}
 
 
// Automatically generated. Retrieves the `instant-to-dc` constructor field of the `:calendar` type.
export function calendar_fs_instant_to_dc(calendar) /* (calendar : calendar) -> ((i : std/time/instant/instant, tzdelta : std/time/duration/duration) -> (std/time/date/date, std/time/date/clock)) */  {
  return calendar.instant_to_dc;
}
 
 
// Automatically generated. Retrieves the `dc-to-instant` constructor field of the `:calendar` type.
export function calendar_fs_dc_to_instant(calendar) /* (calendar : calendar) -> ((std/time/date/date, std/time/date/clock, timezone, std/time/instant/timescale) -> std/time/instant/instant) */  {
  return calendar.dc_to_instant;
}
 
 
// Automatically generated. Retrieves the `days-to-date` constructor field of the `:calendar` type.
export function calendar_fs_days_to_date(calendar) /* (calendar : calendar) -> ((days : int) -> std/time/date/date) */  {
  return calendar.days_to_date;
}
 
 
// Automatically generated. Retrieves the `date-to-days` constructor field of the `:calendar` type.
export function calendar_fs_date_to_days(calendar) /* (calendar : calendar) -> ((date : std/time/date/date) -> int) */  {
  return calendar.date_to_days;
}
 
export function calendar_fs__copy(_this, name, long_name, month_prefix, show_era, instant_to_dc, dc_to_instant, days_to_date, date_to_days) /* (calendar, name : ? string, long-name : ? string, month-prefix : ? string, show-era : ? ((std/time/date/date) -> string), instant-to-dc : ? ((i : std/time/instant/instant, tzdelta : std/time/duration/duration) -> (std/time/date/date, std/time/date/clock)), dc-to-instant : ? ((std/time/date/date, std/time/date/clock, timezone, std/time/instant/timescale) -> std/time/instant/instant), days-to-date : ? ((days : int) -> std/time/date/date), date-to-days : ? ((date : std/time/date/date) -> int)) -> calendar */  {
  if (name !== undefined) {
    var _x0 = name;
  }
  else {
    var _x0 = _this.name;
  }
  if (long_name !== undefined) {
    var _x1 = long_name;
  }
  else {
    var _x1 = _this.long_name;
  }
  if (month_prefix !== undefined) {
    var _x2 = month_prefix;
  }
  else {
    var _x2 = _this.month_prefix;
  }
  if (show_era !== undefined) {
    var _x3 = show_era;
  }
  else {
    var _x3 = _this.show_era;
  }
  if (instant_to_dc !== undefined) {
    var _x4 = instant_to_dc;
  }
  else {
    var _x4 = _this.instant_to_dc;
  }
  if (dc_to_instant !== undefined) {
    var _x5 = dc_to_instant;
  }
  else {
    var _x5 = _this.dc_to_instant;
  }
  if (days_to_date !== undefined) {
    var _x6 = days_to_date;
  }
  else {
    var _x6 = _this.days_to_date;
  }
  if (date_to_days !== undefined) {
    var _x7 = date_to_days;
  }
  else {
    var _x7 = _this.date_to_days;
  }
  return Calendar(_x0, _x1, _x2, _x3, _x4, _x5, _x6, _x7);
}
 
 
// Automatically generated. Retrieves the `name` constructor field of the `:timezone` type.
export function timezone_fs_name(timezone) /* (timezone : timezone) -> string */  {
  return timezone.name;
}
 
 
// Check if two calendars use the same date calculations. (Display of era names etc. may differ)
export function _lp__eq__eq__rp_(c1, c2) /* (c1 : calendar, c2 : calendar) -> bool */  {
  var _x8 = c1.long_name;
  var _x9 = c2.long_name;
  return (_x8 === _x9);
}
 
 
// Same timezone?
export function timezone_fs__lp__eq__eq__rp_(tz1, tz2) /* (tz1 : timezone, tz2 : timezone) -> bool */  {
  var _x10 = tz1.name;
  var _x11 = tz2.name;
  return (_x10 === _x11);
}
 
 
// A `:timezone` determines a time offset with respect to the UTC / GMT timezone. For
// using general time zones, see the [`std/time/timezone`](std_time_timezone.html) module.\
// The `tz-utc` and `tz-local` time zones are used for UTC/GMT time and the local system time.\
// The `name` field contains the time zone name as IANA timezone identifier.
// The `utc-delta` field returns the time zone offset
// relative to UTC (for GMT-08:00 it returns a duration of `-8*3600` seconds) and the abbreviated
// time zone name (``PDT`` for example).\
// The optional `utc-inverse` field returns for an instant in the time zone, the associated UTC time.
// By default it returns `Nothing` in which case a generic algorithm is used to determine the
// inverse.
export function _create_Timezone(name, utc_delta, utc_inverse) /* (name : string, utc-delta : (std/time/instant/instant) -> (std/time/duration/duration, string), utc-inverse : ? ((std/time/instant/instant) -> maybe<std/time/instant/instant>)) -> timezone */  {
  var _x12 = (utc_inverse !== undefined) ? utc_inverse : function(i /* std/time/instant/instant */ ) {
    return $std_core_types.Nothing;
  };
  return Timezone(name, utc_delta, _x12);
}
 
 
// Automatically generated. Retrieves the `utc-delta` constructor field of the `:timezone` type.
export function timezone_fs_utc_delta(timezone) /* (timezone : timezone) -> ((std/time/instant/instant) -> (std/time/duration/duration, string)) */  {
  return timezone.utc_delta;
}
 
 
// Automatically generated. Retrieves the `utc-inverse` constructor field of the `:timezone` type.
export function timezone_fs_utc_inverse(timezone) /* (timezone : timezone) -> ((std/time/instant/instant) -> maybe<std/time/instant/instant>) */  {
  return timezone.utc_inverse;
}
 
export function timezone_fs__copy(_this, name, utc_delta, utc_inverse) /* (timezone, name : ? string, utc-delta : ? ((std/time/instant/instant) -> (std/time/duration/duration, string)), utc-inverse : ? ((std/time/instant/instant) -> maybe<std/time/instant/instant>)) -> timezone */  {
  if (name !== undefined) {
    var _x13 = name;
  }
  else {
    var _x13 = _this.name;
  }
  if (utc_delta !== undefined) {
    var _x14 = utc_delta;
  }
  else {
    var _x14 = _this.utc_delta;
  }
  if (utc_inverse !== undefined) {
    var _x15 = utc_inverse;
  }
  else {
    var _x15 = _this.utc_inverse;
  }
  return Timezone(_x13, _x14, _x15);
}
 
export function string_fs_tz_fixed(name, delta, abbrv) /* (name : string, delta : std/time/duration/duration, abbrv : ? string) -> timezone */  {
  return Timezone(name, function(i_0 /* std/time/instant/instant */ ) {
      var _x16 = (abbrv !== undefined) ? abbrv : "";
      return $std_core_types.Tuple2(delta, _x16);
    }, function(i_0_0 /* std/time/instant/instant */ ) {
      var _x18 = delta.hi;
      var _x17 = (_x18 === (0.0));
      if (_x17) {
        return $std_core_types.Just(i_0_0);
      }
      else {
        return $std_core_types.Nothing;
      }
    });
}
 
 
// The standard UTC time zone with a 0 delta.
export var tz_utc;
var tz_utc = Timezone("UTC", function(i_0 /* std/time/instant/instant */ ) {
    var _x20 = undefined;
    var _x19 = (_x20 !== undefined) ? _x20 : "";
    return $std_core_types.Tuple2($std_time_duration.zero, _x19);
  }, function(i_0_0 /* std/time/instant/instant */ ) {
    var _x22 = $std_time_duration.zero.hi;
    var _x21 = (_x22 === (0.0));
    if (_x21) {
      return $std_core_types.Just(i_0_0);
    }
    else {
      return $std_core_types.Nothing;
    }
  });
 
 
// Create a time zone with a fixed number of hours and optional minutes (=`0`) difference from UTC/GMT.
// The time zone name and abbreviation is optional, and the default name
// will reflect the offset from UTC, for example `"UTC+1:20"`, or `"UTC-8"`.
// The default abbreviation is the empty string.
// The sign of the minutes will be matched to the hours (unless `hours` is `0`).
// For example, `tz-fixed(-1,30)` gives a time zone offset of -90 minutes, "UTC-1:30".
// Returns `tz-utc` if both `hours` and `mins` are zero.
export function tz_fixed(hours, mins, name, abbrv, hourwidth) /* (hours : int, mins : ? int, name : ? string, abbrv : ? string, hourwidth : ? int) -> timezone */  {
   
  var _uniq_mins_871 = (mins !== undefined) ? mins : 0;
  if ($std_core_types._int_iszero(hours)) {
    if ($std_core_types._int_iszero(_uniq_mins_871)) {
      return tz_utc;
    }
    else {
       
      if ($std_core_types._int_gt(hours,0)) {
        var xmins = $std_core_types._int_abs(_uniq_mins_871);
      }
      else {
        if ($std_core_types._int_lt(hours,0)) {
           
          var i_1_10021 = $std_core_types._int_abs(_uniq_mins_871);
          var xmins = $std_core_types._int_negate(i_1_10021);
        }
        else {
          var xmins = _uniq_mins_871;
        }
      }
       
      var x_10022 = $std_core_types._int_mul(hours,3600);
       
      var y_10023 = $std_core_types._int_mul(xmins,60);
       
      var secs = $std_core_types._int_add(x_10022,y_10023);
       
      var _x23 = ($std_core_types._int_lt(secs,0)) ? "-" : "+";
      var _x24 = (hourwidth !== undefined) ? hourwidth : 1;
      if ($std_core_types._int_iszero(_uniq_mins_871)) {
        var _x25 = "";
      }
      else {
        var _x25 = $std_core_types._lp__plus__plus__rp_(":", $std_core_string.pad_left($std_core_int.show($std_core_types._int_abs(_uniq_mins_871)), 2, 0x0030));
      }
      var y_0_10025 = $std_core_types._lp__plus__plus__rp_("UTC", $std_core_types._lp__plus__plus__rp_(_x23, $std_core_types._lp__plus__plus__rp_($std_core_string.pad_left($std_core_int.show($std_core_types._int_abs(hours)), _x24, 0x0030), _x25)));
       
      var _x27 = (name !== undefined) ? name : "";
      var _x26 = (_x27 === (""));
      if (_x26) {
        var tzonename = y_0_10025;
      }
      else {
        var tzonename = (name !== undefined) ? name : "";
      }
       
      var _x29 = undefined;
      var _x28 = (_x29 !== undefined) ? _x29 : 0.0;
      var delta = $std_time_timestamp.int_fs_timespan(secs, _x28);
      return Timezone(tzonename, function(i_0 /* std/time/instant/instant */ ) {
          var _x23 = (abbrv !== undefined) ? abbrv : "";
          return $std_core_types.Tuple2(delta, _x23);
        }, function(i_0_0 /* std/time/instant/instant */ ) {
          var _x25 = delta.hi;
          var _x24 = (_x25 === (0.0));
          if (_x24) {
            return $std_core_types.Just(i_0_0);
          }
          else {
            return $std_core_types.Nothing;
          }
        });
    }
  }
  else {
     
    if ($std_core_types._int_gt(hours,0)) {
      var xmins_0 = $std_core_types._int_abs(_uniq_mins_871);
    }
    else {
      if ($std_core_types._int_lt(hours,0)) {
         
        var i_5_10031 = $std_core_types._int_abs(_uniq_mins_871);
        var xmins_0 = $std_core_types._int_negate(i_5_10031);
      }
      else {
        var xmins_0 = _uniq_mins_871;
      }
    }
     
    var x_1_10032 = $std_core_types._int_mul(hours,3600);
     
    var y_1_10033 = $std_core_types._int_mul(xmins_0,60);
     
    var secs_0_0 = $std_core_types._int_add(x_1_10032,y_1_10033);
     
    var _x26 = ($std_core_types._int_lt(secs_0_0,0)) ? "-" : "+";
    var _x27 = (hourwidth !== undefined) ? hourwidth : 1;
    if ($std_core_types._int_iszero(_uniq_mins_871)) {
      var _x28 = "";
    }
    else {
      var _x28 = $std_core_types._lp__plus__plus__rp_(":", $std_core_string.pad_left($std_core_int.show($std_core_types._int_abs(_uniq_mins_871)), 2, 0x0030));
    }
    var y_2_10035 = $std_core_types._lp__plus__plus__rp_("UTC", $std_core_types._lp__plus__plus__rp_(_x26, $std_core_types._lp__plus__plus__rp_($std_core_string.pad_left($std_core_int.show($std_core_types._int_abs(hours)), _x27, 0x0030), _x28)));
     
    var _x30 = (name !== undefined) ? name : "";
    var _x29 = (_x30 === (""));
    if (_x29) {
      var tzonename_0 = y_2_10035;
    }
    else {
      var tzonename_0 = (name !== undefined) ? name : "";
    }
     
    var _x32 = undefined;
    var _x31 = (_x32 !== undefined) ? _x32 : 0.0;
    var delta_0_0 = $std_time_timestamp.int_fs_timespan(secs_0_0, _x31);
    return Timezone(tzonename_0, function(i_0_1 /* std/time/instant/instant */ ) {
        var _x26 = (abbrv !== undefined) ? abbrv : "";
        return $std_core_types.Tuple2(delta_0_0, _x26);
      }, function(i_0_0_0 /* std/time/instant/instant */ ) {
        var _x28 = delta_0_0.hi;
        var _x27 = (_x28 === (0.0));
        if (_x27) {
          return $std_core_types.Just(i_0_0_0);
        }
        else {
          return $std_core_types.Nothing;
        }
      });
  }
}
 
 
// Is this the UTC timezone?
export function is_tz_utc(tz) /* (tz : timezone) -> bool */  {
  var _x29 = tz.name;
  var _x30 = tz_utc.name;
  return (_x29 === _x30);
}
 
 
// Get the current local timezone structure.
// The timezone name is allowed to be the empty string.
export function local_get_timezone() /* () -> ndet local-timezone */  {
  return _local_get_timezone();
}
 
 
// Return the utc delta in fractional seconds given a local timezone structure
// and time `i` in fractional seconds since the UNIX epoch (1970-01-01).
// Also returns time zone abbreviation.
export function local_utc_delta(tz, i) /* (tz : local-timezone, i : float64) -> (float64, string) */  {
  return _local_utc_delta(tz,i);
}
 
 
// Return the local timezone on the current system.
export function tz_local() /* () -> ndet timezone */  {
   
  var tz = local_get_timezone();
  var _x34 = undefined;
  var _x33 = (_x34 !== undefined) ? _x34 : function(i /* std/time/instant/instant */ ) {
    return $std_core_types.Nothing;
  };
  return Timezone("", function(i_0 /* std/time/instant/instant */ ) {
       
      var _x31 = i_0.since;
      var _x32 = i_0.ts;
      var x_10046 = $std_num_ddouble._lp__plus__rp_($std_time_utc.unix2000, $std_time_timestamp.unsafe_timespan_withleap($std_time_instant.convert(_x31, _x32, $std_time_utc.ts_ti)));
      var _x32 = x_10046.hi;
      var _x31 = local_utc_delta(tz, _x32);
      return $std_core_types.Tuple2($std_num_ddouble.Ddouble(_x31.fst, 0.0), _x31.snd);
    }, _x33);
}
 
export function earth_timestamp_to_dc(t, tzdelta, days_to_date) /* (t : std/time/timestamp/timestamp, tzdelta : std/time/timestamp/timespan, days-to-date : (int) -> std/time/date/date) -> (std/time/date/date, std/time/date/clock) */  {
   
  var _x35 = t.since;
  var _x36 = t.leap32;
  var ts_10049 = $std_time_timestamp.Timestamp($std_num_ddouble._lp__plus__rp_(_x35, tzdelta), _x36);
  var _x35 = $std_time_timestamp.days_seconds(ts_10049);
  var _x37 = ts_10049.leap32;
  var _x36 = $std_time_date.leap_fs_clock(_x35.snd, $std_core_types._int_from_int32(_x37));
  return $std_core_types.Tuple2(days_to_date(_x35.fst), _x36);
}
 
export function dc_timestamp(d, c, date_to_days, has_leap_seconds) /* (d : std/time/date/date, c : std/time/date/clock, date-to-days : (std/time/date/date) -> int, has-leap-seconds : ? bool) -> std/time/timestamp/timestamp */  {
  if (has_leap_seconds !== undefined) {
    if (has_leap_seconds) {
       
      var y_10055 = $std_num_ddouble.ddouble_int_exp(60, 0);
      var _x42 = c.seconds.hi;
      var _x43 = y_10055.hi;
      var _x41 = $std_num_float64.cmp(_x42, _x43);
      if (_x41 === 2) {
        var _x44 = c.seconds.lo;
        var _x45 = y_10055.lo;
        var _x40 = $std_num_float64.cmp(_x44, _x45);
      }
      else {
        var _x40 = _x41;
      }
      var _x39 = $std_core_order._lp__eq__eq__rp_(_x40, $std_core_types.Lt);
      if (_x39) {
        var _x46 = c.seconds;
        var _x38 = $std_core_types.Tuple2(_x46, 0);
      }
      else {
         
        var _x47 = c.seconds;
        var x_0_10059 = $std_num_ddouble.int($std_num_ddouble.floor(_x47));
         
        var leap = $std_core_types._int_sub(x_0_10059,59);
         
        var y_1_10063 = $std_num_ddouble.ddouble_int_exp(leap, 0);
        var _x47 = c.seconds;
        var _x48 = y_1_10063.hi;
        var _x49 = y_1_10063.lo;
        var _x38 = $std_core_types.Tuple2($std_num_ddouble._lp__plus__rp_(_x47, $std_num_ddouble.Ddouble((-_x48), (-_x49))), leap);
      }
    }
    else {
      var _x50 = c.seconds;
      var _x38 = $std_core_types.Tuple2(_x50, 0);
    }
  }
  else {
    var _x51 = c.seconds;
    var _x38 = $std_core_types.Tuple2(_x51, 0);
  }
   
  var _x52 = c.hours;
  var x_2_10067 = $std_core_types._int_mul(_x52,60);
   
  var _x53 = c.minutes;
  var i_1_10066 = $std_core_types._int_mul(($std_core_types._int_add(x_2_10067,_x53)),60);
   
  var secs = $std_num_ddouble._lp__plus__rp_($std_num_ddouble.ddouble_int_exp(i_1_10066, 0), _x38.fst);
   
  var days = date_to_days(d);
  return $std_time_timestamp.timestamp_days(days, secs, _x38.snd);
}
 
export function iso_is_leap(year) /* (year : int) -> bool */  {
  var _x52 = $std_core_types._int_eq(($std_core_types._int_mod(year,4)),0);
  if (_x52) {
    var _x53 = $std_core_types._int_ne(($std_core_types._int_mod(year,100)),0);
    if (_x53) {
      return true;
    }
    else {
      return $std_core_types._int_eq(($std_core_types._int_mod(year,400)),0);
    }
  }
  else {
    return false;
  }
}
 
export function iso_adjust(is_before_march, year) /* (is-before-march : bool, year : int) -> int */  {
  if (is_before_march) {
    return 0;
  }
  else {
    var _x54 = iso_is_leap(year);
    return (_x54) ? 1 : 2;
  }
}
 
export function iso_days_before_month(year, month) /* (year : int, month : int) -> int */  {
   
  var is_before_march_10071 = $std_core_types._int_le(month,2);
   
  if (is_before_march_10071) {
    var adj = 0;
  }
  else {
    var _x55 = iso_is_leap(year);
    var adj = (_x55) ? 1 : 2;
  }
   
  var x_0_10075 = $std_core_types._int_mul(367,month);
   
  var x_10073 = $std_core_types._int_div(($std_core_types._int_sub(x_0_10075,362)),12);
  return $std_core_types._int_sub(x_10073,adj);
}
 
export function iso_doy_to_month(year, doy) /* (year : int, doy : int) -> int */  {
   
  var is_before_march_10077 = $std_core_types._int_le(doy,58);
   
  if (is_before_march_10077) {
    var adj = 0;
  }
  else {
    var _x55 = iso_is_leap(year);
    var adj = (_x55) ? 1 : 2;
  }
   
  var x_10079 = $std_core_types._int_mul(12,($std_core_types._int_add(doy,adj)));
  return $std_core_types._int_div(($std_core_types._int_add(x_10079,373)),367);
}
 
export function iso_days_before_year(year) /* (year : int) -> int */  {
   
  var y = $std_core_types._int_sub(year,1);
   
  var x_0_10086 = $std_core_types._int_div(y,4);
   
  var y_1_10087 = $std_core_types._int_div(y,100);
   
  var x_10084 = $std_core_types._int_sub(x_0_10086,y_1_10087);
   
  var y_0_10085 = $std_core_types._int_div(y,400);
   
  var leapdays = $std_core_types._int_add(x_10084,y_0_10085);
   
  var x_1_10088 = $std_core_types._int_mul(365,y);
  return $std_core_types._int_add(x_1_10088,leapdays);
}
 
export function iso_estimate_year(days) /* (days : int) -> (int, maybe<int>) */  {
  var _x55 = $std_core_int.divmod(days, 146097);
   
  var y_0_10093 = $std_core_types._int_mul(400,(_x55.fst));
   
  var x_10090 = $std_core_types._int_add(1,y_0_10093);
   
  var y_10091 = $std_core_types._int_div(($std_core_types._int_mul(100,(_x55.snd))),36525);
  return $std_core_types.Tuple2($std_core_types._int_add(x_10090,y_10091), $std_core_types.Just(363));
}
 
export function earth_timestamp_to_instant(t, tz, ts) /* (t : std/time/timestamp/timestamp, tz : timezone, ts : std/time/instant/timescale) -> std/time/instant/instant */  {
   
  var i = $std_time_instant.Instant(t, ts);
  var _x57 = tz.name;
  var _x58 = tz_utc.name;
  var _x56 = (_x57 === _x58);
  if (_x56) {
    return i;
  }
  else {
    var _x59 = tz.utc_inverse(i);
    if (_x59 !== null) {
      return _x59.value;
    }
    else {
       
      var tuple2_10102 = tz.utc_delta(i);
       
      var _x60 = tuple2_10102.fst;
      var t_1_10105 = $std_time_timestamp._lp__dash__rp_(t, _x60);
       
      var tzi1 = $std_time_instant.Instant(t_1_10105, ts);
       
      var tuple2_0_10107 = tz.utc_delta(tzi1);
      var _x63 = tuple2_10102.fst.hi;
      var _x64 = tuple2_0_10107.fst.hi;
      var _x62 = $std_num_float64.cmp(_x63, _x64);
      if (_x62 === 2) {
        var _x65 = tuple2_10102.fst.lo;
        var _x66 = tuple2_0_10107.fst.lo;
        var _x61 = $std_num_float64.cmp(_x65, _x66);
      }
      else {
        var _x61 = _x62;
      }
      var _x60 = $std_core_order._lp__eq__eq__rp_(_x61, $std_core_types.Eq);
      if (_x60) {
        return tzi1;
      }
      else {
         
        var _x67 = tuple2_0_10107.fst;
        var t_2_10112 = $std_time_timestamp._lp__dash__rp_(t, _x67);
        return $std_time_instant.Instant(t_2_10112, ts);
      }
    }
  }
}
 
 
// Create a calendar where we assume that each
// day has 24 hours with 60 minutes, and where minutes are 60 seconds
// (with possibly leap seconds). The function takes care of timezones,
// epoch shifts, and leap seconds, and only needs a calendar `name`,
// a function `days-to-date` to calculate a date given a number of days since the `epoch`,
// a function `date-to-days` to calculate days since the `epoch` for a given date,
// and an optional time scale to be used (=`ts-utc`).\
// `month-prefix` (=`""`) is used when displaying numeric months and is set for
// the ISO week dates to `"W"` for example. The `show-era` function can be given
// to display the era of a date.
export function earth_calendar(name, long_name, days_to_date, date_to_days, month_prefix, show_era) /* (name : string, long-name : string, days-to-date : (days : int) -> std/time/date/date, date-to-days : (std/time/date/date) -> int, month-prefix : ? string, show-era : ? ((std/time/date/date) -> string)) -> calendar */  {
  var _x67 = (month_prefix !== undefined) ? month_prefix : "";
  var _x68 = (show_era !== undefined) ? show_era : function(d /* std/time/date/date */ ) {
    return "";
  };
  return Calendar(name, long_name, _x67, _x68, function(i /* std/time/instant/instant */ , tzdelta /* std/time/duration/duration */ ) {
       
      var _x69 = i.since.since;
      var _x70 = tzdelta;
      var _x71 = i.since.leap32;
      var ts_10049 = $std_time_timestamp.Timestamp($std_num_ddouble._lp__plus__rp_(_x69, _x70), _x71);
      var _x69 = $std_time_timestamp.days_seconds(ts_10049);
      var _x71 = ts_10049.leap32;
      var _x70 = $std_time_date.leap_fs_clock(_x69.snd, $std_core_types._int_from_int32(_x71));
      return $std_core_types.Tuple2(days_to_date(_x69.fst), _x70);
    }, function(d_0_0 /* std/time/date/date */ , c /* std/time/date/clock */ , tz /* timezone */ , ts /* std/time/instant/timescale */ ) {
      var _x72 = (ts.mb_seconds_in_day !== null);
      return earth_timestamp_to_instant(dc_timestamp(d_0_0, c, date_to_days, _x72), tz, ts);
    }, days_to_date, date_to_days);
}
 
 
// An abstraction over solar calendars. Almost all calendars can be defined
// in terms of this function (and `solar-ecalendar`). Given functions to
// convert days to and from a year, and day-of-the-year to and from month
// and month day, this constructs a full calendar.\
// Also needs an `epoch-shift` that gives the number of days from the
// calendar epoch to the date ``2000-01-01``. By default this is `730119`
// which corresponds to a ``0001-01-01`` ISO calendar epoch. The
// `has-year-zero` (=`True`) parameter is used when the calendar has a
// year zero. The `month-prefix` (=`""`) is used for displaying numeric
// months (and is `"W"` for `cal-iso-week` for example). The `show-era` function
// can be given to display an era for a given date. Finally the `ts` (=`ts-ti`)
// gives the time scale for the calendar.
export function solar_calendar(name, long_name, days_before_year, days_to_yeardoy, days_before_month, days_to_month, epoch_shift, has_year_zero, month_prefix, show_era) /* (name : string, long-name : string, days-before-year : (year : int) -> int, days-to-yeardoy : (days : int) -> (int, int), days-before-month : (year : int, month : int) -> int, days-to-month : (year : int, doy : int) -> int, epoch-shift : ? int, has-year-zero : ? bool, month-prefix : ? string, show-era : ? ((std/time/date/date) -> string)) -> calendar */  {
  var _x75 = (month_prefix !== undefined) ? month_prefix : "";
  var _x76 = (show_era !== undefined) ? show_era : function(d /* std/time/date/date */ ) {
    return "";
  };
  return earth_calendar(name, long_name, function(days0 /* int */ ) {
       
      var _x73 = (epoch_shift !== undefined) ? epoch_shift : 730119;
      var days = $std_core_types._int_add(days0,_x73);
      var _x73 = days_to_yeardoy(days);
       
      var month = days_to_month(_x73.fst, _x73.snd);
       
      var y_2_10122 = days_before_month(_x73.fst, month);
       
      var x_0_10119 = $std_core_types._int_sub((_x73.snd),y_2_10122);
       
      var day = $std_core_types._int_add(x_0_10119,1);
       
      if (has_year_zero !== undefined) {
        if (has_year_zero) {
          var year = _x73.fst;
        }
        else {
          var year = ($std_core_types._int_gt((_x73.fst),0)) ? _x73.fst : $std_core_types._int_sub((_x73.fst),1);
        }
      }
      else {
        var year = _x73.fst;
      }
      return $std_time_date.$Date(year, month, day);
    }, function(d_0 /* std/time/date/date */ ) {
       
      if (has_year_zero !== undefined) {
        if (has_year_zero) {
          var y_3 = d_0.year;
        }
        else {
          var _x75 = d_0.year;
          var _x74 = $std_core_types._int_gt(_x75,0);
          if (_x74) {
            var y_3 = d_0.year;
          }
          else {
            var _x76 = d_0.year;
            var y_3 = $std_core_types._int_add(_x76,1);
          }
        }
      }
      else {
        var y_3 = d_0.year;
      }
       
      var x_4_10135 = days_before_year(y_3);
       
      var _x77 = d_0.month;
      var y_6_10136 = days_before_month(y_3, _x77);
       
      var x_3_10133 = $std_core_types._int_add(x_4_10135,y_6_10136);
       
      var _x78 = d_0.day;
      var y_5_10134 = $std_core_types._int_sub(_x78,1);
       
      var x_2_10131 = $std_core_types._int_add(x_3_10133,y_5_10134);
      var _x74 = (epoch_shift !== undefined) ? epoch_shift : 730119;
      return $std_core_types._int_sub(x_2_10131,_x74);
    }, _x75, _x76);
}
 
 
// An abstraction over solar calendars. Almost all calendars can be defined
// in terms of this function (and `solar-calendar`). Given functions to
// convert days to and from a year, and day-of-the-year to and from month
// and month day, this constructs a full calendar. In contrast to `solar-calendar`
// this function just needs an estimation function from days to a year -- for most
// calendars this is much more convenient. The estimation function returns a
// conservative (i.e. lowest) estimate for the year and a `maybe<int>` that
// is the `safe-day`: if the day of the year is lower or equal to that the estimate
// is surely correct. Otherwise the wrapper will calculate the start day for the next
// year until the correct year is found.\
// Also needs an `epoch-shift` that gives the number of days from the
// calendar epoch to the date ``2000-01-01``. By default this is `730119`
// which corresponds to a ``0001-01-01`` ISO calendar epoch. The
// `has-year-zero` (=`True`) parameter is used when the calendar has a
// year zero. The `month-prefix` (=`""`) is used for displaying numeric
// months (and is `"W"` for `cal-iso-week` for example). The `show-era` function
// can be given to display an era for a given date. Finally the `ts` (=`ts-utc`)
// gives the time scale for the calendar.
export function solar_ecalendar(name, long_name, days_before_year, estimate_year, days_before_month, days_to_month, epoch_shift, has_year_zero, month_prefix, show_era) /* (name : string, long-name : string, days-before-year : (year : int) -> int, estimate-year : (days : int) -> (int, maybe<int>), days-before-month : (year : int, month : int) -> int, days-to-month : (year : int, doy : int) -> int, epoch-shift : ? int, has-year-zero : ? bool, month-prefix : ? string, show-era : ? ((std/time/date/date) -> string)) -> calendar */  {
  var _x80 = (epoch_shift !== undefined) ? epoch_shift : 730119;
  var _x81 = (has_year_zero !== undefined) ? has_year_zero : true;
  var _x82 = (month_prefix !== undefined) ? month_prefix : "";
  var _x83 = (show_era !== undefined) ? show_era : function(d /* std/time/date/date */ ) {
    return "";
  };
  return solar_calendar(name, long_name, days_before_year, function(days /* int */ ) {
      var _x77 = estimate_year(days);
       
      var y_10141 = days_before_year(_x77.fst);
       
      var doy1 = $std_core_types._int_sub(days,y_10141);
      var _x79 = (_x77.snd === null) ? 0 : _x77.snd.value;
      var _x78 = $std_core_types._int_le(doy1,_x79);
      if (_x78) {
        return $std_core_types.Tuple2(_x77.fst, doy1);
      }
      else {
         
        var y_0_10145 = days_before_year($std_core_types._int_add((_x77.fst),1));
         
        var doy2 = $std_core_types._int_sub(days,y_0_10145);
        if ($std_core_types._int_lt(doy2,0)) {
          return $std_core_types.Tuple2(_x77.fst, doy1);
        }
        else {
          return $std_core_types.Tuple2($std_core_types._int_add((_x77.fst),1), doy2);
        }
      }
    }, days_before_month, days_to_month, _x80, _x81, _x82, _x83);
}
 
 
// Create a standard ISO calendar using a particular time scale
// and calendar `name` (=`ts.name`).
export function iso_calendar(___wildcard_x422__23, name, long_name) /* (std/time/instant/timescale, name : string, long-name : ? string) -> calendar */  {
  var _x84 = (long_name !== undefined) ? long_name : name;
  return solar_ecalendar(name, _x84, iso_days_before_year, iso_estimate_year, iso_days_before_month, iso_doy_to_month);
}
 
 
// The standard [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) calendar
// using TI time (`ts-ti`). This is a proleptic Gregorian
// calendar except that it uses the year 0 for 1 BCE, -1 for 2 BCE etc.
//
// This is the default calendar used in the library as it guarantees deterministic
// date calculations while still taking historical leap seconds into account.
export var cal_iso;
var cal_iso = solar_ecalendar("", "ISO", iso_days_before_year, iso_estimate_year, iso_days_before_month, iso_doy_to_month);
 
 
// Return the instant in time for a given `:date` and `:clock` (= `clock0`) interpreted by
// calendar `cal` (=`cal-iso`) in a timezone `tz` (=`tz-utc` by default).
export function date_fs_instant(ts, d, c, tz, cal) /* (ts : std/time/instant/timescale, d : std/time/date/date, c : ? std/time/date/clock, tz : ? timezone, cal : ? calendar) -> std/time/instant/instant */  {
  var _x85 = (cal !== undefined) ? cal : cal_iso;
  var _x86 = (c !== undefined) ? c : $std_time_date.clock0;
  var _x87 = (tz !== undefined) ? tz : tz_utc;
  return _x85.dc_to_instant(d, _x86, _x87, ts);
}
 
 
/* Return the instant in time for a given date and clock interpreted by
   calendar `cal` (=`cal-iso`) in a timezone `tz` (=`tz-utc` by default).
  The `month`, `day`, `hour`, `minutes` may be outside their usual ranges
  and will be normalized during the conversion. For example, January 33 converts to February 2.
  This makes it very easy to add- or subtract days or months to an existing time.
  When the `seconds` or fraction of seconds `frac` add up over 60 those extra seconds are
  interpreted as leap seconds.
  Due to timezone transitions, or leap seconds, it is possible to specify dates that never
  happened (as it was skipped by a timezone change), or ambiguous times (as a timezone springs back).
  In such cases, the time is always interpreted in the earlier timezone.
*/
export function timescale_fs_instant(tscale, year, month, day, hours, minutes, seconds, frac, tz, cal) /* (tscale : std/time/instant/timescale, year : int, month : ? int, day : ? int, hours : ? int, minutes : ? int, seconds : ? int, frac : ? float64, tz : ? timezone, cal : ? calendar) -> std/time/instant/instant */  {
   
  var _x88 = (seconds !== undefined) ? seconds : 0;
  var _x89 = (frac !== undefined) ? frac : 0.0;
  var fsecs = $std_num_ddouble._lp__plus__rp_($std_num_ddouble.ddouble_int_exp(_x88, 0), $std_num_ddouble.Ddouble(_x89, 0.0));
  var _x88 = (cal !== undefined) ? cal : cal_iso;
  var _x89 = (month !== undefined) ? month : 1;
  var _x90 = (day !== undefined) ? day : 1;
  var _x91 = (hours !== undefined) ? hours : 0;
  var _x92 = (minutes !== undefined) ? minutes : 0;
  var _x93 = (tz !== undefined) ? tz : tz_utc;
  return _x88.dc_to_instant($std_time_date.$Date(year, _x89, _x90), $std_time_date.Clock(_x91, _x92, fsecs), _x93, tscale);
}
 
 
// monadic lift
export function utc_fs__mlift_instant_10252(cal, day, frac, hours, minutes, month, seconds, tz, year, _c_x10227) /* (cal : ? calendar, day : ? int, frac : ? float64, hours : ? int, minutes : ? int, month : ? int, seconds : ? int, tz : ? timezone, year : int, std/time/instant/timescale) -> std/time/instant/instant */  {
  return $std_core_hnd._open_none0(function() {
    var _x94 = (month !== undefined) ? month : 1;
    var _x95 = (day !== undefined) ? day : 1;
    var _x96 = (hours !== undefined) ? hours : 0;
    var _x97 = (minutes !== undefined) ? minutes : 0;
    var _x98 = (seconds !== undefined) ? seconds : 0;
    var _x99 = (frac !== undefined) ? frac : 0.0;
    var _x100 = (tz !== undefined) ? tz : tz_utc;
    var _x101 = (cal !== undefined) ? cal : cal_iso;
    return timescale_fs_instant(_c_x10227, year, _x94, _x95, _x96, _x97, _x98, _x99, _x100, _x101);
  });
}
 
export function utc_fs_instant(year, month, day, hours, minutes, seconds, frac, tz, cal, ts) /* (year : int, month : ? int, day : ? int, hours : ? int, minutes : ? int, seconds : ? int, frac : ? float64, tz : ? timezone, cal : ? calendar, ts : ? std/time/instant/timescale) -> std/time/utc/utc std/time/instant/instant */  {
   
  if (ts !== undefined) {
    var x_10254 = ts;
  }
  else {
     
    var ev_10257 = $std_core_hnd._evv_at(0);
    var _x102 = $std_time_utc.utc_fs__select(ev_10257.hnd);
    var x_10254 = _x102(ev_10257.marker, ev_10257);
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_c_x10227 /* std/time/instant/timescale */ ) {
      return utc_fs__mlift_instant_10252(cal, day, frac, hours, minutes, month, seconds, tz, year, _c_x10227);
    });
  }
  else {
    return $std_core_hnd._open_none0(function() {
      var _x102 = (month !== undefined) ? month : 1;
      var _x103 = (day !== undefined) ? day : 1;
      var _x104 = (hours !== undefined) ? hours : 0;
      var _x105 = (minutes !== undefined) ? minutes : 0;
      var _x106 = (seconds !== undefined) ? seconds : 0;
      var _x107 = (frac !== undefined) ? frac : 0.0;
      var _x108 = (tz !== undefined) ? tz : tz_utc;
      var _x109 = (cal !== undefined) ? cal : cal_iso;
      return timescale_fs_instant(x_10254, year, _x102, _x103, _x104, _x105, _x106, _x107, _x108, _x109);
    });
  }
}
 
 
// monadic lift
export function utcdate_fs__mlift_instant_10253(c, cal, d, tz, _c_x10229) /* (c : ? std/time/date/clock, cal : ? calendar, d : std/time/date/date, tz : ? timezone, std/time/instant/timescale) -> std/time/instant/instant */  {
  return $std_core_hnd._open_none0(function() {
    var _x110 = (cal !== undefined) ? cal : cal_iso;
    var _x111 = (c !== undefined) ? c : $std_time_date.clock0;
    var _x112 = (tz !== undefined) ? tz : tz_utc;
    return _x110.dc_to_instant(d, _x111, _x112, _c_x10229);
  });
}
 
export function utcdate_fs_instant(d, c, tz, cal, ts) /* (d : std/time/date/date, c : ? std/time/date/clock, tz : ? timezone, cal : ? calendar, ts : ? std/time/instant/timescale) -> std/time/utc/utc std/time/instant/instant */  {
   
  if (ts !== undefined) {
    var x_10259 = ts;
  }
  else {
     
    var ev_10262 = $std_core_hnd._evv_at(0);
    var _x113 = $std_time_utc.utc_fs__select(ev_10262.hnd);
    var x_10259 = _x113(ev_10262.marker, ev_10262);
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_c_x10229 /* std/time/instant/timescale */ ) {
      return utcdate_fs__mlift_instant_10253(c, cal, d, tz, _c_x10229);
    });
  }
  else {
    return $std_core_hnd._open_none0(function() {
      var _x113 = (cal !== undefined) ? cal : cal_iso;
      var _x114 = (c !== undefined) ? c : $std_time_date.clock0;
      var _x115 = (tz !== undefined) ? tz : tz_utc;
      return _x113.dc_to_instant(d, _x114, _x115, x_10259);
    });
  }
}
 
 
// Convert an `:instant` to a `:date`, `:clock`, timezone delta and abbreviation,
// for a given timezone `tz` (=`tz-utc` by default) and calendar (=`cal-iso` by default).
export function instant_dc(i, tz, cal) /* (i : std/time/instant/instant, tz : ? timezone, cal : ? calendar) -> (std/time/date/date, std/time/date/clock, std/time/duration/duration, string) */  {
  var _x116 = (tz !== undefined) ? tz : tz_utc;
  var _x117 = _x116.utc_delta(i);
  var _x118 = (cal !== undefined) ? cal : cal_iso;
  var _x119 = _x118.instant_to_dc(i, _x117.fst);
  return $std_core_types.Tuple4(_x119.fst, _x119.snd, _x117.fst, _x117.snd);
}
 
 
// Return the day of the week for a calendar `cal` (=`cal-iso`).
export function weekday(d, cal) /* (d : std/time/date/date, cal : calendar) -> std/time/date/weekday */  {
   
  var x_10163 = cal.date_to_days(d);
   
  var dow = $std_core_types._int_mod(($std_core_types._int_add(x_10163,6)),7);
  return $std_time_date.weekday(dow);
}
 
 
// Return the days between two dates interpreted by calendar `cal`.
export function days_until(cal, d1, d2) /* (cal : calendar, d1 : std/time/date/date, d2 : std/time/date/date) -> int */  {
   
  var x = cal.date_to_days(d2);
   
  var y = cal.date_to_days(d1);
  return $std_core_types._int_sub(x,y);
}
 
export function cal_timestamp(cal, ts, d, c) /* (cal : calendar, ts : std/time/instant/timescale, d : std/time/date/date, c : std/time/date/clock) -> std/time/timestamp/timestamp */  {
  var _x120 = cal.date_to_days;
  var _x121 = (ts.mb_seconds_in_day !== null);
  return dc_timestamp(d, c, _x120, _x121);
}
 
 
// Create a new calendar from by combining two other calendars. The `switch-date`
// is in terms of the second calendar (`cal2`) and time after (and including) the switch
// date is displayed in `cal2` while times before it in `cal1`. This function is used
// for example for the Julian Gregorian calendar.
export function combine_earth_calendars(name, long_name, switch_date, cal1, cal2, mb_show_era) /* (name : string, long-name : string, switch-date : std/time/date/date, cal1 : calendar, cal2 : calendar, mb-show-era : ? (maybe<(std/time/date/date) -> string>)) -> calendar */  {
  return earth_calendar(name, long_name, function(days /* int */ ) {
       
      var _x123 = cal2.date_to_days(switch_date);
      var _x122 = $std_core_types._int_lt(days,_x123);
      var calendar_0_10170 = (_x122) ? cal1 : cal2;
      return calendar_0_10170.days_to_date(days);
    }, function(d /* std/time/date/date */ ) {
       
      var _x122 = $std_core_order._lp__eq__eq__rp_($std_time_date.cmp(d, switch_date), $std_core_types.Lt);
      var calendar_1_10172 = (_x122) ? cal1 : cal2;
      return calendar_1_10172.date_to_days(d);
    }, undefined, function(d_0 /* std/time/date/date */ ) {
      var _x122 = (mb_show_era !== undefined) ? mb_show_era : $std_core_types.Nothing;
      if (_x122 !== null) {
        return _x122.value(d_0);
      }
      else {
         
        var _x123 = $std_core_order._lp__eq__eq__rp_($std_time_date.cmp(d_0, switch_date), $std_core_types.Lt);
        var calendar_2_10175 = (_x123) ? cal1 : cal2;
        return calendar_2_10175.show_era(d_0);
      }
    });
}
 
 
// Create a new calendar from anoter calendar `cal` by adding an offset to the
// years (`year-shift`). This is used for example to create the Ethiopian calendar
// from the Coptic calendar, by using:
// `year-shift-earth-calendar( "EC", "Ethiopian", ~276, cal-coptic )`
export function year_shift_earth_calendar(name, long_name, year_shift, cal, month_prefix, show_era) /* (name : string, long-name : string, year-shift : int, cal : calendar, month-prefix : ? string, show-era : ? ((std/time/date/date) -> string)) -> calendar */  {
  var _x133 = (month_prefix !== undefined) ? month_prefix : "";
  var _x134 = (show_era !== undefined) ? show_era : function(d /* std/time/date/date */ ) {
    return "";
  };
  return earth_calendar(name, long_name, function(days /* int */ ) {
       
      var d_0 = cal.days_to_date(days);
       
      var _x123 = d_0.year;
      var year_10180 = $std_core_types._int_sub(_x123,year_shift);
      if (year_10180 !== undefined) {
        var _x123 = year_10180;
      }
      else {
        var _x123 = d_0.year;
      }
      var _x125 = undefined;
      if (_x125 !== undefined) {
        var _x124 = _x125;
      }
      else {
        var _x124 = d_0.month;
      }
      var _x127 = undefined;
      if (_x127 !== undefined) {
        var _x126 = _x127;
      }
      else {
        var _x126 = d_0.day;
      }
      return $std_time_date.$Date(_x123, _x124, _x126);
    }, function(d_1 /* std/time/date/date */ ) {
       
      var _x128 = d_1.year;
      var year_0_10188 = $std_core_types._int_add(_x128,year_shift);
      if (year_0_10188 !== undefined) {
        var _x128 = year_0_10188;
      }
      else {
        var _x128 = d_1.year;
      }
      var _x130 = undefined;
      if (_x130 !== undefined) {
        var _x129 = _x130;
      }
      else {
        var _x129 = d_1.month;
      }
      var _x132 = undefined;
      if (_x132 !== undefined) {
        var _x131 = _x132;
      }
      else {
        var _x131 = d_1.day;
      }
      return cal.date_to_days($std_time_date.$Date(_x128, _x129, _x131));
    }, _x133, _x134);
}
 
 
// Create a standard [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) calendar
// using UTC time (`ts-utc-create`) given a provide leap second table (`:leaps-table`).
// This is a proleptic Gregorian
// calendar except that it uses the year 0 for 1 BCE, -1 for 2 BCE etc.
//
// You can create an ISO 8601 UTC calendar using the latest IERS leap
// second data using [`cal-utc-load`](std_time_download.html#cal_utc_load).
export function cal_utc_create(leaps) /* (leaps : std/time/utc/leaps-table) -> calendar */  {
   
  var ts_utc = $std_time_utc.utc_timescale("UTC", leaps);
  return solar_ecalendar("", "ISO", iso_days_before_year, iso_estimate_year, iso_days_before_month, iso_doy_to_month);
}
 
 
/* The standard [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) calendar
but using TI-SLS time (`ts-ti-sls`), i.e. TI with
[smoothed leap seconds](https://www.cl.cam.ac.uk/~mgk25/time/utc-sls/).
This calendar is equivalent to the ISO calendar except for the last 1000 seconds of a day where
a leap second occurs. On such day, the leap second time step (positive or negative)
is distributed over the last 1000 seconds of the day. On the full hour, ISO and ISO-SLS are equal again.
This is a recommended calendar to use for
time stamps or communication with other services since it avoids any potential trouble
with leap seconds while still being quite precise.
Other good properties include:
- All days have 86400 seconds, and the time 23:59:60 never appears
- The time never differs more than one second from UTC
- Time as always equal to UTC on the full and half hour
*/
export var cal_iso_sls;
var cal_iso_sls = solar_ecalendar("SLS", "ISO-SLS", iso_days_before_year, iso_estimate_year, iso_days_before_month, iso_doy_to_month);
 
 
// Create a new ISO 8601 calendar based on UTC-SLS time, i.e. UTC with smoothed leap
// seconds.
export function cal_utc_sls_create(leaps) /* (leaps : std/time/utc/leaps-table) -> calendar */  {
   
  var ts_utc_sls = $std_time_utc.ts_utc_sls_create(leaps);
  return solar_ecalendar("SLS", "ISO-SLS", iso_days_before_year, iso_estimate_year, iso_days_before_month, iso_doy_to_month);
}
 
 
// The (proleptic) [Gregorian calendar](https://en.wikipedia.org/wiki/Gregorian_calendar).
// Just like the ISO calendar except that the year 0 does not exist, i.e. after 1 BCE (=`~1`)
// we have 1 CE (=`1`).
// The calendar short name is ``GC``.
export var cal_gregorian;
var cal_gregorian = solar_ecalendar("GC", "Gregorian", iso_days_before_year, iso_estimate_year, iso_days_before_month, iso_doy_to_month, undefined, false, undefined, function(d /* std/time/date/date */ ) {
    var _x136 = d.year;
    var _x135 = $std_core_types._int_lt(_x136,0);
    return (_x135) ? "BCE" : "CE";
  });