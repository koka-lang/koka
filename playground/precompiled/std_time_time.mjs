// Koka generated module: std/time/time, koka version: 3.2.4
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
import * as $std_time_timestamp from './std_time_timestamp.mjs';
import * as $std_time_duration from './std_time_duration.mjs';
import * as $std_time_instant from './std_time_instant.mjs';
import * as $std_time_utc from './std_time_utc.mjs';
import * as $std_time_date from './std_time_date.mjs';
import * as $std_time_calendar from './std_time_calendar.mjs';
import * as $std_time_chrono from './std_time_chrono.mjs';
 
// externals
 
// type declarations
// type time
export function Time(date, clock, calendar, tzdelta, tzabbrv, timezone, instant) /* (date : std/time/date/date, clock : std/time/date/clock, calendar : std/time/calendar/calendar, tzdelta : std/time/duration/duration, tzabbrv : string, timezone : std/time/calendar/timezone, instant : std/time/instant/instant) -> time */  {
  return { date: date, clock: clock, calendar: calendar, tzdelta: tzdelta, tzabbrv: tzabbrv, timezone: timezone, instant: instant };
}
 
// declarations
 
 
// Represents an instant in time for a certain calendar and timezone.
export function _create_Time(date, clock, calendar, tzdelta, tzabbrv, timezone, instant) /* (date : std/time/date/date, clock : std/time/date/clock, calendar : std/time/calendar/calendar, tzdelta : ? std/time/duration/duration, tzabbrv : ? string, timezone : ? std/time/calendar/timezone, instant : std/time/instant/instant) -> time */  {
  var _x0 = (tzdelta !== undefined) ? tzdelta : $std_time_duration.zero;
  var _x1 = (tzabbrv !== undefined) ? tzabbrv : "";
  var _x2 = (timezone !== undefined) ? timezone : $std_time_calendar.tz_utc;
  return Time(date, clock, calendar, _x0, _x1, _x2, instant);
}
 
 
// Automatically generated. Retrieves the `date` constructor field of the `:time` type.
export function time_fs_date(time_0) /* (time : time) -> std/time/date/date */  {
  return time_0.date;
}
 
 
// Automatically generated. Retrieves the `clock` constructor field of the `:time` type.
export function time_fs_clock(time_0) /* (time : time) -> std/time/date/clock */  {
  return time_0.clock;
}
 
 
// Automatically generated. Retrieves the `calendar` constructor field of the `:time` type.
export function time_fs_calendar(time_0) /* (time : time) -> std/time/calendar/calendar */  {
  return time_0.calendar;
}
 
 
// Automatically generated. Retrieves the `tzdelta` constructor field of the `:time` type.
export function time_fs_tzdelta(time_0) /* (time : time) -> std/time/duration/duration */  {
  return time_0.tzdelta;
}
 
 
// Automatically generated. Retrieves the `tzabbrv` constructor field of the `:time` type.
export function time_fs_tzabbrv(time_0) /* (time : time) -> string */  {
  return time_0.tzabbrv;
}
 
 
// Automatically generated. Retrieves the `timezone` constructor field of the `:time` type.
export function time_fs_timezone(time_0) /* (time : time) -> std/time/calendar/timezone */  {
  return time_0.timezone;
}
 
 
// Automatically generated. Retrieves the `instant` constructor field of the `:time` type.
export function time_fs_instant(time_0) /* (time : time) -> std/time/instant/instant */  {
  return time_0.instant;
}
 
export function time_fs__copy(_this, date, clock, calendar, tzdelta, tzabbrv, timezone, instant) /* (time, date : ? std/time/date/date, clock : ? std/time/date/clock, calendar : ? std/time/calendar/calendar, tzdelta : ? std/time/duration/duration, tzabbrv : ? string, timezone : ? std/time/calendar/timezone, instant : ? std/time/instant/instant) -> time */  {
  if (date !== undefined) {
    var _x3 = date;
  }
  else {
    var _x3 = _this.date;
  }
  if (clock !== undefined) {
    var _x4 = clock;
  }
  else {
    var _x4 = _this.clock;
  }
  if (calendar !== undefined) {
    var _x5 = calendar;
  }
  else {
    var _x5 = _this.calendar;
  }
  if (tzdelta !== undefined) {
    var _x6 = tzdelta;
  }
  else {
    var _x6 = _this.tzdelta;
  }
  if (tzabbrv !== undefined) {
    var _x7 = tzabbrv;
  }
  else {
    var _x7 = _this.tzabbrv;
  }
  if (timezone !== undefined) {
    var _x8 = timezone;
  }
  else {
    var _x8 = _this.timezone;
  }
  if (instant !== undefined) {
    var _x9 = instant;
  }
  else {
    var _x9 = _this.instant;
  }
  return Time(_x3, _x4, _x5, _x6, _x7, _x8, _x9);
}
 
 
// The `:timescale` of the time.
export function timescale(t) /* (t : time) -> std/time/instant/timescale */  {
  return t.instant.ts;
}
 
 
// Return the year of a `:time`.
export function year(t) /* (t : time) -> int */  {
  return t.date.year;
}
 
 
// Return the month of a `:time`. (starting at 1)
export function month(t) /* (t : time) -> int */  {
  return t.date.month;
}
 
 
// Return the day of the month of a `:time`. (starting at 1)
export function day(t) /* (t : time) -> int */  {
  return t.date.day;
}
 
 
// Return the whole hours of a `:time`.
export function hours(t) /* (t : time) -> int */  {
  return t.clock.hours;
}
 
 
// Return the whole minutes of a `:time`.
export function minutes(t) /* (t : time) -> int */  {
  return t.clock.minutes;
}
 
 
// Return the fractional seconds of a `:time`.
export function seconds(t) /* (t : time) -> std/time/timestamp/timespan */  {
  return t.clock.seconds;
}
 
 
// Compare two `:time`s. Compares the actual instants in time
// and can thus compare across calendars and timezones.\
// `time(2001,7,2,tz=tz-fixed("GMT+1",duration(3600))) > time(2001,7,1,23,30,0)` &quad; (`False`!)
export function cmp(t1, t2) /* (t1 : time, t2 : time) -> order */  {
  var _x10 = t1.instant;
  var _x11 = t2.instant;
  return $std_time_instant.cmp(_x10, _x11);
}
 
export function _lp__eq__eq__rp_(i, j) /* (i : time, j : time) -> bool */  {
  var _x12 = i.instant;
  var _x13 = j.instant;
  return $std_core_order._lp__eq__eq__rp_($std_time_instant.cmp(_x12, _x13), $std_core_types.Eq);
}
 
export function _lp__lt__rp_(i, j) /* (i : time, j : time) -> bool */  {
  var _x14 = i.instant;
  var _x15 = j.instant;
  return $std_core_order._lp__eq__eq__rp_($std_time_instant.cmp(_x14, _x15), $std_core_types.Lt);
}
 
export function _lp__excl__eq__rp_(i, j) /* (i : time, j : time) -> bool */  {
  var _x16 = i.instant;
  var _x17 = j.instant;
  return $std_core_order._lp__excl__eq__rp_($std_time_instant.cmp(_x16, _x17), $std_core_types.Eq);
}
 
export function _lp__lt__eq__rp_(i, j) /* (i : time, j : time) -> bool */  {
  var _x18 = i.instant;
  var _x19 = j.instant;
  return $std_core_order._lp__excl__eq__rp_($std_time_instant.cmp(_x18, _x19), $std_core_types.Gt);
}
 
export function _lp__gt__rp_(i, j) /* (i : time, j : time) -> bool */  {
  var _x20 = i.instant;
  var _x21 = j.instant;
  return $std_core_order._lp__eq__eq__rp_($std_time_instant.cmp(_x20, _x21), $std_core_types.Gt);
}
 
export function _lp__gt__eq__rp_(i, j) /* (i : time, j : time) -> bool */  {
  var _x22 = i.instant;
  var _x23 = j.instant;
  return $std_core_order._lp__excl__eq__rp_($std_time_instant.cmp(_x22, _x23), $std_core_types.Lt);
}
 
 
// The minimum of two times (as by their actual instant in time)
export function min(i, j) /* (i : time, j : time) -> time */  {
  var _x25 = i.instant;
  var _x26 = j.instant;
  var _x24 = $std_core_order._lp__excl__eq__rp_($std_time_instant.cmp(_x25, _x26), $std_core_types.Gt);
  return (_x24) ? i : j;
}
 
 
// The maximum of two times (as by their actual instant in time)
export function max(i, j) /* (i : time, j : time) -> time */  {
  var _x28 = i.instant;
  var _x29 = j.instant;
  var _x27 = $std_core_order._lp__excl__eq__rp_($std_time_instant.cmp(_x28, _x29), $std_core_types.Lt);
  return (_x27) ? i : j;
}
 
export function is_numeric(abbrv) /* (abbrv : string) -> bool */  {
  if ((abbrv === (""))) {
    return true;
  }
  else {
    var _x30 = (($std_core_sslice.head(abbrv)) === ("+"));
    if (_x30) {
      return true;
    }
    else {
      return (($std_core_sslice.head(abbrv)) === ("-"));
    }
  }
}
 
 
// Convert an `:instant` to a `:time` value in a given timezone `tz` (=`tz-utc` by default)
// and calendar (=`cal-iso` by default).
export function instant_fs_time(i, tz, cal, ts) /* (i : std/time/instant/instant, tz : ? std/time/calendar/timezone, cal : ? std/time/calendar/calendar, ts : ? std/time/instant/timescale) -> time */  {
   
  var _x32 = i.ts.name;
  if (ts !== undefined) {
    var _x34 = ts;
  }
  else {
    var _x34 = i.ts;
  }
  var _x33 = _x34.name;
  var _x31 = (_x32 === _x33);
  if (_x31) {
    var j = i;
  }
  else {
    var _x35 = i.since;
    var _x36 = i.ts;
    if (ts !== undefined) {
      var _x37 = ts;
    }
    else {
      var _x37 = i.ts;
    }
    if (ts !== undefined) {
      var _x38 = ts;
    }
    else {
      var _x38 = i.ts;
    }
    var j = $std_time_instant.Instant($std_time_instant.convert(_x35, _x36, _x37), _x38);
  }
  var _x32 = (tz !== undefined) ? tz : $std_time_calendar.tz_utc;
  var _x33 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
  var _x31 = $std_time_calendar.instant_dc(j, _x32, _x33);
  var _x34 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
  var _x35 = (tz !== undefined) ? tz : $std_time_calendar.tz_utc;
  return Time(_x31.fst, _x31.snd, _x34, _x31.thd, _x31.field4, _x35, j);
}
 
export function timescale_fs_time(tscale, year_0, month_0, day_0, hours_0, minutes_0, secs, frac, tz, cal) /* (tscale : std/time/instant/timescale, year : int, month : ? int, day : ? int, hours : ? int, minutes : ? int, secs : ? int, frac : ? float64, tz : ? std/time/calendar/timezone, cal : ? std/time/calendar/calendar) -> time */  {
  var _x36 = (month_0 !== undefined) ? month_0 : 1;
  var _x37 = (day_0 !== undefined) ? day_0 : 1;
  var _x38 = (hours_0 !== undefined) ? hours_0 : 0;
  var _x39 = (minutes_0 !== undefined) ? minutes_0 : 0;
  var _x40 = (secs !== undefined) ? secs : 0;
  var _x41 = (frac !== undefined) ? frac : 0.0;
  var _x42 = (tz !== undefined) ? tz : $std_time_calendar.tz_utc;
  var _x43 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
  var _x44 = (tz !== undefined) ? tz : $std_time_calendar.tz_utc;
  var _x45 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
  return instant_fs_time($std_time_calendar.timescale_fs_instant(tscale, year_0, _x36, _x37, _x38, _x39, _x40, _x41, _x42, _x43), _x44, _x45, tscale);
}
 
 
// monadic lift
export function _mlift_time_10405(cal, day_0, frac, hours_0, minutes_0, month_0, secs, tz, year_0, _c_x10330) /* (cal : ? std/time/calendar/calendar, day@0 : ? int, frac : ? float64, hours@0 : ? int, minutes@0 : ? int, month@0 : ? int, secs : ? int, tz : ? std/time/calendar/timezone, year@0 : int, std/time/instant/timescale) -> time */  {
  return $std_core_hnd._open_none0(function() {
    var _x46 = (month_0 !== undefined) ? month_0 : 1;
    var _x47 = (day_0 !== undefined) ? day_0 : 1;
    var _x48 = (hours_0 !== undefined) ? hours_0 : 0;
    var _x49 = (minutes_0 !== undefined) ? minutes_0 : 0;
    var _x50 = (secs !== undefined) ? secs : 0;
    var _x51 = (frac !== undefined) ? frac : 0.0;
    var _x52 = (tz !== undefined) ? tz : $std_time_calendar.tz_utc;
    var _x53 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
    return timescale_fs_time(_c_x10330, year_0, _x46, _x47, _x48, _x49, _x50, _x51, _x52, _x53);
  });
}
 
 
/* Return the `:time` value for a given date and clock in a timezone `tz` (=`tz-utc` by default)
interpreted by calendar `cal` (=`cal-iso`).
The `month`, `day`, `hour`, `minutes` may be outside their usual ranges
and will be normalized during the conversion. For example, January 33 converts to February 2.
This makes it very easy to add- or subtract days or months to an existing time.
When the `seconds` or fraction of seconds `frac` add up over 60 though, those extra seconds are
interpreted as leap seconds.
Due to timezone transitions, or leap seconds, it is possible to specify dates that never
happened (as it was skipped by a timezone change), or ambiguous times (as a timezone springs back).
In such cases, the time is always interpreted in the earlier timezone.
*/
export function time(year_0, month_0, day_0, hours_0, minutes_0, secs, frac, tz, cal, ts) /* (year : int, month : ? int, day : ? int, hours : ? int, minutes : ? int, secs : ? int, frac : ? float64, tz : ? std/time/calendar/timezone, cal : ? std/time/calendar/calendar, ts : ? std/time/instant/timescale) -> std/time/utc/utc time */  {
   
  if (ts !== undefined) {
    var x_10411 = ts;
  }
  else {
     
    var ev_10414 = $std_core_hnd._evv_at(0);
    var _x54 = $std_time_utc.utc_fs__select(ev_10414.hnd);
    var x_10411 = _x54(ev_10414.marker, ev_10414);
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_c_x10330 /* std/time/instant/timescale */ ) {
      return _mlift_time_10405(cal, day_0, frac, hours_0, minutes_0, month_0, secs, tz, year_0, _c_x10330);
    });
  }
  else {
    return $std_core_hnd._open_none0(function() {
      var _x54 = (month_0 !== undefined) ? month_0 : 1;
      var _x55 = (day_0 !== undefined) ? day_0 : 1;
      var _x56 = (hours_0 !== undefined) ? hours_0 : 0;
      var _x57 = (minutes_0 !== undefined) ? minutes_0 : 0;
      var _x58 = (secs !== undefined) ? secs : 0;
      var _x59 = (frac !== undefined) ? frac : 0.0;
      var _x60 = (tz !== undefined) ? tz : $std_time_calendar.tz_utc;
      var _x61 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
      return timescale_fs_time(x_10411, year_0, _x54, _x55, _x56, _x57, _x58, _x59, _x60, _x61);
    });
  }
}
 
export function timescale_fs_date_fs_time(ts, d, c, tz, cal) /* (ts : std/time/instant/timescale, d : std/time/date/date, c : ? std/time/date/clock, tz : ? std/time/calendar/timezone, cal : ? std/time/calendar/calendar) -> time */  {
  var _x63 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
  var _x64 = (c !== undefined) ? c : $std_time_date.clock0;
  var _x65 = (tz !== undefined) ? tz : $std_time_calendar.tz_utc;
  var _x62 = _x63.dc_to_instant(d, _x64, _x65, ts);
  var _x66 = (tz !== undefined) ? tz : $std_time_calendar.tz_utc;
  var _x67 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
  return instant_fs_time(_x62, _x66, _x67, ts);
}
 
 
// monadic lift
export function date_fs__mlift_time_10406(c, cal, d, tz, _c_x10332) /* (c : ? std/time/date/clock, cal : ? std/time/calendar/calendar, d : std/time/date/date, tz : ? std/time/calendar/timezone, std/time/instant/timescale) -> time */  {
  return $std_core_hnd._open_none0(function() {
    var _x68 = (c !== undefined) ? c : $std_time_date.clock0;
    var _x69 = (tz !== undefined) ? tz : $std_time_calendar.tz_utc;
    var _x70 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
    return timescale_fs_date_fs_time(_c_x10332, d, _x68, _x69, _x70);
  });
}
 
 
// Return the `:time` value for a given `:date` and `:clock` (=`clock0`) in a timezone `tz` (=`tz-utc` by default)
// interpreted by calendar `cal` (=`cal-iso`)
export function date_fs_time(d, c, tz, cal, ts) /* (d : std/time/date/date, c : ? std/time/date/clock, tz : ? std/time/calendar/timezone, cal : ? std/time/calendar/calendar, ts : ? std/time/instant/timescale) -> std/time/utc/utc time */  {
   
  if (ts !== undefined) {
    var x_10416 = ts;
  }
  else {
     
    var ev_10419 = $std_core_hnd._evv_at(0);
    var _x71 = $std_time_utc.utc_fs__select(ev_10419.hnd);
    var x_10416 = _x71(ev_10419.marker, ev_10419);
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_c_x10332 /* std/time/instant/timescale */ ) {
      return date_fs__mlift_time_10406(c, cal, d, tz, _c_x10332);
    });
  }
  else {
    return $std_core_hnd._open_none0(function() {
      var _x71 = (c !== undefined) ? c : $std_time_date.clock0;
      var _x72 = (tz !== undefined) ? tz : $std_time_calendar.tz_utc;
      var _x73 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
      return timescale_fs_date_fs_time(x_10416, d, _x71, _x72, _x73);
    });
  }
}
 
 
// Convert a `:time` `t` to a new `:time` value in a potentially different
// timezone `tz` (=`t.timezone` by default) and calendar (=`t.calendar` by default).
export function time_fs_time(t, tz, cal, ts) /* (t : time, tz : ? std/time/calendar/timezone, cal : ? std/time/calendar/calendar, ts : ? std/time/instant/timescale) -> time */  {
  var _x74 = t.instant;
  if (tz !== undefined) {
    var _x75 = tz;
  }
  else {
    var _x75 = t.timezone;
  }
  if (cal !== undefined) {
    var _x76 = cal;
  }
  else {
    var _x76 = t.calendar;
  }
  if (ts !== undefined) {
    var _x77 = ts;
  }
  else {
    var _x77 = t.instant.ts;
  }
  return instant_fs_time(_x74, _x75, _x76, _x77);
}
 
 
// Round a time to a specified second precision.
export function round_to_prec(t, prec) /* (t : time, prec : int) -> time */  {
  if ($std_core_types._int_lt(prec,0)) {
    return t;
  }
  else {
     
    var _x78 = t.clock.seconds;
    var secs = $std_num_ddouble.round_to_prec(_x78, prec);
     
    var _x79 = t.instant;
    var ri = $std_time_instant.round_to_prec(_x79, prec);
     
    var _x81 = secs.hi;
    var _x80 = (_x81 < (0.0));
    if (_x80) {
      var x_10072 = $std_num_ddouble.ceiling(secs);
    }
    else {
      var x_10072 = $std_num_ddouble.floor(secs);
    }
     
    var _x83 = t.clock.seconds.hi;
    var _x82 = (_x83 < (0.0));
    if (_x82) {
      var _x84 = t.clock.seconds;
      var y_10073 = $std_num_ddouble.ceiling(_x84);
    }
    else {
      var _x85 = t.clock.seconds;
      var y_10073 = $std_num_ddouble.floor(_x85);
    }
    var _x81 = x_10072.hi;
    var _x82 = y_10073.hi;
    var _x80 = $std_num_float64.cmp(_x81, _x82);
    if (_x80 === 2) {
      var _x83 = x_10072.lo;
      var _x84 = y_10073.lo;
      var _x79 = $std_num_float64.cmp(_x83, _x84);
    }
    else {
      var _x79 = _x80;
    }
    var _x78 = $std_core_order._lp__eq__eq__rp_(_x79, $std_core_types.Eq);
    if (_x78) {
      var _x86 = undefined;
      if (_x86 !== undefined) {
        var _x85 = _x86;
      }
      else {
        var _x85 = t.date;
      }
      var _x88 = undefined;
      if (_x88 !== undefined) {
        var _x87 = _x88;
      }
      else {
        var _x87 = t.clock.hours;
      }
      var _x90 = undefined;
      if (_x90 !== undefined) {
        var _x89 = _x90;
      }
      else {
        var _x89 = t.clock.minutes;
      }
      var _x92 = undefined;
      if (_x92 !== undefined) {
        var _x91 = _x92;
      }
      else {
        var _x91 = t.calendar;
      }
      var _x94 = undefined;
      if (_x94 !== undefined) {
        var _x93 = _x94;
      }
      else {
        var _x93 = t.tzdelta;
      }
      var _x96 = undefined;
      if (_x96 !== undefined) {
        var _x95 = _x96;
      }
      else {
        var _x95 = t.tzabbrv;
      }
      var _x98 = undefined;
      if (_x98 !== undefined) {
        var _x97 = _x98;
      }
      else {
        var _x97 = t.timezone;
      }
      return Time(_x85, $std_time_date.Clock(_x87, _x89, secs), _x91, _x93, _x95, _x97, ri);
    }
    else {
      var _x99 = t.timezone;
      var _x100 = t.calendar;
      return instant_fs_time(ri, _x99, _x100);
    }
  }
}
 
 
// pad with zeros
export function show0(i, width) /* (i : int, width : ? int) -> string */  {
  var _x101 = (width !== undefined) ? width : 2;
  return $std_core_string.pad_left($std_core_int.show(i), _x101, 0x0030);
}
 
 
// Show a time zone delta.
// Optional `utc` for displaying a zero timezone delta (=`"Z"`).
// Optional `hmsep` for the hour-minute separator (=`":"`).
// Optional `hrwidth` to give the minimal width of the hour field (=`2`).
export function show_tzdelta(delta, utc, hmsep, hrwidth) /* (delta : std/time/duration/duration, utc : ? string, hmsep : ? string, hrwidth : ? int) -> string */  {
   
  var _x102 = delta;
  var dt = $std_num_ddouble.int(_x102);
  if ($std_core_types._int_iszero(dt)) {
    return (utc !== undefined) ? utc : "Z";
  }
  else {
    var _x102 = $std_core_int.divmod($std_core_types._int_abs(dt), 60);
     
    var i_0_10087 = $std_core_types._int_div((_x102.fst),60);
     
    var i_1_10089 = $std_core_types._int_mod((_x102.fst),60);
     
    var _x103 = ($std_core_types._int_lt(dt,0)) ? "-" : "+";
    var _x104 = (hrwidth !== undefined) ? hrwidth : 2;
    var _x105 = (hmsep !== undefined) ? hmsep : ":";
    var _x107 = undefined;
    var _x106 = (_x107 !== undefined) ? _x107 : 2;
    var tz = $std_core_types._lp__plus__plus__rp_(_x103, $std_core_types._lp__plus__plus__rp_($std_core_string.pad_left($std_core_int.show(i_0_10087), _x104, 0x0030), $std_core_types._lp__plus__plus__rp_(_x105, $std_core_string.pad_left($std_core_int.show(i_1_10089), _x106, 0x0030))));
     
    if ($std_core_types._int_iszero((_x102.snd))) {
      var tzs = "";
    }
    else {
      var _x109 = undefined;
      var _x108 = (_x109 !== undefined) ? _x109 : 2;
      var tzs = $std_core_types._lp__plus__plus__rp_(":", $std_core_string.pad_left($std_core_int.show(_x102.snd), _x108, 0x0030));
    }
    return $std_core_types._lp__plus__plus__rp_(tz, tzs);
  }
}
 
export function show_raw(tp, prec) /* (tp : time, prec : ? int) -> string */  {
  var _x103 = tp.date;
  var _x104 = tp.calendar.month_prefix;
  var _x105 = tp.clock;
  var _x106 = (prec !== undefined) ? prec : 9;
  var _x107 = tp.tzdelta;
  var _x110 = tp.tzabbrv;
  var _x109 = (_x110 === (""));
  if (_x109) {
    var _x108 = "";
  }
  else {
    var _x112 = tp.tzabbrv;
    var _x111 = (($std_core_sslice.head(_x112)) === ("+"));
    if (_x111) {
      var _x108 = "";
    }
    else {
      var _x114 = tp.tzabbrv;
      var _x113 = (($std_core_sslice.head(_x114)) === ("-"));
      if (_x113) {
        var _x108 = "";
      }
      else {
        var _x115 = tp.tzabbrv;
        var _x108 = $std_core_types._lp__plus__plus__rp_(" (", $std_core_types._lp__plus__plus__rp_(_x115, ")"));
      }
    }
  }
  var _x118 = tp.calendar.name;
  var _x117 = (_x118 === (""));
  if (_x117) {
    var _x116 = "";
  }
  else {
    var _x120 = tp.calendar.month_prefix;
    var _x119 = (_x120 !== (""));
    if (_x119) {
      var _x116 = "";
    }
    else {
      var _x121 = tp.calendar.name;
      var _x116 = $std_core_types._lp__plus__plus__rp_(" ", _x121);
    }
  }
  var _x124 = tp.instant.ts.name;
  var _x123 = (_x124 === (""));
  if (_x123) {
    var _x122 = "";
  }
  else {
    var _x126 = tp.instant.ts.name;
    var _x125 = (_x126 === ("UTC"));
    if (_x125) {
      var _x122 = "";
    }
    else {
      var _x127 = tp.instant.ts.name;
      var _x122 = $std_core_types._lp__plus__plus__rp_(" ", _x127);
    }
  }
  return $std_core_types._lp__plus__plus__rp_($std_time_date.show(_x103, _x104), $std_core_types._lp__plus__plus__rp_("T", $std_core_types._lp__plus__plus__rp_($std_time_date.clock_fs_show(_x105, _x106), $std_core_types._lp__plus__plus__rp_(show_tzdelta(_x107), $std_core_types._lp__plus__plus__rp_(_x108, $std_core_types._lp__plus__plus__rp_(_x116, _x122))))));
}
 
 
// Show a `:time` in [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) format.
export function show(t, prec) /* (t : time, prec : ? int) -> string */  {
   
  var _x128 = (prec !== undefined) ? prec : 9;
  var tp = round_to_prec(t, _x128);
  var _x128 = (prec !== undefined) ? prec : 9;
  return show_raw(tp, _x128);
}
 
export function show_date(d) /* (d : std/time/date/date) -> string */  {
  return $std_time_date.show(d);
}
 
 
// monadic lift
export function _mlift_time_utc_10407(cal, i, tz, _y_x10333) /* (cal : ? std/time/calendar/calendar, i : std/time/instant/instant, tz : ? std/time/calendar/timezone, std/time/instant/timescale) -> std/time/utc/utc time */  {
  var _x129 = (tz !== undefined) ? tz : $std_time_calendar.tz_utc;
  var _x130 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
  return $std_core_hnd._open_none4(instant_fs_time, i, _x129, _x130, _y_x10333);
}
 
export function time_utc(i, tz, cal) /* (i : std/time/instant/instant, tz : ? std/time/calendar/timezone, cal : ? std/time/calendar/calendar) -> std/time/utc/utc time */  {
   
  var ev_10432 = $std_core_hnd._evv_at(0);
   
  var _x131 = $std_time_utc.utc_fs__select(ev_10432.hnd);
  var x_10429 = _x131(ev_10432.marker, ev_10432);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10333 /* std/time/instant/timescale */ ) {
      return _mlift_time_utc_10407(cal, i, tz, _y_x10333);
    });
  }
  else {
    var _x131 = (tz !== undefined) ? tz : $std_time_calendar.tz_utc;
    var _x132 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
    return $std_core_hnd._open_none4(instant_fs_time, i, _x131, _x132, x_10429);
  }
}
 
export function time_tai(i, tz, cal) /* (i : std/time/instant/instant, tz : ? std/time/calendar/timezone, cal : ? std/time/calendar/calendar) -> std/time/utc/utc time */  {
  var _x133 = (tz !== undefined) ? tz : $std_time_calendar.tz_utc;
  var _x134 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
  return $std_core_hnd._open_none4(instant_fs_time, i, _x133, _x134, $std_time_instant.ts_tai);
}
 
export function time_gps(i, tz, cal) /* (i : std/time/instant/instant, tz : ? std/time/calendar/timezone, cal : ? std/time/calendar/calendar) -> std/time/utc/utc time */  {
  var _x135 = (tz !== undefined) ? tz : $std_time_calendar.tz_utc;
  var _x136 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
  return $std_core_hnd._open_none4(instant_fs_time, i, _x135, _x136, $std_time_instant.ts_gps);
}
 
export function time_tt(i, tz, cal) /* (i : std/time/instant/instant, tz : ? std/time/calendar/timezone, cal : ? std/time/calendar/calendar) -> std/time/utc/utc time */  {
  var _x137 = (tz !== undefined) ? tz : $std_time_calendar.tz_utc;
  var _x138 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
  return $std_core_hnd._open_none4(instant_fs_time, i, _x137, _x138, $std_time_instant.ts_tt);
}
 
export function time_ti(i, tz, cal) /* (i : std/time/instant/instant, tz : ? std/time/calendar/timezone, cal : ? std/time/calendar/calendar) -> std/time/utc/utc time */  {
  var _x139 = (tz !== undefined) ? tz : $std_time_calendar.tz_utc;
  var _x140 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
  return $std_core_hnd._open_none4(instant_fs_time, i, _x139, _x140, $std_time_utc.ts_ti);
}
 
 
// Add an SI second `:duration` to a time.\
// `(time(2015,12,31,23,59,59,0.5) + duration(1)).show == "2016-01-01T00:00:00.500Z"`\
// `(time(2016,12,31,23,59,59,0.5) + duration(1)).show == "2016-12-31T23:59:60.500Z"` &quad; (into a leap second)
export function duration_fs__lp__plus__rp_(t, d) /* (t : time, d : std/time/duration/duration) -> time */  {
  var _x141 = t.instant;
  var _x142 = t.timezone;
  var _x143 = t.calendar;
  return instant_fs_time($std_time_instant._lp__plus__rp_(_x141, d), _x142, _x143);
}
 
 
// Add a date (years, months, days) and optional clock to a time.  Takes leap years, leap seconds, etc. into account.
export function add_date(t, d, c) /* (t : time, d : std/time/date/date, c : ? std/time/date/clock) -> time */  {
  var _x144 = t.instant.ts;
  var _x145 = t.date.year;
  var _x146 = d.year;
  var _x147 = t.date.month;
  var _x148 = d.month;
  var _x149 = t.date.day;
  var _x150 = d.day;
  var _x151 = t.clock.hours;
  var _x153 = (c !== undefined) ? c : $std_time_date.clock0;
  var _x152 = _x153.hours;
  var _x154 = t.clock.minutes;
  var _x156 = (c !== undefined) ? c : $std_time_date.clock0;
  var _x155 = _x156.minutes;
  var _x157 = t.clock.seconds;
  var _x159 = (c !== undefined) ? c : $std_time_date.clock0;
  var _x158 = _x159.seconds;
  var _x160 = t.timezone;
  var _x161 = t.calendar;
  return timescale_fs_date_fs_time(_x144, $std_time_date.$Date($std_core_types._int_add(_x145,_x146), $std_core_types._int_add(_x147,_x148), $std_core_types._int_add(_x149,_x150)), $std_time_date.Clock($std_core_types._int_add(_x151,_x152), $std_core_types._int_add(_x154,_x155), $std_num_ddouble._lp__plus__rp_(_x157, _x158)), _x160, _x161);
}
 
 
// Add a clock (hours, minutes, seconds) to a time. Takes leap years, leap seconds, etc. into account.
export function add_clock(t, c) /* (t : time, c : std/time/date/clock) -> time */  {
  var _x162 = t.instant.ts;
  var _x163 = t.date;
  var _x164 = t.clock.hours;
  var _x165 = c.hours;
  var _x166 = t.clock.minutes;
  var _x167 = c.minutes;
  var _x168 = t.clock.seconds;
  var _x169 = c.seconds;
  var _x170 = t.timezone;
  var _x171 = t.calendar;
  return timescale_fs_date_fs_time(_x162, _x163, $std_time_date.Clock($std_core_types._int_add(_x164,_x165), $std_core_types._int_add(_x166,_x167), $std_num_ddouble._lp__plus__rp_(_x168, _x169)), _x170, _x171);
}
 
 
// Add  a specified number of days to a calendar time. Takes leap years etc. into account.\
// `time(2016,12,31,12).add-days(1).show == "2017-01-01T12:00:00Z"` &quad; (over a leap second)\
// `time(1582,10,4,cal=cal-jg).add-days(1).show == "1582-10-15T00:00:00Z JG"` &quad; (transition from Julian (`cal-julian`) to Gregorian (`cal-gregorian`) calendar)\
export function add_days(t, days, c) /* (t : time, days : int, c : ? std/time/date/clock) -> time */  {
  var _x172 = (c !== undefined) ? c : $std_time_date.clock0;
  return add_date(t, $std_time_date.$Date(0, 0, days), _x172);
}
 
 
// Add  a specified number of weeks to a calendar time.
export function add_weeks(t, weeks) /* (t : time, weeks : int) -> time */  {
   
  var days_10139 = $std_core_types._int_mul(weeks,7);
  var _x174 = undefined;
  var _x173 = (_x174 !== undefined) ? _x174 : $std_time_date.clock0;
  return add_date(t, $std_time_date.$Date(0, 0, days_10139), _x173);
}
 
 
// Add  a specified number of months to a calendar time.
export function add_months(t, months) /* (t : time, months : int) -> time */  {
  return add_date(t, $std_time_date.$Date(0, months, 0));
}
 
 
// Add  a specified number of years to a calendar time.
export function add_years(t, years) /* (t : time, years : int) -> time */  {
  return add_date(t, $std_time_date.$Date(years, 0, 0));
}
 
 
// Subtract an SI second `:duration` from a time.\
// `(time(2016,1,1,0,0,0,0.5) - duration(1)).show == "2015-12-31T23:59:59.500Z"`\
// `(time(2017,1,1,0,0,0,0.5) - duration(1)).show == "2016-12-31T23:59:60.500Z"` &quad; (into a leap second)
export function duration_fs__lp__dash__rp_(t, d) /* (t : time, d : std/time/duration/duration) -> time */  {
   
  var _x175 = d.hi;
  var _x176 = d.lo;
  var d_0_10142 = $std_num_ddouble.Ddouble((-_x175), (-_x176));
  var _x175 = t.instant;
  var _x176 = t.timezone;
  var _x177 = t.calendar;
  return instant_fs_time($std_time_instant._lp__plus__rp_(_x175, d_0_10142), _x176, _x177);
}
 
 
// Return the exact SI second duration between to times.\
// `(time(2016,1,1,0,0,0,0.5) - time(2015,12,31,23,59,59)).show == "1.5s"`\
// `(time(2017,1,1,0,0,0,0.5) - time(2016,12,31,23,59,59)).show == "2.5s"`  &quad; (over a leap second)\
// `(time(2017,1,1,0,0,0,0.5) - time(2017,1,1,2,59,59,tz=tz-fixed(3)).show == "2.5s"`  &quad; (GMT+3, and over a leap second)\
export function _lp__dash__rp_(t1, t2) /* (t1 : time, t2 : time) -> std/time/duration/duration */  {
  var _x178 = t1.instant;
  var _x179 = t2.instant;
  return $std_time_instant._lp__dash__rp_(_x178, _x179);
}
 
 
// Copy a `:time` with optionally changing any of its attributes.\
// `t.copy(month=1,day=1,hours=0,minutes=0,seconds=zero)` &quad; (copy to start of the year)\
// `t.copy(month=t.month+2,day=t.day+3)                 ` &quad; (add 2 months and 3 days)\
// `t.copy(tz=tz-local())                               ` &quad; (change to a different time zone)
export function copy(t, year_0, month_0, day_0, hours_0, minutes_0, seconds_0, tz, cal, ts) /* (t : time, year : ? int, month : ? int, day : ? int, hours : ? int, minutes : ? int, seconds : ? std/num/ddouble/ddouble, tz : ? std/time/calendar/timezone, cal : ? std/time/calendar/calendar, ts : ? std/time/instant/timescale) -> time */  {
  if (ts !== undefined) {
    var _x180 = ts;
  }
  else {
    var _x180 = t.instant.ts;
  }
  if (year_0 !== undefined) {
    var _x181 = year_0;
  }
  else {
    var _x181 = t.date.year;
  }
  if (month_0 !== undefined) {
    var _x182 = month_0;
  }
  else {
    var _x182 = t.date.month;
  }
  if (day_0 !== undefined) {
    var _x183 = day_0;
  }
  else {
    var _x183 = t.date.day;
  }
  if (hours_0 !== undefined) {
    var _x184 = hours_0;
  }
  else {
    var _x184 = t.clock.hours;
  }
  if (minutes_0 !== undefined) {
    var _x185 = minutes_0;
  }
  else {
    var _x185 = t.clock.minutes;
  }
  if (seconds_0 !== undefined) {
    var _x186 = seconds_0;
  }
  else {
    var _x186 = t.clock.seconds;
  }
  if (tz !== undefined) {
    var _x187 = tz;
  }
  else {
    var _x187 = t.timezone;
  }
  if (cal !== undefined) {
    var _x188 = cal;
  }
  else {
    var _x188 = t.calendar;
  }
  return timescale_fs_date_fs_time(_x180, $std_time_date.$Date(_x181, _x182, _x183), $std_time_date.Clock(_x184, _x185, _x186), _x187, _x188);
}
 
 
// Copy a `:time` with a new `:date` and optional `:clock` (=`t.clock`), and optionally a new
// timezone (=`t.timezone`) and calendar (=`t.calendar`).
export function copy_dc(t, d, c, tz, cal, ts) /* (t : time, d : ? std/time/date/date, c : ? std/time/date/clock, tz : ? std/time/calendar/timezone, cal : ? std/time/calendar/calendar, ts : ? std/time/instant/timescale) -> time */  {
  if (ts !== undefined) {
    var _x189 = ts;
  }
  else {
    var _x189 = t.instant.ts;
  }
  if (d !== undefined) {
    var _x190 = d;
  }
  else {
    var _x190 = t.date;
  }
  if (c !== undefined) {
    var _x191 = c;
  }
  else {
    var _x191 = t.clock;
  }
  if (tz !== undefined) {
    var _x192 = tz;
  }
  else {
    var _x192 = t.timezone;
  }
  if (cal !== undefined) {
    var _x193 = cal;
  }
  else {
    var _x193 = t.calendar;
  }
  return timescale_fs_date_fs_time(_x189, _x190, _x191, _x192, _x193);
}
 
 
// Return the time at the start of the day of time `t`.
export function start_of_day(t) /* (t : time) -> time */  {
  var _x194 = t.date;
  return copy_dc(t, _x194, $std_time_date.clock0);
}
 
 
// Return the time at the start of the month of time `t`.
export function start_of_month(t) /* (t : time) -> time */  {
  var _x196 = undefined;
  if (_x196 !== undefined) {
    var _x195 = _x196;
  }
  else {
    var _x195 = t.date.year;
  }
  var _x198 = undefined;
  if (_x198 !== undefined) {
    var _x197 = _x198;
  }
  else {
    var _x197 = t.date.month;
  }
  return copy_dc(t, $std_time_date.$Date(_x195, _x197, 1), $std_time_date.clock0);
}
 
 
// Return the days between two times. Uses the calendar and timezone
// of the first time `t1` to determine the date of `t2`.
// ```
// days-until( time(2000,1,1), time(2000,1,1) ) == 0
// days-until( time(2000,1,1), time(2000,1,2) ) == 1
// days-until( time(2000,1,1), time(2000,1,2,tz=tz-fixed(1)) ) == 0
// days-until( time(2000,1,1), time(2000,3,1) ) == 60
// days-until( time(2000,2,1), time(2000,1,1) ) == -1
// ```
export function days_until(t1, t2) /* (t1 : time, t2 : time) -> int */  {
   
  var _x200 = t1.calendar.long_name;
  var _x201 = t2.calendar.long_name;
  var _x199 = (_x200 === _x201);
  if (_x199) {
    var _x203 = t1.timezone.name;
    var _x204 = t2.timezone.name;
    var _x202 = (_x203 === _x204);
    if (_x202) {
      var time_0_10185 = t2;
    }
    else {
      var _x205 = t2.instant;
      var _x206 = t1.timezone;
      var _x207 = t1.calendar;
      var time_0_10185 = instant_fs_time(_x205, _x206, _x207);
    }
  }
  else {
    var _x208 = t2.instant;
    var _x209 = t1.timezone;
    var _x210 = t1.calendar;
    var time_0_10185 = instant_fs_time(_x208, _x209, _x210);
  }
   
  var _x211 = time_0_10185.date;
  var x = t1.calendar.date_to_days(_x211);
   
  var _x212 = t1.date;
  var y = t1.calendar.date_to_days(_x212);
  return $std_core_types._int_sub(x,y);
}
 
 
// Return the day of the year of time `t` (starting at 1).
export function day_of_year(t) /* (t : time) -> int */  {
   
  var _x199 = t.date;
  var x_0 = t.calendar.date_to_days(_x199);
   
  var _x200 = t.date.year;
  var y_0 = t.calendar.date_to_days($std_time_date.$Date(_x200, 1, 1));
   
  var x_10205 = $std_core_types._int_sub(x_0,y_0);
  return $std_core_types._int_add(x_10205,1);
}
 
 
// Return the total days in the month of time `t`.
export function days_in_month(t) /* (t : time) -> int */  {
   
  var _x199 = t.date.year;
  var _x200 = t.date.month;
  var d2_10218 = $std_time_date.$Date(_x199, $std_core_types._int_add(_x200,1), 1);
   
  var x = t.calendar.date_to_days(d2_10218);
   
  var _x201 = t.date.year;
  var _x202 = t.date.month;
  var y = t.calendar.date_to_days($std_time_date.$Date(_x201, _x202, 1));
  return $std_core_types._int_sub(x,y);
}
 
 
// Return the total days in the year of time `t`.
export function days_in_year(t) /* (t : time) -> int */  {
   
  var _x199 = t.date.year;
  var d2_10228 = $std_time_date.$Date($std_core_types._int_add(_x199,1), 1, 1);
   
  var x = t.calendar.date_to_days(d2_10228);
   
  var _x200 = t.date.year;
  var y = t.calendar.date_to_days($std_time_date.$Date(_x200, 1, 1));
  return $std_core_types._int_sub(x,y);
}
 
 
// Return the modified Julian date ([MJD](https://en.wikipedia.org/wiki/Julian_day#Variants))
// number for a given `:time`. This interprets the Modified Julian Date in the calendar
// system of `t` with the timezone applied.\
// `time(1972,1,2,tz=tz-fixed(1)).mjd == "41318"`\
// `time(1972,1,2,tz=tz-fixed(1)).instant.mjd(ts-utc).show == "41317.958333335"` &quad; (one hour earlier)
//
// Also takes leap seconds into account:\
// `time(2015,12,31,12,0,0).mjd.show == "57387.5"` &quad; (exactly mid-day)\
// `time(2016,12,31,12,0,0).mjd.show(9) == "57753.499994213"` &quad; (this day has a leap second, so it is just before the real middle of the day)\
// `time(2016,12,31,12,0,0,0.5).mjd.show == "57753.5"` &quad; (real middle of the day)\
export function mjd(t) /* (t : time) -> std/num/ddouble/ddouble */  {
  var _x199 = t.instant;
  var _x200 = t.instant.ts;
  var _x201 = t.tzdelta;
  return $std_time_instant.mjd(_x199, _x200, _x201);
}
 
 
// Return the weekday of a given time `t`.
export function weekday(t) /* (t : time) -> std/time/date/weekday */  {
   
  var _x202 = t.instant;
  var _x203 = t.instant.ts;
  var _x204 = t.tzdelta;
  var days = $std_num_ddouble.int($std_num_ddouble.floor($std_time_instant.mjd(_x202, _x203, _x204)));
   
  var dow = $std_core_types._int_mod(($std_core_types._int_add(days,3)),7);
  return $std_time_date.weekday(dow);
}
 
 
// Return the time at the start of the week (Monday) of time `t`.
export function start_of_week(t) /* (t : time) -> time */  {
   
  var dow = weekday(t);
  var _x202 = $std_core_order._lp__eq__eq__rp_($std_time_date.weekday_fs_cmp(dow, $std_time_date.Mon), $std_core_types.Eq);
  if (_x202) {
    var _x203 = t.date;
    return copy_dc(t, _x203, $std_time_date.clock0);
  }
  else {
     
    var y_0_10251 = $std_time_date.int(dow);
     
    var _x204 = t.date.day;
    var x_10248 = $std_core_types._int_sub(_x204,y_0_10251);
     
    var _arg_x5394 = $std_core_types._int_add(x_10248,1);
    var _x205 = undefined;
    if (_x205 !== undefined) {
      var _x204 = _x205;
    }
    else {
      var _x204 = t.date.year;
    }
    var _x207 = undefined;
    if (_x207 !== undefined) {
      var _x206 = _x207;
    }
    else {
      var _x206 = t.date.month;
    }
    if (_arg_x5394 !== undefined) {
      var _x208 = _arg_x5394;
    }
    else {
      var _x208 = t.date.day;
    }
    return copy_dc(t, $std_time_date.$Date(_x204, _x206, _x208), $std_time_date.clock0);
  }
}
 
 
// Return the time at the start of the year of time `t`.
export function start_of_year(t) /* (t : time) -> time */  {
  var _x209 = t.date.year;
  return copy_dc(t, $std_time_date.$Date(_x209, 1, 1), $std_time_date.clock0);
}
 
 
// Return a `:time` as a fractional year.\
// `year-frac(time(2000,7,2)) == fixed(2000.5)`
export function year_frac(t) /* (t : time) -> std/num/ddouble/ddouble */  {
   
  var _x210 = t.instant;
  var _x211 = t.instant.ts;
  var _x212 = t.tzdelta;
  var x_10263 = $std_num_ddouble.fraction($std_time_instant.mjd(_x210, _x211, _x212));
   
  var x_0_10264 = day_of_year(t);
   
  var _x213 = x_10263.hi;
  var yfrac = (((($std_core_types._int_to_double(($std_core_types._int_sub(x_0_10264,1)))) + _x213)) / ($std_core_types._int_to_double((days_in_year(t)))));
  var _x210 = t.date.year;
  return $std_time_timestamp.int_fs_timespan(_x210, yfrac);
}
 
 
// Return the total months in the year of time `t`.
// For Gregorian calendars this is always 12 but some calendars have a varying number of months per year.
export function months_in_year(t) /* (t : time) -> int */  {
   
  var _x211 = t.date.year;
  var t_0_10269 = copy_dc(t, $std_time_date.$Date($std_core_types._int_add(_x211,1), 1, 0));
  return t_0_10269.date.month;
}
 
 
// Return the `n`th week day following (and including) time `t`.
// Use `n = 1` for the first week day `wd` following `t`;
// Use `0` for the last occurrence of week day `wd` before `t`.\
// `time(2016,10,10).nth-weekday(1,Sun)` &quad; (2016-10-13, first Sunday following October 10, 2016)
// `time(2016,11,16).start-of-month.nth-weekday(0,Wed)` &quad; (2016-10-26, the last Wednesday before 2016-11-01)
export function nth_weekday(t, n, wd) /* (t : time, n : int, wd : std/time/date/weekday) -> time */  {
   
  var dow = weekday(t);
   
  var x_10029 = $std_time_date.int(wd);
   
  var y_10030 = $std_time_date.int(dow);
   
  var inc = $std_core_types._int_mod(($std_core_types._int_sub(x_10029,y_10030)),7);
   
  var _x211 = t.date.day;
  var x_10279 = $std_core_types._int_add(_x211,inc);
   
  var y_10280 = $std_core_types._int_mul(7,($std_core_types._int_sub(n,1)));
   
  var wday = $std_core_types._int_add(x_10279,y_10280);
  var _x211 = t.date.year;
  var _x212 = t.date.month;
  return copy_dc(t, $std_time_date.$Date(_x211, _x212, wday));
}
 
 
// Return the first week day following (and including) time `t`.
export function first_weekday(t, wd) /* (t : time, wd : std/time/date/weekday) -> time */  {
  return nth_weekday(t, 1, wd);
}
 
 
// Return the last week day before time `t`.\
// `time(2016,11,1).last-weekday(Sun)` &quad;  (2016-10-30, Last Sunday of October 2016)
export function last_weekday(t, wd) /* (t : time, wd : std/time/date/weekday) -> time */  {
  return nth_weekday(t, 0, wd);
}
 
 
// Return the `n`th week day since the beginning of the month of time `t`.\
// `time(2016,10,10).nth-weekday-of-month(2,Sun).date` &quad; (2016-10-09, Second Sunday of October 2016)\
// `time(2016,10,10).nth-weekday-of-month(60,Sun).date` &quad; (2017-11-19, 60th Sunday since October 1, 2016)\
export function nth_weekday_of_month(t, n, wd) /* (t : time, n : int, wd : std/time/date/weekday) -> time */  {
  var _x213 = t.date.year;
  var _x214 = t.date.month;
  return nth_weekday(copy_dc(t, $std_time_date.$Date(_x213, _x214, 1)), n, wd);
}
 
 
// Return the first week day of the month of time `t`.\
// `time(2016,10,10).first-weekday-of-month(Sun)` &quad; (2016-10-02, First Sunday of October 2016)
export function first_weekday_of_month(t, wd) /* (t : time, wd : std/time/date/weekday) -> time */  {
  var _x215 = t.date.year;
  var _x216 = t.date.month;
  return nth_weekday(copy_dc(t, $std_time_date.$Date(_x215, _x216, 1)), 1, wd);
}
 
 
// Return the last week day of the month of time `t`.\
// `time(2016,10,10).last-weekday-of-month(Sun).date` &quad; (2016-10-30, Last Sunday of October 2016)
export function last_weekday_of_month(t, wd) /* (t : time, wd : std/time/date/weekday) -> time */  {
   
  var _x217 = t.date.year;
  var _x218 = t.date.month;
  var t_0_10300 = copy_dc(t, $std_time_date.$Date(_x217, $std_core_types._int_add(_x218,1), 1));
  return nth_weekday(t_0_10300, 0, wd);
}
 
export var mjd_epoch_delta;
var mjd_epoch_delta = $std_time_timestamp.int_fs_timespan(51544);
 
export var jd_epoch_delta;
var jd_epoch_delta = $std_num_ddouble.Ddouble(2400000.5, 0.0);
 
 
/* Return the Julian date ([JD](https://en.wikipedia.org/wiki/Julian_day))
number for a given `:time` `t`. This interprets the Julian date in the calendar of `t` with
the timezone of `t` applied.\
`time(-4713,11,24,12,cal=cal-tt).jd == "0"`\
`time(1972,1,2,tz=tz-fixed(1)).jd == "2441318.5"`\
`time(1972,1,2,tz=tz-fixed(1)).instant.jd(ts-ti).show(9) == "2441318.458333333"`
Takes leap seconds into account when calculating the fraction of the day,
see `mjd` for examples.
*/
export function jd(t) /* (t : time) -> std/num/ddouble/ddouble */  {
  var _x217 = t.instant;
  var _x218 = t.instant.ts;
  var _x219 = t.tzdelta;
  return $std_num_ddouble._lp__plus__rp_($std_time_instant.mjd(_x217, _x218, _x219), jd_epoch_delta);
}
 
 
// Return the current time in an optional timezone (=`tz-utc`) and optional calendar (=`cal-iso`).
export function timescale_fs_time_now(ts, tz, cal) /* (ts : std/time/instant/timescale, tz : ? std/time/calendar/timezone, cal : ? std/time/calendar/calendar) -> ndet time */  {
  var _x220 = (tz !== undefined) ? tz : $std_time_calendar.tz_utc;
  var _x221 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
  return instant_fs_time($std_time_chrono.now_in(ts), _x220, _x221);
}
 
 
// monadic lift
export function timezone_fs__mlift_time_now_10408(cal, tz, _c_x10335) /* (cal : ? std/time/calendar/calendar, tz : ? std/time/calendar/timezone, std/time/instant/timescale) -> time */  {
  var _x224 = (tz !== undefined) ? tz : $std_time_calendar.tz_utc;
  var _x225 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
  return $std_core_hnd._open_none3(function(ts_0 /* std/time/instant/timescale */ , tz_0 /* ? std/time/calendar/timezone */ , cal_0 /* ? std/time/calendar/calendar */ ) {
      var _x222 = (tz_0 !== undefined) ? tz_0 : $std_time_calendar.tz_utc;
      var _x223 = (cal_0 !== undefined) ? cal_0 : $std_time_calendar.cal_iso;
      return instant_fs_time($std_time_chrono.now_in(ts_0), _x222, _x223);
    }, _c_x10335, _x224, _x225);
}
 
 
// Return the current time in an optional timezone (=`tz-utc`) and optional calendar (=`cal-iso`).
export function timezone_fs_time_now(tz, cal, ts) /* (tz : ? std/time/calendar/timezone, cal : ? std/time/calendar/calendar, ts : ? std/time/instant/timescale) -> <ndet,std/time/utc/utc> time */  {
   
  if (ts !== undefined) {
    var x_10434 = ts;
  }
  else {
     
    var ev_10437 = $std_core_hnd._evv_at(0);
    var _x226 = $std_time_utc.utc_fs__select(ev_10437.hnd);
    var x_10434 = _x226(ev_10437.marker, ev_10437);
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_c_x10335 /* std/time/instant/timescale */ ) {
      return timezone_fs__mlift_time_now_10408(cal, tz, _c_x10335);
    });
  }
  else {
    var _x228 = (tz !== undefined) ? tz : $std_time_calendar.tz_utc;
    var _x229 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
    return $std_core_hnd._open_none3(function(ts_0 /* std/time/instant/timescale */ , tz_0 /* ? std/time/calendar/timezone */ , cal_0 /* ? std/time/calendar/calendar */ ) {
        var _x226 = (tz_0 !== undefined) ? tz_0 : $std_time_calendar.tz_utc;
        var _x227 = (cal_0 !== undefined) ? cal_0 : $std_time_calendar.cal_iso;
        return instant_fs_time($std_time_chrono.now_in(ts_0), _x226, _x227);
      }, x_10434, _x228, _x229);
  }
}
 
 
// Return the current time in the local timezone and optional calendar (=`cal-iso`).
export function timescale_fs_local_time_now(ts, cal) /* (ts : std/time/instant/timescale, cal : ? std/time/calendar/calendar) -> ndet time */  {
   
  var tz_10312 = $std_time_calendar.tz_local();
  var _x230 = (tz_10312 !== undefined) ? tz_10312 : $std_time_calendar.tz_utc;
  var _x231 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
  return instant_fs_time($std_time_chrono.now_in(ts), _x230, _x231);
}
 
 
// Return the current time in the local timezone and optional calendar (=`cal-iso`).
export function calendar_fs_local_time_now(cal) /* (cal : ? std/time/calendar/calendar) -> <ndet,std/time/utc/utc> time */  {
  var _x232 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
  return timezone_fs_time_now($std_core_hnd._open_none0($std_time_calendar.tz_local), _x232);
}
 
 
// Convert a `:time` `t` to a new `:time` value in the local time zone
// in an optional calendar (=`t.calendar` by default).
export function local_time(t, cal) /* (t : time, cal : ? std/time/calendar/calendar) -> ndet time */  {
   
  var tz_10326 = $std_time_calendar.tz_local();
  var _x233 = t.instant;
  if (tz_10326 !== undefined) {
    var _x234 = tz_10326;
  }
  else {
    var _x234 = t.timezone;
  }
  if (cal !== undefined) {
    var _x235 = cal;
  }
  else {
    var _x235 = t.calendar;
  }
  var _x237 = undefined;
  if (_x237 !== undefined) {
    var _x236 = _x237;
  }
  else {
    var _x236 = t.instant.ts;
  }
  return instant_fs_time(_x233, _x234, _x235, _x236);
}
 
 
// Convert an `:instant` to a `:time` value in the local timezone, in an optional calendar (=`cal-iso` by default).
export function instant_fs_local_time(i, cal) /* (i : std/time/instant/instant, cal : ? std/time/calendar/calendar) -> ndet time */  {
  var _x238 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
  return instant_fs_time(i, $std_time_calendar.tz_local(), _x238);
}
 
 
// Return the `:time` value for a given date and clock in the local timezone
// interpreted by calendar `cal` (=`cal-iso`). See `instant` for roll-over behavior.
export function timescale_fs_local_time(ts, year_0, month_0, day_0, hours_0, minutes_0, secs, frac, cal) /* (ts : std/time/instant/timescale, year : int, month : ? int, day : ? int, hours : ? int, minutes : ? int, secs : ? int, frac : ? float64, cal : ? std/time/calendar/calendar) -> ndet time */  {
  var _x239 = (month_0 !== undefined) ? month_0 : 1;
  var _x240 = (day_0 !== undefined) ? day_0 : 1;
  var _x241 = (hours_0 !== undefined) ? hours_0 : 0;
  var _x242 = (minutes_0 !== undefined) ? minutes_0 : 0;
  var _x243 = (secs !== undefined) ? secs : 0;
  var _x244 = (frac !== undefined) ? frac : 0.0;
  var _x245 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
  return timescale_fs_time(ts, year_0, _x239, _x240, _x241, _x242, _x243, _x244, $std_time_calendar.tz_local(), _x245);
}
 
 
// Return the `:time` value for a given `:date` and `:clock` (=`clock0`) in the local timezone
// interpreted by calendar `cal` (=`cal-iso`)
export function date_fs_local_time(ts, d, c, cal) /* (ts : std/time/instant/timescale, d : std/time/date/date, c : ? std/time/date/clock, cal : ? std/time/calendar/calendar) -> ndet time */  {
  var _x246 = (c !== undefined) ? c : $std_time_date.clock0;
  var _x247 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
  return timescale_fs_date_fs_time(ts, d, _x246, $std_time_calendar.tz_local(), _x247);
}
 
 
// monadic lift
export function dateutc_fs__mlift_local_time_10409(c, cal, d, _c_x10338) /* (c : ? std/time/date/clock, cal : ? std/time/calendar/calendar, d : std/time/date/date, std/time/instant/timescale) -> time */  {
   
  var _x_x4_10394 = $std_core_hnd._open_none0($std_time_calendar.tz_local);
  return $std_core_hnd._open_none0(function() {
    var _x248 = (c !== undefined) ? c : $std_time_date.clock0;
    var _x249 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
    return timescale_fs_date_fs_time(_c_x10338, d, _x248, _x_x4_10394, _x249);
  });
}
 
 
// Return the `:time` value for a given `:date` and `:clock` (=`clock0`) in the local timezone
// interpreted by calendar `cal` (=`cal-iso`)
export function dateutc_fs_local_time(d, c, cal, ts) /* (d : std/time/date/date, c : ? std/time/date/clock, cal : ? std/time/calendar/calendar, ts : ? std/time/instant/timescale) -> <ndet,std/time/utc/utc> time */  {
   
  if (ts !== undefined) {
    var x_10439 = ts;
  }
  else {
     
    var ev_10442 = $std_core_hnd._evv_at(0);
    var _x250 = $std_time_utc.utc_fs__select(ev_10442.hnd);
    var x_10439 = _x250(ev_10442.marker, ev_10442);
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_c_x10338 /* std/time/instant/timescale */ ) {
      return dateutc_fs__mlift_local_time_10409(c, cal, d, _c_x10338);
    });
  }
  else {
     
    var _x_x4_10394 = $std_core_hnd._open_none0($std_time_calendar.tz_local);
    return $std_core_hnd._open_none0(function() {
      var _x250 = (c !== undefined) ? c : $std_time_date.clock0;
      var _x251 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
      return timescale_fs_date_fs_time(x_10439, d, _x250, _x_x4_10394, _x251);
    });
  }
}
 
 
// monadic lift
export function year_fs__mlift_local_time_10410(cal, day_0, frac, hours_0, minutes_0, month_0, secs, year_0, _c_x10340) /* (cal : ? std/time/calendar/calendar, day@0 : ? int, frac : ? float64, hours@0 : ? int, minutes@0 : ? int, month@0 : ? int, secs : ? int, year@0 : int, std/time/instant/timescale) -> time */  {
  return $std_core_hnd._open_none0(function() {
    var _x252 = (month_0 !== undefined) ? month_0 : 1;
    var _x253 = (day_0 !== undefined) ? day_0 : 1;
    var _x254 = (hours_0 !== undefined) ? hours_0 : 0;
    var _x255 = (minutes_0 !== undefined) ? minutes_0 : 0;
    var _x256 = (secs !== undefined) ? secs : 0;
    var _x257 = (frac !== undefined) ? frac : 0.0;
    var _x258 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
    return timescale_fs_local_time(_c_x10340, year_0, _x252, _x253, _x254, _x255, _x256, _x257, _x258);
  });
}
 
 
// Return the `:time` value for a given date and clock in the local timezone
// interpreted by calendar `cal` (=`cal-iso`). See `instant` for roll-over behavior.
export function year_fs_local_time(year_0, month_0, day_0, hours_0, minutes_0, secs, frac, cal, ts) /* (year : int, month : ? int, day : ? int, hours : ? int, minutes : ? int, secs : ? int, frac : ? float64, cal : ? std/time/calendar/calendar, ts : ? std/time/instant/timescale) -> <ndet,std/time/utc/utc> time */  {
   
  if (ts !== undefined) {
    var x_10444 = ts;
  }
  else {
     
    var ev_10447 = $std_core_hnd._evv_at(0);
    var _x259 = $std_time_utc.utc_fs__select(ev_10447.hnd);
    var x_10444 = _x259(ev_10447.marker, ev_10447);
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_c_x10340 /* std/time/instant/timescale */ ) {
      return year_fs__mlift_local_time_10410(cal, day_0, frac, hours_0, minutes_0, month_0, secs, year_0, _c_x10340);
    });
  }
  else {
    return $std_core_hnd._open_none0(function() {
      var _x259 = (month_0 !== undefined) ? month_0 : 1;
      var _x260 = (day_0 !== undefined) ? day_0 : 1;
      var _x261 = (hours_0 !== undefined) ? hours_0 : 0;
      var _x262 = (minutes_0 !== undefined) ? minutes_0 : 0;
      var _x263 = (secs !== undefined) ? secs : 0;
      var _x264 = (frac !== undefined) ? frac : 0.0;
      var _x265 = (cal !== undefined) ? cal : $std_time_calendar.cal_iso;
      return timescale_fs_local_time(x_10444, year_0, _x259, _x260, _x261, _x262, _x263, _x264, _x265);
    });
  }
}