// Koka generated module: std/time/date, koka version: 3.2.4
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
 
// externals
 
// type declarations
// type clock
export function Clock(hours, minutes, seconds) /* (hours : int, minutes : int, seconds : std/num/ddouble/ddouble) -> clock */  {
  return { hours: hours, minutes: minutes, seconds: seconds };
}
// type date
export function $Date(year, month, day) /* (year : int, month : int, day : int) -> date */  {
  return { year: year, month: month, day: day };
}
// type weekday
export const Mon = 1; // weekday
export const Tue = 2; // weekday
export const Wed = 3; // weekday
export const Thu = 4; // weekday
export const Fri = 5; // weekday
export const Sat = 6; // weekday
export const Sun = 7; // weekday
 
// declarations
 
 
// Automatically generated. Retrieves the `year` constructor field of the `:date` type.
export function date_fs_year(date) /* (date : date) -> int */  {
  return date.year;
}
 
 
// Automatically generated. Retrieves the `month` constructor field of the `:date` type.
export function date_fs_month(date) /* (date : date) -> int */  {
  return date.month;
}
 
 
// Automatically generated. Retrieves the `day` constructor field of the `:date` type.
export function date_fs_day(date) /* (date : date) -> int */  {
  return date.day;
}
 
export function date_fs__copy(_this, year, month, day) /* (date, year : ? int, month : ? int, day : ? int) -> date */  {
  if (year !== undefined) {
    var _x0 = year;
  }
  else {
    var _x0 = _this.year;
  }
  if (month !== undefined) {
    var _x1 = month;
  }
  else {
    var _x1 = _this.month;
  }
  if (day !== undefined) {
    var _x2 = day;
  }
  else {
    var _x2 = _this.day;
  }
  return $Date(_x0, _x1, _x2);
}
 
 
// Automatically generated. Retrieves the `hours` constructor field of the `:clock` type.
export function clock_fs_hours(clock) /* (clock : clock) -> int */  {
  return clock.hours;
}
 
 
// Automatically generated. Retrieves the `minutes` constructor field of the `:clock` type.
export function clock_fs_minutes(clock) /* (clock : clock) -> int */  {
  return clock.minutes;
}
 
 
// Automatically generated. Retrieves the `seconds` constructor field of the `:clock` type.
export function clock_fs_seconds(clock) /* (clock : clock) -> std/num/ddouble/ddouble */  {
  return clock.seconds;
}
 
 
// Convert a `:weekday` to an `:int` using the ISO definition which starts at Monday as 1,
// up to Sunday as 7.
export function int(wd) /* (wd : weekday) -> int */  {
  if (wd === 1) {
    return 1;
  }
  else if (wd === 2) {
    return 2;
  }
  else if (wd === 3) {
    return 3;
  }
  else if (wd === 4) {
    return 4;
  }
  else if (wd === 5) {
    return 5;
  }
  else if (wd === 6) {
    return 6;
  }
  else {
    return 7;
  }
}
 
 
// Create an ISO weekdate where the "month" is the ISO week number.
export function weekdate(year, month, weekday_0) /* (year : int, month : int, weekday : weekday) -> date */  {
  return $Date(year, month, int(weekday_0));
}
 
 
// Add two dates field-wise together.
export function _lp__plus__rp_(d1, d2) /* (d1 : date, d2 : date) -> date */  {
  var _x3 = d1.year;
  var _x4 = d2.year;
  var _x5 = d1.month;
  var _x6 = d2.month;
  var _x7 = d1.day;
  var _x8 = d2.day;
  return $Date($std_core_types._int_add(_x3,_x4), $std_core_types._int_add(_x5,_x6), $std_core_types._int_add(_x7,_x8));
}
 
 
// Add two clock together.
export function clock_fs__lp__plus__rp_(c, d) /* (c : clock, d : clock) -> clock */  {
  var _x9 = c.hours;
  var _x10 = d.hours;
  var _x11 = c.minutes;
  var _x12 = d.minutes;
  var _x13 = c.seconds;
  var _x14 = d.seconds;
  return Clock($std_core_types._int_add(_x9,_x10), $std_core_types._int_add(_x11,_x12), $std_num_ddouble._lp__plus__rp_(_x13, _x14));
}
 
 
// Convert a weekday number to a `:weekday`(starting at Monday (=1) up to Sunday (=7)).
// Takes the integer `i - 1` modulo 7, so `0` or `14` also become Sunday etc.
export function weekday(i) /* (i : int) -> weekday */  {
   
  var d = $std_core_types._int_mod(($std_core_types._int_sub(i,1)),7);
  if ($std_core_types._int_eq(d,0)) {
    return Mon;
  }
  else {
    if ($std_core_types._int_eq(d,1)) {
      return Tue;
    }
    else {
      if ($std_core_types._int_eq(d,2)) {
        return Wed;
      }
      else {
        if ($std_core_types._int_eq(d,3)) {
          return Thu;
        }
        else {
          if ($std_core_types._int_eq(d,4)) {
            return Fri;
          }
          else {
            return ($std_core_types._int_eq(d,5)) ? Sat : Sun;
          }
        }
      }
    }
  }
}
 
 
// Return the `:weekday` that comes `n` days after week day `wd`.
export function weekday_fs__lp__plus__rp_(wd, n) /* (wd : weekday, n : int) -> weekday */  {
   
  var x_10027 = int(wd);
  return weekday($std_core_types._int_add(x_10027,n));
}
 
 
// Return the difference between two week days:\
// `wd2 == wd1 + (wd2 - wd1)`
export function weekday_fs__lp__dash__rp_(wd1, wd2) /* (wd1 : weekday, wd2 : weekday) -> int */  {
   
  var x_10029 = int(wd1);
   
  var y_10030 = int(wd2);
  return $std_core_types._int_mod(($std_core_types._int_sub(x_10029,y_10030)),7);
}
 
 
// Return the `:weekday` that comes `n` days before week day `wd`.
export function weekdayint_fs__lp__dash__rp_(wd, n) /* (wd : weekday, n : int) -> weekday */  {
   
  var x_10031 = int(wd);
  return weekday($std_core_types._int_sub(x_10031,n));
}
 
 
// Compare two dates by fields.
export function cmp(d, e) /* (d : date, e : date) -> order */  {
  var _x16 = d.year;
  var _x17 = e.year;
  var _x15 = $std_core_types._int_eq(_x16,_x17);
  if (_x15) {
    var _x19 = d.month;
    var _x20 = e.month;
    var _x18 = $std_core_types._int_eq(_x19,_x20);
    if (_x18) {
      var _x22 = d.day;
      var _x23 = e.day;
      var _x21 = $std_core_types._int_eq(_x22,_x23);
      if (_x21) {
        return $std_core_types.Eq;
      }
      else {
        var _x25 = d.day;
        var _x26 = e.day;
        var _x24 = $std_core_types._int_gt(_x25,_x26);
        return (_x24) ? $std_core_types.Gt : $std_core_types.Lt;
      }
    }
    else {
      var _x28 = d.month;
      var _x29 = e.month;
      var _x27 = $std_core_types._int_gt(_x28,_x29);
      return (_x27) ? $std_core_types.Gt : $std_core_types.Lt;
    }
  }
  else {
    var _x31 = d.year;
    var _x32 = e.year;
    var _x30 = $std_core_types._int_gt(_x31,_x32);
    return (_x30) ? $std_core_types.Gt : $std_core_types.Lt;
  }
}
 
export function _lp__eq__eq__rp_(i, j) /* (i : date, j : date) -> bool */  {
  return $std_core_order._lp__eq__eq__rp_(cmp(i, j), $std_core_types.Eq);
}
 
 
// Return the total seconds of a `:clock` assuming 60 seconds per
// minute and 60 minutes per hour.
export function total_seconds(c) /* (c : clock) -> std/num/ddouble/ddouble */  {
   
  var _x33 = c.hours;
  var x_10046 = $std_core_types._int_mul(_x33,60);
   
  var _x34 = c.minutes;
  var i_10045 = $std_core_types._int_mul(($std_core_types._int_add(x_10046,_x34)),60);
  var _x33 = c.seconds;
  return $std_num_ddouble._lp__plus__rp_($std_num_ddouble.ddouble_int_exp(i_10045, 0), _x33);
}
 
 
// Compare two clocks as by their total seconds.
export function clock_fs_cmp(c, d) /* (c : clock, d : clock) -> order */  {
   
  var x_10051 = total_seconds(c);
   
  var y_10052 = total_seconds(d);
  var _x35 = x_10051.hi;
  var _x36 = y_10052.hi;
  var _x34 = $std_num_float64.cmp(_x35, _x36);
  if (_x34 === 2) {
    var _x37 = x_10051.lo;
    var _x38 = y_10052.lo;
    return $std_num_float64.cmp(_x37, _x38);
  }
  else {
    return _x34;
  }
}
 
export function clock_fs__lp__eq__eq__rp_(i, j) /* (i : clock, j : clock) -> bool */  {
  return $std_core_order._lp__eq__eq__rp_(clock_fs_cmp(i, j), $std_core_types.Eq);
}
 
 
// Compare weekdays.
export function weekday_fs_cmp(wd1, wd2) /* (wd1 : weekday, wd2 : weekday) -> order */  {
   
  var x_10053 = int(wd1);
   
  var y_10054 = int(wd2);
  if ($std_core_types._int_eq(x_10053,y_10054)) {
    return $std_core_types.Eq;
  }
  else {
    return ($std_core_types._int_gt(x_10053,y_10054)) ? $std_core_types.Gt : $std_core_types.Lt;
  }
}
 
export function weekday_fs__lp__eq__eq__rp_(i, j) /* (i : weekday, j : weekday) -> bool */  {
  return $std_core_order._lp__eq__eq__rp_(weekday_fs_cmp(i, j), $std_core_types.Eq);
}
 
export function _lp__lt__rp_(i, j) /* (i : date, j : date) -> bool */  {
  return $std_core_order._lp__eq__eq__rp_(cmp(i, j), $std_core_types.Lt);
}
 
export function _lp__excl__eq__rp_(i, j) /* (i : date, j : date) -> bool */  {
  return $std_core_order._lp__excl__eq__rp_(cmp(i, j), $std_core_types.Eq);
}
 
export function clock_fs__lp__excl__eq__rp_(i, j) /* (i : clock, j : clock) -> bool */  {
  return $std_core_order._lp__excl__eq__rp_(clock_fs_cmp(i, j), $std_core_types.Eq);
}
 
export function weekday_fs__lp__excl__eq__rp_(i, j) /* (i : weekday, j : weekday) -> bool */  {
  return $std_core_order._lp__excl__eq__rp_(weekday_fs_cmp(i, j), $std_core_types.Eq);
}
 
export function _lp__lt__eq__rp_(i, j) /* (i : date, j : date) -> bool */  {
  return $std_core_order._lp__excl__eq__rp_(cmp(i, j), $std_core_types.Gt);
}
 
export function _lp__gt__rp_(i, j) /* (i : date, j : date) -> bool */  {
  return $std_core_order._lp__eq__eq__rp_(cmp(i, j), $std_core_types.Gt);
}
 
export function _lp__gt__eq__rp_(i, j) /* (i : date, j : date) -> bool */  {
  return $std_core_order._lp__excl__eq__rp_(cmp(i, j), $std_core_types.Lt);
}
 
export function clock_fs__copy(_this, hours, minutes, seconds) /* (clock, hours : ? int, minutes : ? int, seconds : ? std/num/ddouble/ddouble) -> clock */  {
  if (hours !== undefined) {
    var _x39 = hours;
  }
  else {
    var _x39 = _this.hours;
  }
  if (minutes !== undefined) {
    var _x40 = minutes;
  }
  else {
    var _x40 = _this.minutes;
  }
  if (seconds !== undefined) {
    var _x41 = seconds;
  }
  else {
    var _x41 = _this.seconds;
  }
  return Clock(_x39, _x40, _x41);
}
 
 
// Create a clock from a seconds as an `:int` with an optional fraction.
// Normalizes the clock with seconds and minutes under 60 but
// adds the fraction as is to the final seconds, so that might
// be `>= 60` if the fraction `>= 1.0`;
export function intddouble_fs_clock(seconds, frac) /* (seconds : int, frac : ? std/num/ddouble/ddouble) -> clock */  {
  var _x42 = $std_core_int.divmod(seconds, 60);
  var _x43 = $std_core_int.divmod(_x42.fst, 60);
  var _x44 = (frac !== undefined) ? frac : $std_num_ddouble.zero;
  return Clock(_x43.fst, _x43.snd, $std_num_ddouble._lp__plus__rp_($std_num_ddouble.ddouble_int_exp(_x42.snd, 0), _x44));
}
 
 
// Create a clock from seconds; normalizes the clock with seconds and minutes under 60.
export function ddouble_fs_clock(seconds) /* (seconds : std/num/ddouble/ddouble) -> clock */  {
   
  var seconds_0_10059 = $std_num_ddouble.int($std_num_ddouble.floor(seconds));
   
  var frac_10060 = $std_num_ddouble.ffraction(seconds);
  var _x45 = $std_core_int.divmod(seconds_0_10059, 60);
  var _x46 = $std_core_int.divmod(_x45.fst, 60);
  var _x47 = (frac_10060 !== undefined) ? frac_10060 : $std_num_ddouble.zero;
  return Clock(_x46.fst, _x46.snd, $std_num_ddouble._lp__plus__rp_($std_num_ddouble.ddouble_int_exp(_x45.snd, 0), _x47));
}
 
 
// Create a clock from a seconds as an `:int` with an optional fraction.
// Normalizes the clock with seconds and minutes under 60 but
// adds the fraction as is to the final seconds, so that might
// be `>= 60` if the fraction `>= 1.0`;
export function intfloat64_fs_clock(seconds, frac) /* (seconds : int, frac : float64) -> clock */  {
  var _x48 = $std_core_int.divmod(seconds, 60);
  var _x49 = $std_core_int.divmod(_x48.fst, 60);
  return Clock(_x49.fst, _x49.snd, $std_num_ddouble._lp__plus__rp_($std_num_ddouble.ddouble_int_exp(_x48.snd, 0), $std_num_ddouble.Ddouble(frac, 0.0)));
}
 
export function leap_fs_clock(seconds, leap) /* (seconds : std/num/ddouble/ddouble, leap : int) -> clock */  {
   
  var seconds_0_10066 = $std_num_ddouble.int($std_num_ddouble.floor(seconds));
   
  var frac_10067 = $std_num_ddouble._lp__plus__rp_($std_num_ddouble.ffraction(seconds), $std_num_ddouble.ddouble_int_exp(leap, 0));
  var _x50 = $std_core_int.divmod(seconds_0_10066, 60);
  var _x51 = $std_core_int.divmod(_x50.fst, 60);
  var _x52 = (frac_10067 !== undefined) ? frac_10067 : $std_num_ddouble.zero;
  return Clock(_x51.fst, _x51.snd, $std_num_ddouble._lp__plus__rp_($std_num_ddouble.ddouble_int_exp(_x50.snd, 0), _x52));
}
 
 
// The zero clock
export var clock0;
var clock0 = Clock(0, 0, $std_num_ddouble.zero);
 
 
// Is this a zero clock?
export function is_zero(c) /* (c : clock) -> bool */  {
  var _x54 = c.hours;
  var _x53 = $std_core_types._int_iszero(_x54);
  if (_x53) {
    var _x56 = c.minutes;
    var _x55 = $std_core_types._int_iszero(_x56);
    if (_x55) {
      var _x57 = c.seconds.hi;
      return (_x57 === (0.0));
    }
    else {
      return false;
    }
  }
  else {
    return false;
  }
}
 
 
// Return the fraction of the seconds as milli-seconds (10^-3^).
export function milli_seconds(c) /* (c : clock) -> int */  {
   
  var _x58 = c.seconds;
  var x_10075 = $std_num_ddouble.fraction(_x58);
   
  var _x59 = x_10075.hi;
  var d_10074 = (_x59 * (1000.0));
  var _x58 = ((d_10074 >= (0.0))) ? Math.floor(d_10074) : Math.ceil(d_10074);
  return $std_core_types._int_double(_x58);
}
 
 
// Return the fraction of seconds as nano-seconds ((10^-9^).
export function nano_seconds(c) /* (c : clock) -> int */  {
   
  var _x59 = c.seconds;
  var x_10078 = $std_num_ddouble.fraction(_x59);
   
  var _x60 = x_10078.hi;
  var d_10077 = (_x60 * (1.0e9));
  var _x59 = ((d_10077 >= (0.0))) ? Math.floor(d_10077) : Math.ceil(d_10077);
  return $std_core_types._int_double(_x59);
}
 
 
// Round a clock time to a certain number of digits precision (of the fraction of seconds) (default `9`, nano seconds).
export function round_to_prec(c, prec) /* (c : clock, prec : ? int) -> clock */  {
  var _x60 = c.hours;
  var _x61 = c.minutes;
  var _x62 = c.seconds;
  var _x63 = (prec !== undefined) ? prec : 9;
  return Clock(_x60, _x61, $std_num_ddouble.round_to_prec(_x62, _x63));
}
 
 
// Show seconds
export function show_seconds(secs, max_prec, secs_width, unit) /* (secs : std/num/ddouble/ddouble, max-prec : ? int, secs-width : ? int, unit : ? string) -> string */  {
   
  var _x64 = (max_prec !== undefined) ? max_prec : 9;
  var s = $std_num_ddouble.show_fixed(secs, $std_core_types._int_negate(($std_core_types._int_abs(_x64))));
  var _x64 = $std_core_sslice.find(s, ".");
  if (_x64 === null) {
    var _x65 = (secs_width !== undefined) ? secs_width : 1;
    return $std_core_string.pad_left(s, _x65, 0x0030);
  }
  else {
     
    var f = $std_core_sslice.string($std_core_sslice.after(_x64.value));
     
    var x_10083 = $std_core_string.chars_fs_count(f);
     
    var len3 = $std_core_types._int_mul(($std_core_types._int_div(($std_core_types._int_add(x_10083,2)),3)),3);
    var _x66 = $std_core_sslice.Sslice(_x64.value.str, 0, _x64.value.start);
    var _x67 = (secs_width !== undefined) ? secs_width : 1;
    var _x68 = (unit !== undefined) ? unit : "";
    return $std_core_types._lp__plus__plus__rp_($std_core_string.pad_left($std_core_sslice.string(_x66), _x67, 0x0030), $std_core_types._lp__plus__plus__rp_(".", $std_core_types._lp__plus__plus__rp_($std_core_string.pad_right(f, len3, 0x0030), _x68)));
  }
}
 
export function clock_fs__lp__lt__rp_(i, j) /* (i : clock, j : clock) -> bool */  {
  return $std_core_order._lp__eq__eq__rp_(clock_fs_cmp(i, j), $std_core_types.Lt);
}
 
export function clock_fs__lp__lt__eq__rp_(i, j) /* (i : clock, j : clock) -> bool */  {
  return $std_core_order._lp__excl__eq__rp_(clock_fs_cmp(i, j), $std_core_types.Gt);
}
 
export function clock_fs__lp__gt__rp_(i, j) /* (i : clock, j : clock) -> bool */  {
  return $std_core_order._lp__eq__eq__rp_(clock_fs_cmp(i, j), $std_core_types.Gt);
}
 
export function clock_fs__lp__gt__eq__rp_(i, j) /* (i : clock, j : clock) -> bool */  {
  return $std_core_order._lp__excl__eq__rp_(clock_fs_cmp(i, j), $std_core_types.Lt);
}
 
 
// Automatically generated. Tests for the `Mon` constructor of the `:weekday` type.
export function is_mon(weekday_0) /* (weekday : weekday) -> bool */  {
  return (weekday_0 === 1);
}
 
 
// Automatically generated. Tests for the `Tue` constructor of the `:weekday` type.
export function is_tue(weekday_0) /* (weekday : weekday) -> bool */  {
  return (weekday_0 === 2);
}
 
 
// Automatically generated. Tests for the `Wed` constructor of the `:weekday` type.
export function is_wed(weekday_0) /* (weekday : weekday) -> bool */  {
  return (weekday_0 === 3);
}
 
 
// Automatically generated. Tests for the `Thu` constructor of the `:weekday` type.
export function is_thu(weekday_0) /* (weekday : weekday) -> bool */  {
  return (weekday_0 === 4);
}
 
 
// Automatically generated. Tests for the `Fri` constructor of the `:weekday` type.
export function is_fri(weekday_0) /* (weekday : weekday) -> bool */  {
  return (weekday_0 === 5);
}
 
 
// Automatically generated. Tests for the `Sat` constructor of the `:weekday` type.
export function is_sat(weekday_0) /* (weekday : weekday) -> bool */  {
  return (weekday_0 === 6);
}
 
 
// Automatically generated. Tests for the `Sun` constructor of the `:weekday` type.
export function is_sun(weekday_0) /* (weekday : weekday) -> bool */  {
  return (weekday_0 === 7);
}
 
 
// Show a `:weekday` as an English string (`Sun.show == "Sunday"`).
export function weekday_fs_show(wd) /* (wd : weekday) -> string */  {
  if (wd === 1) {
    return "Monday";
  }
  else if (wd === 2) {
    return "Tuesday";
  }
  else if (wd === 3) {
    return "Wednesday";
  }
  else if (wd === 4) {
    return "Thursday";
  }
  else if (wd === 5) {
    return "Friday";
  }
  else if (wd === 6) {
    return "Saturday";
  }
  else {
    return "Sunday";
  }
}
 
export function weekday_fs__lp__lt__rp_(i, j) /* (i : weekday, j : weekday) -> bool */  {
  return $std_core_order._lp__eq__eq__rp_(weekday_fs_cmp(i, j), $std_core_types.Lt);
}
 
export function weekday_fs__lp__lt__eq__rp_(i, j) /* (i : weekday, j : weekday) -> bool */  {
  return $std_core_order._lp__excl__eq__rp_(weekday_fs_cmp(i, j), $std_core_types.Gt);
}
 
export function weekday_fs__lp__gt__rp_(i, j) /* (i : weekday, j : weekday) -> bool */  {
  return $std_core_order._lp__eq__eq__rp_(weekday_fs_cmp(i, j), $std_core_types.Gt);
}
 
 
// pad with zeros
export function show0(i, width) /* (i : int, width : ? int) -> string */  {
  var _x69 = (width !== undefined) ? width : 2;
  return $std_core_string.pad_left($std_core_int.show(i), _x69, 0x0030);
}
 
 
// Show a year in ISO format (using 5+ digits and explicit sign for years < 0 or years > 9999)).
export function show_year(year) /* (year : int) -> string */  {
  if ($std_core_types._int_gt(year,9999)) {
     
    var i_10086 = $std_core_types._int_abs(year);
    return $std_core_types._lp__plus__plus__rp_("+", $std_core_string.pad_left($std_core_int.show(i_10086), 5, 0x0030));
  }
  else {
    if ($std_core_types._int_lt(year,0)) {
       
      var i_1_10089 = $std_core_types._int_abs(year);
      return $std_core_types._lp__plus__plus__rp_("-", $std_core_string.pad_left($std_core_int.show(i_1_10089), 5, 0x0030));
    }
    else {
       
      var i_2_10091 = $std_core_types._int_abs(year);
      return $std_core_types._lp__plus__plus__rp_("", $std_core_string.pad_left($std_core_int.show(i_2_10091), 4, 0x0030));
    }
  }
}
 
 
// Show a date in ISO format. `Date(2000,1,1).show == "2000-01-01"`.
// Takes an optional `month-prefix` (=`""`) that is used by the ISO week
// and month calendar to add a `"W"` or `"M"` prefix respectively.
export function show(d, month_prefix) /* (d : date, month-prefix : ? string) -> string */  {
   
  var _x71 = (month_prefix !== undefined) ? month_prefix : "";
  var _x70 = (_x71 === ("W"));
  var day_width = (_x70) ? 1 : 2;
  var _x70 = d.year;
  var _x71 = (month_prefix !== undefined) ? month_prefix : "";
  var _x72 = d.month;
  var _x74 = undefined;
  var _x73 = (_x74 !== undefined) ? _x74 : 2;
  var _x75 = d.day;
  return $std_core_types._lp__plus__plus__rp_(show_year(_x70), $std_core_types._lp__plus__plus__rp_("-", $std_core_types._lp__plus__plus__rp_(_x71, $std_core_types._lp__plus__plus__rp_($std_core_string.pad_left($std_core_int.show(_x72), _x73, 0x0030), $std_core_types._lp__plus__plus__rp_("-", $std_core_string.pad_left($std_core_int.show(_x75), day_width, 0x0030))))));
}
 
 
// Show a clock in ISO format up to an optional maximum precision (=`9`).\
// `Clock(23,30,fixed(1.123)).show == "23:30:01.123"`\
// `Clock(23,30,fixed(1.123)).show(0) == "23:30:01"`
export function clock_fs_show(c, prec) /* (c : clock, prec : ? int) -> string */  {
  var _x76 = c.hours;
  var _x78 = undefined;
  var _x77 = (_x78 !== undefined) ? _x78 : 2;
  var _x79 = c.minutes;
  var _x81 = undefined;
  var _x80 = (_x81 !== undefined) ? _x81 : 2;
  var _x82 = c.seconds;
  var _x83 = (prec !== undefined) ? prec : 9;
  return $std_core_types._lp__plus__plus__rp_($std_core_string.pad_left($std_core_int.show(_x76), _x77, 0x0030), $std_core_types._lp__plus__plus__rp_(":", $std_core_types._lp__plus__plus__rp_($std_core_string.pad_left($std_core_int.show(_x79), _x80, 0x0030), $std_core_types._lp__plus__plus__rp_(":", show_seconds(_x82, _x83, 2)))));
}
 
 
// Return the whole seconds part of a `:clock`.
export function whole_seconds(c) /* (c : clock) -> int */  {
  var _x86 = c.seconds.hi;
  var _x85 = (_x86 < (0.0));
  if (_x85) {
    var _x87 = c.seconds;
    var _x84 = $std_num_ddouble.ceiling(_x87);
  }
  else {
    var _x88 = c.seconds;
    var _x84 = $std_num_ddouble.floor(_x88);
  }
  return $std_num_ddouble.int(_x84);
}
 
 
// Show a `:weekday` as a 3 letter English string (`Sun.show-short == "Sun"`)
export function show_short(wd) /* (wd : weekday) -> string */  {
  return $std_core_sslice.string($std_core_sslice.first(weekday_fs_show(wd), 3));
}
 
export function weekday_fs__lp__gt__eq__rp_(i, j) /* (i : weekday, j : weekday) -> bool */  {
  return $std_core_order._lp__excl__eq__rp_(weekday_fs_cmp(i, j), $std_core_types.Lt);
}
 
 
// Return the ISO calendar date of Easter in a given year (Algorithm by [J.M. Oudin](https://aa.usno.navy.mil/faq/docs/easter.php)).
export function easter(year) /* (year : int) -> date */  {
   
  var c = $std_core_types._int_div(year,100);
   
  var y_10110 = $std_core_types._int_mul(19,($std_core_types._int_div(year,19)));
   
  var n = $std_core_types._int_sub(year,y_10110);
   
  var k = $std_core_types._int_div(($std_core_types._int_sub(c,17)),25);
   
  var y_4_10120 = $std_core_types._int_div(c,4);
   
  var x_3_10117 = $std_core_types._int_sub(c,y_4_10120);
   
  var y_3_10118 = $std_core_types._int_div(($std_core_types._int_sub(c,k)),3);
   
  var x_2_10115 = $std_core_types._int_sub(x_3_10117,y_3_10118);
   
  var y_2_10116 = $std_core_types._int_mul(19,n);
   
  var x_1_10113 = $std_core_types._int_add(x_2_10115,y_2_10116);
   
  var i0 = $std_core_types._int_add(x_1_10113,15);
   
  var y_6_10124 = $std_core_types._int_mul(30,($std_core_types._int_div(i0,30)));
   
  var i1 = $std_core_types._int_sub(i0,y_6_10124);
   
  var y_8_10128 = $std_core_types._int_mul(($std_core_types._int_mul(($std_core_types._int_div(i1,28)),($std_core_types._int_div(29,($std_core_types._int_add(i1,1)))))),($std_core_types._int_div(($std_core_types._int_sub(21,n)),11)));
   
  var y_7_10126 = $std_core_types._int_mul(($std_core_types._int_div(i1,28)),($std_core_types._int_sub(1,y_8_10128)));
   
  var i = $std_core_types._int_sub(i1,y_7_10126);
   
  var y_15_10142 = $std_core_types._int_div(year,4);
   
  var x_14_10139 = $std_core_types._int_add(year,y_15_10142);
   
  var x_13_10137 = $std_core_types._int_add(x_14_10139,i);
   
  var x_12_10135 = $std_core_types._int_add(x_13_10137,2);
   
  var x_11_10133 = $std_core_types._int_sub(x_12_10135,c);
   
  var y_11_10134 = $std_core_types._int_div(c,4);
   
  var j0 = $std_core_types._int_add(x_11_10133,y_11_10134);
   
  var y_16_10144 = $std_core_types._int_mul(7,($std_core_types._int_div(j0,7)));
   
  var j = $std_core_types._int_sub(j0,y_16_10144);
   
  var l = $std_core_types._int_sub(i,j);
   
  var y_18_10148 = $std_core_types._int_div(($std_core_types._int_add(l,40)),44);
   
  var m = $std_core_types._int_add(3,y_18_10148);
   
  var x_20_10151 = $std_core_types._int_add(l,28);
   
  var y_20_10152 = $std_core_types._int_mul(31,($std_core_types._int_div(m,4)));
   
  var d = $std_core_types._int_sub(x_20_10151,y_20_10152);
  return $Date(year, m, d);
}