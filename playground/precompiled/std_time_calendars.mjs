// Koka generated module: std/time/calendars, koka version: 3.2.4
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
import * as $std_time_instant from './std_time_instant.mjs';
import * as $std_time_utc from './std_time_utc.mjs';
import * as $std_time_date from './std_time_date.mjs';
import * as $std_time_calendar from './std_time_calendar.mjs';
import * as $std_time_duration from './std_time_duration.mjs';
 
// externals
 
// type declarations
 
// declarations
 
export function coptic_days_before_month(year, month) /* (year : int, month : int) -> int */  {
  return $std_core_types._int_mul(30,($std_core_types._int_sub(month,1)));
}
 
export function coptic_doy_to_month(year, doy) /* (year : int, doy : int) -> int */  {
   
  var x_10002 = $std_core_types._int_div(doy,30);
  return $std_core_types._int_add(x_10002,1);
}
 
export function coptic_days_before_year(year) /* (year : int) -> int */  {
   
  var x_10004 = $std_core_types._int_mul(365,($std_core_types._int_sub(year,1)));
   
  var y_10005 = $std_core_types._int_div(year,4);
  return $std_core_types._int_add(x_10004,y_10005);
}
 
export function coptic_estimate_year(days) /* (days : int) -> (int, maybe<int>) */  {
  var _x0 = $std_core_int.divmod($std_core_types._int_add(days,365), 1461);
   
  var x_0_10009 = $std_core_types._int_mul(4,(_x0.fst));
   
  var y_0_10010 = $std_core_types._int_div((_x0.snd),366);
  return $std_core_types.Tuple2($std_core_types._int_add(x_0_10009,y_0_10010), $std_core_types.Just(364));
}
 
 
// The [Coptic](https://en.wikipedia.org/wiki/Coptic_calendar) calendar.
// The calendar short name is ``CC``.
export var cal_coptic;
var cal_coptic = $std_time_calendar.solar_ecalendar("CC", "Coptic", coptic_days_before_year, coptic_estimate_year, coptic_days_before_month, coptic_doy_to_month, 626515, undefined, undefined, function(d /* std/time/date/date */ ) {
    var _x2 = d.year;
    var _x1 = $std_core_types._int_gt(_x2,0);
    return (_x1) ? "A.M." : "";
  });
 
export function coptic_days_to_yeardoy(days) /* (days : int) -> (int, int) */  {
  var _x3 = $std_core_int.divmod($std_core_types._int_add(days,365), 1461);
   
  var i_10015 = $std_core_types._int_div((_x3.snd),365);
   
  var yoe = ($std_core_types._int_le(i_10015,3)) ? i_10015 : 3;
   
  var x_0_10017 = $std_core_types._int_mul(4,(_x3.fst));
   
  var y_1_10020 = $std_core_types._int_mul(365,yoe);
  return $std_core_types.Tuple2($std_core_types._int_add(x_0_10017,yoe), $std_core_types._int_sub((_x3.snd),y_1_10020));
}
 
 
// The [Ethiopian](https://en.wikipedia.org/wiki/Ethiopian_calendar) calendar.
// The calendar short name is ``EC``.
export var cal_ethiopian;
var cal_ethiopian = $std_time_calendar.year_shift_earth_calendar("EC", "Ethiopian", $std_core_types._int_negate(276), cal_coptic);
 
export function isow_days_before_month(year, month) /* (year : int, month : int) -> int */  {
  return $std_core_types._int_mul(7,($std_core_types._int_sub(month,1)));
}
 
export function isow_doy_to_month(year, doy) /* (year : int, doy : int) -> int */  {
   
  var x_10023 = $std_core_types._int_div(doy,7);
  return $std_core_types._int_add(x_10023,1);
}
 
 
// duplicate from std/time/calendar to reduce dependencies
export function iso_days_before_year(year) /* (year : int) -> int */  {
   
  var y = $std_core_types._int_sub(year,1);
   
  var x_0_10028 = $std_core_types._int_div(y,4);
   
  var y_1_10029 = $std_core_types._int_div(y,100);
   
  var x_10026 = $std_core_types._int_sub(x_0_10028,y_1_10029);
   
  var y_0_10027 = $std_core_types._int_div(y,400);
   
  var leapdays = $std_core_types._int_add(x_10026,y_0_10027);
   
  var x_1_10030 = $std_core_types._int_mul(365,y);
  return $std_core_types._int_add(x_1_10030,leapdays);
}
 
export function isow_days_before_year(year) /* (year : int) -> int */  {
   
  var gdays = iso_days_before_year(year);
   
  var weekday = $std_core_types._int_mod(gdays,7);
   
  var adjust = ($std_core_types._int_le(weekday,3)) ? $std_core_types._int_sub(0,weekday) : $std_core_types._int_sub(7,weekday);
  return $std_core_types._int_add(gdays,adjust);
}
 
export function iso_estimate_year(days) /* (days : int) -> (int, maybe<int>) */  {
  var _x4 = $std_core_int.divmod(days, 146097);
   
  var y_0_10041 = $std_core_types._int_mul(400,(_x4.fst));
   
  var x_10038 = $std_core_types._int_add(1,y_0_10041);
   
  var y_10039 = $std_core_types._int_div(($std_core_types._int_mul(100,(_x4.snd))),36525);
  return $std_core_types.Tuple2($std_core_types._int_add(x_10038,y_10039), $std_core_types.Just(363));
}
 
export function isow_estimate_year(days) /* (days : int) -> (int, maybe<int>) */  {
   
  var days_0_10080 = $std_core_types._int_sub(days,3);
  var _x5 = $std_core_int.divmod(days_0_10080, 146097);
   
  var y_0_10041 = $std_core_types._int_mul(400,(_x5.fst));
   
  var x_10038 = $std_core_types._int_add(1,y_0_10041);
   
  var y_10039 = $std_core_types._int_div(($std_core_types._int_mul(100,(_x5.snd))),36525);
  return $std_core_types.Tuple2($std_core_types._int_add(x_10038,y_10039), $std_core_types.Just(363));
}
 
 
/* The 'ISO week' calendar. This implements the [ISO week date](https://en.wikipedia.org/wiki/ISO_week_date)
calendar the week number is interpreted as "month" number instead, i.e. every year has 52 (or 53)
months of 7 week days each. Short name is `"IW"`.
Since its introduction in 1988, the ISO week calendar is a widely accepted standard for a weekly
calendar. It is used mainly by businesses for fiscal year calculations.
Weeks start on Monday (as day 1) and every common year has 52 weeks.
Every 5 to 7 years, there is a 'leap' year with an extra week 53.
The ISO week calendar is directly based on the Gregorian calendar.
For example, Tuesday 2001-01-02 is denoted as 2001-W01-2 in the ISO week
calendar: weekday 2 (Tuesday) of week 1 in 2001.
The year of an ISO week is defined as the Gregorian year that has the
Thursday of that week, &ie; contains the most days of that week. For
example, Thursday 2004-01-01 has week date 2004-W01-4. This means that
sometimes the Gregorian year is different for a first- or last week:
Wednesday 2003-12-31 falls in the first week of 2004W and has week date
2004-W01-3. Similarly, Saturday 2005-01-01 falls in the last (leap) week of
2004W and has week date 2004-W53-6.
*/
export var cal_iso_week;
var cal_iso_week = $std_time_calendar.solar_ecalendar("IW", "ISO Week", isow_days_before_year, isow_estimate_year, isow_days_before_month, isow_doy_to_month, undefined, undefined, "W");
 
export function isom_days_before_month(year, month) /* (year : int, month : int) -> int */  {
   
  var x_10044 = $std_core_types._int_mul(30,($std_core_types._int_sub(month,1)));
   
  var y_10045 = $std_core_types._int_div(($std_core_types._int_sub(month,1)),3);
  return $std_core_types._int_add(x_10044,y_10045);
}
 
export function isom_doy_to_month(year, doy) /* (year : int, doy : int) -> int */  {
   
  var m = $std_core_types._int_div(($std_core_types._int_mul(100,($std_core_types._int_add(doy,1)))),3034);
   
  var i_0_10049 = $std_core_types._int_add(m,1);
  return ($std_core_types._int_le(i_0_10049,12)) ? i_0_10049 : 12;
}
 
 
/*
<!--meta
.calendar
  .sans-serif;
  border: 1px solid black;
.mheader
  font-weight: bold;
.separator
  padding-bottom:0.2ex;
.noborder
  border-left:none;
  border-right:none;
.spacer
  border-left:none;
  border-right:none;
  height:0.4ex;
.month
  td-padding-right: 1ex;
  tr-padding-top: 0.25ex;
  tbody-tr-1-padding-top: 0.5ex;
  .sans-serif
-->
This is _not_ a standard ISO calendar -- it is named this way because
it is based directly on the standard [ISO week
date](https://en.wikipedia.org/wiki/ISO_week_date) calendar.
The short name of the calendar is `"IM"`.
The "ISO month" calendar takes the ISO week calendar _as is_, but divides
it up in 12 months. This is more familiar than using week numbers to
denote a date. A common ISO week year of 52 weeks is divided into 4
quarters of 13 weeks each. Each quarter has 3 months of 30, 30, and 31
days respectively. So, January has 30 days, February has 30 days too,
March has 31 days, April has 30 days again, etc. On a leap year, we have
an extra leap week 53 that is inserted at the end of the last month,
&ie; adds days 32 to 38 to December. Since quarters are meant to be equal
for business purposes, we generally don't count the leap week as part of
the 4th quarter.
This monthly calendar has many good properties, not the least that it
matches the Gregorian calendar dates very closely[^fn-match], and is based a widely supported standard ISO calendar. Moreover,
it is [perennial](https://en.wikipedia.org/wiki/Perennial_calendar) where
every date in the year always has the same weekday. For example, every
year and every quarter always start on a Monday and end on a Sunday.
Thanksgiving (the 4th Thursday of November) is always on November 23, and
always 31 days before Christmas. Christmas and New year are always on
Sunday, and there is never a Friday the 13th.
[^fn-match]: The month date always matches within 5 days of the
    Gregorian calendar; and 90% of the time, the date is within 3 days.
When writing down a date in the monthly calendar we prefix the
month with a capital [M]{.sans-serif} in order to distinguish these
dates from regular Gregorian dates or ISO week dates. For example,
|-----|----------------|----------------|----------------------------------------------------------|---------------|
| Day | Gregorian date | Month date     | Remarks                                                | ISO week date |
+-----|:--------------:|:--------------:+----------------------------------------------------------|:-------------:+
| Mon | 2018-01-01     | 2018-M01-01    | Matches since Monday starts the first week of the year.  | 2018-W01-1    |
| Thu | 2016-11-24     | 2016-M11-23    | Thanksgiving, always on Thursday M11-23 every year.      | 2016-W47-5    |
| Thu | 2013-07-04     | 2013-M07-04    | Independence day is always on Thursday M07-04.           | 2013-W27-4    |
|-----|----------------|----------------|----------------------------------------------------------|---------------|
| Thu | 2004-01-01     | 2004-M01-04    | Since the first week falls partly in 2003M.              | 2004-W01-4    |
| Sun | 2005-01-02     | 2004-M12-38    | Since it is the last day of the leap week of 2004M.      | 2004-W53-7    |
|-----|----------------|----------------|----------------------------------------------------------|---------------|
{ white-space:nowrap; col-4-white-space:normal; col-3-padding-right:1ex;   }
With the regularity of the new calendar, we can reuse the same calendar
pattern for each quarter over and over; unlike the Gregorian calendar it
never changes which makes planning for businesses, schools, government
etc. much simpler. Here is the (perpetual) calendar with the
corresponding ISO week numbers:
~ Begin Calendar { .sans-serif; border: 1px solid #AAA; padding:1ex; }
+~~~:|~~~:|~~~:|~~~:|~~~:|~~~:|~~~:+~~~|~~~~:|~~~~:|~~~~:|~~~~:|~~~~~~~~~~~~~|
|  Quarterly Calendar        ||||||| &quad;  | ISO week numbers   ||||             |{.noborder; text-align:center}
|    |    |    |    |    |    |    |   | Q1  | Q2  | Q3  | Q4  |             |{.noborder; .mheader}
|----|----|----|----|----|----|----|   |     |     |     |     |             |
| Mo | Tu | We | Th | Fr | Sa | Su |   | Jan | Apr | Jul |Oct  |             |{.mheader}
|----|----|----|----|----|----|----|   |     |     |     |     |             |
| 1  | 2  | 3  | 4  | 5  | 6  | 7  |   | 1   | 14  | 27  | 40  |             |
| 8  | 9  | 10 | 11 | 12 | 13 | 14 |   | 2   | 15  | 28  | 41  |             |
| 15 | 16 | 17 | 18 | 19 | 20 | 21 |   | 3   | 16  | 29  | 42  |             |
| 22 | 23 | 24 | 25 | 26 | 27 | 28 |   | 4   | 17  | 30  | 43  |             |
| 29 | 30 |    |    |    |    |    |   | 5   | 18  | 31  | 44  |             |
|----|----|----|----|----|----|----|   |     |     |     |     |             |
|    |    |    |    |    |    |    |   |     |     |     |     |             |{.spacer}
|----|----|----|----|----|----|----|   |     |     |     |     |             |
| Mo | Tu | We | Th | Fr | Sa | Su |   | Feb | May | Aug | Nov |             |{.mheader}
|----|----|----|----|----|----|----|   |     |     |     |     |             |
|    |    | 1  | 2  | 3  | 4  | 5  |   | 5   | 18  | 31  | 44  |             |
| 6  | 7  | 8  | 9  | 10 | 11 | 12 |   | 6   | 19  | 32  | 45  |             |
| 13 | 14 | 15 | 16 | 17 | 18 | 19 |   | 7   | 20  | 33  | 46  |             |
| 20 | 21 | 22 | 23 | 24 | 25 | 26 |   | 8   | 21  | 34  | 47  |             |
| 27 | 28 | 29 | 30 |    |    |    |   | 9   | 22  | 35  | 48  |             |
|----|----|----|----|----|----|----|   |     |     |     |     |             |
|    |    |    |    |    |    |    |   |     |     |     |     |             |{.spacer}
|----|----|----|----|----|----|----|   |     |     |     |     |             |
| Mo | Tu | We | Th | Fr | Sa | Su |   | Mar | Jun | Sep | Dec |             |{.mheader}
|----|----|----|----|----|----|----|   |     |     |     |     |             |
|    |    |    |    | 1  | 2  | 3  |   | 9   | 22  | 35  | 48  |             |
| 4  | 5  | 6  | 7  | 8  | 9  | 10 |   | 10  | 23  | 36  | 49  |             |
| 11 | 12 | 13 | 14 | 15 | 16 | 17 |   | 11  | 24  | 37  | 50  |             |
| 18 | 19 | 20 | 21 | 22 | 23 | 24 |   | 12  | 25  | 38  | 51  |             |
| 25 | 26 | 27 | 28 | 29 | 30 | 31 |   | 13  | 26  | 39  | 52  |             |
|----|----|----|----|----|----|----|   |     |     |     |     |             |
|    |    |    |    |    |    |    |   |     |     |     |     |             |{.spacer}
|----|----|----|----|----|----|----|   |     |     |     |     |             |
|\ 32|\ 33|\ 34|\ 35|\ 36|\ 37|\ 38|   | leap week^&dagger;^     ||| 53  |             |
|----|----|----|----|----|----|----|   |     |     |     |     |             |
{ .month; margin-left:auto; margin-right:auto }
&nbsp;
^&dagger;^ A leap week is inserted
  at the end of December. Upcoming years with a leap week are 2020, 2026, 2032, 2037, and 2043. In
  general, a leap week is inserted whenever the corresponding Gregorian
  year starts and/or ends on a Thursday.
~ End Calendar
-- Daan Leijen, 2016.
*/
export var cal_iso_month;
var cal_iso_month = $std_time_calendar.solar_ecalendar("IM", "ISO Month", isow_days_before_year, isow_estimate_year, isom_days_before_month, isom_doy_to_month, undefined, undefined, "M");
 
export function julian_adjust(is_before_march, year) /* (is-before-march : bool, year : int) -> int */  {
  if (is_before_march) {
    return 0;
  }
  else {
    var _x6 = $std_core_types._int_eq(($std_core_types._int_mod(year,4)),0);
    return (_x6) ? 1 : 2;
  }
}
 
export function julian_days_before_month(year, month) /* (year : int, month : int) -> int */  {
   
  var is_before_march_10052 = $std_core_types._int_le(month,2);
   
  if (is_before_march_10052) {
    var adj = 0;
  }
  else {
    var _x7 = $std_core_types._int_eq(($std_core_types._int_mod(year,4)),0);
    var adj = (_x7) ? 1 : 2;
  }
   
  var x_0_10056 = $std_core_types._int_mul(367,month);
   
  var x_10054 = $std_core_types._int_div(($std_core_types._int_sub(x_0_10056,362)),12);
  return $std_core_types._int_sub(x_10054,adj);
}
 
export function julian_doy_to_month(year, doy) /* (year : int, doy : int) -> int */  {
   
  var is_before_march_10058 = $std_core_types._int_le(doy,58);
   
  if (is_before_march_10058) {
    var adj = 0;
  }
  else {
    var _x7 = $std_core_types._int_eq(($std_core_types._int_mod(year,4)),0);
    var adj = (_x7) ? 1 : 2;
  }
   
  var x_10060 = $std_core_types._int_mul(12,($std_core_types._int_add(doy,adj)));
  return $std_core_types._int_div(($std_core_types._int_add(x_10060,373)),367);
}
 
export function julian_days_before_year(year) /* (year : int) -> int */  {
   
  var leapdays = $std_core_types._int_div(($std_core_types._int_sub(year,1)),4);
   
  var x_10065 = $std_core_types._int_mul(365,($std_core_types._int_sub(year,1)));
  return $std_core_types._int_add(x_10065,leapdays);
}
 
export function julian_estimate_year(days) /* (days : int) -> (int, maybe<int>) */  {
  var _x7 = $std_core_int.divmod($std_core_types._int_add(days,365), 1461);
   
  var x_0_10070 = $std_core_types._int_mul(4,(_x7.fst));
   
  var y_0_10071 = $std_core_types._int_div((_x7.snd),366);
  return $std_core_types.Tuple2($std_core_types._int_add(x_0_10070,y_0_10071), $std_core_types.Just(364));
}
 
 
// The [Julian calendar](https://en.wikipedia.org/wiki/Julian_calendar).
// Uses old-style ``BC`` and ``AD`` to display era's.
// The calendar short name is ``JC``.
export var cal_julian;
var cal_julian = $std_time_calendar.solar_ecalendar("JC", "Julian", julian_days_before_year, julian_estimate_year, julian_days_before_month, julian_doy_to_month, 730121, false, undefined, function(d /* std/time/date/date */ ) {
    var _x9 = d.year;
    var _x8 = $std_core_types._int_lt(_x9,0);
    return (_x8) ? "BC" : "AD";
  });
 
export function julian_gregorian(switch_date) /* (switch-date : ? std/time/date/date) -> std/time/calendar/calendar */  {
   
  if (switch_date !== undefined) {
    var _uniq_switch_date_1158 = switch_date;
  }
  else {
    var _uniq_switch_date_1158 = $std_time_date.$Date(1582, 10, 15);
  }
  return $std_time_calendar.combine_earth_calendars("JG", "Julian-Gregorian", _uniq_switch_date_1158, cal_julian, $std_time_calendar.cal_gregorian, $std_core_types.Just(function(d /* std/time/date/date */ ) {
      var _x10 = $std_core_order._lp__eq__eq__rp_($std_time_date.cmp(d, _uniq_switch_date_1158), $std_core_types.Lt);
      if (_x10) {
        var _x12 = d.year;
        var _x11 = $std_core_types._int_ge(_x12,1500);
        if (_x11) {
          return "CE (O.S.)";
        }
        else {
          return cal_julian.show_era(d);
        }
      }
      else {
        var _x14 = d.year;
        var _x13 = $std_core_types._int_le(_x14,1926);
        if (_x13) {
          return "CE (N.S.)";
        }
        else {
          return $std_time_calendar.cal_gregorian.show_era(d);
        }
      }
    }));
}
 
 
// The combined Julian / Gregorian calendar, using the Julian calendar for dates
// before 1582-10-15 and the Gregorian calendar otherwise. It is possible to
// specify a different switch date using the `julian-gregorian` function.
// The calendar short name is ``JG``.
export var cal_jg;
var cal_jg = julian_gregorian();