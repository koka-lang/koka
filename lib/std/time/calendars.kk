/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Various calendars\: TAI, GPS, ISO week, Julian, Coptic, etc.
  
*/
module std/time/calendars

import std/time/instant
import std/time/utc
import std/time/date
import std/time/calendar

/*----------------------------------------------------------------------------
  Time scale calendars
----------------------------------------------------------------------------*/

// The TAI Calendar. This is a standard ISO 8601 calender using
// TAI time where every day is exactly 86400 SI seconds (unlike
// standard UTC time which can insert leap seconds).
public val cal-tai = iso-calendar( ts-tai )

// The GPS calendar is always `TAI - 19` but with epoch 1980-01-06Z
public val cal-gps = iso-calendar( ts-gps ) //(ts-gps.to-tai)(timestamp(fixed(~19))).seconds )



// ----------------------------------------------------
// Coptic calendar
// ----------------------------------------------------

// The [Coptic](https://en.wikipedia.org/wiki/Coptic_calendar) calendar.
// The calendar short name is ``CC``.
public val cal-coptic = solar-ecalendar(
  "CC", "Coptic", 
  coptic-days-before-year, coptic-estimate-year, // coptic-days-to-yeardoy, 
  coptic-days-before-month, coptic-doy-to-month,
  epoch-shift = 626515,  // 2001-01-01 - 284-08-29 Julian
  show-era = fun(d:date){ if (d.year.pos?) then "A.M." else "" }  // Anno Martyrum
)

fun coptic-days-before-month( year : int, month : int ) : int { 
  30*(month - 1)
}

fun coptic-doy-to-month( year : int, doy : int ) : int {
  doy/30 + 1
}   

fun coptic-days-before-year( year : int ) {
  365*year.dec + year/4
}

fun coptic-estimate-year( days : int ) : (int,maybe<int>) {
  val (era,doe) = divmod(days + 365, 1461)
  (4*era + doe/366, Just(364))
}

fun coptic-days-to-yeardoy( days : int ) : (int,int) {
  val (era,doe) = divmod(days + 365, 1461)
  val yoe = min( doe/365, 3)
  (4*era + yoe, doe - 365*yoe)
}

// The [Ethiopian](https://en.wikipedia.org/wiki/Ethiopian_calendar) calendar.
// The calendar short name is ``EC``.
public val cal-ethiopian = year-shift-earth-calendar( "EC", "Ethiopian", ~276, cal-coptic )


// ----------------------------------------------------
// ISO week calendar
// ----------------------------------------------------

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
public val cal-iso-week = solar-ecalendar(
  "IW", "ISO Week", 
  isow-days-before-year, isow-estimate-year, 
  isow-days-before-month, isow-doy-to-month,
  month-prefix = "W"  
)

fun isow-days-before-month( year : int, month : int ) : int { 
  7*(month - 1)
}

fun isow-doy-to-month( year : int, doy : int ) : int {
  doy/7 + 1
}

fun isow-days-before-year( year : int ) {
  val gdays   = iso-days-before-year(year)
  val weekday = gdays%7  // Monday=0, Saturday=6 (because 0001-01-01 is a Monday)
  val adjust  = if (weekday <= 3) then (0 - weekday) else (7 - weekday)
  gdays + adjust
}

fun isow-estimate-year( days : int ) : (int,maybe<int>) {
  iso-estimate-year( days - 3 )  // because we stick out at most 3 days into the next year
}

// duplicate from std/time/calendar to reduce dependencies
fun iso-days-before-year( year : int ) {
  val y = year.dec
  val leapdays = (y/4 - y/100) + y/400
  365*y + leapdays
}

fun iso-estimate-year( days : int ) : (int,maybe<int>) {
  val (era,yoe) = divmod(days,146097)
  (1 + 400*era + (100*yoe)/36525, Just(363))
}


// ----------------------------------------------------
// ISO month calendar
// ----------------------------------------------------

/*
<!--meta
.calendar {
  .sans-serif;
  border: 1px solid black;
}
.mheader {
  font-weight: bold;
}
.separator {
  padding-bottom:0.2ex;
}
.noborder {
  border-left:none;
  border-right:none;
}
.spacer {
  border-left:none;
  border-right:none;
  height:0.4ex;
}
.month {
  td-padding-right: 1ex;
  tr-padding-top: 0.25ex;
  tbody-tr-1-padding-top: 0.5ex;
  .sans-serif
}
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
public val cal-iso-month = solar-ecalendar(
  "IM", "ISO Month", 
  isow-days-before-year, isow-estimate-year, 
  isom-days-before-month, isom-doy-to-month,
  month-prefix = "M"  
)

fun isom-days-before-month( year : int, month : int ) : int { 
  30*month.dec + month.dec/3
}

fun isom-doy-to-month( year : int, doy : int ) : int {
  val m = (100*doy.inc)/3034   
  min(m.inc,12)
}



// ------------------------------------------------------------------------
// Julian calendar
// ------------------------------------------------------------------------

// The [Julian calendar](https://en.wikipedia.org/wiki/Julian_calendar).
// Uses old-style ``BC`` and ``AD`` to display era's.
// The calendar short name is ``JC``.
public val cal-julian = solar-ecalendar(
  "JC", "Julian", 
  julian-days-before-year, julian-estimate-year, 
  julian-days-before-month, julian-doy-to-month,
  epoch-shift    = 730121, // 2000-01-01 - 0000-12-30  // 730121 + 366
  has-year-zero? = False,
  show-era       = fun(d:date){ if (d.year.neg?) then "BC" else "AD" }
)

fun julian-days-before-month( year : int, month : int ) : int { 
  val adj = julian-adjust( month<=2, year )
  (367*month - 362)/12 - adj
}

fun julian-doy-to-month( year : int, doy : int ) : int {
  val adj = julian-adjust( doy<=58,  year)
  (12*(doy + adj) + 373)/367
}

fun julian-adjust( before-march? : bool, year : int ) : int {
  if (before-march?) then 0 elif (year%4==0) then 1 else 2
}
    
fun julian-days-before-year( year : int ) {
  val leapdays = year.dec/4
  365*year.dec + leapdays
}

fun julian-estimate-year( days : int ) : (int,maybe<int>) {
  val (era,yoe) = divmod(days + 365, 1461)
  (4*era + yoe/366, Just(364))
}


// ------------------------------------------------------------------------
// Julian/Gregorian calendar
// ------------------------------------------------------------------------

// The combined Julian / Gregorian calendar, using the Julian calendar for dates 
// before 1582-10-15 and the Gregorian calendar otherwise. It is possible to 
// specify a different switch date using the `julian-gregorian` function.
// The calendar short name is ``JG``.
public val cal-jg = julian-gregorian()

public fun julian-gregorian( switch-date: date = Date(1582,10,15) ) : calendar {
  fun showera( d:date ) { 
    if (d < switch-date) {
      if (d.year >= 1500) then "CE (O.S.)" else (cal-julian.show-era)(d)
    }
    else {
      if (d.year <= 1926) then "CE (N.S.)" else (cal-gregorian.show-era)(d)
    }
  }
  combine-earth-calendars( "JG", "Julian-Gregorian", switch-date, cal-julian, cal-gregorian, Just(showera) )
}

