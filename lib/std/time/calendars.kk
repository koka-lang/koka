/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Various calendars: TAI, GPS, ISO week, Julian, Coptic, etc.
  
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

// The 'ISO week' calendar. This implements the [ISO week date](https://en.wikipedia.org/wiki/ISO_week_date)
// calendar the week number is interpreted as "month" number instead, i.e. every year has 52 (or 53)
// months of 7 week days each. Short name is `"IW"`.
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
This is _not_ a standard ISO calendar -- it is named this way because
it is based directly on the standard [ISO week
date](https://en.wikipedia.org/wiki/ISO_week_date) calendar.

The 'ISO' month calendar matches the ISO week date calendar directly, but
denotes dates as usual using months and a day of the month (instead of
using a week number and day of the week).

Every year always starts on the first day of week 1 in the ISO week
calendar, and therefore always starts on a Monday (on January 1st) and
ends on a Sunday. Each regular year is divided in 4 quarters of 13 ISO
weeks each. A quarter also always starts on Monday and ends on Sunday.
The three months of each quarter are 30, 30, and 31 days respectively.
So, January has 30 days, February has 30 days too, March has 31 days,
April has 30 days again, etc.

When there is an ISO week 53, we call it a leap year where the 'leap
week' 53 gets inserted at the end of the last month (and adds days 32 to
38 to December). The epoch is Monday 0001-01-01 IM which coincides with
Monday 0001-01-01 in the ISO/Gregorian calendar.

Note that with
a [perennial](https://en.wikipedia.org/wiki/Perennial_calendar) calendar,
things like finding the _n_\/th weekday is suddenly trivial; For example,
Thanksgiving (the 4th Thursday of November) is always on November 23 IM,
and always 31 days before from Christmas. Similarly, Chrismas and New
year are always on a Sunday.

The short name of the calendar is `"IM"`.

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

^&dagger;^ A leap week sometimes inserted
  at the end of December. Upcoming years with a leap week are 2020, 2026, 2032, 2037, and 2043. In
  general, a leap week is inserted whenever the corresponding Gregorian
  year starts and/or ends on a Thursday.

~ End Calendar

Specific advantages:

* Every year (and quarter) starts on a Monday and ends on a Sunday.
* Dates always have the same week day in every year (&ie; the calendar
  is [perennial](https://en.wikipedia.org/wiki/Perennial_calendar)).
* A year divides in 4 equal quarters which simplifies financial
  calculations.
* Weeks are always 7 days, years are always 12 months.
* Thanksgiving (4th Thursday of November) is always on November 23.
* Christmas and New year are always on Sunday.
* There is never a Friday the 13th.
* Closely matches the existing Gregorian calendar in the months and month
  lengths. A date is never more than 5 days apart from the corresponding
  Gregorian date (90% of the time it is within 3 days).
* Precisely matches the widely accepted and implemented standard 
  [ISO 8601 week date](https://en.wikipedia.org/wiki/ISO_week_date) 
  calendar. This makes it much simpler to adapt computer systems to
  handle the new dates as they map directly onto each other. 
* The leap week could be used to give everyone an extra week of vacation
  every 5 or 6 years between Christmas and New year.

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

