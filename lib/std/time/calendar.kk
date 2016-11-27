/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Calendars.

  A Calendar determines how a `:date` and `:clock` relates to an `:instant` in time
  with respect to a `:timezone`. These functions turn `:instant`'s into "human" dates. 
  Years, months, days etc. are always 1-based.  
*/
module std/time/calendar

import std/num/fixed
import std/time/timestamp
import std/time/duration
import std/time/instant
import std/time/utc
import std/time/date

extern include {
  js file "calendar-inline.js"
}

// A Calendar determines how a `:date` and `:clock` relates to an `:instant` in time. 
public struct calendar(
  name        : string,                 
  long-name   : string,
  timescale   : timescale,
  private instant-to-dc : (i:instant,tzdelta:duration) -> (date,clock),
  private dc-to-instant : (date,clock) -> instant,
  month-prefix: string = "",
  show-era    : (date) -> string = fun(d){ "" }
)


/*----------------------------------------------------------------------------
  Time zone
----------------------------------------------------------------------------*/

// A `:timezone` determines a time offset with respect to the UTC / GMT timezone.
// The `tz-utc` and `tz-local` time zones are used for UTC/GMT time and the local system time.\
// The `name` field contains the time zone name as IANA timezone identifier.
// The `utc-delta` field returns the time zone offset
// relative to UTC (for GMT-08:00 it returns a duration of `~8*3600` seconds) and the abbreviated 
// time zone name (``PDT`` for example).\
// The optional `utc-inverse` field returns for an instant in the time zone, the associated UTC time.
// By default it returns `Nothing` in which case a generic algorithm is used to determine the
// inverse.  
public struct timezone(
  name    : string,                      
  private utc-delta  : (instant) -> (duration,string),
  private utc-inverse: (instant) -> maybe<instant> = fun(i) { Nothing }
)


// Is this the UTC timezone?
public fun tz-utc?( tz : timezone ) : bool {
  (tz.name == tz-utc.name)
}

// Create a time zone with a fixed number of hours and optional minutes (=`0`) difference from UTC/GMT.
// The time zone name and abbreviation is optional, and the default name
// will reflect the offset from UTC, for example `"UTC+1:20"`, or `"UTC-8"`.
// The default abbreviation is the empty string.
// The sign of the minutes will be matched to the hours (unless `hours` is `0`). 
// Returns `tz-utc` if both `hours` and `mins` are zero.
public fun tz-fixed( hours: int, mins : int = 0, name : string = "", abbrv : string = "", hourwidth : int = 1 ) : timezone {
  if (hours.zero? && mins.zero?) then return tz-utc
  val xmins = if (hours.pos?) then mins.abs elif (hours.neg?) then (mins.abs).negate else mins
  val secs  = hours*3600 + xmins*60
  val tzname = name || ("UTC" + (if (secs.neg?) then "-" else "+")
                          + hours.abs.show.pad-left(hourwidth,'0')
                          + (if (mins.zero?) then "" else ":" + mins.abs.show.pad-left(2,'0')))
  val delta  = duration(secs)
  tz-fixed(tzname,delta,abbrv)
}

fun tz-fixed( name : string, delta : duration, abbrv : string = "" ) : timezone {
  Timezone( name, 
    fun(i) { (delta,abbrv) },
    fun(i) { if (delta.zero?) then Just(i) else Nothing } 
  )
}

// The standard UTC time zone with a 0 delta.
public val tz-utc : timezone = tz-fixed("UTC",duration0)

// Return the local timezone on the current system.
public fun tz-local() : ndet timezone {
  val tz = local-get-timezone()
  Timezone( 
    "",
    fun(i) { 
      val (ofs,abbrv) = local-utc-delta(tz, i.timestamp(ts-unix).seconds.double) 
      (duration(ofs.fixed),abbrv)
    }
  )
}

// A local timezone structure provided by the host system
type local-timezone

// Get the current local timezone structure.
extern local-get-timezone() : ndet local-timezone {
  js "_local_get_timezone"
}

// Return the utc delta in fractional seconds given a local timezone structure
// in fractional seconds since the UNIX epoch (1970-01-01) together with the
// time zone abbreviation.
extern local-utc-delta( tz : local-timezone, i : double ) : (double,string) {
  js "_local_utc_delta"
}



/*----------------------------------------------------------------------------
  Standard calendars
----------------------------------------------------------------------------*/

// The standard [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) calendar
// using UTC time. This is a proleptic Gregorian 
// calendar except that it uses the year 0 for 1BC, -1 for 2BC etc. 
public val cal-iso : calendar = iso-calendar( ts-utc, "", "ISO" )

// The [Julian calendar](https://en.wikipedia.org/wiki/Julian_calendar).
// Uses old-style ``BC`` and ``AD`` to display era's.
// The calendar short name is ``JC``.
public val cal-julian = earth-calendar(
  "JC",
  "Julian",
  julian-days-to-date,
  julian-date-to-days,
  show-era = fun(d:date){ if (d.year.neg?) then "BC" else "AD" }
)

// The (proleptic) [Gregorian calendar](https://en.wikipedia.org/wiki/Gregorian_calendar).
// Just like the ISO calendar except that the year 0 does not exist, i.e. after 1 BCE (=`~1`)
// we have 1 CE (=`1`). 
// The calendar short name is ``GC``.
public val cal-gregorian = earth-calendar(
  "GC",
  "Gregorian",
  gregorian-days-to-date,
  gregorian-date-to-days,
  show-era = fun(d:date){ if (d.year.neg?) then "BCE" else "CE" }
)

// The [Coptic](https://en.wikipedia.org/wiki/Coptic_calendar) calendar.
// The calendar short name is ``CC``.
public val cal-coptic = earth-calendar(
  "CC",
  "Coptic",
  coptic-days-to-date,
  coptic-date-to-days,
  show-era = fun(d:date){ if (d.year.pos?) then "A.M." else "" }  // Anno Martyrum
)

// The [Ethiopian](https://en.wikipedia.org/wiki/Ethiopian_calendar) calendar.
// The calendar short name is ``EC``.
public val cal-ethiopian = earth-calendar(
  "EC",
  "Ethiopian",
  ethiopian-days-to-date,
  ethiopian-date-to-days
)


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
public val cal-iso-month = earth-calendar(
  "IM",
  "ISO Month",
  isom-days-to-date,
  isom-date-to-days,
  "M"
)


// The 'ISO week' calendar. This implements the [ISO week date](https://en.wikipedia.org/wiki/ISO_week_date)
// calendar the week number is interpreted as "month" number instead, i.e. every year has 52 (or 53)
// months of 7 week days each. Short name is `"IW"`.
public val cal-iso-week = earth-calendar(
  "IW",
  "ISO Week",
  isow-days-to-date,
  isow-date-to-days,
  "W"
)


// The combined Julian / Gregorian calendar, using the Julian calendar for dates 
// before 1582-10-15 and the Gregorian calendar otherwise. It is possible to 
// specify a different switch date using the `julian-gregorian` function.
// The calendar short name is ``JG``.
public val cal-jg = julian-gregorian()

/*----------------------------------------------------------------------------
  Time scale calendars
----------------------------------------------------------------------------*/

// The TAI Calendar. This is a standard ISO 8601 calender using
// TAI time where every day is exactly 86400 SI seconds (unlike
// standard UTC time which can insert leap seconds).
public val cal-tai = iso-calendar( ts-tai )

// The GPS calendar is always `TAI - 19` but with epoch 1980-01-06Z
public val cal-gps = iso-calendar( ts-gps ) //(ts-gps.to-tai)(timestamp(fixed(~19))).seconds )


// Create a standard ISO calendar using a particular time scale
// and calendar `name` (=`ts.name`).
public fun iso-calendar( ts : timescale, name : string = ts.name, long-name : string = name ) : calendar {
  earth-calendar(
    name,
    long-name,
    iso-days-to-date,
    iso-date-to-days,
    ts=ts
  )
}


/*----------------------------------------------------------------------------
  Earth calendars
----------------------------------------------------------------------------*/

// Used for earth calendars
val secs-per-day = 86400

// Create a calendar where we assume that each
// day has 24 hours with 60 minutes, and where minutes are 60 seconds
// (with possibly leap seconds). 
//
// This function constructs a calendar given a calendar `name`,
// a function `days-to-date` to calculate a date given a number of days since the `epoch`,
// a function `date-to-days` to calculate days since the `epoch` for a given date,
// and an optional time scale to be used (=`ts-utc`).
public fun earth-calendar(
              name        : string,
              long-name   : string,
              days-to-date  : (days:int) -> date,
              date-to-days  : date -> int,
              month-prefix  : string = "",
              show-era      : (date)->string = fun(d){ "" },
              ts   : timescale = ts-utc
            ) : calendar
{
  val epoch-shift = ts.timescale-y2k.timespan(ts)  
  fun instant-to-dc(i : instant, tzdelta : duration) { 
    earth-timestamp-to-dc( (i.timestamp(ts) - epoch-shift) + tzdelta.timespan, days-to-date ) 
  }
  fun dc-to-instant(d : date, c : clock) { 
    unsafe-instant-at( earth-dc-to-timestamp( d, c, date-to-days ) + epoch-shift, ts ) 
  }
  Calendar(name,long-name,ts,instant-to-dc,dc-to-instant,month-prefix,show-era)
}


fun earth-timestamp-to-dc( t : timestamp, days-to-date: (int) -> date ) : (date,clock) {
  // get whole seconds and fraction, such that fraction is always positive (into the day)
  val (nlsecs,nlfrac,leap)  = t.calendar-seconds  
  // get the date from the whole days; use euclidean division such that daysecs is always positive                                           
  val (days,daysecs)   = divmod( nlsecs, secs-per-day)
  val date             = days-to-date(days)
  // Finally set the clock taking care of leap seconds
  val (hours,minsecs)  = divmod(daysecs,3600)
  val (mins,xsecs)     = divmod(minsecs,60)
  val tsecs            = fixed(xsecs + leap, nlfrac)  // adjust for leap seconds
  (date, Clock(hours,mins,tsecs))
}

fun earth-dc-to-timestamp( d : date, c : clock, date-to-days: (date) -> int ) : timestamp {
  // Get seconds and leap seconds (if the clock has seconds > 59)
  val secs = c.seconds.trunc
  val frac = c.seconds.fraction
  val (fsecs,leaps)  = if (secs < 60) then (secs,0) else ( 59, secs - 59 )
  val (xdays,xsecs) = divmod( ((((c.hours*60) + c.minutes)*60) + fsecs), secs-per-day )
  val sdays = date-to-days( d(day = d.day + xdays) )
  timestamp( sdays*secs-per-day + xsecs, frac ).add-leap-seconds(leaps.fixed)  
}


// Create a Julian-Gregorian calendar which uses Julian dates before the optional `switch`
// (Gregorian) date (=`Date(1582,10,15`)) and Gr egorian dates otherwise.
public fun julian-gregorian( switch : date = Date(1582,10,15) ) : calendar {
  val switch-days = iso-date-to-days(switch) //+ skip
  earth-calendar( "JG", "Julian-Gregorian",
    fun(days) { julgreg-days-to-date(days,switch-days) },
    fun(d)    { julgreg-date-to-days(d,switch) },
    show-era = fun(d:date) { 
      if (d < switch) {
        if (d.year >= 1500) then "CE (O.S.)" else (cal-julian.show-era)(d)
      }
      else {
        if (d.year <= 1926) then "CE (N.S.)" else (cal-gregorian.show-era)(d)
      }
    }
  )
}


/*----------------------------------------------------------------------------
  Time and instant conversion using a calendar
----------------------------------------------------------------------------*/

// Return the instant in time for a given `:date` and `:clock` (= `clock0`) interpreted by 
// calendar `cal` (=`cal-iso`) in a timezone `tz` (=`tz-utc` by default).
public fun instant-at( d : date, c : clock = clock0, tz : timezone = tz-utc, cal : calendar = cal-iso ) : instant 
{
  // the year/month/day is interpreted as if in UTC first.
  val i = (cal.dc-to-instant)( d, c )   
  if (tz.tz-utc?) return i
  // we need to adjust according to timezone 
  match((tz.utc-inverse)(i)) {
    Just(inv) -> inv
    Nothing -> {
      // no explicit inverse,
      // do a double pass to accommodate jumping over a DST boundary.
      if (i.timescale.has-leap-seconds) then {
        // If in UTC, we calculate directly on UTC timespan's as to ignore any possible
        // leap seconds during the timezone offset change. 
        // (for example: instant-at(1979,1,1,3,29,59,tz=timezone("Asia/Tehran")).time.show == "1978-12-31T23:59:59Z"))
        val utci = i.timestamp(i.timescale)
        val tzd1 = (tz.utc-delta)(i).fst
        val tzi1 = unsafe-instant-at(utci - tzd1.timespan, i.timescale)
        val tzd2 = (tz.utc-delta)(tzi1).fst
        val tzi2 = unsafe-instant-at(utci - tzd2.timespan, i.timescale)
        tzi2
      }
      else {
        // But in other cases, we interpret a timezone offset in seconds of that time scale,
        // and we should *not* convert to UTC or otherwise we may lose/add a leap seconds
        // in a time zone transition.
        // (for example: time(1973,1,1,0,0,11,tz=tz-fixed(~1),cal=cal-tai).show == "1973-01-01T00:00:11-01:00 TAI")
        val tzd1 = (tz.utc-delta)(i).fst
        val tzi1 = i - tzd1
        val tzd2 = (tz.utc-delta)(tzi1).fst
        val tzi2 = i - tzd2
        tzi2
      }
    }
  }
}

// Convert an `:instant` to a `:date`, `:clock`, timezone delta and abbreviation, 
// for a given timezone `tz` (=`tz-utc` by default) and calendar (=`cal-iso` by default).
public fun instant-dc( i : instant, tz :timezone = tz-utc, cal : calendar = cal-iso ) : (date,clock,duration,string) {
  val (tzdelta,tzabbrv) = (tz.utc-delta)(i)
  val (d,c) = (cal.instant-to-dc)( i, tzdelta )
  (d,c,tzdelta,tzabbrv)
}

/* Return the instant in time for a given date and clock interpreted by 
   calendar `cal` (=`cal-iso`) in a timezone `tz` (=`tz-utc` by default).

  The `month`, `day`, `hour`, `minutes`, and `seconds` may be outside their usual ranges 
  and will be normalized during the conversion. For example, January 33 converts to February 2.
  This makes it very easy to add- or subtract days or months to an existing time.

  Due to timezone transitions, or leap seconds, it is possible to specify dates that never
  happend (as it was skipped by a timezone change), or ambigious times (as a timezone springs back).
  In such cases, the functions returns always interpreted in the earlier timezone.
*/
public fun instant-at( year : int, month : int = 1, day : int = 1, 
                    hours : int = 0, minutes : int = 0, seconds : int = 0, 
                    frac : double = 0.0, tz : timezone = tz-utc, cal : calendar = cal-iso ) : instant 
{
  val fsecs = if (frac.zero?) then fixed(seconds) else fixed(seconds) + fixed(frac)
  instant-at( Date(year,month,day), Clock(hours,minutes,fsecs), tz, cal )
}

// Return the instant in time for a given `:weekdate` and `:clock` (=`clock0`) interpreted by 
// calendar `cal` (=`cal-iso`) in a timezone `tz` (=`tz-utc` by default).
// It is also possible to pass the weekday that determines if 
// a week falls in the current year or not (which is Thursday for the ISO definition)
public fun instant-at( wd : weekdate, c : clock = clock0, tz : timezone = tz-utc, cal : calendar = cal-iso, weekday-in-year : weekday = Thu ) : instant 
{  
  val adj = Date(wd.year,1,weekday-in-year.int).weekday(cal=cal).int + (7 - weekday-in-year.int)
  val d   = Date(wd.year, 1, (7*wd.week + wd.day.int) - adj)  
  instant-at( d, c, tz, cal )
}

// Return the day of the week for a calendar `cal` (=`cal-iso`).
fun weekday( d : date, cal : calendar ) : weekday {
  // Generic algorithm for any calendar
  val i    = instant-at(d.year,d.month,d.day,cal=cal)
  val days = i.mjd(cal.timescale).floor
  val dow  = (days+3)%7        // plus 3 since MJD epoch was a wednesday  (note: 0 = sunday, 1 = monday, ..)
  dow.weekday                  // to ISO week day 
}

// Return the difference in days between two dates interpreted by calendar `cal`.
public fun day-diff( cal : calendar, d1 : date, d2 :date ) : int {
  val i1 = instant-at(d1,cal=cal)
  val i2 = instant-at(d2,cal=cal)
  val ts = cal.timescale
  (i1.mjd(ts) - i2.mjd(ts)).floor
}


/*----------------------------------------------------------------------------
  ISO calendar, Proleptic Gregorian calendar, and Julian calendar.
  Algorithms as described by Howard Hinnant <https://howardhinnant.github.io/date_algorithms.html>
----------------------------------------------------------------------------*/

val iso-epoch-shift = 730425   // shift internal epoch from 0000-03-01 to 2000-01-01
val iso-days-in-era = 146097   // 365*400 + 100 - 3
val iso-years-in-era= 400

// Convert days since `epoch` to a date in the [ISO8601] calendar,
// a proleptic Gregorian calendar where year 0 is 1BC.
//
// [ISO8601]: https://en.wikipedia.org/wiki/ISO_8601
fun iso-days-to-date( days : int ) : date {
  val z    = days + iso-epoch-shift          // shift epoch from 2000-01-01 to 0000-03-01
  val (era,doe) = divmod(z,iso-days-in-era)  // day of era: 0 <= doe < days-in-era
  val yoe  = (((doe - (doe/1460)) + (doe/36524)) - (doe/146096)) / 365;  // year of era: 0 <= yoe < years-in-era 
  val doy  = doe - ((yoe*365) + (yoe/4) - (yoe/100))   // day-of-year: 0 <= doy <= 365
  val mp   = ((doy*5) + 2)/153                         // 0 <= mp <= 0,11
  val day  = (doy - (((mp*153) + 2) / 5)) + 1          // 1 <= day <= 31
  val month= mp + (if (mp < 10) then 3 else ~9)        // 1 <= month <= 12
  val year = yoe + (iso-years-in-era * era) + (if (month <= 2) then 1 else 0)
  Date(year,month,day)
}


// Calculate days from a year/month/day triple in the proleptic Gregorian calendar
// `month` must be between 1 and 12. `day` must be between 1 and `days-in-era`.
fun iso-datex-to-days( year : int, month : int, day : int ) : int {
  val y = if (month <= 2) then year.dec else year   // to internal year starting in Feb
  val (era,yoe) = divmod(y,iso-years-in-era)             // year of era: 0<= yoe < 400
  val mdoy = (((153 * (month + (if (month > 2) then ~3 else 9))) + 2)/5) 
  val doe  = (((yoe*365) + (yoe/4)) - (yoe/100)) + mdoy + day.dec
  ((iso-days-in-era * era) + doe) - iso-epoch-shift
}

// Return days from a year, month, and day in the [ISO8601] calendar.
// Months and days can be any value.
fun iso-date-to-days( d : date ) : int {
  val (xera,doe)  = divmod(d.day.dec, iso-days-in-era)
  val (xyear,moy) = divmod(d.month.dec, 12)
  iso-datex-to-days( d.year + (xera * iso-years-in-era) + xyear, moy.inc, doe.inc )
}

// Return days from a date in the proleptic Gregorian calendar.
fun gregorian-date-to-days( d : date ) : int {
  if (d.year <= 0) 
    then iso-date-to-days(d(year = d.year + 1)) 
    else iso-date-to-days(d)
}

// Return a date from a number of days in the proleptic Gregorian calendar.
fun gregorian-days-to-date( days : int ) : date {
  val d = iso-days-to-date(days)
  if (d.year<=0) then d(year = d.year - 1) else d
}

val julian-days-in-era  = 1461
val julian-years-in-era = 4
val julian-epoch-shift  = 730427 // Shift from 2000-01-01 to 0000-03-01

// Return a date from a number of days in the Julian calendar.
fun julian-days-to-date( days : int ) : date {  
  val z = days + julian-epoch-shift
  val (era,doe) = divmod(z,julian-days-in-era)  // 0 <= doe <= 1460
  val yoe   = (doe - doe/1460) / 365            // 0 <= yoe <= 3
  val doy   = doe - 365*yoe                     // 0 <= doy <= 365
  val mp    = (doy*5 + 2) / 153
  val day   = (doy - ((153*mp + 2)/5)) + 1
  val month = mp + (if (mp < 10) then 3 else ~9)
  val year  = yoe + (julian-years-in-era * era) + (if (month <= 2) then 1 else 0)
  val yyear = if (year<=0) then year.dec else year
  val d = Date(yyear,month,day)
  // trace("julian days to date: " + days.show + " -> " + d.show)  
  d
}

fun julian-datex-to-days( year : int, month : int, day : int ) : int {
  val y = if (month <= 2) then year.dec else year
  val (era,yoe) = divmod(y,julian-years-in-era)
  val doy = ((153*(month + (if (month>2) then ~3 else 9)) + 2)/5 + day) - 1
  val doe = yoe*365 + doy
  ((era * julian-days-in-era) + doe) - julian-epoch-shift
}  

// Return the days from a date in the Julian calendar.
fun julian-date-to-days( d : date ) : int {
  val (xera,doe)  = divmod(d.day.dec, julian-days-in-era)
  val (xyear,moy) = divmod(d.month.dec, 12)
  val yyear = if (d.year<=0) then d.year.inc else d.year
  val days = julian-datex-to-days( yyear + (xera * julian-years-in-era) + xyear, moy.inc, doe.inc )    
  // trace("julian date to days: " + d.show + " -> " + days.show)  
  days
}


fun julgreg-date-to-days( d : date, switch : date ) : int {
  if (compare(d,switch).lt?) 
    then julian-date-to-days(d)
    else iso-date-to-days(d) 
}

fun julgreg-days-to-date( days : int, switch : int ) : date {
  if (days < switch) 
   then julian-days-to-date(days)
   else iso-days-to-date(days)
}

/*----------------------------------------------------------------------------
  Coptic & Ethiopian calendars
----------------------------------------------------------------------------*/

val coptic-days-in-era  = 1461
val coptic-years-in-era = 4
val coptic-epoch-shift  = 626515 // Shift from 2000-01-01 to 0284-08-29 JC

// Return a date from a number of days in the Coptic calendar.
fun coptic-days-to-date( days : int ) : date {  
  val z = days + coptic-epoch-shift
  val (era,doe) = divmod(z,coptic-days-in-era)  // 0 <= doe <= 1460
  val yoe   = (doe - doe/1460) / 365            // 0 <= yoe <= 3
  val doy   = doe - 365*yoe + yoe/3             // 0 <= doy <= 365
  val mp    = (doy / 30) 
  val day   = (doy - (30 * mp)) + 1
  val month = mp + 1
  val year  = yoe + (coptic-years-in-era * era) + 1
  //val yyear = if (year<=0) then year.dec else year  // no zero year
  val d = Date(year,month,day)
  // trace("coptic days to date: " + days.show + " -> " + d.show)  
  d
}

fun coptic-datex-to-days( year : int, month : int, day : int ) : int {
  val (era,yoe) = divmod(year.dec,coptic-years-in-era)  // 0 <= yoe <= 3
  val doy = 30*(month - 1) + (day - 1)
  val doe = yoe*365 + yoe/3 + doy
  ((era * coptic-days-in-era) + doe) - coptic-epoch-shift
}  

// Return the days from a date in the Coptic calendar.
public fun coptic-date-to-days( d : date ) : int {
  val (xera,doe)  = divmod(d.day.dec, coptic-days-in-era)
  val (xyear,moy) = divmod(d.month.dec, 13)
  // val yyear = if (d.year<=0) then d.year.inc else d.year  // no zero year, treat 0 as 1
  val days = coptic-datex-to-days( d.year + (xera * coptic-years-in-era) + xyear, moy.inc, doe.inc )    
  // trace("coptic date to days: " + d.show + " -> " + days.show)  
  days
}

// The Ethiopian calendar is just Coptic but with a different epoch
val ethiopian-epoch-shift  = 727324   // 2001-01-01 to 0008-08-29 JC
val ethiopian-coptic-shift = (ethiopian-epoch-shift - coptic-epoch-shift)

public fun ethiopian-date-to-days( d : date ) : int {
  coptic-date-to-days(d) - ethiopian-coptic-shift
}

public fun ethiopian-days-to-date( days : int ) : date {
  coptic-days-to-date(days + ethiopian-coptic-shift)
}



/*----------------------------------------------------------------------------
  The ISO week and ISO month calendars
  The ISO week calendar is the ISO week date calendar where every 'month' is
  the week of the year. The algorithms here are stand-alone and don't
  rely on the Gregorian calendar.
  The ISO month calendar is based upon the week calendar and expressses
  the week/day-of-week as month/day-of-month.
----------------------------------------------------------------------------*/

//--------------------------------------------------------------
// ISO week calendar: date to days
//--------------------------------------------------------------

val isow-epoch-shift = 730119 // to Monday, 0001-01-01  

// Convert a `:date` in the ISO week calendar (the weeks are the `month`s of the year) to days since the epoch.
fun isow-date-to-days(d : date ) : int {
  isow-doy-to-days(d.year, 7*d.month.dec + d.day.dec)
}

fun isow-doy-to-days( year : int, doy : int ) : int {
  (isow-before-year(year) + doy) - isow-epoch-shift
}

// Returns the days before `year` from epoch 0001-01-01
fun isow-before-year(year : int) : int {
  val gdays   = gbefore-year(year)
  val weekday = gdays%7  // monday=0, sunday=6
  val adjust  = if (weekday <= 3) then (0 - weekday) else (7 - weekday)
  gdays + adjust
}

// Return the days before Gregorian year `gyear`.
fun gbefore-year(gyear : int ) : int {
  365*(gyear - 1) + gleapdays-before(gyear) 
}

// Number of leap days before a Gregorian year.
fun gleapdays-before(gyear : int) : int {
 ((gyear - 1)/4 - (gyear - 1)/100) + (gyear - 1)/400
}

//--------------------------------------------------------------
// ISO week calendar: days to date
//--------------------------------------------------------------

// Convert days since epoch to an ISO week date.
fun isow-days-to-date(days : int) : date {
  val (year,doy) = isow-days-to-doy(days)
  val (week,day) = divmod(doy,7)
  Date(year,week.inc,day.inc)
}

// Convert days since epoch to an ISO year and day of year.
fun isow-days-to-doy( days:int ) : (int,int) {
  val edays   = days + isow-epoch-shift
  val approx  = gyear-of(edays - 3) // -3 so we are at most one year early
  val doy     = edays - isow-before-year(approx)
  if (doy < 364 || doy < isow-days-in-year(approx))
   then (approx,doy)
   else (approx.inc, edays - isow-before-year(approx.inc))
}


// Return the Gregorian year that contains `days` after epoch (0001-01-01)
fun gyear-of(days : int ) : int {
  1 + (days - gleapdays-of(days))/365
}

// Return the Gregorian leap days that are contained in `days` after epoch.
fun gleapdays-of(days : int) : int {
  val ldays = days + 306   // since 0000-03-01
  (ldays/1460 - ldays/36524) + ldays/146096
}

// Return the number of days in an ISO week/month year.
fun isow-days-in-year( year : int ) : int {
  fun weekday(y:int) { 
    (y + gleapdays-before(y)).dec % 7     // The first weekday of the year `y` (0001-01-01 is a Monday)
  }
  if (weekday(year)==3 || weekday(year+1)==4) // `year` starts or ends on a Thursday
   then 371 else 364
}

//--------------------------------------------------------------
//  ISO month calendar
//--------------------------------------------------------------

// Convert an ISO month date to days since epoch. Built upon the ISO week calendar.
fun isom-date-to-days(d : date) : int {
  val (xyear,xmonth) = divmod(d.month.dec,12) // roll over months
  val doy = isom-before-month(xmonth) + d.day.dec
  isow-doy-to-days(d.year + xyear, doy)
}

// Convert days since epoch to an ISO month date. Built upon the ISO week calendar.
fun isom-days-to-date(days:int) : date {
  val (year,doy) = isow-days-to-doy(days)
  val month = isom-month-of( doy )
  val day   = doy - isom-before-month(month)
  Date(year,month.inc,day.inc)
}

// return the days before zero-based month `m`
fun isom-before-month( month : int ) : int {
  30*month + month/3
}

// given a zero based day of the year, returns its zero based month
fun isom-month-of( doy : int ) : int {
  val m = (100*doy.inc)/3034   // (doy - (doy+1)/91)/30
  min(m,11)
}
