/*----------------------------------------------------------------------------
   Copyright (C) 2012-2019 Microsoft Corporation

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Time represents instants as human readable calendar time.

  A `:time` represents an `:instant` in time in a given `:calendar` and `:timezone`
  as a "human" calendar time. Months, days etc. are always 1-based.
*/
module std/time/time

import std/num/ddouble
import std/time/timestamp
import std/time/duration
import std/time/instant
import std/time/utc
import std/time/date
import std/time/calendar
import std/time/chrono

// Represents an instant in time for a certain calendar and timezone.
abstract struct time (
  // The year.
  public  date    : date,
  public  clock   : clock,
  public  calendar: calendar,
  public  tzdelta : duration = zero,
  public  tzabbrv : string   = "",
  public  timezone: timezone = tz-utc,
  public  instant : instant   // the timescale of the instant is the timescale of the calendar
)

// The `:timescale` of the time.
public fun timescale( t : time ) : timescale {
  t.instant.timescale
}

// Return the year of a `:time`.
public fun year( t : time ) : int {
  t.date.year
}

// Return the month of a `:time`. (starting at 1)
public fun month( t : time ) : int {
  t.date.month
}

// Return the day of the month of a `:time`. (starting at 1)
public fun day( t : time ) : int {
  t.date.day
}

// Return the whole hours of a `:time`.
public fun hours( t : time ) : int {
  t.clock.hours
}

// Return the whole minutes of a `:time`.
public fun minutes( t : time ) : int {
  t.clock.minutes
}

// Return the fractional seconds of a `:time`.
public fun seconds( t : time ) : timespan {
  t.clock.seconds
}

// Compare two `:time`s. Compares the actual instants in time
// and can thus compare across calendars and timezones.\
// `time(2001,7,2,tz=tz-fixed("GMT+1",duration(3600))) > time(2001,7,1,23,30,0)` &quad; (`False`!)
public fun compare( t1 : time, t2 : time ) : order {
  compare(t1.instant,t2.instant)
}

public fun (<)( i : time, j : time )  : bool { compare(i,j) == Lt }
public fun (<=)( i : time, j : time ) : bool { compare(i,j) != Gt }
public fun (>)( i : time, j : time )  : bool { compare(i,j) == Gt }
public fun (>=)( i : time, j : time ) : bool { compare(i,j) != Lt }
public fun (==)( i : time, j : time ) : bool { compare(i,j) == Eq }
public fun (!=)( i : time, j : time ) : bool { compare(i,j) != Eq }

// The minimum of two times (as by their actual instant in time)
public fun min( i : time, j : time ) : time {
  if (i <= j) then i else j
}

// The maximum of two times (as by their actual instant in time)
public fun max( i : time, j : time ) : time {
  if (i >= j) then i else j
}


// Round a time to a specified second precision.
public fun round-to-prec( t : time, prec : int ) : time {
  if (prec.is-neg) then return t
  val c    = t.clock
  val secs = c.seconds.round-to-prec(prec)
  val ri   = t.instant.round-to-prec(prec)
  //trace("time.round-to-prec: " + t.show-raw + ", t.seconds: " + t.seconds.show + ", secs: " + secs.show)
  if (secs.truncate == t.seconds.truncate) then {
    // whole seconds stay the same, just update with the rounded seconds and instant
    t(clock=c(seconds=secs), instant = ri)
  }
  else {
    // otherwise recalculate the time from the rounded instant
    ri.time(t.timezone,t.calendar)
  }
}

// Show a `:time` in [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) format.
public fun show(t : time, prec : int = 9 ) : string {
  val tp = t.round-to-prec(prec)
  tp.show-raw(prec)
}

public fun show-raw( tp : time, prec : int = 9 ) : string {
  val tsname = tp.timescale.name
  tp.date.show(tp.calendar.month-prefix)
   ++ "T" ++ tp.clock.show(prec)
   ++ show-tzdelta(tp.tzdelta )
   ++ (if (tp.tzabbrv.is-numeric) then "" else " (" ++ tp.tzabbrv ++ ")")
   ++ (if (tp.calendar.name.is-empty || tp.calendar.month-prefix.is-notempty) then "" else " " ++ tp.calendar.name)
   ++ (if (tsname.is-empty || tsname == "UTC") then "" else " " ++ tsname)
}

fun is-numeric( abbrv : string ) : bool {
  (abbrv.is-empty || abbrv.head=="+" || abbrv.head=="-")
}

fun show-date(d : date) : string {
  d.show
}

// Show a time zone delta.
// Optional `utc` for displaying a zero timezone delta (=`"Z"`).
// Optional `hmsep` for the hour-minute separator (=`":"`).
// Optional `hrwidth` to give the minimal width of the hour field (=`2`).
public fun show-tzdelta(delta : duration, utc : string = "Z", hmsep : string = ":", hrwidth : int = 2) : string {
  val dt = delta.seconds.int
  if (dt.is-zero) return utc
  val (mins,secs) = divmod(dt.abs,60)
  val tz   = (if (dt.is-neg) then "-" else "+") ++ (mins/60).show0(hrwidth) ++ hmsep ++ (mins%60).show0
  val tzs  = if (secs.is-zero) then "" else ":" ++ secs.show0
  tz ++ tzs
}


// pad with zeros
fun show0( i : int, width : int = 2) : string {
  i.show.pad-left(width,'0')
}


/*----------------------------------------------------------------------------
  Convenience functions over time
----------------------------------------------------------------------------*/

// Convert an `:instant` to a `:time` value in a given timezone `tz` (=`tz-utc` by default)
// and calendar (=`cal-iso` by default).
public fun time( i : instant, tz : timezone = tz-utc, cal : calendar = cal-iso, ts : timescale = i.timescale ) : time {
  val j = i.use-timescale(ts)
  val (d,c,tzdelta,tzabbrv) = instant-dc(j,tz,cal)
  Time(d,c,cal,tzdelta,tzabbrv,tz,j)
}

// Convert a `:time` `t` to a new `:time` value in a potentially different
// timezone `tz` (=`t.timezone` by default) and calendar (=`t.calendar` by default).
public fun time( t : time, tz : timezone = t.timezone, cal : calendar = t.calendar, ts : timescale = t.instant.timescale ) : time {
  time(t.instant,tz,cal,ts)
}

public fun time( ts : timescale, year : int, month : int = 1, day : int = 1,
                 hours : int = 0, minutes : int = 0, secs : int = 0,
                 frac : double = 0.0, tz : timezone = tz-utc,
                 cal : calendar = cal-iso ) : time
{
  time(ts.instant(year,month,day,hours,minutes,secs,frac,tz,cal),tz,cal,ts)
}


/* Return the `:time` value for a given date and clock in a timezone `tz` (=`tz-utc` by default)
interpreted by calendar `cal` (=`cal-iso`).

The `month`, `day`, `hour`, `minutes` may be outside their usual ranges
and will be normalized during the conversion. For example, January 33 converts to February 2.
This makes it very easy to add- or subtract days or months to an existing time.

When the `seconds` or fraction of seconds `frac` add up over 60 though, those extra seconds are
interpreted as leap seconds.

Due to timezone transitions, or leap seconds, it is possible to specify dates that never
happened (as it was skipped by a timezone change), or ambigious times (as a timezone springs back).
In such cases, the time is always interpreted in the earlier timezone.
*/
public fun time( year : int, month : int = 1, day : int = 1,
                 hours : int = 0, minutes : int = 0, secs : int = 0,
                 frac : double = 0.0, tz : timezone = tz-utc,
                 cal : calendar = cal-iso, ts : timescale = utc() ) : <utc> time
{
  ts.time(year,month,day,hours,minutes,secs,frac,tz,cal)
}

public fun time-utc( i : instant, tz : timezone = tz-utc, cal : calendar = cal-iso) : <utc> time {
  time(i,tz,cal,utc())
}

public fun time-tai( i : instant, tz : timezone = tz-utc, cal : calendar = cal-iso ) : <utc> time {
  time(i,tz,cal,ts-tai)
}

public fun time-gps( i : instant, tz : timezone = tz-utc, cal : calendar = cal-iso ) : <utc> time {
  time(i,tz,cal,ts-gps)
}

public fun time-tt( i : instant, tz : timezone = tz-utc, cal : calendar = cal-iso ) : <utc> time {
  time(i,tz,cal,ts-tt)
}

public fun time-ti( i : instant, tz : timezone = tz-utc, cal : calendar = cal-iso ) : <utc> time {
  time(i,tz,cal,ts-ti)
}


public fun time( ts : timescale, d : date, c : clock = clock0, tz : timezone = tz-utc, cal : calendar = cal-iso) : time
{
  time(ts.instant(d,c,tz,cal),tz,cal,ts)
}

// Return the `:time` value for a given `:date` and `:clock` (=`clock0`) in a timezone `tz` (=`tz-utc` by default)
// interpreted by calendar `cal` (=`cal-iso`)
public fun time( d : date, c : clock = clock0, tz : timezone = tz-utc, cal : calendar = cal-iso, ts : timescale = utc() ) : <utc> time
{
  ts.time(d,c,tz,cal)
}

// Add a date (years, months, days) and optional clock to a time.  Takes leap years, leap seconds, etc. into account.
public fun add-date( t : time, d : date, c : clock = clock0 ) : time {
  t.timescale.time( t.date + d, t.clock + c, t.timezone, t.calendar )
}

// Add a clock (hours, minutes, seconds) to a time. Takes leap years, leap seconds, etc. into account.
public fun add-clock( t : time, c : clock ) : time {
  t.timescale.time( t.date, t.clock + c, t.timezone, t.calendar )
}

// Add  a specified number of days to a calendar time. Takes leap years etc. into account.\
// `time(2016,12,31,12).add-days(1).show == "2017-01-01T12:00:00Z"` &quad; (over a leap second)\
// `time(1582,10,4,cal=cal-jg).add-days(1).show == "1582-10-15T00:00:00Z JG"` &quad; (transition from Julian (`cal-julian`) to Gregorian (`cal-gregorian`) calendar)\
public fun add-days( t : time, days : int, c : clock = clock0 ) : time {
  t.add-date( Date(0, 0, days), c )
}

// Add  a specified number of weeks to a calendar time.
public fun add-weeks( t : time, weeks : int ) : time {
  t.add-days( weeks*7 )
}

// Add  a specified number of months to a calendar time.
public fun add-months( t : time, months : int ) : time {
  t.add-date( Date(0,months,0) )
}

// Add  a specified number of years to a calendar time.
public fun add-years( t : time, years : int ) : time {
  t.add-date( Date(years,0,0) )
}


// Add an SI second `:duration` to a time.\
// `(time(2015,12,31,23,59,59,0.5) + duration(1)).show == "2016-01-01T00:00:00.500Z"`\
// `(time(2016,12,31,23,59,59,0.5) + duration(1)).show == "2016-12-31T23:59:60.500Z"` &quad; (into a leap second)
public fun (+)( t : time, d : duration ) : time  {
  (t.instant + d).time(t.timezone,t.calendar)
}

// Subtract an SI second `:duration` from a time.\
// `(time(2016,1,1,0,0,0,0.5) - duration(1)).show == "2015-12-31T23:59:59.500Z"`\
// `(time(2017,1,1,0,0,0,0.5) - duration(1)).show == "2016-12-31T23:59:60.500Z"` &quad; (into a leap second)
public fun (-)( t : time, d : duration ) : time {
  t + (~d)
}

// Return the exact SI second duration between to times.\
// `(time(2016,1,1,0,0,0,0.5) - time(2015,12,31,23,59,59)).show == "1.5s"`\
// `(time(2017,1,1,0,0,0,0.5) - time(2016,12,31,23,59,59)).show == "2.5s"`  &quad; (over a leap second)\
// `(time(2017,1,1,0,0,0,0.5) - time(2017,1,1,2,59,59,tz=tz-fixed(3)).show == "2.5s"`  &quad; (GMT+3, and over a leap second)\
public fun (-)( t1 : time, t2 : time ) : duration {
  (t1.instant - t2.instant)
}

// Copy a `:time` with optionally changing any of its attributes.\
// `t.copy(month=1,day=1,hours=0,minutes=0,seconds=zero)` &quad; (copy to start of the year)\
// `t.copy(month=t.month+2,day=t.day+3)                 ` &quad; (add 2 months and 3 days)\
// `t.copy(tz=tz-local())                               ` &quad; (change to a different time zone)
public fun copy( t : time,
                 year : int = t.year, month : int = t.month, day : int = t.day,
                 hours : int = t.hours, minutes : int = t.minutes, seconds : ddouble = t.seconds,
                 tz : timezone = t.timezone, cal : calendar = t.calendar,
                 ts : timescale = t.timescale ) : time
{
  ts.time( Date(year,month,day), Clock(hours,minutes,seconds), tz, cal)
}

// Copy a `:time` with a new `:date` and optional `:clock` (=`t.clock`), and optionally a new
// timezone (=`t.timezone`) and calendar (=`t.calendar`).
public fun copy-dc( t : time, d : date = t.date, c : clock = t.clock,
                    tz : timezone = t.timezone, cal : calendar = t.calendar,
                    ts : timescale = t.timescale ) : time
{
  ts.time( d, c, tz, cal)
}

// Return the time at the start of the day of time `t`.
public fun start-of-day( t : time ) : time {
  t.copy-dc(t.date,clock0)
}

// Return the time at the start of the month of time `t`.
public fun start-of-month( t : time ) : time {
  t.copy-dc((t.date)(day=1),clock0)
}

// Return the time at the start of the week (Monday) of time `t`.
public fun start-of-week( t : time ) : time {
  val dow = t.weekday
  if (dow==Mon) then t.start-of-day
                else t.copy-dc( (t.date)(day=(t.day - dow.int)+1), clock0 )
}

// Return the time at the start of the year of time `t`.
public fun start-of-year( t : time ) : time {
  t.copy-dc(Date(t.year,1,1),clock0)
}


/*----------------------------------------------------------------------------
  Generic calendar calculations;
  Surprisingly, all of these work for any calendar and do not make
  assumptions about the length of month, the number of months in a year etc.
----------------------------------------------------------------------------*/

// Return the weekday of a given time `t`.
public fun weekday( t : time ) : weekday {
  val days = t.mjd.floor.int
  val dow  = (days+3)%7        // plus 3 since MJD epoch was a wednesday  (note: 0 = sunday, 1 = monday, ..)
  dow.weekday                  // to ISO week day
}


// Return a `:time` as a fractional year.\
// `year-frac(time(2000,7,2)) == fixed(2000.5)`
public fun year-frac( t : time ) : ddouble {
  val dfrac = t.mjd.fraction.double // t.clock.total-seconds.double / 86400.0
  val yfrac = ((t.day-of-year - 1).double + dfrac) / t.days-in-year.double
  timespan(t.year,yfrac)
}

// Return the total months in the year of time `t`.
// For Gregorian calendars this is always 12 but some calendars have a varying number of months per year.
public fun months-in-year( t : time ) : int {
  t.copy-dc( Date(t.year + 1,1,0) ).month
}

// Return the `n`th week day following (and including) time `t`.
// Use `n = 1` for the first week day `wd` following `t`;
// Use `0` for the last occurence of week day `wd` before `t`.\
// `time(2016,10,10).nth-weekday(1,Sun)` &quad; (2016-10-13, first Sunday following October 10, 2016)
// `time(2016,11,16).start-of-month.nth-weekday(0,Wed)` &quad; (2016-10-26, the last Wednesday before 2016-11-01)
public fun nth-weekday( t : time, n : int, wd : weekday ) : time {
  val dow = t.weekday
  val inc = wd - dow
  val wday= t.day + inc + 7*(n - 1)
  t.copy-dc( Date(t.year,t.month,wday) )
}

// Return the first week day following (and including) time `t`.
fun first-weekday( t : time, wd : weekday ) : time {
  t.nth-weekday(1,wd)
}

// Return the last week day before time `t`.\
// `time(2016,11,1).last-weekday(Sun)` &quad;  (2016-10-30, Last Sunday of October 2016)
fun last-weekday( t : time, wd : weekday ) : time {
  t.nth-weekday(0,wd)
}

// Return the `n`th week day since the beginning of the month of time `t`.\
// `time(2016,10,10).nth-weekday-of-month(2,Sun).date` &quad; (2016-10-09, Second Sunday of October 2016)\
// `time(2016,10,10).nth-weekday-of-month(60,Sun).date` &quad; (2017-11-19, 60th Sunday since Octorber 1, 2016)\
public fun nth-weekday-of-month( t : time, n : int, wd : weekday ) : time {
  t.copy-dc(Date(t.year,t.month,1)).nth-weekday(n,wd)
}

// Return the first week day of the month of time `t`.\
// `time(2016,10,10).first-weekday-of-month(Sun)` &quad; (2016-10-02, First Sunday of October 2016)
fun first-weekday-of-month( t : time, wd : weekday ) : time {
  t.nth-weekday-of-month(1,wd)
}

// Return the last week day of the month of time `t`.\
// `time(2016,10,10).last-weekday-of-month(Sun).date` &quad; (2016-10-30, Last Sunday of October 2016)
public fun last-weekday-of-month( t : time, wd : weekday ) : time {
  t.copy-dc(Date(t.year,t.month+1,1)).last-weekday(wd)
}


// ---------------------------------------------------
// The following functions are generic in the calendar
// and are relied upon by other routines for efficiency.
// We may want to optimize this by using specialized
// implementations if the ISO calendar is in use.
// ---------------------------------------------------

// Return the day of the year of time `t` (starting at 1).
public fun day-of-year( t : time ) : int {
  t.calendar.days-until(Date(t.year,1,1),t.date) + 1
}

// Return the total days in the month of time `t`.
public fun days-in-month( t : time ) : int {
  val d = t.date
  t.calendar.days-until(Date(d.year,d.month,1),Date(d.year,d.month+1,1))
}

// Return the total days in the year of time `t`.
public fun days-in-year( t : time) : int {
  t.calendar.days-until(Date(t.year,1,1),Date(t.year+1,1,1))
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
public fun days-until( t1 : time, t2 : time ) : int {
  val d2 = if (t1.calendar == t2.calendar && t1.timezone == t2.timezone)
            then t2.date
            else t2.instant.time(tz=t1.timezone,cal=t1.calendar).date
  t1.calendar.days-until( t1.date, d2 )
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
public fun mjd( t : time ) : ddouble {
  //trace("offset: " + t.tzdelta.show)
  // adjust with the timezone delta so we display the mjd on the date.
  val i = t.instant
  i.mjd( i.timescale, t.tzdelta.timespan )
}
val mjd-epoch-delta = timespan(51544)

/* Return the Julian date ([JD](https://en.wikipedia.org/wiki/Julian_day))
number for a given `:time` `t`. This interprets the Julian date in the calendar of `t` with
the timezone of `t` applied.\
`time(-4713,11,24,12,cal=cal-tt).jd == "0"`\
`time(1972,1,2,tz=tz-fixed(1)).jd == "2441318.5"`\
`time(1972,1,2,tz=tz-fixed(1)).instant.jd(ts-ti).show(9) == "2441318.458333333"`

Takes leap seconds into account when calculating the fraction of the day,
see `mjd` for examples.
*/
public fun jd( t : time ) : ddouble {
  //trace("offset: " + t.tzdelta.show)
  // adjust with the timezone delta so we display the mjd on the date.
  t.mjd() + jd-epoch-delta
}
val jd-epoch-delta  = timespan(2400000.5)


// Return the current time in an optional timezone (=`tz-utc`) and optional calendar (=`cal-iso`).
public fun time-now(ts : timescale, tz : timezone = tz-utc, cal : calendar = cal-iso) : <ndet> time {
  now-in(ts).time(tz,cal)
}

// Return the current time in an optional timezone (=`tz-utc`) and optional calendar (=`cal-iso`).
public fun time-now(tz : timezone = tz-utc, cal : calendar = cal-iso, ts : timescale = utc() ) : <ndet,utc> time {
  ts.time-now(tz,cal)
}

// Return the current time in the local timezone and optional calendar (=`cal-iso`).
public fun local-time-now(ts : timescale, cal : calendar = cal-iso) : ndet time {
  ts.time-now(tz-local(),cal)
}

// Return the current time in the local timezone and optional calendar (=`cal-iso`).
public fun local-time-now(cal : calendar = cal-iso) : <ndet,utc> time {
  time-now(tz-local(),cal)
}

// Convert a `:time` `t` to a new `:time` value in the local time zone
// in an optional calendar (=`t.calendar` by default).
public fun local-time( t : time, cal : calendar = t.calendar ) : ndet time {
  t.time(tz-local(),cal)
}

// Convert an `:instant` to a `:time` value in the local timezone, in an optional calendar (=`cal-iso` by default).
public fun local-time( i : instant, cal : calendar = cal-iso ) : ndet time {
  time(i,tz-local(),cal)
}

// Return the `:time` value for a given date and clock in the local timezone
// interpreted by calendar `cal` (=`cal-iso`). See `instant` for roll-over behaviour.
public fun local-time( ts : timescale,
                       year : int, month : int = 1, day : int = 1,
                       hours : int = 0, minutes : int = 0, secs : int = 0,
                       frac : double = 0.0, cal : calendar = cal-iso ) : ndet time
{
  ts.time(year,month,day,hours,minutes,secs,frac,tz-local(),cal)
}
// Return the `:time` value for a given date and clock in the local timezone
// interpreted by calendar `cal` (=`cal-iso`). See `instant` for roll-over behaviour.
public fun local-time( year : int, month : int = 1, day : int = 1,
                       hours : int = 0, minutes : int = 0, secs : int = 0,
                       frac : double = 0.0, cal : calendar = cal-iso, ts : timescale = utc() ) : <ndet,utc> time
{
  ts.local-time(year,month,day,hours,minutes,secs,frac,cal)
}


// Return the `:time` value for a given `:date` and `:clock` (=`clock0`) in the local timezone
// interpreted by calendar `cal` (=`cal-iso`)
public fun local-time( ts : timescale, d : date, c : clock = clock0, cal : calendar = cal-iso ) : ndet time
{
  ts.time(d,c,tz-local(),cal)
}

// Return the `:time` value for a given `:date` and `:clock` (=`clock0`) in the local timezone
// interpreted by calendar `cal` (=`cal-iso`)
public fun local-time( d : date, c : clock = clock0, cal : calendar = cal-iso, ts : timescale = utc() ) : <ndet,utc> time
{
  ts.time(d,c,tz-local(),cal)
}
