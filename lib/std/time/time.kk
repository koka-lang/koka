/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Time represents instants as human readable calendar time.

  A `:time` represents an `:instant` in time in a given `:calendar` and `:timezone`
  as a "human" calendar time. Years, months, days etc. are always 1-based.  
*/
module std/time/time

import std/fixed
import std/time/timespan
import std/time/duration
import std/time/instant
import std/time/utc
import std/time/date
import std/time/calendar


// Represents an instant in time for a certain calendar and timezone.
abstract struct time (
  // The year.
  public  date    : date,
  public  clock   : clock,
  public  calendar: calendar,
  public  tzdelta : duration = duration0,
  public  tzabbrv : string   = "",
  public  timezone: timezone = tz-utc,
  public  instant : instant
)

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
public fun seconds( t : time ) : fixed {
  t.clock.seconds
}

// Compare two `:time`s. Compares the actual instants in time
// and can thus compare across calendars and timezones.\
// `time(2001,7,2,tz=tz-fixed("GMT+1",duration(3600))) > time(2001,7,1,23,30,0)` &quad; (False!)`
public fun compare( t1 : time, t2 : time ) : order {
  compare(t1.instant,t2.instant)
}  

public fun (<)( i : time, j : time )  : bool { compare(i,j) == Lt }
public fun (<=)( i : time, j : time ) : bool { compare(i,j) != Gt }
public fun (>)( i : time, j : time )  : bool { compare(i,j) == Gt }
public fun (>=)( i : time, j : time ) : bool { compare(i,j) != Lt }
public fun (==)( i : time, j : time ) : bool { compare(i,j) == Eq }
public fun (!=)( i : time, j : time ) : bool { compare(i,j) != Eq }



// Round a time to a specified second precision.
public fun round-to-prec( t : time, prec : int ) : time {
  if (prec.neg?) then return t
  val c    = t.clock
  val secs = c.seconds.round-to-prec(prec)
  val ri   = t.instant.use(t.calendar.timescale).round-to-prec(prec)
  if (secs.trunc == t.seconds.trunc) then {
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
  tp.date.show 
   + "T" + tp.clock.show(prec)
   + show-tzdelta(tp.tzdelta ) 
   + (if (tp.tzabbrv.empty?) then "" else " (" + tp.tzabbrv + ")") 
   + (if (tp.calendar.name.empty?) then "" else " " + tp.calendar.name)
}

// Show a time zone delta. 
// Optional `utc` for displaying a zero timezone delta (=`"Z"`).
// Optional `hmsep` for the hour-minute separator (=`":"`).
// Optional `hrwidth` to give the minimal width of the hour field (=`2`).
public fun show-tzdelta(delta : duration, utc : string = "Z", hmsep : string = ":", hrwidth : int = 2) : string {
  val dt = delta.seconds
  if (dt.zero?) return utc
  val (fmins,secs) = divmod(dt.abs,fixed(60),6) // millisecond precision
  val mins = fmins.trunc.int
  val tz   = (if (dt.neg?) then "-" else "+") + (mins/60).show0(hrwidth) + hmsep + (mins%60).show0
  val tzs  = if (secs.zero?) then "" else ":" + show( timestamp(secs), 3 )
  tz + tzs
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
public fun time( i : instant, tz : timezone = tz-utc, cal : calendar = cal-iso ) : time {
  val (d,c,tzdelta,tzabbrv) = instant-dc(i,tz,cal)
  Time(d,c,cal,tzdelta,tzabbrv,tz,i)
}

// Convert a `:time` `t` to a new `:time` value in a potentially different 
// timezone `tz` (=`t.timezone` by default) and calendar (=`t.calendar` by default).
public fun time( t : time, tz : timezone = t.timezone, cal : calendar = t.calendar ) : time {
  time(t.instant,tz,cal)
}

// Return the `:time` value for a given date and clock in a timezone `tz` (=`tz-utc` by default) 
// interpreted by calendar `cal` (=`cal-iso`). See `instant-at` for roll-over behaviour.
public fun time( year : int, month : int = 1, day : int = 1, 
                 hours : int = 0, minutes : int = 0, secs : int = 0, 
                 frac : double = 0.0, tz : timezone = tz-utc, cal : calendar = cal-iso ) : time 
{
  time(instant-at(year,month,day,hours,minutes,secs,frac,tz,cal),tz,cal)
}


// Return the `:time` value for a given `:date` and `:clock` (=`clock0`) in a timezone `tz` (=`tz-utc` by default) 
// interpreted by calendar `cal` (=`cal-iso`)
public fun time( d : date, c : clock = clock0, tz : timezone = tz-utc, cal : calendar = cal-iso ) : time 
{
  time(instant-at(d,c,tz,cal),tz,cal)
}

// Return the `:time` value for a given `:weekdate` and `:clock` (=`clock0`) in a timezone `tz` (=`tz-utc` by default) 
// interpreted by calendar `cal` (=`cal-iso`).
// It is also possible to pass the weekday that determines if 
// a week falls in the current year or not (which is Thursday for the ISO definition).\
// `time(Weekdate(2004,53,Sat).date.show == "2005-01-01"`\
// `time(Weekdate(2005,52,Sat).date.show == "2005-12-31"`\
// `time(Weekdate(2009,1,Wed)).date.show == "2008-12-31"`
public fun time( wd : weekdate, c : clock = clock0, 
                  tz : timezone = tz-utc, cal : calendar = cal-iso, 
                  weekday-in-year : weekday = Thu ) : time 
{
  time(instant-at(wd,c,tz,cal,weekday-in-year),tz,cal)
}

// Add a date (years, months, days) and optional clock to a time. Takes leap years etc. into account.
public fun add-date( t : time, d : date, c : clock = clock0 ) : time {
  time( t.date + d, t.clock + c, t.timezone, t.calendar )
}

// Add a clock (hours, minutes, seconds) to a time. Takes leap years etc. into account.
public fun add-clock( t : time, c : clock ) : time {
  time( t.date, t.clock + c, t.timezone, t.calendar )
}

// Add  a specified number of days to a calendar time.
public fun add-days( t : time, days : int, c : clock = clock0 ) : time {
  t.add-date( Date(t.year, t.month, t.day + days), c )
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


// Copy a `:time` with optionally changing any of its attributes.\
// `t.copy(month=1,day=1,hours=0,minutes=0,seconds=zero)` &quad; (copy to start of the year)\
// `t.copy(month=t.month+2,day=t.day+3)                 ` &quad; (add 2 months and 3 days)\
// `t.copy(tz=tz-local())                               ` &quad; (change to a different time zone)
public fun copy( t : time, 
                 year : int = t.year, month : int = t.month, day : int = t.day, 
                 hours : int = t.hours, minutes : int = t.minutes, seconds : fixed = t.seconds, 
                 tz : timezone = t.timezone, cal : calendar = t.calendar ) : time 
{
  time( Date(year,month,day), Clock(hours,minutes,seconds), tz, cal )
}

// Copy a `:time` with a new `:date` and optional `:clock` (=`t.clock`), and optionally a new
// timezone (=`t.timezone`) and calendar (=`t.calendar`).
public fun copy-dc( t : time, d : date = t.date, c : clock = t.clock,
                    tz : timezone = t.timezone, cal : calendar = t.calendar ) : time 
{
  time( d, c, tz, cal )
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

// Return the [ISO week date](https://en.wikipedia.org/wiki/ISO_week_date) for a time `t`. 
// It is also possible to pass the weekday that determines if 
// a week falls in the current year or not (which is Thursday for the ISO definition). 
// Note that a week can fall in the year before or the year after. For example:\
// `time(2005,1,1).weekdate.show   == "2004-W53-6"`\
// `time(2005,12,31).weekdate.show == "2005-W52-6"`\
// `time(2008,12,31).weekdate.show == "2009-W01-3"`
public fun weekdate( t : time, weekday-in-year : weekday = Thu ) : weekdate {  
  val dow  = t.weekday
  val sun  = (t.day-of-year - dow.int) 
  val wday = sun + (weekday-in-year.int % 7)
  if (wday <= 0) then {
    // last week of previous year 
    val wk = (t.copy(month=1,day=wday).day-of-year + 6)/7
    Weekdate(t.year - 1, wk, dow)
  }
  elif (wday > t.days-in-year) then 
    // first week in the next year
    Weekdate(t.year + 1, 1, dow)
  else 
    // week in this year
    Weekdate(t.year, (wday+6)/7, dow)
}

// Return the weekday of a given time `t`.
public fun weekday( t : time ) : weekday {
  val days = t.mjd.floor.int
  val dow  = (days+3)%7        // plus 3 since MJD epoch was a wednesday  (note: 0 = sunday, 1 = monday, ..)
  dow.weekday                  // to ISO week day 
}


// Return a `:time` as a fractional year.\
// `year-frac(time(2000,7,2)) == fixed(2000.5)`
public fun year-frac( t : time ) : fixed {
  val dfrac = t.clock.total-seconds.double / 86400.0
  val yfrac = ((t.day-of-year.double - 1.0) + dfrac) / t.days-in-year.double
  fixed-frac(t.year,yfrac)
}

// Return the total months in the year of time `t`.
// For Gregorian calendars this is always 12 but some calendars have a varying number of months per year.
public fun months-in-year( t : time ) : int {
  t.copy-dc( Date(t.year + 1,1,0) ).month
}

// Return the total number of ISO weeks in the year of time `t` (= 52 or 53).
public fun weeks-in-year( t : time, weekday-in-year : weekday = Thu ) : int {
  t.copy-dc( Date(t.year+1,1,1 - weekday-in-year.int) ).weekdate.week
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
// The following 4 functions are generic in the calendar
// and are relied upon by other routines for efficiency.
// We may want to optimize this by using specialized
// implementations if the ISO calendar is in use.
// ---------------------------------------------------

// Return the day of the year of time `t` (starting at 1).
public fun day-of-year( t : time ) : int {
  t.calendar.day-diff(t.date,Date(t.year,1,1)) + 1
}

// Return the total days in the month of time `t`.
public fun days-in-month( t : time ) : int {
  val d = t.date
  t.calendar.day-diff(Date(d.year,d.month+1,1),Date(d.year,d.month,1))  
}

// Return the total days in the year of time `t`.
public fun days-in-year( t : time) : int {
  t.calendar.day-diff(Date(t.year+1,1,1),Date(t.year,1,1))  
}

// Return the modified Julian date ([MJD](https://en.wikipedia.org/wiki/Julian_day#Variants)) 
// number for a given `:time`. This interprets the Julian date in that calendar date with
// the timezone applied.\
// `time(1972,1,2,tz=tz-gmt(1)).mjd == "41318"`\
// `time(1972,1,2,tz=tz-gmt(1)).instant.mjd(ts-utc).show == "41317.958333335"` 
public fun mjd( t : time ) : fixed {
  //trace("offset: " + t.tzdelta.show)
  // adjust with the timezone delta so we display the mjd on the date.
  t.instant.mjd( t.calendar.timescale, delta=t.tzdelta.timespan )
}

/*
// Is this a Gregorian leap year?
public fun gregorian-leap-year?( year : int ) : bool {
  ((year%4)==0 && ((year%100)!=0 || (year%400)==0))
}

// The number of days in the given year (in a Gregorian calendar).
fun gregorian-days-in-year( year : int ) : int {
  if (year.gregorian-leap-year?) then 366 else 365
}

// Number of days in a certain month (in a Gregorian calendar).
fun gregorian-days-in-month( year: int, month : int ) : int {
  if (month==2 && year.gregorian-leap-year?) then return 29
  month-counts[(month - 1)%12].maybe(0)  
}
val month-counts : list<int> = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

// Return the day of the year (starting at 1) (in a Gregorian calendar).
fun gregorian-day-of-year( d : date ) : int {
  val n = day-counts[(d.month - 1)%12].maybe(0) + d.day
  if (d.month > 2 && d.year.leap-year?) then n+1 else n
}
val day-counts : list<int> = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]

fun gregorian-day-of-week( d : date ) : int {
  // Zeller's algorithm <https://en.wikipedia.org/wiki/Zeller%27s_congruence>
  // Just for Gregorian calendar
  val yr= if (d.month < 3) then d.year - 1 else d.year
  val m = if (d.month < 3) then d.month + 12 else d.month
  val c = yr/100
  val y = yr%100
  val w = ((d.day + ((13*(m+1))/5) + y + (y/4) + (c/4)) - 2*c)%7  
  (if (w > 1) then (w - 1) else (w + 6)) // adjust for ISO week number
}
*/



// Return the current time in an optional timezone (=`tz-utc`) and optinional calendar (=`cal-iso`).
public fun time-now(tz : timezone = tz-utc, cal : calendar = cal-iso) : ndet time {
  now().time(tz,cal)
}

// Return the current time in the local timezone and optinional calendar (=`cal-iso`).
public fun local-time-now(cal : calendar = cal-iso) : ndet time {
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
// interpreted by calendar `cal` (=`cal-iso`). See `instant-at` for roll-over behaviour.
public fun local-time( year : int, month : int = 1, day : int = 1, 
                       hours : int = 0, minutes : int = 0, secs : int = 0, 
                       frac : double = 0.0, cal : calendar = cal-iso ) : ndet time 
{
  time(year,month,day,hours,minutes,secs,frac,tz-local(),cal)
}


// Return the `:time` value for a given `:date` and `:clock` (=`clock0`) in the local timezone
// interpreted by calendar `cal` (=`cal-iso`)
public fun local-time( d : date, c : clock = clock0, cal : calendar = cal-iso ) : ndet time 
{
  time(d,c,tz-local(),cal)
}

// Return the `:time` value for a given `:weekdate` and `:clock` (=`clock0`) in the local timezone `tz`
// interpreted by calendar `cal` (=`cal-iso`).
// It is also possible to pass the weekday that determines if 
// a week falls in the current year or not (which is Thursday for the ISO definition).
public fun local-time( wd : weekdate, c : clock = clock0, 
                        cal : calendar = cal-iso, 
                        weekday-in-year : weekday = Thu ) : ndet time {
  time(wd,c,tz-local(),cal,weekday-in-year)
}
