/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Calendars and time.

  A Calendar determines how a `:date` and `:clock` relates to an `:instant` in time
  with respect to a `:timezone`. These functions turn `:instant`'s into "human" dates. 
  Years, months, days etc. are always 1-based.  
*/
module std/time/calendar

import std/fixed
import std/time/timespan
import std/time/duration
import std/time/instant
import std/time/utc
import std/time/date

extern include {
  js file "calendar-inline.js"
}

// A Calendar determines how a `:date` and `:clock` relates to an `:instant` in time. 
public struct calendar(
  name      : string,                 
  timescale : timescale,
  private instant-to-dc : (i:instant,tzdelta:duration) -> (date,clock),
  private dc-to-instant : (date,clock) -> instant
)


// Represents an instant in time for a certain calendar and timezone.
abstract struct time (
  // The year.
  public  year    : int,
  public  month   : int,
  public  day     : int, 
  public  hours   : int,
  public  minutes : int,
  public  seconds : fixed,
  public  calendar: calendar,
  public  tzdelta : duration = duration0,
  public  tzabbrv : string   = "",
  public  timezone: timezone = tz-utc,
  public  instant : instant
)

// Return the year, month, and day of a `:time`.
public fun date( t : time ) : date {
  Date(t.year,t.month,t.day)
}

// Return the hours, minutes, seconds, and fraction of the second of a `:time`.
public fun clock( t : time ) : clock {
  Clock(t.hours,t.minutes,t.seconds)
}


// Round a time to a specified second precision.
public fun round-to-prec( t : time, prec : int ) : time {
  val secs = t.seconds.round-to-prec(prec)
  val ri   = t.instant.use(t.calendar.timescale).round-to-prec(prec)
  if (secs.trunc == t.seconds.trunc) then {
    // whole seconds stay the same, just update with the rounded seconds and instant
    t(seconds = secs, instant = ri)
  } 
  else {
    // otherwise recalculate the time from the rounded instant
    ri.time(t.timezone,t.calendar)
  } 
}

// Show a `:time` in [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) format.
public fun show(t0 : time, prec : int = 9 ) : string {
  val t = t0.round-to-prec(prec)
  t.date.show 
   + "T" + t.clock.show(prec)
   + show-tzdelta(t.tzdelta ) 
   + (if (t.tzabbrv.empty?) then "" else " (" + t.tzabbrv + ")") 
   + (if (t.calendar.name.empty?) then "" else " " + t.calendar.name)
}

// Show a time zone delta. 
// Optional `utc` for displaying a zero timezone offset (=`"Z"`).
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
  (tz.name == "UTC")
}

// Create a time zone with a fixed delta in seconds from UTC.\
// Takes a `name`, as `delta` from UTC, and optional time zone abbreviation (=`name`).
// For example, for a fixed time zone``+07:00``, use `tz-fixed( "GMT+7", duration(7 * 3600) )`
public fun tz-fixed( name : string, delta : duration, abbrv : string = "" ) : timezone {
  Timezone( 
    name,            
    fun(i) { (delta,abbrv) }, 
    fun(i) { if (delta.zero?) then Just(i) else Nothing }
  )
}

// The standard UTC time zone with a 0 delta.
public val tz-utc : timezone = tz-fixed("UTC",duration0,"")

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
public val cal-iso : calendar = iso-calendar( ts-utc, "" )

// The [Julian calendar](https://en.wikipedia.org/wiki/Julian_calendar).
public val cal-julian = solar-calendar(
  "JC",
  julian-date-from-days,
  julian-days-from-date
)

// The (proleptic) [Gregorian calendar](https://en.wikipedia.org/wiki/Gregorian_calendar).
// Just like the ISO calendar except that the year 0 does not exist, i.e. after 1BC (=`~1`)
// we have 1AD (=`1`).
public val cal-gregorian = solar-calendar(
  "GC",
  gregorian-date-from-days,
  gregorian-days-from-date
)

// The combined Julian / Gregorian calendar, using the Julian calendar for dates 
// before 1582-10-15 and the Gregorian calendar otherwise. It is possible to 
// specify a different switch date using the `julian-gregorian` function.
public val cal-jg = julian-gregorian()

// The TAI Calendar. This is a standard ISO 8601 calender using
// TAI time where every day is exactly 86400 SI seconds (unlike
// standard UTC time which can insert leap seconds).
public val cal-tai = iso-calendar( ts-tai )

// The GPS calendar is always `TAI - 19` but with epoch 1980-01-06Z
public val cal-gps = iso-calendar( ts-gps ) //(ts-gps.to-tai)(timestamp(fixed(~19))).seconds )


// Create a standard ISO calendar using a particular time scale
// and calendar `name` (=`ts.name`).
public fun iso-calendar( ts : timescale, name : string = ts.name ) : calendar {
  solar-calendar(
    name,
    iso-date-from-days,
    iso-days-from-date,
    ts
  )
}

/*----------------------------------------------------------------------------
  Solar calendars
----------------------------------------------------------------------------*/

// Used for solar calendars
val secs-per-day = 86400

// Create a solar calendar given a calendar name `name`, 
// a function to calculate a date given a number of days since the `epoch`,
// a function to calculate days since the `epoch` for a given date,
// and an optional time scale to be used (=`ts-utc`) and an optional offset from 
// the standard `epoch` (=`duration0`).
public fun solar-calendar(
              name        : string,
              date-from-days  : (days:int) -> date,
              days-from-date  : date -> int,
              ts   : timescale = ts-utc
            ) : calendar
{
  val epoch-shift = ts.timescale-y2k.timespan(ts)  
  fun instant-to-dc(i : instant, tzdelta : duration) { 
    solar-timestamp-to-dc( (i.timestamp(ts) - epoch-shift) + tzdelta.timespan, date-from-days ) 
  }
  fun dc-to-instant(d : date, c : clock) { 
    unsafe-instant-at( solar-dc-to-timestamp( d, c, days-from-date ) + epoch-shift, ts ) 
  }
  Calendar(name,ts,instant-to-dc,dc-to-instant)
}


fun solar-timestamp-to-dc( t : timestamp, date-from-days: (int) -> date ) : (date,clock) {
  // get whole seconds and fraction, such that fraction is always positive (into the day)
  val (nlsecs,nlfrac,leap)  = t.calendar-seconds  
  // get the date from the whole days; use euclidean division such that daysecs is always positive                                           
  val (days,daysecs)   = divmod( nlsecs, secs-per-day)
  val date             = date-from-days(days)
  // Finally set the clock taking care of leap seconds
  val (hours,minsecs)  = divmod(daysecs,3600)
  val (mins,xsecs)     = divmod(minsecs,60)
  val tsecs            = fixed-frac(xsecs + leap, nlfrac)  // adjust for leap seconds
  (date, Clock(hours,mins,tsecs))
}

fun solar-dc-to-timestamp( d : date, c : clock, days-from-date: (date) -> int ) : timestamp {
  // Get seconds and leap seconds (if the clock has seconds > 59)
  val secs = c.seconds.trunc.int
  val frac = c.seconds.fraction.double
  val (fsecs,leaps)  = if (secs < 60) then (secs,0) else ( 59, secs - 59 )
  val (xdays,xsecs) = divmod( ((((c.hours*60) + c.minutes)*60) + fsecs), secs-per-day )
  val sdays = days-from-date( d(day = d.day + xdays) )
  timestamp( sdays*secs-per-day + xsecs, frac ).add-leap-seconds(leaps.double)  
}


// Create a Julian-Gregorian calendar which uses Julian dates before the optional `switch`
// (Gregorian) date (=`Date(1582,10,15`)) and Gr egorian dates otherwise.
public fun julian-gregorian( switch : date = Date(1582,10,15) ) : calendar {
  val switch-days = iso-days-from-date(switch) //+ skip
  solar-calendar( "JG", 
    fun(days) { julgreg-date-from-days(days,switch-days) },
    fun(d)    { julgreg-days-from-date(d,switch) }
  )
}


/*----------------------------------------------------------------------------
  Time and instant conversion using a calendar
----------------------------------------------------------------------------*/

// Return the instant in time for a given date.
fun instantz( d : date, c : clock, cal : calendar = cal-iso ) : instant {
  (cal.dc-to-instant)( d, c )   
}


// Return the instant in time for a given `:date` and `:clock` interpreted by 
// calendar `cal` (=`cal-iso`) in a timezone `tz` (=`tz-utc` by default).
public fun instant-at( d : date, c : clock, tz : timezone = tz-utc, cal : calendar = cal-iso ) : instant 
{
  // the year/month/day is interpreted as if in UTC first.
  val i = instantz(d,c,cal)
  if (tz.tz-utc?) return i
  // we need to adjust according to timezone 
  match((tz.utc-inverse)(i)) {
    Just(inv) -> inv
    Nothing -> {
      // no explicit inverse,
      // do a double pass to accommodate jumping over a DST boundary.
      // we calculate directly on UTC timespan's as to ignore any possible
      // leap seconds during the timezone offset change. 
      // (for example: instant-at(1979,1,1,3,29,59,tz=timezone("Asia/Tehran")).time.show == "1978-12-31T23:59:59Z"))
      val utci = i.timestamp(ts-utc)
      val tzd1 = (tz.utc-delta)(i).fst
      val tzi1 = unsafe-instant-at(utci - tzd1.timespan, ts-utc)
      val tzd2 = (tz.utc-delta)(tzi1).fst
      val tzi2 = unsafe-instant-at(utci - tzd2.timespan, ts-utc)
      tzi2     
    }
  }
}


// Return a `:time` for a given instant.
fun timez( i : instant, tzdelta : duration = duration0, tzabbrv : string = "", tz : timezone = tz-utc, cal : calendar = cal-iso ) : time {
  val (d,c) = (cal.instant-to-dc)( i, tzdelta )
  Time(d.year,d.month,d.day,c.hours,c.minutes,c.seconds,cal,tzdelta,tzabbrv,tz,i)
}


// Convert an `:instant` to a `:time` value in a given timezone `tz` (=`tz-utc` by default)
// and calendar (=`cal-iso` by default).
public fun time( i : instant, tz : timezone = tz-utc, cal : calendar = cal-iso ) : time {
  if (tz.tz-utc?) then timez(i,duration0,"",tz,cal) else {
    val (tzdelta,tzabbrv) = (tz.utc-delta)(i)
    timez( i, tzdelta, tzabbrv, tz, cal )
  }
}


/*----------------------------------------------------------------------------
  Convenience functions over time
----------------------------------------------------------------------------*/

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
  time( Date(t.year + d.year, t.month + d.month, t.day + d.day), t.clock + c, t.timezone, t.calendar )
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
// `t.copy(month=1,day=1,hours=0,minutes=0,seconds=zero)` (set to start of the year)\
// `t.copy(month=t.month+2,day=t.day+3)`  (add 2 months and 3 days)\
// `t.copy(tz=tz-local())` (change to a different time zone)
public fun copy( t : time, 
                 year : int = t.year, month : int = t.month, day : int = t.day, 
                 hours : int = t.hours, minutes : int = t.minutes, seconds : fixed = t.seconds, 
                 tz : timezone = t.timezone, cal : calendar = t.calendar ) : time 
{
  time( Date(year,month,day),Clock(hours,minutes,seconds),tz,cal )
}

// Return the time at the start of the day of time `t`.
public fun start-of-day( t : time ) : time {
  t.copy(hours=0,minutes=0,seconds=zero)
}

// Return the time at the start of the month of time `t`.
public fun start-of-month( t : time ) : time {
  t.copy(day=1,hours=0,minutes=0,seconds=zero)
}

// Return the time at the start of the week (Monday) of time `t`.
public fun start-of-week( t : time ) : time {
  val dow = t.weekday
  if (dow==Mon) then t.start-of-day else t.copy( day=(t.day - dow.int)+1, hours=0,minutes=0,seconds=zero )
}

// Return the time at the start of the year of time `t`.
public fun start-of-year( t : time ) : time {
  t.copy(month=1,day=1,hours=0,minutes=0,seconds=zero)
}

// Return the [ISO week date](https://en.wikipedia.org/wiki/ISO_week_date) for a time `t`. 
// It is also possible to pass the weekday that determines if 
// a week falls in the current year or not (which is Thursday for the ISO definition). 
// Note that a week can fall in the year before or the year after. For example:\
// `time(2005,1,1).weekdate.show   == "2004-W53-6"`\
// `time(2005,12,31).weekdate.show == "2005-W52-6"`\
// `time(2008,12,31).weekdate.show == "2009-W01-3"`
public fun weekdate( t : time, weekday-in-year : weekday = Thu ) : weekdate {
  t.date.weekdate( t.calendar, weekday-in-year )
}

// Return the day of the year of time `t` (starting at 1).
public fun day-of-year( t : time ) : int {
  t.date.day-of-year( t.calendar )
}

// Return the `n`th week day following (and including) time `t`.
// Use `n = 1` for the first week day `wd` following `t`; 
// Use `0` for the last occurence of week day `wd` before `t`.\
// `time(2016,11,16).start-of-month.nth-weekday(0,Wed)` (The last wednesday before 2016-11-01)
public fun nth-weekday( t : time, n : int, wd : weekday ) : time {
  nth-weekday( n, wd, t.date, t.calendar ).time( t.clock, t.timezone, t.calendar )
}

// Return the first week day following (and including) time `t`.
fun first-weekday( t : time, wd : weekday ) : time {
  t.nth-weekday(1,wd)
}

// Return the last week day before time `t`.\
// `time(2016,11,1).last-weekday(Sun)` (2016-10-30, Last Sunday of October 2016)
fun last-weekday( t : time, wd : weekday ) : time {
  t.nth-weekday(0,wd)
}

// Return the `n`th week day since the beginning of the month of time `t`.\
// `time(2016,10,10).nth-weekday-of-month(2,Sun).date` (2016-10-09, Second Sunday of October 2016)\
// `time(2016,10,10).nth-weekday-of-month(60,Sun).date` (2017-11-19, 60th Sunday since Octorber 1, 2016)
public fun nth-weekday-of-month( t : time, n : int, wd : weekday ) : time {
  t.copy(day=1).nth-weekday(n,wd)
}

// Return the first week day of the month of time `t`.\
// `time(2016,10,10).first-weekday-of-month(Sun)` (2016-10-02, First Sunday of October 2016)
fun first-weekday-of-month( t : time, wd : weekday ) : time {
  t.nth-weekday-of-month(1,wd)
}

// Return the last week day of the month of time `t`.\
// `time(2016,10,10).last-weekday-of-month(Sun).date` (2016-10-30, Last Sunday of October 2016)
public fun last-weekday-of-month( t : time, wd : weekday ) : time {
  t.copy(month=t.month+1, day=1).last-weekday(wd)
}

// Return the total days in the month of time `t`.
public fun days-in-month( t : time ) : int {
  days-in-month(t.year,t.month,t.calendar)
}

// Return the total days in the year of time `t`.
public fun days-in-year( t : time) : int {
  days-in-year(t.year,t.calendar)  
}



/*----------------------------------------------------------------------------
  Generic calendar calculations;
  Surprisingly, all of these work for any calendar and do not make 
  assumptions about the length of month, the number of months in a year etc.
----------------------------------------------------------------------------*/

// Return the date of the `n`th week day `wd` starting from (and including) the specified date,
// in an optional calendar (=`cal-iso`).
// Use `n = 1` for the first week day `wd` in the month; Use `0` for the last occurence of week day `wd` in the
// previous month. See also `first-weekday` and `last-weekday`.
fun nth-weekday( n : int, wd : weekday,  d : date, cal : calendar ) : date {
  val dow = weekday(d,cal)
  val inc = wd - dow
  val day = d.day + inc + 7*(n - 1)
  if (1 <= day && day <= 28 && 1 <= d.month && d.month <= 12) then {
    // test assumes that a month has at least 28 days, and a year at least 12 months...
    Date(d.year,d.month,day) 
  }
  else {
    // may roll over into subsequent or previous months
    time(d.year,d.month,day,cal=cal).date
  }
}

// Return the [ISO week date](https://en.wikipedia.org/wiki/ISO_week_date) for a date. 
// Can pass a calendar `cal` (=`cal-iso`) to
// interpret the date. It is also possible to pass the weekday that determines if 
// a week falls in the current year or not (which is Thursday for the ISO definition). 
// Note that a week can fall in the year before or the year after. For example:\
// `weekdate(Date(2005,1,1)).show   == "2004-W53-6"`\
// `weekdate(Date(2005,12,31)).show == "2005-W52-6"`\
// `weekdate(Date(2008,12,31)).show == "2009-W01-3"`
fun weekdate( d : date, cal : calendar, weekday-in-year : weekday ) : weekdate {
  val dow  = d.weekday(cal)
  val sun  = (d.day-of-year(cal) - dow.int) 
  val wday = sun + (weekday-in-year.int % 7)
  if (wday <= 0) then {
    // last week of previous year 
    val wk = (time(d.year,1,wday).date.day-of-year(cal) + 6)/7
    Weekdate(d.year - 1, wk, dow)
  }
  elif (wday > days-in-year(d.year,cal)) then 
    // first week in the next year
    Weekdate(d.year + 1, 1, dow)
  else 
    // week in this year
    Weekdate(d.year, (wday+6)/7, dow)
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
  val days = i.mjd(cal.timescale).floor.int
  val dow  = (days+3)%7        // plus 3 since MJD epoch was a wednesday  (note: 0 = sunday, 1 = monday, ..)
  dow.weekday                  // to ISO week day 
}

// Return the weekday of a given time `t`.
public fun weekday( t : time ) : weekday {
  val days = t.mjd.floor.int
  val dow  = (days+3)%7        // plus 3 since MJD epoch was a wednesday  (note: 0 = sunday, 1 = monday, ..)
  dow.weekday                  // to ISO week day 
}


// Return the day of the year for a given date in calendar `cal` (=`cal-iso`).
// Starts at 1 for the first day of the year.
fun day-of-year( d : date, cal : calendar ) : int {
  day-diff(d.year,d.month,d.day,d.year,1,1,cal) + 1
}

// Return the days in a given month in calendar `cal` (=`cal-iso`).
fun days-in-month( year : int, month : int , cal : calendar  ) : int {
  day-diff(year,month+1,1,year,month,1,cal)  
}

// Return the days in a given year and calendar (=`cal-iso`).
fun days-in-year( year : int, cal : calendar ) : int {
  day-diff(year+1,1,1,year,1,1,cal)  
}

fun day-diff( y1 : int, m1 : int, d1 : int, y2 : int, m2 : int, d2 : int, cal : calendar ) : int {
  val i1 = instant-at(y1,m1,d1,cal=cal)
  val i2 = instant-at(y2,m2,d2,cal=cal)
  val ts = cal.timescale
  (i1.mjd(ts) - i2.mjd(ts)).double.floor.int    
}

// Return the modified Julian date ([MJD](https://en.wikipedia.org/wiki/Julian_day#Variants)) 
// number for a given `:time`.
public fun mjd( t : time ) : fixed {
  //trace("offset: " + t.tzdelta.show)
  // adjust with the timezone offset so we display the mjd on the date.
  (t.instant + t.tzdelta).mjd(t.calendar.timescale)
}

// Return a `:time` as a fractional year.\
// `year-frac(time(2000,7,2)) == fixed(2000.5)`
public fun year-frac( t : time ) : fixed {
  val dfrac = t.clock.total-seconds.double / secs-per-day.double
  val yfrac = ((t.date.day-of-year(t.calendar).double - 1.0) + dfrac) / t.year.days-in-year(t.calendar).double
  fixed-frac(t.year,yfrac)
}


/*----------------------------------------------------------------------------
  ISO calendar, Proleptic Gregorian calendar, and Julian calendar.
----------------------------------------------------------------------------*/

val iso-epoch-shift = 730425   // shift internal epoch from 0000-03-01 to 2000-01-01
val iso-days-in-era = 146097   // 365*400 + 100 - 3
val iso-years-in-era= 400

// Convert days since `epoch` to a date in the [ISO8601] calendar,
// a proleptic Gregorian calendar where year 0 is 1BC.
//
// [ISO8601]: https://en.wikipedia.org/wiki/ISO_8601
fun iso-date-from-days( days : int ) : date {
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
fun iso-days-from-datex( year : int, month : int, day : int ) : int {
  val y = if (month <= 2) then year.dec else year   // to internal year starting in Feb
  val (era,yoe) = divmod(y,iso-years-in-era)             // year of era: 0<= yoe < 400
  val mdoy = (((153 * (month + (if (month > 2) then ~3 else 9))) + 2)/5) 
  val doe  = (((yoe*365) + (yoe/4)) - (yoe/100)) + mdoy + day.dec
  ((iso-days-in-era * era) + doe) - iso-epoch-shift
}

// Return days from a year, month, and day in the [ISO8601] calendar.
// Months and days can be any value.
fun iso-days-from-date( d : date ) : int {
  val (xera,doe)  = divmod(d.day.dec, iso-days-in-era)
  val (xyear,moy) = divmod(d.month.dec, 12)
  iso-days-from-datex( d.year + (xera * iso-years-in-era) + xyear, moy.inc, doe.inc )
}

// Return days from a date in the proleptic Gregorian calendar.
fun gregorian-days-from-date( d : date ) : int {
  if (d.year <= 0) 
    then iso-days-from-date(d(year = d.year + 1)) 
    else iso-days-from-date(d)
}

// Return a date from a number of days in the proleptic Gregorian calendar.
fun gregorian-date-from-days( days : int ) : date {
  val d = iso-date-from-days(days)
  if (d.year<=0) then d(year = d.year - 1) else d
}

val julian-days-in-era  = 1461
val julian-years-in-era = 4
val julian-epoch-shift  = 730427 // Shift from 2000-01-01 to 0000-03-01

// Return a date from a number of days in the Julian calendar.
fun julian-date-from-days( days : int ) : date {  
  val z = days + julian-epoch-shift
  val (era,doe) = divmod(z,julian-days-in-era)
  val (yoe,doy) = divmod(doe,365)
  val mp    = (doy*5 + 2) / 153
  val day   = (doy - ((153*mp + 2)/5)) + 1
  val month = mp + (if (mp < 10) then 3 else ~9)
  val year  = yoe + (julian-years-in-era * era) + (if (month <= 2) then 1 else 0)
  val yyear = if (year<=0) then year.dec else year
  Date(yyear,month,day)
}

fun julian-days-from-datex( year : int, month : int, day : int ) : int {
  val y = if (month <= 2) then year.dec else year
  val (era,yoe) = divmod(y,julian-years-in-era)
  val doy = ((153*(month + (if (month>2) then ~3 else 9)) + 2)/5 + day) - 1
  val doe = yoe*365 + doy
  ((era * julian-days-in-era) + doe) - julian-epoch-shift
}  

// Return the days from a date in the Julian calendar.
fun julian-days-from-date( d : date ) : int {
  val (xera,doe)  = divmod(d.day.dec, julian-days-in-era)
  val (xyear,moy) = divmod(d.month.dec, 12)
  val yyear = if (d.year<=0) then d.year.inc else d.year
  julian-days-from-datex( yyear + (xera * julian-years-in-era) + xyear, moy.inc, doe.inc )    
}


fun julgreg-days-from-date( d : date, switch : date  ) : int {
  if (compare(d,switch).lt?) 
    then julian-days-from-date(d)
    else iso-days-from-date(d) 
}

fun julgreg-date-from-days( days : int, switch : int ) : date {
  if (days < switch) 
   then julian-date-from-days(days)
   else iso-date-from-days(days)
}


// pad with zeros
fun show0( i : int, width : int = 2) : string {
  i.show.pad-left(width,'0')
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