/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Basic date and time funs.

   Years, months, days etc. are always 1-based, while durations are always in (fractional) seconds.
*/
module std/time/calendar

import std/fixed
import std/time/timespan
import std/time/duration
import std/time/instant
import std/time/timescale

extern include {
  js file "calendar-inline.js"
}

// Represents an instant in time for a certain calendar and timezone.
abstract struct time (
  // The year.
  public  year   : int,
  public  month  : int,
  public  day    : int, 
  public  hours  : int,
  public  minutes: int,
  public  seconds: fixed,
  public  calendar: calendar,
  public  tzofs  : duration = duration0,
  public  tzabbrv: string   = "",
  public  tz     : timezone = tz-utc,
  public  instant: instant
)


/*----------------------------------------------------------------------------
  Time
----------------------------------------------------------------------------*/

// Return the hours, minutes, seconds, and fraction of the second.
public fun clock( t : time ) : clock {
  Clock(t.hours,t.minutes,t.seconds)
}

// Return the year, month, and day.
public fun date( t : time ) : date {
  Date(t.year,t.month,t.day)
}

public fun show-year( year : int ) : string {
  val (ysign,ywidth) = if (year > 9999) then ("+",5) elif (year.neg?) then ("-",5) else ("",4)
  ysign + year.abs.show0(ywidth)
}

public fun show( d : date ) : string {
  d.year.show-year + "-" + d.month.show0 + "-" + d.day.show0
}

// 
public fun round-to-prec( t : time, prec : int ) : time {
  val secs = t.seconds.round-to-prec(prec)
  if (secs.trunc == t.seconds.trunc) then {
    t(seconds = secs, instant = t.instant.round-to-prec(prec))
  } 
  else {
    t.instant.round-to-prec(prec).time(t.tz,t.calendar)
  } 
}

// Show a `:time` in [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) format.
public fun show(t0 : time, prec : int = 9 ) : string {
  val t = t0.round-to-prec(prec)
  t.date.show + "T" +
   t.hours.show0 + ":" + t.minutes.show0 + ":" + 
   show(timestamp(t.seconds),prec,2) + show-tzofs(t.tzofs ) +
   (if (t.tzabbrv.empty?) then "" else " (" + t.tzabbrv + ")") +
   (if (t.calendar.cal-name.empty?) then "" else " " + t.calendar.cal-name)
}

// Show a time zone offset. 
// Optional `utc` for displaying a zero timezone offset (=`"Z"`).
// Optional `hmsep` for the hour-minute separator (=`":"`).
// Optional `hrwidth` to give the minimal width of the hour field (=`2`).
public fun show-tzofs(offset : duration, utc : string = "Z", hmsep : string = ":", hrwidth : int = 2) : string {
  val ofs = offset.seconds
  if (ofs.zero?) return utc
  val (fmins,secs) = divmod(ofs.abs,fixed(60),6) // millisecond precision
  val mins = fmins.trunc.int
  val tz   = (if (ofs.neg?) then "-" else "+") + (mins/60).show0(hrwidth) + hmsep + (mins%60).show0
  val tzs  = if (secs.zero?) then "" else ":" + show( timestamp(secs), 3 )
  tz + tzs
}


/*----------------------------------------------------------------------------
  Time zone
----------------------------------------------------------------------------*/

// A `:timezone` determines a time offset with respect to the UTC / GMT timezone.
// The `tz-utc` and `tz-local` time zones are used for UTC/GMT time and the local system time.\
// The `tz-name` field contains the time zone name.\The `utc-offset` field returns the time zone offset
// relative to UTC (for GMT-08:00 it returns a duration of `~8*3600` seconds) and the abbreviated 
// time zone name (``PDT`` for example).\
// The optional `utc-inverse` field returns for an instant in the time zone, the associated UTC time.
// By default it returns `Nothing` in which case a generic algorithm is used to determine the
// inverse.  
public struct timezone(
  tz-name    : string,                      
  private utc-offset : (instant) -> (duration,string),
  private utc-inverse: (instant) -> maybe<instant> = fun(i) { Nothing }
)


// Is this the UTC timezone?
public fun tz-utc?( tz : timezone ) : bool {
  (tz.tz-name == "UTC")
}

// Create a time zone with a fixed offset in seconds from UTC.\
// Takes a `name`, an `offset` from UTC, and optional time zone abbreviation (=`name`).
// For example, for a fixed time zone``+07:00``, use `tz-fixed( "GMT+7", duration(7 * 3600) )`
public fun tz-fixed( name : string, offset : duration, abbrv : string = "" ) : timezone {
  Timezone( 
    name,            
    fun(i) { (offset,abbrv) }, 
    fun(i) { if (offset.zero?) then Just(i) else Nothing }
  )
}

// The standard UTC time zone with a 0 offset.
public val tz-utc : timezone = tz-fixed("UTC",duration0,"")

// Return the local timezone on the current system.
public fun tz-local() : ndet timezone {
  val tz = local-get-timezone()
  Timezone( 
    "",
    fun(i) { 
      val ofs = local-utc-offset(tz, i.timestamp(ts-unix).seconds.double) 
      (duration(ofs.fixed),"")
    }
  )
}

// A local timezone structure provided by the host system
type local-timezone

// Get the current local timezone structure.
extern local-get-timezone() : ndet local-timezone {
  js "_local_get_timezone"
}

// Return the utc-offset in fractional seconds given a local timezone structure
// and fractional seconds since the UNIX epoch (1970-01-01).
extern local-utc-offset( tz : local-timezone, i : double ) : double {
  js "_local_utc_offset"
}


/*----------------------------------------------------------------------------
  Calendar
----------------------------------------------------------------------------*/

// A date consists of a the year, month, and day.
public struct date(
  year : int,
  month: int,
  day  : int
)

// Compare two dates
public fun compare( d : date, e : date ) : order {
  match(compare(d.year,e.year)) {
    Eq -> match(compare(d.month,e.month)) {
      Eq  -> compare(d.day,e.day)
      ord -> ord
    }
    ord -> ord
  }
}

// A clock consists of the hour, minute, second, and fractional second (between ``0.0` and `1.0`).
public struct clock(
  hours   : int,
  minutes : int,
  seconds : fixed
)

// A Calendar determines how a `:date` and `:clock` relates to an `:instant` in time. 
public struct calendar(
  cal-name: string,                 
  cal-ts  : timescale,
  private dc-from-instant : (i:instant,tzofs:duration) -> (date,clock),
  private instant-from-dc : (date,clock) -> instant
)

// Return the total seconds of a `:clock` assuming 60 seconds per 
// minute and 60 minutes per hour.
public fun total-seconds( c : clock ) : fixed {
  fixed((c.hours*60 + c.minutes)*60) + c.seconds
}

/*----------------------------------------------------------------------------
  Generic calendar algorithms
  These work for any calendar.
----------------------------------------------------------------------------*/

// An [ISO `:weekdate`](https://en.wikipedia.org/wiki/ISO_week_date) 
// represents a date as a year, the week of the year, and the day of the week.
public struct weekdate(
  year : int,
  week : int,
  day  : int                       
)

// Show a `:weekdate` in the ISO week date format: ``2000-W01-1`` (for 2000-01-03Z).
public fun show( wd : weekdate ) : string {
  wd.year.show-year + "-W" + wd.week.show0 + "-" + wd.day.show
}

// Return the date of the first `weekday` of the specified month and calendar (=`cal-iso`).
public fun first-weekday-of-month( year : int, month : int, weekday : int, cal : calendar = cal-iso ) : date {
  val dow = day-of-week(Date(year,month,1),cal)
  val inc = weekday-diff(weekday,dow)
  Date(year,month,1 + inc)
}

// Return the date of the last `weekday` of the specified month and calendar (=`cal-iso`).
public fun last-weekday-of-month( year : int, month : int, weekday : int, cal : calendar = cal-iso ) : date {
  val first = first-weekday-of-month(year,month + 1,weekday,cal).day
  Date(year,month,days-in-month(year,month,cal) + first - 7)
}

// Return the [ISO week date](https://en.wikipedia.org/wiki/ISO_week_date) for a date. 
// Can pass a calendar `cal` (=`cal-iso`) to
// interpret the date. It is also possible to pass the weekday that determines if 
// a week falls in the current year or not, which is Thursday for the ISO definition
// (=`4`). 
// Note that a week can fall in the year before or the year after. For example:\
// `weekdate(Date(2005,1,1)).show   == "2004-W53-6"`\
// `weekdate(Date(2005,12,31)).show == "2005-W52-6"`\
// `weekdate(Date(2008,12,31)).show == "2009-W01-3"`
public fun weekdate( d : date, cal : calendar = cal-iso, weekday-in-year : int = 4 ) : weekdate {
  val dow  = d.day-of-week(cal)
  val sun  = (d.day-of-year(cal) - dow) 
  val wday = sun + weekday-in-year
  if (wday <= 0) then {
    // last week of previous year 
    val wk = (Date(d.year - 1, 12, 31 + wday).day-of-year + 6)/7
    Weekdate(d.year - 1, wk, dow)
  }
  elif (wday > days-in-year(d.year,cal)) then 
    // first week in the next year
    Weekdate(d.year + 1, 1, dow)
  else 
    // week in this year
    Weekdate(d.year, (wday+6)/7, dow)
}

// Return the difference between two weekdays.
public fun weekday-diff( wd1 : int, wd2 : int ) : int {
  val d = wd1 - wd2
  d%7
}

// Find the weekday after `inc` days after weekday `wd`.
public fun weekday-add( wd : int, inc : int = 1 ) : int {
  ((wd - 1) + inc)%7 + 1
}

// Return the day of the week for a calendar `cal` (=`cal-iso`).
// Uses the ISO definition where Monday is day 1, and Sunday day 7.
public fun day-of-week( d : date, cal : calendar = cal-iso ) : int {
  // Generic algorithm for any calendar
  val days = instant-at(d.year,d.month,d.day,cal=cal).mjd(cal.cal-ts).floor.int
  val dow  = (days+3)%7        // plus 3 since MJD epoch was a wednesday
  if (dow==0) then 7 else dow  // to ISO week day number (monday=1, .. sunday=7)
}

// Return the day of the year for a given date in calendar `cal` (=`cal-iso`).
// Starts at 1 for the first day of the year.
public fun day-of-year( d : date, cal : calendar = cal-iso ) : int {
  day-diff(d.year,d.month,d.day,d.year,1,1,cal) + 1
}

// Return the days in a given month in calendar `cal` (=`cal-iso`).
public fun days-in-month( year : int, month : int , cal : calendar = cal-iso ) : int {
  day-diff(year,month+1,1,year,month,1,cal)  
}

// Return the days in a given year and calendar (=`cal-iso`).
public fun days-in-year( year : int, cal : calendar = cal-iso) : int {
  day-diff(year+1,1,1,year,1,1,cal)  
}

fun day-diff( y1 : int, m1 : int, d1 : int, y2 : int, m2 : int, d2 : int, cal : calendar ) : int {
  val i1 = instant-at(y1,m1,d1,cal=cal)
  val i2 = instant-at(y2,m2,d2,cal=cal)
  val ts = cal.cal-ts
  (i1.mjd(ts) - i2.mjd(ts)).double.floor.int    
}

// Return the modified Julian date ([MJD](https://en.wikipedia.org/wiki/Julian_day#Variants)) 
// number for a given `:time`.
public fun mjd( t : time ) : fixed {
  //trace("offset: " + t.tzofs.show)
  // adjust with the timezone offset so we display the mjd on the date.
  t.instant.mjd(t.calendar.cal-ts) + (t.tzofs.seconds / secs-per-day.fixed)
}

// Return a `:time` as a fractional year.\
// `year-frac(time(2000,7,2)) == fixed(2000.5)`
public fun year-frac( t : time ) : fixed {
  val dfrac = t.clock.total-seconds.double / secs-per-day.double
  val yfrac = ((t.date.day-of-year(t.calendar).double - 1.0) + dfrac) / t.year.days-in-year(t.calendar).double
  fixed-frac(t.year,yfrac)
}


/*----------------------------------------------------------------------------
  Standard calendars
----------------------------------------------------------------------------*/

// The standard [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) calendar
// using UTC time. This is a proleptic Gregorian 
// calendar except that it uses the year 0 for 1BC, -1 for 2BC etc. 
public val cal-iso : calendar = iso-calendar( "" )

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
public val cal-tai = iso-calendar( "TAI", ts-tai )

// The GPS calendar is always `TAI - 19` but with epoch 1980-01-06Z
public val cal-gps = iso-calendar( "GPS", ts-gps, ts-gps.to-tai(timestamp(~19)).timespan ) //(ts-gps.to-tai)(timestamp(fixed(~19))).seconds )

// The Terrestrial (or Ephemeris) time calendar is always `TAI + 32.184`
public val cal-tt  = iso-calendar( "TT", ts-tt)//, fixed(32184,3) )


// The TCG (Geometric coordinate time) calendar. See also `ts-tcg`
public val cal-tcg  = iso-calendar( "TCG", ts-tcg)//, fixed(32184,3) )

// The TDB (Barycentric dynamical time) calendar. See also `ts-tdb`. Always within 2ms of TT (`cal-tt`).
public val cal-tdb  = iso-calendar( "TDB", ts-tdb)//, fixed(32184,3) )

// The TCB (Barycentric coordinate time) calendar. See also `ts-tcb`.
public val cal-tcb  = iso-calendar( "TCB", ts-tcb)//, fixed(32184,3) )


// Create a standard ISO calendar using a particular time scale and epoch shift relative
// to the `epoch`.
public fun iso-calendar( name : string, ts : timescale = ts-utc, shift : timespan = timespan0 ) : calendar {
  solar-calendar(
    name,
    iso-date-from-days,
    iso-days-from-date,
    ts,
    shift                 
  )
}

/*----------------------------------------------------------------------------
  Solar calendars
----------------------------------------------------------------------------*/

// Create a solar calendar given a calendar name `cal-name`, 
// a function to calculate a date given a number of days since the `epoch`,
// a function to calculate days since the `epoch` for a given date,
// and an optional time scale to be used (=`ts-utc`) and an optional offset from 
// the standard `epoch` (=`duration0`).
public fun solar-calendar(
              cal-name        : string,
              date-from-days  : (days:int) -> date,
              days-from-date  : date -> int,
              ts   : timescale = ts-utc,
              shift: timespan  = timespan0 
            ) : calendar
{
  Calendar(cal-name,ts,
    fun(i,tzofs) { solar-dc-from-cal( i.timestamp(ts) + shift + tzofs.timespan, date-from-days ) },
    fun(d,c)     { unsafe-instant-at( solar-cal-from-dc( d, c, days-from-date ) - shift, ts ) }
  )
}


fun solar-dc-from-cal( t : timestamp, date-from-days: (int) -> date ) : (date,clock) {
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

fun solar-cal-from-dc( d : date, c : clock, days-from-date: (date) -> int ) : timestamp {
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
fun instantz( year : int, month : int = 1, day : int = 1, 
              hours : int = 0, minutes : int = 0, secs : int = 0, frac : double = 0.0,
              cal : calendar = cal-iso ) : instant 
{
  val fsecs = if (frac.zero?) then fixed(secs) else fixed(secs) + fixed(frac)
  (cal.instant-from-dc)( Date(year,month,day), Clock(hours,minutes,fsecs) )   
}


// Return the instant in time for a given date and clock interpreted by 
// calendar `cal` (=`cal-iso`) in a timezone `tz` (=`tz-utc` by default).
public fun instant-at( year : int, month : int = 1, day : int = 1, 
                    hours : int = 0, minutes : int = 0, secs : int = 0, 
                    frac : double = 0.0, tz : timezone = tz-utc, cal : calendar = cal-iso ) : instant 
{
  // the year/month/day is interpreted as if in UTC first.
  val i = instantz(year,month,day,hours,minutes,secs,frac,cal)
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
      val utci   = i.timestamp(ts-utc)
      val tzofs1 = (tz.utc-offset)(i).fst
      val tzi1   = unsafe-instant-at(utci - tzofs1.timespan, ts-utc)
      val tzofs2 = (tz.utc-offset)(tzi1).fst
      val tzi2   = unsafe-instant-at(utci - tzofs2.timespan, ts-utc)
      tzi2     
    }
  }
}


// Return a `:time` for a given instant.
fun timez( i : instant, tzofs : duration = duration0, tzabbrv : string = "", tz : timezone = tz-utc, cal : calendar = cal-iso ) : time {
  val (d,c) = (cal.dc-from-instant)( i, tzofs )
  Time(d.year,d.month,d.day,c.hours,c.minutes,c.seconds,
       cal, tzofs, tzabbrv, tz, i )
}


// Convert an `:instant` to a `:time` value in a given timezone `tz` (=`tz-utc` by default)
// and calendar (=`cal-iso` by default).
public fun time( i : instant, tz : timezone = tz-utc, cal : calendar = cal-iso ) : time {
  if (tz.tz-utc?) then timez(i,duration0,"",tz,cal) else {
    val (tzofs,tzabbrv) = (tz.utc-offset)(i)
    timez( i, tzofs, tzabbrv, tz, cal )
  }
}


// Return the `:time` value for a given date and clock in a timezone `tz` (=`tz-utc` by default) 
// interpreted by calendar `cal` (=`cal-iso`)
public fun time( year : int, month : int = 1, day : int = 1, 
                 hours : int = 0, minutes : int = 0, secs : int = 0, 
                 frac : double = 0.0, tz : timezone = tz-utc, cal : calendar = cal-iso ) : time 
{
  time(instant-at(year,month,day,hours,minutes,secs,frac,tz,cal),tz,cal)
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

// Return the ISO calendar date of Easter in a given year (Algorithm by [J.M. Oudin](http://aa.usno.navy.mil/faq/docs/easter.php)).
public fun easter( y : int ) : date {
  val c  = y/100
  val n  = y - 19*(y/19)
  val k  = (c - 17)/25
  val i0 = (((c - c/4) - (c - k)/3) + 19*n) + 15
  val i1 = i0 - 30*(i0/30)
  val i  = i1 - (i1/28)*(1 - (i1/28)*(29/(i1 + 1))*((21 - n)/11))
  val j0 = ((y + y/4 + i + 2) - c) + c/4
  val j  = j0 - 7*(j0/7)
  val l  = i - j
  val m  = 3 + (l + 40)/44
  val d  = (l + 28) - 31*(m/4)
  Date(y,m,d)
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