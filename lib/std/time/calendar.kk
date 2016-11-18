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

// Create a time zone with a fixed number of hours difference from UTC/GMT.
public fun tz-gmt( hours-delta: int ) : timezone {
  tz-fixed("GMT" + (if (hours-delta.neg?) then "-" + hours-delta.abs.show else "+" + hours-delta.show),
            duration(hours-delta * 3600) )
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
public val cal-julian = earth-calendar(
  "JC",
  julian-days-to-date,
  julian-date-to-days
)

// The (proleptic) [Gregorian calendar](https://en.wikipedia.org/wiki/Gregorian_calendar).
// Just like the ISO calendar except that the year 0 does not exist, i.e. after 1BC (=`~1`)
// we have 1AD (=`1`).
public val cal-gregorian = earth-calendar(
  "GC",
  gregorian-days-to-date,
  gregorian-date-to-days
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
  earth-calendar(
    name,
    iso-days-to-date,
    iso-date-to-days,
    ts
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
              days-to-date  : (days:int) -> date,
              date-to-days  : date -> int,
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
  Calendar(name,ts,instant-to-dc,dc-to-instant)
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
  val tsecs            = fixed-frac(xsecs + leap, nlfrac)  // adjust for leap seconds
  (date, Clock(hours,mins,tsecs))
}

fun earth-dc-to-timestamp( d : date, c : clock, date-to-days: (date) -> int ) : timestamp {
  // Get seconds and leap seconds (if the clock has seconds > 59)
  val secs = c.seconds.trunc.int
  val frac = c.seconds.fraction.double
  val (fsecs,leaps)  = if (secs < 60) then (secs,0) else ( 59, secs - 59 )
  val (xdays,xsecs) = divmod( ((((c.hours*60) + c.minutes)*60) + fsecs), secs-per-day )
  val sdays = date-to-days( d(day = d.day + xdays) )
  timestamp( sdays*secs-per-day + xsecs, frac ).add-leap-seconds(leaps.double)  
}


// Create a Julian-Gregorian calendar which uses Julian dates before the optional `switch`
// (Gregorian) date (=`Date(1582,10,15`)) and Gr egorian dates otherwise.
public fun julian-gregorian( switch : date = Date(1582,10,15) ) : calendar {
  val switch-days = iso-date-to-days(switch) //+ skip
  earth-calendar( "JG", 
    fun(days) { julgreg-days-to-date(days,switch-days) },
    fun(d)    { julgreg-date-to-days(d,switch) }
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
        val utci = i.timestamp(ts-utc)
        val tzd1 = (tz.utc-delta)(i).fst
        val tzi1 = unsafe-instant-at(utci - tzd1.timespan, ts-utc)
        val tzd2 = (tz.utc-delta)(tzi1).fst
        val tzi2 = unsafe-instant-at(utci - tzd2.timespan, ts-utc)
        tzi2
      }
      else {
        // But in other cases, we interpret a timezone offset in seconds of that time scale,
        // and we should *not* convert to UTC or otherwise we may lose/add a leap seconds
        // in a time zone transition.
        // (for example: time(1973,1,1,0,0,11,tz=tz-gmt(~1),cal=cal-tai).show == "1973-01-01T00:00:11-01:00 TAI")
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
  val days = i.mjd(cal.timescale).floor.int
  val dow  = (days+3)%7        // plus 3 since MJD epoch was a wednesday  (note: 0 = sunday, 1 = monday, ..)
  dow.weekday                  // to ISO week day 
}

// Return the difference in days between two dates interpreted by calendar `cal`.
public fun day-diff( cal : calendar, d1 : date, d2 :date ) : int {
  val i1 = instant-at(d1,cal=cal)
  val i2 = instant-at(d2,cal=cal)
  val ts = cal.timescale
  (i1.mjd(ts) - i2.mjd(ts)).floor.int    
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


