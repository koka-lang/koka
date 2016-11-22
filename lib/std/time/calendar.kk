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
  name      : string,                 
  long-name : string,
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
public val cal-iso : calendar = iso-calendar( ts-utc, "", "ISO" )

// The [Julian calendar](https://en.wikipedia.org/wiki/Julian_calendar).
public val cal-julian = earth-calendar(
  "JC",
  "Julian",
  julian-days-to-date,
  julian-date-to-days
)

// The (proleptic) [Gregorian calendar](https://en.wikipedia.org/wiki/Gregorian_calendar).
// Just like the ISO calendar except that the year 0 does not exist, i.e. after 1BC (=`~1`)
// we have 1AD (=`1`).
public val cal-gregorian = earth-calendar(
  "GC",
  "Gregorian",
  gregorian-days-to-date,
  gregorian-date-to-days
)

// The [Coptic](https://en.wikipedia.org/wiki/Coptic_calendar) calendar.
public val cal-coptic = earth-calendar(
  "CC",
  "Coptic",
  coptic-days-to-date,
  coptic-date-to-days
)

// The [Ethiopian](https://en.wikipedia.org/wiki/Ethiopian_calendar) calendar.
public val cal-ethiopian = earth-calendar(
  "EC",
  "Ethiopian",
  ethiopian-days-to-date,
  ethiopian-date-to-days
)



// The _ISO month_ calendar.
// 
// This is a (non-standard) extension of the [ISO standard week date](https://en.wikipedia.org/wiki/ISO_week_date) calendar.
// Normally, ISO week dates are expressed as a year, a week number, and a day of the week.
// This makes a great calendar as it is _perennial_ where dates always fall on the same week day,
// and it closely follows the Gregorian calendar where weeks always have 7 days.
//
// The _ISO month_ calendar takes this calendar as its base, but expresses it more regularly using
// months and day of the month (instead of weeks and day of the week). 
//
// Every year always starts on Monday (January 1st). There are four quarters of 13 ISO weeks that always start on Monday
// and end on Sunday (and there is never a Friday the 13th :-). The three months of a quarter are 
// 30, 30, and 31 days. When there is an ISO week 53, we call it a leap year where the 'leap week'
// gets inserted at the end of the last month (and adds days 32 to 38 to December).
// This way, dates in the ISO month calendar are never  more than 5 days apart from the Gregorian calendar. 
// (Just like ISO week dates, a year is a leap year whenever the corresponding Gregorian calendar year starts- or ends with a Thursday.)
// The epoch is Monday 0001-01-01 IM which coincides with Monday 0001-01-01 in the ISO/Gregorian calendar.
//
// Note that with a perennial calendar, things like finding the `n`th weekday is suddenly
// trivial; For example, Thanksgiving (the fourth Thursday of November) is always on November 23 IMC,
// and always 31 days before from Christmas. 
//
// Note that this calendar coincides with The [Permanent](https://en.wikipedia.org/wiki/Hanke%E2%80%93Henry_Permanent_Calendar) 
// calendar proposal except that we propose to base it directly on the standard ISO week calendar.
public val cal-iso-month = iso-month-calendar("IMC","ISO month",30,30)

public fun iso-month-calendar( name : string, long-name : string, m1 : int = 30, m2 : int = 30 ) : calendar {
  earth-calendar(
    name,
    long-name,
    fun(days:int) { isom-days-to-date(days,m1,m2) },
    fun(d:date)   { isom-date-to-days(d,m1,m2) }
  )
}

// The 'ISO week' calendar.
public val cal-iso-week = earth-calendar(
  "IWC",
  "ISO Week",
  isow-days-to-date,
  isow-date-to-days
)


// The combined Julian / Gregorian calendar, using the Julian calendar for dates 
// before 1582-10-15 and the Gregorian calendar otherwise. It is possible to 
// specify a different switch date using the `julian-gregorian` function.
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
              long-name   : string,
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
  Calendar(name,long-name,ts,instant-to-dc,dc-to-instant)
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
  val doy   = doe - 365*yoe                     // 0 <= doy <= 365
  val mp    = (doy / 30) 
  val day   = (doy - (30 * mp)) + 1
  val month = mp + 1
  val year  = yoe + (coptic-years-in-era * era) 
  val yyear = if (year<=0) then year.dec else year  // no zero year
  val d = Date(yyear,month,day)
  // trace("coptic days to date: " + days.show + " -> " + d.show)  
  d
}

fun coptic-datex-to-days( year : int, month : int, day : int ) : int {
  val (era,yoe) = divmod(year.dec,coptic-years-in-era)
  val doy = 30*(month - 1) + (day - 1)
  val doe = yoe*365 + doy
  ((era * coptic-days-in-era) + doe) - coptic-epoch-shift
}  

// Return the days from a date in the Coptic calendar.
public fun coptic-date-to-days( d : date ) : int {
  val (xera,doe)  = divmod(d.day.dec, coptic-days-in-era)
  val (xyear,moy) = divmod(d.month.dec, 13)
  val yyear = if (d.year<=0) then d.year.inc else d.year  // no zero year, treat 0 as 1
  val days = coptic-datex-to-days( yyear + (xera * coptic-years-in-era) + xyear, moy.inc, doe.inc )    
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
----------------------------------------------------------------------------*/

val isow-years-in-era = 400
val isow-days-in-era  = 146097   // 365*400 + 100 - 3
val isow-days-in-short= 364      // 52*7
val isow-epoch-shift  = 730485

fun isow-year-to-days( year : int ) : int {
  val (era,yoe) = divmod(year - 1, isow-years-in-era)     // calculate `yoe` of one year before (as we want days up to `year`-01-01)
  val gdays = ((yoe*365 + yoe/4) - yoe/100) + 366         // days to Gregorian year at Jan 1, +366 as year 0 was leap
  val weeks = gdays / 7
  val wday  = (gdays - 7*weeks) - 1                        // weekday of Gregorian Jan 1 ([-1,6]) (0 = Sunday) (because 0000-01-01 is Saturday and 0000 is leap)
  val adj   = if (wday <= 4) then 1 - wday else 8 - wday   // adjust to start of ISO week (with first week containing the first Thursday)
  //trace("isow-year-to-days: " + year.show + ": wday: " + wday.show + ", adj: " + adj.show + ", days; " + gdays.show)
  (era*isow-days-in-era + gdays + adj)
}

// Interpret the 'months' as ISO weeks
fun isow-date-to-days( d : date ) : int {
  val ydays = isow-year-to-days(d.year)
  (ydays + d.month.dec*7 + d.day.dec) - isow-epoch-shift
}

fun isow-year-is-long?( year : int ) : bool {
  val diff = isow-year-to-days(year.inc) - isow-year-to-days(year)
  // trace("isow long year: " + year.show + ": diff: " + diff.show)
  (diff > isow-days-in-short)
}

fun isow-days-to-yearday( days : int ) : (int,int) {
  val gyear = (((days - (days/1460)) + (days/36524)) - (days/146096)) / 365;  // year of era: 0 <= yoe < years-in-era 
  val doy  = days - isow-year-to-days(gyear)
  if (doy.neg?) then { // if it fell in the previous ISO year
    (gyear.dec, days - isow-year-to-days(gyear.dec))
  }
  elif (doy >= isow-days-in-short && !isow-year-is-long?(gyear)) then { //  in the next ISO year?
    (gyear.inc, doy - isow-days-in-short)
  }
  else (gyear,doy)
}

fun isow-days-to-date( days : int ) : date {
  val (year,doy) = isow-days-to-yearday(days + isow-epoch-shift)
  val (weeks,dow) = divmod(doy,7)
  Date(year,weeks.inc,dow.inc)
}

// The ISO month calendar

val isom-days-in-quarter = 91

fun isom-days-to-date( days : int, m1 : int = 30, m2 : int = 30 ) : date {
  val m12     = m1 + m2
  val wdate   = isow-days-to-date(days)
  val (q,doq) = divmod( 7*wdate.month.dec + wdate.day.dec, isom-days-in-quarter )
  val (moq,dom) = if (q==4) then (~1, (doq + isom-days-in-quarter) - m12)  // minus 1 as q == 4 and we need month 12
                  elif (doq >= m12) then (2, doq - m12) 
                  elif (doq >= m1) then (1, doq - m1) 
                  else (0, doq)
  //trace("isom days to date: " + days.show + " -> " + wdate.show)
  Date(wdate.year, q*3 + moq.inc, dom.inc)
}

fun isom-date-to-days( d : date, m1 : int = 30, m2 : int = 30 ) : int {
  val (q,moq) = divmod( d.month.dec, 3)
  val mdoq = if (moq == 2) then m1 + m2 elif (moq==1) then m1 else 0
  val doy  = q*isom-days-in-quarter + mdoq + d.day.dec
  //trace("isom date to days: " + d.show + ", mdoq; " + mdoq.show + ", doy : " + doy.show + ", day: " + d.day.show)
  val days = isow-date-to-days(Date(d.year,1,doy.inc))
  days
}




/*----------------------------------------------------------------------------
  The 'Symetry 454 Calendar' 
----------------------------------------------------------------------------*/

val sym-days-in-era  = 294*364  // 293 years with 52 leap weeks
val sym-years-in-era = 293
val sym-epoch-shift  = 730485 - 366 // Shift from 2000-01-01 to 0000-01-03 PC


// By linear regression, it turns out we can fit a years to days formula
fun sym-yoe-to-days( yoe : int ) : int {
  val w = 52.17743862*yoe.double + 52.00522393            
  (7*w.int) 
}

// Convert a normalized date
fun sym-datex-to-days( year : int, month : int, day : int ) : int {
  val (era,yoe) = divmod( year, sym-years-in-era )
  trace("sym xdays: " + year.show + ", yoe: " + yoe.show)
  val ydays   = sym-yoe-to-days(yoe )
  val (q,moq) = divmod(month - 1, 3)
  val doq     = (day - 1) + (if (moq==0) then 0 elif (moq == 1) then 28 else 63)
  val doy     = q*91 + doq
  val days    = era*sym-days-in-era + ydays + doy
  trace("sym days: " + days.show)
  days - sym-epoch-shift
}

public fun sym-date-to-days( d : date ) : int {
  val (xera,doe)  = divmod(d.day.dec, sym-days-in-era)
  val (xyear,moy) = divmod(d.month.dec, 12)
  sym-datex-to-days( d.year + (xera * sym-years-in-era) + xyear, moy.inc, doe.inc )
}

// Based on: http://individual.utoronto.ca/kalendis/leap/293-leap-pattern.pdf
val cycle62 = 3*cycle17 + cycle11
val cycle28 = cycle17 + cycle11  
val cycle17 = 6209  // 3*371 + 14*364
val cycle11 = 4018  // 22*371 + 9*364

val year15= 5481    // 3*371 + 12*364
val year9 = 3290    // 2*371 + 7*364
val year3 = 1099    // 1*371 + 2*364

// Finds the year and day-of-year for a short 11-year cycle, or a long 17-year cycle.
// An 11-year cycle is: s s l s s s s s l s s
// A 17-year cycle is : s s l s s s s s l s s s s s l s s
// (with s for short years, and l for long years)
fun sym-doe-to-year17( days : int ) : date {
  val (y,d)  = if (days >= year15 - 7) then (15,days - year15)
                elif (days >= year9 - 7) then (9,days - year9)
                 elif (days >= year3 - 7) then (3,days - year3)
                  else (0,days)
  val (dy,doy) = if (d.neg?) then (~1, 371 + d) else divmod(d,364)                    
  Date(y + dy, 0, doy)
}

// Finds the year and day-of-year for a 62-year cycle
// which consists of 3 17-year cycles followed by a short 11-year cycle.
// 62 == 17 + 17 + 17 + 11
fun sym-doe-to-year62( days : int ) : date {
  val (c,doc) = divmod( days, cycle17 )
  val Date(y,m,doy) = sym-doe-to-year17(doc)
  Date(y + c*17, m, doy)
}

// A full 293 year cycle always starts with a 17-year and 11-year cycle,
// followed by 62-year cycles (the last one is partial)
// i.e. 293 == 17 + 11 + 62 + 62 + 62 + 62 + 17
fun sym-doe-to-year( days : int ) : date {
  if (days < cycle28) then return sym-doe-to-year62(days)
  val (c,doc) = divmod(days - cycle28, cycle62)
  val Date(y,m,doy) = sym-doe-to-year62(doc)
  Date(y + 28 + c*62, m, doy)
}

public fun sym-days-to-date( days : int ) : date {
  val (era,doe) = divmod( days + sym-epoch-shift, sym-days-in-era )
  val Date(y,_,d) = sym-doe-to-year(doe)
  val (m,dom) = if (d >= 336) then (11,d - 336) else {
    val (q,doq) = divmod(d,91)
    val mq  = q*3
    (if (doq < 63) then (if (doq < 28) then (mq,doq) else (mq+1,doq - 28)) else (mq+2,doq - 63))
  }
  Date(y + era*sym-years-in-era, m + 1, dom + 1)
}

