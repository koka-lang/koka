/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Basic date and time funs.

   Years, months, days etc. are always 1-based, while durations are always in (fractional) seconds.
*/
module std/time/xcalendar

import std/time/xinstant
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
  public  mins   : int,
  public  seconds: fixed,
  public  cal    : calendar,
  public  tzofs  : double   = 0.0,
  public  tzabbrv: string   = "",
  public  tz     : timezone = tz-utc,
  public  instant: instant
)


/*----------------------------------------------------------------------------
  Time
----------------------------------------------------------------------------*/

// Return the hours, minutes, seconds, and fraction of the second.
fun clock( t : time ) : clock {
  Clock(t.hours,t.mins,t.secs,t.frac)
}

// Return the year, month, and day.
fun date( t : time ) : date {
  Date(t.year,t.month,t.day)
}

// Show a `:time` in [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) format.
public fun show(t : time, prec : int = 9 ) : string {
  val yr = (if (t.year>9999) then "+" elif (t.year.neg?) then "-" else "") + t.year.abs.show0(4)
  yr + "-" + t.month.show0 + "-" + t.day.show0 + "T" +
   t.hours.show0 + ":" + t.mins.show0 + ":" + 
   ts-show(t.secs,prec,2) + show-tzofs(t.tzofs ) +
   (if (t.tzabbrv.empty?) then "" else " (" + t.tzabbrv + ")") +
   (if (t.cal.cal-name.empty?) then "" else " " + t.cal.cal-name)
}

// Show a time zone offset. 
// Optional `utc` for displaying a zero timezone offset (=`"Z"`).
// Optional `hmsep` for the hour-minute separator (=`":"`).
// Optional `hrwidth` to give the minimal width of the hour field (=`2`).
public fun show-tzofs(ofs : double, utc : string = "Z", hmsep : string = ":", hrwidth : int = 2) : string {
  if (ofs.zero?) return utc
  val mins = (ofs.abs / 60.0).int
  val tz = (if (ofs.neg?) then "-" else "+") + (mins/60).show0(hrwidth) + hmsep + (mins%60).show0
  val secs = (ofs.abs % 60.0)
  val tzs = if (secs.zero?) then "" else ":" + ts-show( fixed(secs), 3 )
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
  utc-offset : (instant) -> (duration,string),
  utc-inverse: (instant) -> maybe<instant> = fun(i) { Nothing }
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
      val ofs = local-utc-offset(tz, i.unix) 
      (duration(ofs),"")
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
  hours  : int,
  mins   : int,
  secs   : fixed
)

// A Calendar determines how a `:date` and `:clock` relates to an `:instant` in time. 
public struct calendar(
  cal-name        : string,                 
  dc-from-instant : instant -> (date,clock),
  instant-from-dc : (date,clock) -> instant
)


// The standard [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) calendar
// using UTC time. This is a proleptic Gregorian 
// calendar except that it uses the year 0 for 1BC, -1 for 2BC etc. 
public fun cal-iso() : ndet calendar  -> iso-calendar( "", ts-utc() )

// The [Julian calendar](https://en.wikipedia.org/wiki/Julian_calendar).
public fun cal-julian()  -> solar-calendar(
  "JC",
  julian-date-from-days,
  julian-days-from-date,
  ts-utc()
)

// The (proleptic) [Gregorian calendar](https://en.wikipedia.org/wiki/Gregorian_calendar).
// Just like the ISO calendar except that the year 0 does not exist, i.e. after 1BC (=`~1`)
// we have 1AD (=`1`).
public fun cal-gregorian() -> solar-calendar(
  "GC",
  gregorian-date-from-days,
  gregorian-days-from-date,
  ts-utc()
)

// The combined Julian / Gregorian calendar, using the Julian calendar for dates 
// before 1582-10-15 and the Gregorian calendar otherwise. It is possible to 
// specify a different switch date using the `julian-gregorian` function.
public fun cal-jg()  -> julian-gregorian()

// The TAI Calendar. This is a standard ISO 8601 calender using
// TAI time where every day is exactly 86400 SI seconds (unlike
// standard UTC time which can insert leap seconds).
public val cal-tai = iso-calendar( "TAI", ts-tai )

// The GPS calendar is always `TAI - 19` but with epoch 1980-01-06Z
public val cal-gps = iso-calendar( "GPS", ts-gps, (ts-gps.to-tai)(instant-at(duration(~19))) )

// The Terrestrial calendar is always `TAI + 32.184`
public val cal-tt  = iso-calendar( "TT", ts-tai, fixed(32184,3) )


// Create a standard ISO calendar using a particular time scale and epoch shift relative
// to the `epoch`.
public fun iso-calendar( name : string, ts : timescale, shift : fixed = zero ) : calendar {
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
              ts   : timescale,
              shift: fixed = zero
            ) : calendar
{
  Calendar(cal-name,
    fun(i)   { solar-dc-from-cal( (ts.from-tai)(i) + shift, date-from-days ) },
    fun(d,c) { (ts.to-tai)( solar-cal-from-dc( d, c, days-from-date ) - shift ) }
  )
}


fun solar-dc-from-cal( t : timestamp, date-from-days: (int) -> date ) : (date,clock) {
  val (days,daysecs)   = divmod(t.seconds.trunc.int, secs-per-day)
  val date             = date-from-days(days)
  val (hours,minsecs)  = divmod(daysecs,3600)
  val (mins,xsecs)     = divmod(minsecs,60)
  val tsecs            = fixed(xsecs) + t.seconds.fraction + t.leap-seconds  // adjust for leap seconds
  (date, Clock(hours,mins,tsecs))
}

fun solar-cal-from-dc( d : date, c : clock, days-from-date: (date) -> int ) : timestamp {
  val (secs,leaps)  = if (c.secs < fixed(60)) then (c.secs.trunc.int,0) else ( 59, (c.secs.trunc.int - 59) )
  val (xdays,xsecs) = divmod( ((((c.hours*60) + c.mins)*60) + c.secs), secs-per-day )
  val sdays = days-from-date( d(day = d.day + xdays) )
  duration( fixed(sdays*secs-per-day + xsecs) + c.secs.fraction, fixed(leaps) )   
}


// Create a Julian-Gregorian calendar which uses Julian dates before the optional `switch`
// (Gregorian) date (=`Date(1582,10,15`)) and Gregorian dates otherwise.
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
      val tzofs1 = (tz.utc-offset)(i).fst
      val tzi1   = i - tzofs1
      val tzofs2 = (tz.utc-offset)(tzi1).fst
      val tzi2   = i - tzofs2
      tzi2     
    }
  }
}


// Return a `:time` for a given instant.
fun timez( tzi : instant, tzofs : duration = duration0, tzabbrv : string = "", tz : timezone = tz-utc, cal : calendar = cal-iso, i : instant = tzi) : time {
  val (d,c) = (cal.dc-from-instant)( tzi )
  Time(d.year,d.month,d.day,c.hours,c.mins,c.secs,c.frac,
       cal, tzofs.seconds, tzabbrv, tz, i )
}


// Convert an `:instant` to a `:time` value in a given timezone `tz` (=`tz-utc` by default)
// and calendar (=`cal-iso` by default).
public fun time( i : instant, tz : timezone = tz-utc, cal : calendar = cal-iso ) : time {
  if (tz.tz-utc?) then timez(i,duration0,"",tz,cal,i) else {
    val (tzofs,tzabbrv) = (tz.utc-offset)(i)
    val tzi = instant-at-timestamp( i.timestamp(ts-utc) + tzofs, ts-utc )
    timez( tzi, tzofs, tzabbrv, tz, cal, i )
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

val iso-epoch-shift = 693901   // shift internal epoch from 0000-03-01 to 1900-01-01
val iso-days-in-era = 146097   // 365*400 + 100 - 3
val iso-years-in-era= 400

// Convert days since `epoch` to a date in the [ISO8601] calendar,
// a proleptic Gregorian calendar where year 0 is 1BC.
//
// [ISO8601]: https://en.wikipedia.org/wiki/ISO_8601
fun iso-date-from-days( days : int ) : date {
  val z    = days + iso-epoch-shift          // shift epoch from 1900-01-01 to 0000-03-01
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
public fun gregorian-days-from-date( d : date ) : int {
  if (d.year <= 0) 
    then iso-days-from-date(d(year = d.year + 1)) 
    else iso-days-from-date(d)
}

// Return a date from a number of days in the proleptic Gregorian calendar.
public fun gregorian-date-from-days( days : int ) : date {
  val d = iso-date-from-days(days)
  if (d.year<=0) then d(year = d.year - 1) else d
}

val julian-days-in-era  = 1461
val julian-years-in-era = 4
val julian-epoch-shift  = 693903 // Shift from 1900-01-01 to 0000-03-01

// Return a date from a number of days in the Julian calendar.
public fun julian-date-from-days( days : int ) : date {  
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
public fun julian-days-from-date( d : date ) : int {
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

