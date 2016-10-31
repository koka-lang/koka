/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Basic date and time funs.

   Years, months, days etc. are always 1-based, while durations are always in (fractional) seconds.
*/
public module std/xtime

import std/time/duration
import std/time/leaps
import std/time/instant


// Represents an instant in time for a certain calendar and timezone.
abstract struct time (
  year   : int,
  month  : int,
  day    : int, 
  hours  : int,
  mins   : int,
  secs   : int,
  frac   : double,
  tinstant: instant,
  cal    : calendar,
  tzofs  : double   = 0.0,
  tzabbrv: string   = "",
  tz     : timezone = tz-utc
)


/*----------------------------------------------------------------------------
  Time
----------------------------------------------------------------------------*/

// Get the instant in time of a given `:time` value.
public fun instant( t : time ) : instant {
  t.tinstant
}

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
   ts-show( ts-duration(t.secs,t.frac),prec,2) + show-tzofs(t.tzofs ) +
   (if (t.tzabbrv.empty?) then "" else " (" + t.tzabbrv + ")") +
   (if (t.cal.cal-name.empty?) then "" else " " + t.cal.cal-name)
}

// Show a time zone offset. 
// Optional `utc` for displaying a zero timezone offset (=`"Z"`).
// Optional `hmsep` for the hour-minute separator (=`":"`).
// Optional `hrwidth` to give the minimal width of the hour field (=`2`).
fun show-tzofs(ofs : double, utc : string = "Z", hmsep : string = ":", hrwidth : int = 2) : string {
  if (ofs.zero?) return utc
  val mins = (ofs.abs / 60.0).int
  val tz = (if (ofs.neg?) then "-" else "+") + (mins/60).show0(hrwidth) + hmsep + (mins%60).show0
  val secs = (ofs.abs % 60.0)
  val tzs = if (secs.zero?) then "" else ":" + ts-show( ts-duration(secs), 3 )
  tz + tzs
}


/*----------------------------------------------------------------------------
  Time zone
----------------------------------------------------------------------------*/

// A `:timezone` determines a time offset with respect to the UTC / GMT timezone.
// The `utc` and `local` time zones are used for UTC/GMT time and the local system time.\
public struct timezone(
  tz-name    : string,                      
  utc-offset : (instant) -> (utc-duration,string),
  utc-inverse: (instant) -> maybe<instant> = fun(i) { Nothing }
)

public fun tz-utc?( tz : timezone ) : bool {
  (tz.tz-name == "UTC")
}

// Create a time zone with a fixed offset in seconds from UTC.
public fun timezone-fixed( name : string, secs : int, utc-name : string = "" ) : timezone {
  Timezone( 
    name,            
    fun(i) { (ts-duration(secs),"") }, 
    fun(i) { if (secs.zero?) then Just(i) else Nothing }
  )
}

// The standard UTC time zone with a 0 offset.
public val tz-utc : timezone = timezone-fixed("UTC",0)

// Return the local timezone on the current system.
public fun tz-local() : ndet timezone {
  val tz = local-get-timezone()
  Timezone( 
    "",
    fun(i) { 
      val ofs = local-utc-offset(tz, i.ts-since-epoch(utc).seconds) 
      (utc-duration(ofs),"")
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
// and fractional seconds since the `epoch`.
extern local-utc-offset( tz : local-timezone, i : double ) : double {
  js "_local_utc_offset"
}


/*----------------------------------------------------------------------------
  Calendar
----------------------------------------------------------------------------*/

public struct date(
  year : int,
  month: int,
  day  : int
)

public struct clock(
  hours  : int,
  mins   : int,
  secs   : int,
  frac   : double
)

// A Calendar determines how a date, _year_-_month_-_day_, relates to 
// an `:instant` in time. 
public struct calendar(
  cal-name        : string,                 
  dc-from-instant : instant -> (date,clock),
  instant-from-dc : (date,clock) -> instant
)


// The standard [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) calendar
// using UTC time. This is a proleptic Gregorian 
// calendar except that it uses the year 0 for 1BC, -1 for 2BC etc. 
public val cal-iso = iso-calendar( "", utc )

public val cal-julian = solar-calendar(
  "JC",
  julian-date-from-days,
  julian-days-from-date
)

public val cal-gregorian = solar-calendar(
  "GC",
  pgregorian-date-from-days,
  pgregorian-days-from-date
)

// The TAI Calendar. This is a standard ISO 8601 calender using
// TAI time where every day is exactly 86400 SI seconds (unlike
// standard UTC time which can insert leap seconds).
public val cal-tai   = iso-calendar( "TAI", tai )

// The GPS calendar is always `TAI - 19` but with epoch 1980-01-06Z
public val cal-gps   = iso-calendar( "GPS", gps, (gps.to-tai)(duration(~19)) )

// The Terrestrial calendar is always `TAI - 32.184`
public val cal-tt    = iso-calendar( "TT", tai, duration(~32.184) )


public fun iso-calendar( name : string, ts : time-scale, shift : ts-duration = duration0 ) : calendar {
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

// A Calendar instant is an instant in time relative to the epoch
// used by the calendar. For example, for the Proleptic Gregorian
// calendar functions (`iso-days-from-date`) the calendar
// instant gives the number of solar seconds (86400/day) since
// 1972-01-01.
public alias cal-instant = instant

val solar-secs-per-day  = 86400

public fun solar-calendar(
              cal-name        : string,
              date-from-days  : (days:int) -> date,
              days-from-date  : date -> int,
              ts   : time-scale  = utc,
              shift: ts-duration = duration0
            ) : calendar
{
  Calendar(cal-name,
    fun(i) { solar-dc-from-cal( i.ts-use(ts).ts-add(shift), date-from-days ) },
    fun(d,c)  { solar-cal-from-dc( d, c, days-from-date ).ts-since-epoch(ts).ts-add(~shift) }
  )
}


fun solar-dc-from-cal( i : instant, date-from-days: (int) -> date ) : (date,clock) {
  val (isecs,ifrac)    = i.ts-since-epoch.value   
  val (days,daysecs)   = divmod(isecs, solar-secs-per-day)
  val date             = date-from-days(days)
  val (hours,minsecs)  = divmod(daysecs,3600)
  val (mins,xsecs)     = divmod(minsecs,60)
  val tsecs            = xsecs + ifrac.trunc.int  // adjust for leap seconds
  val tfrac            = ifrac.fraction
  (date, Clock(hours,mins,tsecs,tfrac))
}

fun solar-cal-from-dc( d : date, c : clock, days-from-date: (date) -> int ) : ts-duration {
  val (xdays,xsecs) = divmod( ((((c.hours*60) + c.mins)*60) + c.secs), solar-secs-per-day )
  val sdays = days-from-date( d(day = d.day + xdays) )
  duration((sdays*solar-secs-per-day + xsecs), c.frac)   
}




/*----------------------------------------------------------------------------
  Time and instant conversion using a calendar
----------------------------------------------------------------------------*/

// Return the instant in time for a given date.
fun instantz( year : int, month : int = 1, day : int = 1, 
              hours : int = 0, minutes : int = 0, secs : int = 0, frac : double = 0.0,
              cal : calendar = cal-iso ) : instant 
{
  (cal.instant-from-dc)( Date(year,month,day), Clock(hours,minutes,secs,frac) )   
}


// Return the instant in time for a given date and clock interpreted by 
// calendar `cal` (=`iso`) in a timezone `tz` (=`utc` by default).
public fun instant-at( year : int, month : int = 1, day : int = 1, 
                    hours : int = 0, minutes : int = 0, secs : int = 0, 
                    frac : double = 0.0, tz : timezone = tz-utc, cal : calendar = cal-iso ) : instant 
{
  // the year/month/day is interpreted as if in UTC first.
  val i = instantz(year,month,day,hours,minutes,secs,frac,cal)
  if (tz.tz-utc?) return i
  // we need to adjust according to timezone 
  val utc-i = i.ts-use(utc)  // switch to UTC timescale
  match((tz.utc-inverse)(utc-i)) {
    Just(inv) -> inv
    Nothing -> {
      // no explicit inverse,
      // do a double pass to accommodate jumping over a DST boundary.
      val tzofs1 = (tz.utc-offset)(utc-i).fst
      val tzi1   = utc-i - tzofs1
      val tzofs2 = (tz.utc-offset)(tzi1).fst
      val tzi2   = utc-i - tzofs2
      tzi2     
    }
  }
}


// Return a `:time` for a given instant.
fun timez( tzi : instant, tzofs : duration = duration0, tzabbrv : string = "", tz : timezone = tz-utc, cal : calendar = cal-iso, i : instant = tzi) : time {
  val (d,c) = (cal.dc-from-instant)( tzi )
  Time(d.year,d.month,d.day,c.hours,c.mins,c.secs,c.frac,
       i, cal, tzofs.seconds, tzabbrv, tz )
}


// Convert an `:instant` to a `:time` value in a given timezone `tz` (=`utc` by default)
// and calendar (=`iso` by default).
public fun time( i : instant, tz : timezone = tz-utc, cal : calendar = cal-iso ) : time {
  if (tz.tz-utc?) then timez(i,duration0,"",tz,cal,i) else {
    // use UTC timescale
    val utc-i = i.ts-use(utc)
    val (tzofs,tzabbrv) = (tz.utc-offset)(utc-i)
    val tzi   = ts-add(utc-i,tzofs)
    timez( tzi, tzofs, tzabbrv, tz, cal, utc-i )
  }
}


// Return the `:time` value for a given date and clock in a timezone `tz` (=`utc` by default) 
// interpreted by calendar `cal` (=`iso`)
public fun time( year : int, month : int = 1, day : int = 1, 
                 hours : int = 0, minutes : int = 0, secs : int = 0, 
                 frac : double = 0.0, tz : timezone = tz-utc, cal : calendar = cal-iso ) : time 
{
  time(instant-at(year,month,day,hours,minutes,secs,frac,tz,cal),tz,cal)
}


/*----------------------------------------------------------------------------
  ISO calendar, Proleptic Gregorian calendar, and Julian calendar.
----------------------------------------------------------------------------*/

val iso-epoch-shift = 715085   // shift internal epoch from 0000-03-01 TAI to 1972-01-01T00:00:10 TAI
val iso-days-in-era = 146097   // 365*400 + 100 - 3
val iso-years-in-era= 400

// Convert days since `epoch` to a date in the [ISO8601] calendar,
// a proleptic Gregorian calendar where year 0 is 1BC.
//
// [ISO8601]: https://en.wikipedia.org/wiki/ISO_8601
fun iso-date-from-days( days : int ) : date {
  val z    = days + iso-epoch-shift          // shift epoch from 1970-01-01 to 0000-03-01
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
public fun pgregorian-days-from-date( d : date ) : int {
  if (d.year <= 0) 
    then iso-days-from-date(d(year = d.year + 1)) 
    else iso-days-from-date(d)
}

// Return a date from a number of days in the proleptic Gregorian calendar.
public fun pgregorian-date-from-days( days : int ) : date {
  val d = iso-date-from-days(days)
  if (d.year<=0) then d(year = d.year - 1) else d
}

val julian-days-in-era  = 1461
val julian-years-in-era = 4
val julian-epoch-shift  = 715087  

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

public fun julian-days-from-date( d : date ) : int {
  val (xera,doe)  = divmod(d.day.dec, julian-days-in-era)
  val (xyear,moy) = divmod(d.month.dec, 12)
  val yyear = if (d.year<=0) then d.year.inc else d.year
  julian-days-from-datex( yyear + (xera * julian-years-in-era) + xyear, moy.inc, doe.inc )    
}


// pad with zeros
fun show0( i : int, width : int = 2) : string {
  i.show.pad-left(width,'0')
}

fun check(name,tst) {
  println(name + ": " + (if (tst()) then "ok" else "failed!"))
}

fun test-jd() {
  check("jd0"){ time(2000,1,1,12,0,0).instant.jd == 2451545.0 }
  check("jd1"){ time(2013,1,1,0,30,0).instant.jd == (2456293.5208333335) }
  check("jd2"){ instant-at-jd(0.0).time.show == "-4713-11-24T12:00:00Z" }
  check("jd3"){ instant-at-jd(0.0).time(cal=cal-julian).show == "-4713-01-01T12:00:00Z JC" }
}

fun test-leap() {
  check("leap1"){ instant-at(2017,1,1,0,0,35,cal=cal-tai).time.show == "2016-12-31T23:59:59Z" }
  check("leap2"){ instant-at(2017,1,1,0,0,36,cal=cal-tai).time.show == "2016-12-31T23:59:60Z" }
  check("leap3"){ instant-at(2017,1,1,0,0,37,cal=cal-tai).time.show == "2017-01-01T00:00:00Z" }
  // 'leap second' around 1972-01-01 of 0.1s
  check("leap4"){ instant-at(1972,1,1,0,0,9,cal=cal-tai).time.show == "1971-12-31T23:59:59.107758Z" }
  check("leap5"){ instant-at(1972,1,1,0,0,9,0.8,cal=cal-tai).time.show == "1971-12-31T23:59:59.907758Z" }
  check("leap6"){ instant-at(1972,1,1,0,0,9,0.9,cal=cal-tai).time.show == "1971-12-31T23:59:60.007758Z" }
  check("leap7"){ instant-at(1972,1,1,0,0,10,cal=cal-tai).time.show == "1972-01-01T00:00:00Z" }
  // discontinuity 1961-08-01 of 0.05s
  check("leap8"){ instant-at(1961,8,1,0,0,1,0.6,cal=cal-tai).time.show == "1961-07-31T23:59:59.902430Z" }
  check("leap9"){ instant-at(1961,8,1,0,0,1,0.7,cal=cal-tai).time.show == "1961-08-01T00:00:00.052430Z" }
  // initial 1.422818 leap second in 1961-01-01
  check("leap10"){ instant-at(1961,1,1,0,0,1,0.4,cal=cal-tai).time.show == "1960-12-31T23:59:61.400Z"}
  check("leap11"){ instant-at(1961,1,1,0,0,1,0.5,cal=cal-tai).time.show == "1961-01-01T00:00:00.077182Z"}  
  // GPS
  check("gps1"){ instant-at(1980,1,6,cal=cal-gps).time.show == "1980-01-06T00:00:00Z" }
  check("gps2"){ instant-at(1980,1,6,cal=cal-gps).time(cal=cal-tai).show == "1980-01-06T00:00:19Z TAI" }
  check("gps3"){ instant-at(2017,1,1,0,0,18,cal=cal-gps).time.show == "2017-01-01T00:00:00Z" }
}


fun test() {
  test-jd()
  test-leap()
}