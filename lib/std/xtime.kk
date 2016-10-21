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

import std/regex
import std/int32
import std/dict

extern include {
  // cs file "time-inline.cs"
  js file "xtime-inline.js"
}



// Represents an instant in time. 
// Can be arbitrarily far into the future or past and is accurate up 
// to a tenth of a femto-second (10^-16s).
abstract struct instant (
  secs : int,         // SI seconds since epoch.
  frac : double       // fraction of seconds, always > 0.0 (i.e. into the time)
)

// Represents duration between `:instant`s in time. 
// Can be arbitrarily long and is accurate up 
// to a tenth of a femto-second (10^-16s).
abstract struct duration (
  secs : int,          // SI seconds
  frac : double = 0.0  // fraction of seconds, always in range <-1.0, 1.0>
)

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
  tz     : timezone = utc
)

/*----------------------------------------------------------------------------
  Durations and instants
----------------------------------------------------------------------------*/

// A zero second duration.
public val duration0 : duration = duration(0)

// The _epoch_ is 1972-01-01 UTC which equals 1972-01-01T00:00:10 TAI.
public val epoch = instant-at(0)

// Create a duration given a number of seconds and fraction of seconds.
public fun duration( secs : int, frac : double = 0.0 ) : duration {
  // maintain -1.0 < frac < 1.0
  if (frac.zero?) then return Duration(secs,0.0)
  val fl = frac.floor
  val s  = secs + fl.int
  val f  = frac - fl  // always positive [0,1>
  //trace( fl.show + ", " + s.show + ", " + f.show + ", " + (f - 1.0).show)
  if (s.neg?) then Duration( s.inc, f - 1.0 ) else Duration( s, f )
}

// Create a duration given a number of seconds as a `:double`.
public fun duration( secs : double ) : duration {
  Duration( secs.trunc.int, secs.fraction )
}

// Create an instant in time given a number of seconds since the `epoch`
// and fraction of seconds (=``0.0``).
public fun instant-at( secs : int, frac : double = 0.0 ) : instant {
  // maintain 0 <= frac 
  if (!frac.neg?) then return Instant(secs,frac)
  val fl = frac.floor
  Instant(secs + fl.int, frac - fl)
}

// Create an instant given a number of seconds since the `epoch`
// given as a `:double`.
public fun instant-at( secs : double ) : instant {
  instant-at( secs.trunc.int, secs.fraction )
}

// Return the duration since the `epoch`.
public fun since-epoch( i : instant ) : duration {
  duration(i.secs,i.frac)
}

// Return the instant in time that is `d` duration after the `epoch`.
public fun since-epoch( d : duration ) : instant {
  instant-at(d.secs,d.frac)
}

// Compare two `:duration`s.
public fun compare( i : duration, j : duration ) : order {
  match(compare(i.secs,j.secs)) {
    Eq  -> compare(i.frac,j.frac) 
    ord -> ord
  }
}

// Compare two `:instant`s in time.
public fun compare( i : instant, j : instant ) : order {
  // Note we cannot use `since-epoch` and compare durations
  // as we sometimes use instants with leap seconds encoded as
  // with a fraction `>= 1.0`. We want those to compare lower than
  // the instant on the next second.
  match(compare(i.secs,j.secs)) {
    Eq  -> compare(i.frac,j.frac) 
    ord -> ord
  }
}

public fun (<)( i : duration, j : duration )  : bool { compare(i,j) == Lt }
public fun (<=)( i : duration, j : duration ) : bool { compare(i,j) != Gt }
public fun (>)( i : duration, j : duration )  : bool { compare(i,j) == Gt }
public fun (>=)( i : duration, j : duration ) : bool { compare(i,j) != Lt }
public fun (==)( i : duration, j : duration ) : bool { compare(i,j) == Eq }
public fun (!=)( i : duration, j : duration ) : bool { compare(i,j) != Eq }


public fun (<)( i : instant, j : instant )  : bool { compare(i,j) == Lt }
public fun (<=)( i : instant, j : instant ) : bool { compare(i,j) != Gt }
public fun (>)( i : instant, j : instant )  : bool { compare(i,j) == Gt }
public fun (>=)( i : instant, j : instant ) : bool { compare(i,j) != Lt }
public fun (==)( i : instant, j : instant ) : bool { compare(i,j) == Eq }
public fun (!=)( i : instant, j : instant ) : bool { compare(i,j) != Eq }

// Add two durations
public fun (+)( d : duration, e : duration ) : duration {
  duration(d.secs + e.secs, d.frac + e.frac)
}

// Negate a duration.
public fun (~)( d : duration ) : duration {
  duration(~d.secs,~d.frac)
}

// Subtract a duration from a duration.
public fun (-)( d : duration, e : duration ) : duration {
  d + ~e
}

// Return a duration in seconds. This may lose precision for large durations
// as the result is returned as a `:double`.
public fun seconds( d : duration ) : double {
  d.secs.double + d.frac
}

// Add a duration to an instant in time.
public fun (+)( i : instant, d : duration ) : instant {
  (i.since-epoch + d).since-epoch
}

// Subtract a duration from an instant in time.
public fun (-)( i : instant, d : duration ) : instant {
  i + ~d
}

// Return the difference between to instants in time.
public fun (-)( i : instant, j : instant ) : duration {
  i.since-epoch - j.since-epoch
}

// Show an instant with an optional precision (=`9`, nano-second resolution).
public fun show( i : instant, prec : int = 9 ) : string {
  val leap = i.frac.trunc.int  
  // trace("show instant: " + i.secs.show + "s, " + i.frac.show)
  i.since-epoch.show +
    (if (leap.zero?) then "" else " " + (if (leap.pos?) then "+" else "-") + leap.abs.show)
}

// Show a duration with an optional precision (=`9`, nano-second resolution).
public fun show( i : duration, prec : int = 9 ) : string {   
  //trace("show duration: " + i.secs.show + "s, " + i.frac.show)
  (if (i.secs.zero? && i.frac.neg?) then "-" else "") + 
   i.secs.show + i.frac.show-frac(9) + "s"
}

// Show a fraction of a second up to an optional precision (=`9`)
fun show-frac( frac : double, prec : int = 9 ) : string {
  if (frac.fraction.zero?) return ""
  val xdigits = frac.abs.fraction.show-fixed(prec).list.drop(2)
  val digits  = xdigits.reverse.drop-while(fun(d){d=='0'}).reverse
  if (digits.nil?) return ""
  // always use a multiple of 3 to display a fraction
  val len3 = min(prec, ((digits.length + 2) / 3) * 3)
  "." + digits.string.pad-right( len3, '0') 
}
val rxdigits = regex(@"^\d+\.\(\d+[1-9]\)0*$")


/*----------------------------------------------------------------------------
  Julian Date
----------------------------------------------------------------------------*/

val mjd-epoch-shift     = 2400000.5
val mjd-epoch : instant = instant-at(~3569788810)  // 1858-11-17Z modified julian date epoch.

// Create an instant given a [julian date](https://en.wikipedia.org/wiki/Julian_day).
public fun instant-at-jd( julian-date: double ) : instant {
  instant-at-mjd( julian-date - mjd-epoch-shift )
}

// Create an instant given a [modified julian date](https://en.wikipedia.org/wiki/Julian_day).\
// modified-julian-date = julian-date - 2400000.5
public fun instant-at-mjd( modified-julian-date : double ) : instant {
  val delta = duration( modified-julian-date * solar-secs-per-day.double )
  mjd-epoch + delta
}

// Return the [julian date](https://en.wikipedia.org/wiki/Julian_day) of an instant.
public fun jd( i : instant ) : double {
  i.mjd + mjd-epoch-shift
}

// Return the [modified julian date](https://en.wikipedia.org/wiki/Julian_day) of an instant.
public fun mjd( i : instant ) : double {
  val delta = i - mjd-epoch
  (delta.seconds / solar-secs-per-day.double)
}



/*----------------------------------------------------------------------------
  Now
----------------------------------------------------------------------------*/

// The current `:instant` in time.
public fun now() : ndet instant {
  unix-now() // TODO
}

// Keep track of invalid instants
private alias unix-instant = instant

// Returns an instant in terms of unix time; this still needs
// to be adjusted to our epoch and taking account of leap seconds.
private extern unix-now() : ndet unix-instant {
  js "_now"
}

// The resolution in seconds of the system clock.
public fun now-resolution() : ndet double {
  xnow-resolution()
} 

extern xnow-resolution() : ndet double {
  js "_now_resolution"
}


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
   t.hours.show0 + ":" + t.mins.show0 + ":" + t.secs.show0 + 
   t.frac.show-frac(prec) + show-tzofs(t.tzofs ) +
   (if (t.cal.cal-name.empty?) then "" else " " + t.cal.cal-name)
}

// Show a time zone offset. 
// Optional `utc` for displaying a zero timezone offset (=`"Z"`).
// Optional `hmsep` for the hour-minute separator (=`":"`).
// Optional `hrwidth` to give the minimal width of the hour field (=`2`).
fun show-tzofs(ofs : double, utc : string = "Z", hmsep : string = ":", hrwidth : int = 2) : string {
  if (ofs.zero?) return utc
  val mins = (ofs.abs / 60.0).int32
  val tz = (if (ofs.neg?) then "-" else "+") + (mins/60.int32).show0(hrwidth) + hmsep + (mins%60.int32).show0
  val secs = (ofs.abs % 60.0)
  val tzs = if (secs.zero?) then "" else ":" + secs.trunc.int32.show0 + secs.fraction.show-frac(3)
  tz + tzs
}


// pad with zeros
fun show0( i : int, width : int = 2) : string {
  i.show.pad-left(width,'0')
}

// pad with zeros
fun show0( i : int32, width : int = 2) : string {
  i.show.pad-left(width,'0')
}



/*----------------------------------------------------------------------------
  Time zone
----------------------------------------------------------------------------*/

// A `:timezone` determines a time offset with respect to the UTC / GMT timezone.
// The `utc` and `local` time zones are used for UTC/GMT time and the local system time.\
public struct timezone(
  utc-offset : (instant) -> duration,
  utc-inverse: (instant) -> maybe<instant> = fun(i) { Nothing }
)

// Create a time zone with a fixed offset in seconds from UTC.
public fun timezone-fixed( secs : int, utc-name : string = "" ) : timezone {
  Timezone( 
    fun(i) { duration(secs) }, 
    fun(i) { if (secs.zero?) then Just(i) else Nothing }
  )
}

// The standard UTC time zone with a 0 offset.
public val utc : timezone = timezone-fixed(0)

// Return the local timezone on the current system.
public fun local() : ndet timezone {
  val tz = local-get-timezone()
  Timezone( fun(i) { 
    val ofs = local-utc-offset(tz, i.since-epoch.seconds) 
    duration(ofs)
  })
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


// The standard UTC calendar
val utc-cal = solar-calendar(
  "", 
  pgregorian-date-from-days,
  pgregorian-days-from-date,
  fun(i)  { utc-from-tai( i )  },
  fun(si) { tai-from-utc( si )  }
)

// The TAI Calendar.
val tai   = calendar-fixed-from-tai("TAI",0.0)

// The TAI10 calendar is always `TAI - 10`
val tai10 = calendar-fixed-from-tai( "TAI10", ~10.0 )

// The GPS calendar is always `TAI - 19`
val gps   = calendar-fixed-from-tai( "GPS", ~19.0 )

// The Terrestrial calendar is always `TAI - 32.184`
val tt    = calendar-fixed-from-tai( "TT", ~32.184 )

// Create a calendar that has a fixed `ofs` seconds difference from TAI.
public fun calendar-fixed-from-tai( name : string, ofs : double ) : calendar {
  solar-calendar( name,
    pgregorian-date-from-days,
    pgregorian-days-from-date,
    fun(i)  { i + duration(ofs + 10.0) },
    fun(si) { si - duration(ofs + 10.0) }       
  )
}

/*----------------------------------------------------------------------------
  Solar calendars
----------------------------------------------------------------------------*/

// A Calendar instant is an instant in time relative to the epoch
// used by the calendar. For example, for the Proleptic Gregorian
// calendar functions (`pgregorian-days-from-date`) the calendar
// instant gives the number of solar seconds (86400/day) since
// 1972-01-01.
public alias cal-instant = instant

val solar-secs-per-day  = 86400

public fun solar-calendar(
              cal-name        : string,
              date-from-days  : (days:int) -> date,
              days-from-date  : date -> int,
              cal-from-tai    : (instant) -> cal-instant, 
              tai-from-cal    : (cal-instant) -> instant
            ) : calendar
{
  Calendar(cal-name,
    fun(i)    { solar-dc-from-cal( cal-from-tai(i), date-from-days ) },
    fun(d,c)  { tai-from-cal( solar-cal-from-dc( d, c, days-from-date ) ) }
  )
}

/*
fun shift(i : instant, s : int ) : instant {
  Instant(i.secs + s, i.frac)
}
*/

fun solar-dc-from-cal( i : cal-instant, date-from-days: (int) -> date ) : (date,clock) {
  val (days,daysecs)   = divmod(i.secs, solar-secs-per-day)
  val date             = date-from-days(days)
  val (hours,minsecs)  = divmod(daysecs,3600)
  val (mins,xsecs)     = divmod(minsecs,60)
  val tsecs            = xsecs + i.frac.trunc.int  // adjust for leap seconds
  val tfrac            = i.frac.fraction
  (date, Clock(hours,mins,tsecs,tfrac))
}

fun solar-cal-from-dc( d : date, c : clock, days-from-date: (date) -> int ) : cal-instant {
  val (xdays,xsecs) = divmod( ((((c.hours*60) + c.mins)*60) + c.secs), solar-secs-per-day )
  val sdays = days-from-date( d(day = d.day + xdays) )
  instant-at( ((sdays*solar-secs-per-day + xsecs)), c.frac)   
}


/*----------------------------------------------------------------------------
  Time and instant conversion using a calendar
----------------------------------------------------------------------------*/

// Return the instant in time for a given date.
fun instantz( year : int, month : int = 1, day : int = 1, 
              hours : int = 0, minutes : int = 0, secs : int = 0, frac : double = 0.0,
              cal : calendar = utc-cal ) : instant 
{
  (cal.instant-from-dc)( Date(year,month,day), Clock(hours,minutes,secs,frac) )   
}

// Return the instant in time for a given date and clock interpreted by 
// calendar `cal` (=`utc-cal`) in a timezone `tz` (=`utc` by default).
public fun instant( year : int, month : int = 1, day : int = 1, 
                    hours : int = 0, minutes : int = 0, secs : int = 0, 
                    frac : double = 0.0, tz : timezone = utc, cal : calendar = utc-cal ) : instant 
{
  // the year/month/day is interpreted as if in UTC first.
  val i = instantz(year,month,day,hours,minutes,secs,frac,cal)
  // we need to adjust according to timezone 
  match((tz.utc-inverse)(i)) {
    Just(inv) -> inv
    Nothing -> {
      // no explicit inverse,
      // do a double pass to accommodate jumping over a DST boundary.
      val tzofs1 = (tz.utc-offset)(i)
      val tzi1   = i - tzofs1
      val tzofs2 = (tz.utc-offset)(tzi1)
      val tzi2   = i - tzofs2
      tzi2     
    }
  }
}


// Return a `:time` for a given instant.
fun timez( tzi : instant, tzofs : duration = duration0, tz : timezone = utc, cal : calendar = utc-cal, i : instant = tzi) : time {
  val (d,c) = (cal.dc-from-instant)( tzi )
  Time(d.year,d.month,d.day,c.hours,c.mins,c.secs,c.frac,
       i, cal, tzofs.seconds, tz )
}


// Convert an `:instant` to a `:time` value in a given timezone `tz` (=`utc` by default)
// and calendar (=`utc-cal` by default).
public fun time( i : instant, tz : timezone = utc, cal : calendar = utc-cal ) : time {
  val tzofs : duration = (tz.utc-offset)(i)
  val tzi   = i + tzofs
  timez( tzi, tzofs, tz, cal, i )
}


// Return the `:time` value for a given date and clock in a timezone `tz` (=`utc` by default) 
// interpreted by calendar `cal` (=`utc-cal`)
public fun time( year : int, month : int = 1, day : int = 1, 
                 hours : int = 0, minutes : int = 0, secs : int = 0, 
                 frac : double = 0.0, tz : timezone = utc, cal : calendar = utc-cal ) : time 
{
  time(instant(year,month,day,hours,minutes,secs,frac,tz,cal),tz,cal)
}


/*----------------------------------------------------------------------------
  Proleptic Gregorian calendar
----------------------------------------------------------------------------*/

val epoch-shift = 720198   // shift internal epoch from 0000-03-01 TAI to 1972-01-01T00:00:10 TAI
val days-in-era = 146097   // 365*400 + 100 - 3

// Convert days since `epoch` to a date in the proleptic Gregorian calendar.
fun pgregorian-date-from-days( days : int ) : date {
  val z    = days + epoch-shift           // shift epoch from 1970-01-01 to 0000-03-01
  val (era,idoe) = divmod(z,days-in-era)  // day of era: 0 <= doe < days-in-era
  val doe  = idoe.int32
  val yoe  = (((doe - (doe/1460.int32)) + (doe/36524.int32)) - (doe/146096.int32)) / 365.int32;  // year of era: 0 <= yoe < years-in-era 
  val doy  = doe - ((yoe*365.int32) + (yoe/4.int32) - (yoe/100.int32))   // day-of-year: 0 <= doy <= 365
  val mp   = ((doy*5.int32) + 2.int32)/153.int32                         // 0 <= mp <= 0,11
  val day  = (doy - (((mp*153.int32) + 2.int32) / 5.int32)) + 1.int32    // 1 <= day <= 31
  val month= mp + (if (mp < 10.int32) then 3.int32 else ~9.int32)        // 1 <= month <= 12
  val year = yoe.int + (400*era) + (if (month <= 2.int32) then 1 else 0)
  Date(year,month.int,day.int)
}


// Calculate days from a year/month/day triple in the proleptic Gregorian calendar
// `month` must be between 1 and 12. `day` must be between 1 and `days-in-era`.
fun pgregorian-days-from-datex( year : int, month : int32, day : int32 ) : int {
  val y = if (month <= 2.int32) then year.dec else year   // to internal year starting in Feb
  val (era,iyoe) = divmod(y,400)                    // year of era: 0<= yoe < 400
  val yoe  = iyoe.int32
  val mdoy = (((153.int32 * (month + (if (month > 2.int32) then ~3.int32 else 9.int32))) + 2.int32)/5.int32) 
  val doe  = (((yoe*365.int32) + (yoe/4.int32)) - (yoe/100.int32)) + mdoy + day.dec
  ((days-in-era * era) + doe.int) - epoch-shift
}

// Return days from a year, month, and day in the proleptic Gregorian calendar.
// Months and days can be any value.
fun pgregorian-days-from-date( d : date ) : int {
  val (xera,doe)  = divmod(d.day.dec, days-in-era)
  val (xyear,moy) = divmod(d.month.dec, 12)
  pgregorian-days-from-datex( d.year + (xera * 400) + xyear, moy.int32.inc, doe.int32.inc )
}



// -----------------------------------------------------------
// UTC calendar leap second calculation
// -----------------------------------------------------------

// A `:utc-instant` is an `:instant` in time, but instead
// of measuring SI seconds since `epoch` ``1972-01-01Z``, it measures 
// 'unix seconds' since `epoch`. Unix seconds are seconds as
// returned by the Unix system clock where leap seconds are
// ignored. (Unlike the Unix system clock, we measure since ``1972-01-01Z``
// though, not from ``1970-01-01Z``).
alias utc-instant = instant

// A `:leap-instant` is an `:instant` in time but measures
// since ``1972-01-01 TAI``. This is often convenient to
// make `:leap-instant` coincide with 'unix seconds' since ``1972-01-01Z``.\
// A `:leap-instant` `l` relates to a `:utc-instant` `u` as\
// `l == u + duration(10)`.
alias leap-instant= instant;


// A leap second table is a list of tuples. 
// Each entry gives the start instant and integer leap second adjustment.
struct leaps-table(
  expire : instant,                   
  adjusts: list<leap-adjust>
)

// Leap second adjustments. For an instant `i` after `start`:\
// ``TAI-offset = offset + (delta * days(i - delta-start))``
struct leap-adjust(
  utc-start  : instant,   // start time
  offset     : double,    // base offset
  delta-start: instant = epoch,   // start of delta adjustment
  delta      : double  = 0.0   
)

val zero : leap-adjust = Leap-adjust(epoch,0.0,epoch,0.0)

// Get the leap-second adjustment
fun tai-offset( la : leap-adjust, i : instant ) : duration {
  val secs = if (la.delta.zero?) then la.offset else {
               // pre 1972 is a rubber leap second
               val days = floor((i - la.delta-start).seconds / solar-secs-per-day.double)
               la.offset + (la.delta * days)          
             }
  duration(secs)  
}

// Set up a global leap second table.
// We are going to pretend that this is total even though for future
// dates utc-tai conversion might be off by some future leap seconds.
val global-leaps-table = unsafe-total{ 
  val pre72  = leap-seconds-pre72()
  val post72 = parse-leap-seconds( default-ietf-leap-seconds )
  ref(post72(adjusts = post72.adjusts + pre72))  // reverse order
}

// Get the global leap second table
fun get-leapsecs() : st<global> leaps-table {
  !global-leaps-table
}

// Whole leap seconds started in 1971-01-01Z
val utc1972  : utc-instant  = instant-at(0)   // 1972-01-01Z
val leap1972 : leap-instant = instant-at(10)

// Convert a UTC time (since 1972-01-01Z) where leap seconds
// are ignored, to a correct TAI time with leap seconds added.
// ````plain
// > instant(1972,1,1).time(cal=tai)
// 1972-01-01T00:00:10Z TAI
//
// > instant(2017,1,1).time(cal=tai)
// 2017-01-01T00:00:37Z TAI
// ````
fun tai-from-utc(i : utc-instant ) : instant {
  leap-from-utc(i, unsafe-total(get-leapsecs)) - duration(10)
}

// Convert a TAI time (since 1972-01-01Z) to a correct time with 
// correct leap seconds added.
// ````plain
// > instant(1972,1,1,0,0,10,cal=tai).time      // epoch
// 1972-01-01T00:00:00Z
//
// > instant(2017,1,1,0,0,36,0.1,cal=tai).time  // in a leap second
// 2016-12-31T23:59:60.100Z
// ````
// Correctly calculates TAI offsets for dates before 1972-01-01Z with fractional
// leap seconds. See <http://maia.usno.navy.mil/ser7/tai-utc.date> for the table
// of leap second adjustments.
// ````plain
// > instant(1972,1,1,0,0,0,cal=tai).time
// 1971-12-31T23:59:50.107758Z
// ````
// Treats the discontinuities around 1961 and 1972 as fractional
// leap seconds too. The periods where this occurs are:
// 1961-01-01 TAI to 1961-01-01T00:00:01.422818 TAI  and
// 1972-01-01T00:00:09.892 TAI to 1972-01-01T00:00:10 TAI; these periods are mapped 
// to a leap 'second' before 1961-01-01Z and 1972-01-01Z respectively. This means
// that the leap 'second' becomes 1.422818s in 1961 and
// 0.118s in 1972. For example:\
// ````plain
// > instant(1961,1,1,0,0,1,0.2,cal=tai).time
// 1960-12-31T23:59:61.200Z
//
// > instant(1961,1,1,0,0,1,0.422818,cal=tai).time
// 1961-01-01T00:00:00.000000000Z
// ````
// .
fun utc-from-tai(i : instant ) : utc-instant {
  utc-from-leap( i + duration(10), unsafe-total(get-leapsecs) )
}

// Return a `:leap-instant` for a UTC instant
fun leap-from-utc( i : utc-instant, leaps : leaps-table ) : leap-instant {
  //val leaps : list<leap-adjust> = tai-leaps-pre72() // cached, on-demand
  leaps.adjusts.find-maybe(fun(la) {
    if (la.utc-start > i) then Nothing else {
      val ofs = la.tai-offset(i)
      // trace("found: " + i.show + " >= " + la.utc-start.show + ", ofs:" + ofs.show)
      Just( i + ofs )
    }
  }).default(i)  
}

fun utc-from-leap( i : instant, leaps : leaps-table ) : instant {
  if (i < leap1961) then i else {
    find-utc-from-leap( i, leaps.adjusts )
  }
}
val leap1961    = Instant(~347068800,0.0)  // 1961-01-01Z

fun find-utc-from-leap( i : leap-instant, leaps : list<leap-adjust> ) : utc-instant {
  match(leaps) {
    Nil -> i // should never happen
    Cons(la,earlier) -> {
      val ofs   = la.tai-offset(i)
      val utc-i = i - ofs
      val max   = if (ofs.secs.neg?) then utc-i else i
      //trace("check: utc-i: " + utc-i.show + ", start: " + la.utc-start.show)
      if (la.utc-start > max) then find-utc-from-leap(i,earlier) else {
        if (la.utc-start <= utc-i) then {
          // in the time frame
          trace("utc-from-tai: found: utc: " + utc-i.show + ", tai-ofs: " + ofs.show )
          utc-i
        }
        else {
          // leap-second crosses-over into other time frame            
          // get adjustment using the earlier frame too
          trace("found crossover: " + utc-i.show)
          val la-before = earlier.head.default(zero)
          val ofs-before= la-before.tai-offset(i)
          val utc-prev  = i - ofs-before
          if (la.utc-start > utc-prev) then {
            // use previous leap-second offset            
            utc-prev
          }
          else {
            // this is inside an added leap-second, encode as a fraction `> 1.0`.
            val diff = (ofs - ofs-before).seconds
            utc-i(frac = utc-i.frac + diff)                            
          }
        }
      }  

      /*
      if (la.utc-start > i) then find-utc-from-leap( i, earlier ) else {
        val ofs   = la.tai-offset(i)
        val utc-i = i - ofs
        if (la.utc-start <= utc-i) then {
          // in the time frame
          utc-i
        }
        else {
          // leap-second crosses-over into other time frame            
          // get adjustment using the earlier frame too
          // trace("found crossover: " + utc-i.show)
          val la-before = earlier.head.default(zero)
          val ofs-before= la-before.tai-offset(i)
          val utc-prev  = i - ofs-before
          if (la.utc-start > utc-prev) then {
            // use previous leap-second offset
            utc-prev
          }
          else {
            // this is inside an added leap-second, encode as a fraction `> 1.0`.
            val diff = (ofs - ofs-before).seconds
            utc-i(frac = utc-i.frac + diff)                            
          }
        }
      }
      */
    }
  }
}



// -----------------------------------------------------------
// Parsing UTC leap second tables
// -----------------------------------------------------------

// [NTP](https://en.wikipedia.org/wiki/Network_Time_Protocol) epoch (1900-01-01Z)
public val ntp-epoch = instant-at(~ntp-epoch-shift)
val ntp-epoch-shift  = 2272060800 // (72*365 + 17)*86400 

// Parse a NIST (IETF) leap second table.
// See: <https://www.ietf.org/timezones/data/leap-seconds.list>
fun parse-leap-seconds( leaps : string ) : leaps-table {
  // get leap second adjustments
  val las = leaps.find-all(rxleap).map fun(cap) {
    val ntpsecs = cap.groups[1].parse-int-default(ntp-epoch-shift)  
    val adjust  = cap.groups[2].parse-double-default(0.0)
    val ntpi    = instant-at(ntpsecs - ntp-epoch-shift)
    //trace("leap entry: " + adjust.show + " " + ntpi.show + " " + (ntpsecs - ntp-epoch-shift).show )
    val utc-i = instant-at(ntpsecs - ntp-epoch-shift)
    Leap-adjust(utc-i, adjust, epoch, 0.0)
  }.reverse

  // get expiration date
  val la-final = las.head.default(zero)
  val utc-expire = leaps.find(rxexpire).map fun(cap) {
    val ntpex = cap.groups[1].parse-int-default(ntp-epoch-shift)
    instant-at(ntpex - ntp-epoch-shift)
  }.default(la-final.utc-start + duration(365*solar-secs-per-day))
  val expire = utc-expire + la-final.tai-offset(utc-expire) - duration(10) // convert expire to TAI
  trace("expire: " + expire.show)
  
  Leaps-table(expire,las)
}
val rxleap   = regex(@"^[ \t]*(\d+)[ \t]+(\d+)[ \t]*(?:#.*)?$",multiLine=True)
val rxexpire = regex(@"^[ \t]*#@[ \t]*(\d+)[ \t]*$", multiLine=True)

// IETF leap second data valid until 2017-06-28
val default-ietf-leap-seconds = @"
  # From: https://www.ietf.org/timezones/data/leap-seconds.list
  # Updated through IERS Bulletin C52
  # File expires on:  28 June 2017
  #
  #@  3707596800
  #
  2272060800  10  # 1 Jan 1972
  2287785600  11  # 1 Jul 1972
  2303683200  12  # 1 Jan 1973
  2335219200  13  # 1 Jan 1974
  2366755200  14  # 1 Jan 1975
  2398291200  15  # 1 Jan 1976
  2429913600  16  # 1 Jan 1977
  2461449600  17  # 1 Jan 1978
  2492985600  18  # 1 Jan 1979
  2524521600  19  # 1 Jan 1980
  2571782400  20  # 1 Jul 1981
  2603318400  21  # 1 Jul 1982
  2634854400  22  # 1 Jul 1983
  2698012800  23  # 1 Jul 1985
  2776982400  24  # 1 Jan 1988
  2840140800  25  # 1 Jan 1990
  2871676800  26  # 1 Jan 1991
  2918937600  27  # 1 Jul 1992
  2950473600  28  # 1 Jul 1993
  2982009600  29  # 1 Jul 1994
  3029443200  30  # 1 Jan 1996
  3076704000  31  # 1 Jul 1997
  3124137600  32  # 1 Jan 1999
  3345062400  33  # 1 Jan 2006
  3439756800  34  # 1 Jan 2009
  3550089600  35  # 1 Jul 2012
  3644697600  36  # 1 Jul 2015
  3692217600  37  # 1 Jan 2017"


// -----------------------------------------------------------
// Parsing TAI 'fractional' leap second tables from before 1972
// -----------------------------------------------------------

// Cached leap second table for dates before 1972.
val leap-seconds-pre72 : (() -> list<leap-adjust>) = once{ parse-leap-seconds-dat() }


// Parse the standard UTC leap second adjustment file.\
// See <http://maia.usno.navy.mil/ser7/tai-utc.dat>
fun parse-leap-seconds-dat( s : string = leap-seconds-dat ) : list<leap-adjust> {
  s.find-all(rxtaiadjust).map( fun(cap) {
    val mjd    = cap.groups[1].parse-double-default(mjd-epoch-shift) - (mjd-epoch-shift )
    val ofs    = cap.groups[2].parse-double-default(0.0)
    val dmjd   = cap.groups[3].parse-double-default(0.0)
    val delta  = cap.groups[4].parse-double-default(0.0)
    val start  = utc-mjd-epoch + Duration( mjd.int*solar-secs-per-day,0.0)  //instant(1858,11,17 + mjd.int)
    val dstart = utc-mjd-epoch + Duration( dmjd.int*solar-secs-per-day,0.0) //instant(1858,11,17 + dmjd.int)
    // trace("pre72 start=" + start.show )
    Leap-adjust( start, ofs, dstart, delta )
  }).reverse
}
val utc-mjd-epoch : instant = mjd-epoch + duration(10)
val rxtaiadjust = regex(@"^ *\d[^=]*=JD (\d+\.\d+) *TAI-UTC= *(\d+\.\d+)[^\d]+(\d+)[^\d]+(\d+\.\d+) *S *$", multiLine=True)

// TAI leap second adjustments for dates before 1972-01-01Z
val leap-seconds-dat = @"
  # from: http://maia.usno.navy.mil/ser7/tai-utc.dat
  1961 JAN  1 =JD 2437300.5  TAI-UTC=   1.4228180 S + (MJD - 37300.) X 0.001296 S
  1961 AUG  1 =JD 2437512.5  TAI-UTC=   1.3728180 S + (MJD - 37300.) X 0.001296 S
  1962 JAN  1 =JD 2437665.5  TAI-UTC=   1.8458580 S + (MJD - 37665.) X 0.0011232S
  1963 NOV  1 =JD 2438334.5  TAI-UTC=   1.9458580 S + (MJD - 37665.) X 0.0011232S
  1964 JAN  1 =JD 2438395.5  TAI-UTC=   3.2401300 S + (MJD - 38761.) X 0.001296 S
  1964 APR  1 =JD 2438486.5  TAI-UTC=   3.3401300 S + (MJD - 38761.) X 0.001296 S
  1964 SEP  1 =JD 2438639.5  TAI-UTC=   3.4401300 S + (MJD - 38761.) X 0.001296 S
  1965 JAN  1 =JD 2438761.5  TAI-UTC=   3.5401300 S + (MJD - 38761.) X 0.001296 S
  1965 MAR  1 =JD 2438820.5  TAI-UTC=   3.6401300 S + (MJD - 38761.) X 0.001296 S
  1965 JUL  1 =JD 2438942.5  TAI-UTC=   3.7401300 S + (MJD - 38761.) X 0.001296 S
  1965 SEP  1 =JD 2439004.5  TAI-UTC=   3.8401300 S + (MJD - 38761.) X 0.001296 S
  1966 JAN  1 =JD 2439126.5  TAI-UTC=   4.3131700 S + (MJD - 39126.) X 0.002592 S
  1968 FEB  1 =JD 2439887.5  TAI-UTC=   4.2131700 S + (MJD - 39126.) X 0.002592 S"

/*
struct timezone-info(
  name      : string,
  population: int,
  periods   : list<timezone-period>
)

struct timezone-period(
  start : maybe<instant>, // Nothing = beginning of time
  abbrv : string,
  offset: double
)

fun show( tzp : timezone-period ) : string {
  tzp.abbrv.pad-right(4) + " " + tzp.offset.show-tzofs(utc="+00:00") + " from " + tzp.start.map(fun(i){ i.time.show }).default("-infinity")
}

fun show (tzi : timezone-info ) : string {
  tzi.name + ":\n  " + tzi.periods.map(show).join("\n  ")
}

fun parse-packed-zones( s : string = iana-timezones-x20160101 ) : exn list<timezone-info> {// dict<timezone-info> {
  val zonesfld = match(s.find(rxzones)) {
    Nothing   -> error("invalid timezone data; cannot find zones array.")
    Just(cap) -> cap.groups[1].trim
  }
  val zonesarr = zonesfld.lines.map( fun(line) {
    match(line.find(rxzone)) {
      Nothing   -> error("invalid timezone info:\n  " + line)
      Just(cap) -> cap.groups[1]
    }
  })
  val zones = zonesarr.map(parse-packed-zone)
  zones
  //zones.map(fun(tzi){ (tzi.name, tzi) }).dict.list.map
}
val rxzones = regex(@"""zones""\s*:\s*\[([\s\S]*?)^[ \t]*\]", multiLine=True)
val rxzone  = regex(@"^[ \t]*""([^""]+)"",?[ \t]*$")

fun parse-packed-zone( s : string ) : exn timezone-info {
  trace("parseing: " + s)
  val parts   = s.split("|")
  val name    = parts[0].default("")
  val abbrs   = parts[1].default("").split(" ").filter(notempty?)
  val offsets = parts[2].default("0").split(" ").filter(notempty?).map(parse-fixed-base60)
  val indices = parts[3].default("0").list.map(digit60)
  val xdiffs  = parts[4].default("").split(" ").filter(notempty?).map(parse-fixed-base60)
  fun from-indices( xs : list<a>, def : a ) : list<a> {
    indices.map( fun(idx) {xs[idx].default(def) }).reverse
  }
  val (start0,diffs) = match(xdiffs) {
    Nil -> (epoch,[])
    Cons(start,rest) -> (instant-at(start * 60.0), Cons(0.0,rest))
  }
  val population = parts[5].default("0").parse-base60
  if (diffs.length + 1 != indices.length ) error("invalid timezone data:\n  " + s )
  val endings = diffs.foldl((start0,[]), fun(acc:(instant,list<maybe<instant>>),diff) {
                  val i = acc.fst + duration(diff * 60.0) 
                  (i,Cons(Just(i),acc.snd))
                }).snd
  val ofsabrs  = indices.reverse.map( fun(i) { (offsets[i].default(0.0), abbrs[i].default("") ) } )
  val periods  = zipwith( endings + [Nothing], ofsabrs, fun(start,ofsabbr) {
                   Timezone-period(start,ofsabbr.snd,ofsabbr.fst * ~60.0)
                 })
  Timezone-info(name,population,periods)
}

val pzone-la = "America/Los_Angeles|PST PDT PWT PPT|80 70 70 70|010102301010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-261q0 1nX0 11B0 1nX0 SgN0 8x10 iy0 5Wp1 1VaX 3dA0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1a00 1fA0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|15e6"
val pzone-ams = "Europe/Amsterdam|AMT NST NEST NET CEST CET|-j.w -1j.w -1k -k -20 -10|010101010101010101010101010101010101010101012323234545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545|-2aFcj.w 11b0 1iP0 11A0 1io0 1cM0 1fA0 1a00 1fA0 1a00 1fA0 1a00 1co0 1io0 1yo0 Pc0 1a00 1fA0 1Bc0 Mo0 1tc0 Uo0 1tA0 U00 1uo0 W00 1s00 VA0 1so0 Vc0 1sM0 UM0 1wo0 Rc0 1u00 Wo0 1rA0 W00 1s00 VA0 1sM0 UM0 1w00 fV0 BCX.w 1tA0 U00 1u00 Wo0 1sm0 601k WM0 1fA0 1cM0 1cM0 1cM0 16M0 1gMM0 1a00 1fA0 1cM0 1cM0 1cM0 1fA0 1a00 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|16e5"

fun parse-fixed-base60( s : string ) : exn double {
  match(s.find(rxfixbase60)) {
    Nothing -> error("invalid fixed base60 number: " + s)
    Just(cap) -> {
      val sign = if (cap.groups[1]=="-") then ~1 else 1
      val num  = parse-base60(cap.groups[2])
      val frac = cap.groups[3].list.foldr(0.0, fun(d,n) {
                    (n + d.digit60.double) / 60.0
                 })
      (sign.double * num.double) + frac
    }
  }
}
val rxfixbase60 = regex(@"^([\-\+])?([\da-zA-X]+)(?:\.([\da-zA-X]+))?$")

fun parse-base60( s : string ) : exn int {
  match(s.find(rxbase60)) {
    Nothing -> error("invalid base60 number: " + s)
    Just(cap) -> {
      val sign = if (cap.groups[1]=="-") then ~1 else 1
      val num  = cap.groups[2].list.foldl(0, fun(n,d) {
                    n*60 + d.digit60
                  })
      sign*num
    }
  }
}
val rxbase60 = regex(@"^([\-\+])?([\da-zA-X]+)$")

fun digit60( d : char ) : exn int {
  if (d.digit?) then (d - '0').int 
  elif (d.lower?) then (d - 'a').int + 10 
  elif (d.upper?) then (d - 'A').int + 36
  else error("invalid digit in base 60: " + d.show)
}

//fun parse-iana-timezones( packed-text: string = iana-timezones-x20160101 ) : dict<string>

val iana-timezones-x20160101 = @"
{
  ""version"": ""2016g"",
  ""zones"": [
    ""America/Los_Angeles|PST PDT PWT PPT|80 70 70 70|010102301010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-261q0 1nX0 11B0 1nX0 SgN0 8x10 iy0 5Wp1 1VaX 3dA0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1a00 1fA0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|15e6"",
    ""America/New_York|EST EDT EWT EPT|50 40 40 40|01010101010101010101010101010101010101010101010102301010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|-261t0 1nX0 11B0 1nX0 11B0 1qL0 1a10 11z0 1qN0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 11z0 1o10 11z0 RB0 8x40 iv0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1qN0 WL0 1qN0 11z0 1o10 11z0 1o10 11z0 1o10 11z0 1o10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1cN0 1cL0 1cN0 1cL0 s10 1Vz0 LB0 1BX0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 1cN0 1fz0 1a10 1fz0 1cN0 1cL0 1cN0 1cL0 1cN0 1cL0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|21e6"",
    ""Etc/GMT-8|+08|-80|0|"",
    ""Etc/GMT-9|+09|-90|0|"",
    ""Etc/UCT|UCT|0|0|"",
    ""Etc/UTC|UTC|0|0|"",
    ""Europe/Amsterdam|AMT NST NEST NET CEST CET|-j.w -1j.w -1k -k -20 -10|010101010101010101010101010101010101010101012323234545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545454545|-2aFcj.w 11b0 1iP0 11A0 1io0 1cM0 1fA0 1a00 1fA0 1a00 1fA0 1a00 1co0 1io0 1yo0 Pc0 1a00 1fA0 1Bc0 Mo0 1tc0 Uo0 1tA0 U00 1uo0 W00 1s00 VA0 1so0 Vc0 1sM0 UM0 1wo0 Rc0 1u00 Wo0 1rA0 W00 1s00 VA0 1sM0 UM0 1w00 fV0 BCX.w 1tA0 U00 1u00 Wo0 1sm0 601k WM0 1fA0 1cM0 1cM0 1cM0 16M0 1gMM0 1a00 1fA0 1cM0 1cM0 1cM0 1fA0 1a00 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|16e5"",
    ""WET|WET WEST|0 -10|010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010|hDB0 1a00 1fA0 1cM0 1cM0 1cM0 1fA0 1a00 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00""
  ],
  ""links"": [
    ""Africa/Abidjan|Africa/Bamako"",
    ""Africa/Abidjan|Africa/Banjul"",
    ""America/Los_Angeles|US/Pacific"",
    ""America/Los_Angeles|US/Pacific-New"",
    ""Pacific/Pohnpei|Pacific/Ponape""
  ]
}"

*/