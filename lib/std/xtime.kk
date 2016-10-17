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

val secs-per-uday  = 86400
val nsecs-per-sec  = 1.0e9

// Represents an instant in time. 
// Can be arbitrarily far into the future or past and is accurate up 
// to a femto-second (10^-15s).
abstract struct instant (
  secs : int,         // UTC seconds since epoch.
  frac : double       // fraction of seconds.
)

// Represents duration between `:instant`s in time. 
// Can be arbitrarily long and is accurate up to a femto-second (10^-15s).
abstract struct duration (
  secs : int,
  frac : double = 0.0
)

// Represents an instant in time for a certain calendar and timezone.
abstract struct time (
  year   : int,
  month  : int32,
  day    : int32, 
  hours  : int32,
  mins   : int32,
  secs   : int32,
  frac   : double,
  tzofs  : double,
  tinstant: instant,
  tz     : timezone
)

// Compare two `:duration`s.
public fun compare( i : duration, j : duration ) : order {
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

// Compare two `:instant`s in time.
public fun compare( i : instant, j : instant ) : order {
  compare(i.since-epoch,j.since-epoch)
}

// Return the duration since the `epoch`.
public fun since-epoch( i : instant ) : duration {
  Duration(i.secs,i.frac)
}

public fun (<)( i : instant, j : instant )  : bool { compare(i,j) == Lt }
public fun (<=)( i : instant, j : instant ) : bool { compare(i,j) != Gt }
public fun (>)( i : instant, j : instant )  : bool { compare(i,j) == Gt }
public fun (>=)( i : instant, j : instant ) : bool { compare(i,j) != Lt }
public fun (==)( i : instant, j : instant ) : bool { compare(i,j) == Eq }
public fun (!=)( i : instant, j : instant ) : bool { compare(i,j) != Eq }

// Create an instant in time given a number of seconds since the `epoch`
// and fraction of seconds (=``0.0``) in the range ``[0.0,1.0>``.
public fun instant-at( secs : int, frac : double = 0.0 ) : instant{
  Instant(secs,frac)
}

// Create an instant given a number of seconds since the `epoch`
// given as a `:double`.
public fun instant-at( secs : double ) : instant {
  instant-at( secs.floor.int, secs.fraction )
}

// Create an instant given a [julian date](https://en.wikipedia.org/wiki/Julian_day).
public fun instant-at-jd( julian-date: double ) : instant {
  instant-at-mjd( julian-date - mjd-epoch-shift )
}

// Create an instant given a [modified julian date](https://en.wikipedia.org/wiki/Julian_day).\
// modified-julian-date = julian-date - 2400000.5
public fun instant-at-mjd( modified-julian-date : double ) : instant {
  val delta = duration( modified-julian-date * secs-per-uday.double )
  mjd-epoch + delta
}

// Return the [julian date](https://en.wikipedia.org/wiki/Julian_day) of an instant.
public fun jd( i : instant ) : double {
  i.mjd + mjd-epoch-shift
}

// Return the [modified julian date](https://en.wikipedia.org/wiki/Julian_day) of an instant.
public fun mjd( i : instant ) : double {
  val delta = i - mjd-epoch
  (delta.seconds / secs-per-uday.double)
}

// Get the instant in time of a given `:time` value.
public fun instant( t : time ) : instant {
  t.tinstant
}

// Create a duration given a number of seconds and fraction of seconds.
public fun duration( secs : int, frac : double = 0.0 ) : duration {
  Duration( secs + frac.floor.int, frac.fraction )
}

// Create a duration given a number of seconds as a `:double`.
public fun duration( secs : double ) : duration {
  Duration( secs.floor.int, secs.fraction )
}


// The _epoch_ is the unix epoch on 1970-01-01Z.
public val epoch = Instant(0,0.0)

// The current `:instant` in time.
public extern now() : ndet instant {
  js "_now"
}

// The resolution in seconds of the system clock.
public fun now-resolution() : ndet double {
  xnow-resolution()
} 

extern xnow-resolution() : ndet double {
  js "_now_resolution"
}

// Show an instant with an optional precision (=`9`, nano-second resolution).
public fun show( i : instant, prec : int = 9 ) : string {
  val leap = i.frac.floor.int  
  i.secs.show + i.frac.show-frac(9) + "s" +
   (if (leap.zero?) then "" else " " + (if (leap.pos?) then "+" else "-") + leap.abs.show)
}

// Show a duration with an optional precision (=`9`, nano-second resolution).
public fun show( i : duration, prec : int = 9 ) : string {   
  i.secs.show + i.frac.show-frac(9) + "s"
}

// Show a fraction of a second up to an optional precision (=`9`)
fun show-frac( frac : double, prec : int = 9 ) : string {
  if (frac.fraction.zero?) return ""
  val xdigits = frac.fraction.show-fixed(prec).list.drop(2)
  val digits  = xdigits.reverse.drop-while(fun(d){d=='0'}).reverse
  if (digits.nil?) return ""
  // always use a multiple of 3 to display a fraction
  val len3 = min(prec, ((digits.length + 2) / 3) * 3)
  "." + digits.string.pad-right( len3, '0') 
}
val rxdigits = regex(@"^\d+\.\(\d+[1-9]\)0*$")

// pad with zeros
fun show0( i : int, width : int = 2) : string {
  i.show.pad-left(width,'0')
}

// pad with zeros
fun show0( i : int32, width : int = 2) : string {
  i.show.pad-left(width,'0')
}

// Return the hours, minutes, seconds, and fraction of the second.
fun clock( t : time ) : (int,int,int,double) {
  (t.hours.int,t.mins.int,t.secs.int,t.frac)
}

// Return the year, month, and day.
fun date( t : time ) : (int,int,int) {
  (t.year,t.month.int,t.day.int)
}

// Show a `:time` in [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) format.
public fun show(t : time, prec : int = 9 ) : string {
  val yr = (if (t.year>9999) then "+" elif (t.year.neg?) then "-" else "") + t.year.abs.show0(4)
  yr + "-" + t.month.show0 + "-" + t.day.show0 + "T" +
   t.hours.show0 + ":" + t.mins.show0 + ":" + t.secs.show0 + 
   t.frac.show-frac(prec) + show-tzofs(t.tzofs)
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
  val tzs = if (secs.zero?) then "" else ":" + secs.floor.int32.show0 + secs.fraction.show-frac(3)
  tz + tzs
}

// A `:timezone` determines a time offset with respect to [UTC time](https://en.wikipedia.org/wiki/Coordinated_Universal_Time).\
// The `utc` and `local` time zones are used for UTC time and the local system time.\
// There are also a few special timezones, in particular, `tai`, `gps`, and `terrestrial`
// to provide TAI, GPS, and TT time.
public struct timezone(
  utc-offset : (instant) -> duration,
  utc-inverse: (instant) -> maybe<instant> = fun(i) { Nothing }
)

// Create a time zone with a fixed offset in seconds from UTC.
public fun timezone-fixed( secs : int ) : timezone {
  Timezone( 
    fun(i) { duration(secs) }, 
    fun(i) { if (secs.zero?) then Just(i) else Nothing }
  )
}

// The standard UTC time zone with a 0 offset.
public val utc : timezone = timezone-fixed(0)

public val pst : timezone = timezone-fixed(~25200)

// Return the local timezone on the current system.
public fun local() : ndet timezone {
  val tz = local-get-timezone()
  Timezone( fun(i) { 
    val ofs = local-utc-offset(tz, i.since-epoch.seconds) 
    Duration(ofs.floor.int,ofs.fraction)
  })
}

// A local timezone structure.
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


fun leap?(d : double ) {
  d.abs >= 1.0
}

// Add a duration to an instant in time.
public fun (+)( i : instant, d : duration ) : instant {
  if ((d.frac.leap? && !i.frac.leap?) || (!d.frac.leap? && i.frac.leap?)) {
    // maintain leap second info if either instant or duration have a zero fraction
    Instant( i.secs + d.secs, i.frac + d.frac )  
  }
  else {
    // otherwise we normalize the fraction between <-1.0,1.0>
    val f     = i.frac + d.frac
    val secs  = i.secs + d.secs + f.floor.int
    Instant(secs,f.fraction)  
  }
}

// Negate a duration.
public fun (~)( d : duration ) : duration {
  Duration(~d.secs,~d.frac)
}

// Subtract a duration from an instant in time.
public fun (-)( i : instant, d : duration ) : instant {
  i + ~d
}

// Return the difference between to instants in time.
public fun (-)( i : instant, j : instant ) : duration {
  i.since-epoch - j.since-epoch
}

// Add two durations.
public fun (+) ( d : duration, e : duration ) : duration {
  (Instant(d.secs,d.frac) + e).since-epoch
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

val epoch-shift = 719468  // shift epoch from 1970-01-01 to 0000-03-01
val days-in-era = 146097  // 365 * 400 + 100 - 3

// Convert a date in the proleptic gregorian calendar to 'unix days' since the `epoch`.
fun date-from-udays( udays : int ) : (int,int32,int32) {
  val z    = udays + epoch-shift           // shift epoch from 1970-01-01 to 0000-03-01
  val (era,idoe) = divmod(z,days-in-era)  // day of era: 0 <= doe < days-in-era
  val doe  = idoe.int32
  val yoe  = (((doe - (doe/1460.int32)) + (doe/36524.int32)) - (doe/146096.int32)) / 365.int32;  // year of era: 0 <= yoe < years-in-era 
  val doy  = doe - ((yoe*365.int32) + (yoe/4.int32) - (yoe/100.int32))   // day-of-year: 0 <= doy <= 365
  val mp   = ((doy*5.int32) + 2.int32)/153.int32                         // 0 <= mp <= 0,11
  val day  = (doy - (((mp*153.int32) + 2.int32) / 5.int32)) + 1.int32    // 1 <= day <= 31
  val month= mp + (if (mp < 10.int32) then 3.int32 else ~9.int32)        // 1 <= month <= 12
  val year = yoe.int + (400*era) + (if (month <= 2.int32) then 1 else 0)
  (year,month,day)
}

// Convert an instant to days and seconds since the `epoch`.
fun instant-udays( i : instant ) : (int,int32) {
  val (udays,secs) = divmod(i.secs,secs-per-uday)
  (udays,secs.int32)
}

// Convert an instant in time to a `:time` value in a given timezone `tz` (=`utc` by default)
public fun time( i : instant, tz : timezone = utc ) : time {
  val tzofs : duration = (tz.utc-offset)(i)
  val tzi   = i + tzofs
  val (udays,daysecs) = tzi.instant-udays
  val (year,month,day)= date-from-udays(udays)
  val (hours,minsecs) = divmod(daysecs,3600.int32)
  val (mins,xsecs)    = divmod(minsecs,60.int32)
  val tsecs            = xsecs + tzi.frac.floor.int32  // adjust for leap seconds
  val tfrac            = tzi.frac.fraction
  Time(year,month,day,hours,mins,tsecs,tfrac,tzofs.seconds,i,tz)
}

// Calculate unix days from a year/month/day triple. 
// `month` must be between 1 and 12. `day` must be between 1 and `days-in-era`.
fun udays-from-datex( year : int, month : int32, day : int32 ) : int {
  val y = if (month <= 2.int32) then year.dec else year   // to internal year starting in Feb
  val (era,iyoe) = divmod(y,400)                    // year of era: 0<= yoe < 400
  val yoe  = iyoe.int32
  val mdoy = (((153.int32 * (month + (if (month > 2.int32) then ~3.int32 else 9.int32))) + 2.int32)/5.int32) 
  val doe  = (((yoe*365.int32) + (yoe/4.int32)) - (yoe/100.int32)) + mdoy + day.dec
  ((days-in-era * era) + doe.int) - epoch-shift
}

// Return unix days from a year, month, and day triple.
fun udays-from-date( year : int, month : int, day : int ) : int {
  val (xera,doe)  = divmod(day.dec,days-in-era)
  val (xyear,moy) = divmod(month.dec,12)
  udays-from-datex( year + (xera * 400) + xyear, moy.int32.inc, doe.int32.inc )
}

// Return the instant in time for a given UTC date.
fun utc-instant( year : int, month : int = 1, day : int = 1, 
                 hours : int = 0, minutes : int = 0, secs : int = 0, frac : double = 0.0) : instant 
{
  val (xdays,xsecs) = divmod( (((hours*60) + minutes)*60) + secs, secs-per-uday )
  val udays = udays-from-date(year, month, day + xdays )
  Instant(udays*secs-per-uday + xsecs, frac)
}

// Return the instant in time for a given date and clock in a timezone `tz` (=`utc` by default)
public fun instant( year : int, month : int = 1, day : int = 1, 
                    hours : int = 0, minutes : int = 0, secs : int = 0, 
                    frac : double = 0.0, tz : timezone = utc ) : instant 
{
  // the year/month/day is interpreted as utc first.
  val i = utc-instant(year,month,day,hours,minutes,secs,frac)
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

// Return the `:time` value for a given date and clock in a timezone `tz` (=`utc` by default) 
public fun time( year : int, month : int = 1, day : int = 1, 
                 hours : int = 0, minutes : int = 0, secs : int = 0, 
                 frac : double = 0.0, tz : timezone = utc ) : time 
{
  time(instant(year,month,day,hours,minutes,secs,frac,tz),tz)
}


// -----------------------------------------------------------
// GPS & TT timezone
// -----------------------------------------------------------

// Return the default GPS timezone (using the default leap second list from `tai`).
// GPS time is defined as TAI - 19s.
// Cached, so calling `gps()` multiple time is cheap.\
// Use `gps-from` to instantiate a GPS timezone from a recent leap second list. 
// ````plain
// > time( instant(2017,1,1,0,0,18,tz=gps()) )
// 2017-01-01T00:00:00.000000000Z
//
// > time( instant(2017,1,1,0,0,18), tz=gps() )
// 2017-01-01T00:00:36.000000000+00:00:18.000
// ````
// .
public val gps : (() -> timezone) = once{
  tai().timezone-adjust(Duration(~19))
}

// Create GPS timezone given a leap second list after 1972-01-01Z.\
// For example: <https://www.ietf.org/timezones/data/leap-seconds.list>.
public fun gps-from( leaps-list : string ) : timezone {
  tai-from(leaps-list).timezone-adjust(Duration(~19))
}

// Return the default TT (terrestrial time) timezone (using the leap second list from `tai`).
// TT time is defined as TAI + 32.184s.
// Cached, so calling `terrestrial()` multiple time is cheap.\
// Use `terrerstiral-from` to instantiate a TT timezone from a recent leap second list. 
public val terrestrial : (() -> timezone) = once{
  tai().timezone-adjust(Duration(32,0.184))
}


// Create TT timezone given a leap second list after 1972-01-01Z.\
// For example: <https://www.ietf.org/timezones/data/leap-seconds.list>.
public fun terrestrial-from( leaps-list : string ) : timezone {
  tai-from(leaps-list).timezone-adjust(Duration(32,0.184))
}

fun timezone-adjust( tz : timezone, d : duration ) : timezone {
  Timezone(
    fun(i){ (tz.utc-offset)(i) + d },
    fun(i){ (tz.utc-inverse)(i - d) }
  )
}

// -----------------------------------------------------------
// TAI timezone
// -----------------------------------------------------------

// A leap second table is a list of tuples. 
// Each entry gives the start instant and integer leap second adjustment.
alias leaps-table = list<(instant,int)>

// Whole leap seconds started in 1971-01-01Z
val utc1972    = Instant(63072000,0.0) // 1972-01-01Z

// Return the default [TAI](https://en.wikipedia.org/wiki/International_Atomic_Time) 
// timezone (currently valid until 2017-06-28Z).\
// The timezone value is cached automatically and calling `tai()` multiple times is cheap.
// Use `tai-from` to construct a TAI `:timezone` from a most recent leap second 
// list from <https://www.ietf.org/timezones/data/leap-seconds.list>.
// ````plain
// > time( instant(2017,1,1,0,0,35,0.5,tz=tai()) )
// 2016-12-31T23:59:59.500000000Z
//
// > time( instant(2017,1,1,0,0,36,0.5,tz=tai()) )
// 2016-12-31T23:59:60.500000000Z   // 0.5s into the leap second!
//
// > time( instant(2017,1,1,0,0,37,0.5,tz=tai()) )
// 2017-01-01T00:00:00.500000000Z
// ````
// You can see the TAI offset from UTC by displaying the time in the `tai` timezone:
// ````plain
// > time( instant(2017,1,1,0,0,38,tz=tai()), tz=tai() )
// 2017-01-01T00:00:38.000000000+00:00:37.000
// ````
// Correctly calculates TAI offsets for dates before 1972-01-01Z with fractional
// leap seconds. See <http://maia.usno.navy.mil/ser7/tai-utc.date> for the table
// of leap second adjustments.
// ````plain
// > time( instant(1972,1,1,0,0,0,tz=tai()), tz=tai() )
// 1972-01-01T00:00:00.000000000+00:00:09.890
// 
// > time( instant(1972,1,1,0,0,10,tz=tai()) )
// 1972-01-01T00:00:00.000000000Z
// ````
// Treats the discontinuities around 1961 and 1972 as fractional
// leap seconds too. The periods where this occurs are:
// 1961-01-01 TAI to 1961-01-01T00:00:01.422818 TAI  and
// 1972-01-01T00:00:09.892 TAI to 1972-01-01T00:00:10 TAI; these periods are mapped 
// to a leap 'second' before 1961-01-01Z and 1972-01-01Z respectively. This means
// that the leap 'second' becomes 1.422818s in 1961 and
// 0.118s in 1972. For example:\
// ````plain
// > time( instant(1961,1,1,0,0,1,0.2,tz=tai()) )
// 1960-12-31T23:59:61.200000000Z   // 1.2s into the leap second!
//
// > time( instant(1961,1,1,0,0,1,0.422818,tz=tai()) )
// 1961-01-01T00:00:00.000000000Z
// ````
// .
public val tai : (() -> timezone) = once{ tai-from(nist-leap-seconds-x20170628) }

// Return the TAI timezone given a leap second list for dates after 1972-01-01Z.\
// This should be formatted according to the NIST leap second list file.\
// See <https://www.ietf.org/timezones/data/leap-seconds.list>  
public fun tai-from( leaps-list : string ) : timezone 
{
  val leaps = parse-leap-seconds(leaps-list) 
  
  fun utc-offset(i) {
    if (i >= utc1972) 
      then tai-offset(i,leaps) 
      else tai-offset-pre72(i)
  }
  fun utc-inverse(i : instant ) {
    if (i >= utc1972x10) 
     then tai-inv-offset(i,leaps)
     else tai-inv-offset-pre72(i)
  }
  Timezone(utc-offset,utc-inverse)
}

// Return the TAI-offset for a given UTC instant after 1972-01-01Z.
fun tai-offset( i : instant, leaps : leaps-table ) : duration {
  val adj = leaps.lookup( fun(utc-start) { utc-start <= i } ).default(0)
  Duration(adj)
}

// Convert a TAI instant back to a UTC instant. 
// This may result in a UTC instant in a leap second.
// Expects a leap second table starting at the most recent entry.
// 1972-01-01Z <= `i` 
fun tai-inv-offset( i : instant, leaps : leaps-table ) : maybe<instant> 
{
  fun iter( lps ) {
    match(lps) {
      Nil -> Just(i) 
      Cons((start,adj:int),rest) {
        if (i < start) then iter(rest) else {
          val utc-i = i - Duration(adj)
          if (utc-i >= start) 
           then Just(utc-i) 
           else {
            // leap-second cross-over
            val adj-prev = rest.head.map(snd).default(adj - 1)
            val utc-prev = i - Duration(adj-prev)
            if (utc-prev < start) 
             // use previous leap-second offset
             then Just(utc-prev)
             // this is inside an added leap-second, add as fraction
             else Just( utc-i(frac = utc-i.frac + (adj - adj-prev).double) ) 
           }
        }
      }
    }
  }
  iter(leaps)
}

// pre 1972 leap second adjustments. For an instant `i` after `start`:\
// ``TAI-offset = offset + (delta * days(i - delta-start))``
struct taiadjust(
  utc-start  : instant,   // start time
  offset     : double,    // base offset
  delta-start: instant,   // start of delta adjustment
  delta      : double     
)

// Return a TAI offset for a UTC instant < 1972-01-01Z
fun tai-offset-pre72( i : instant ) : duration {
  val leaps : list<taiadjust> = tai-leaps-pre72() // cached, on-demand
  match( leaps.find( fun(ta){ ta.utc-start <= i } ) ) {
    Nothing -> Duration(0)
    Just(ta)-> {
      val days = floor((i - ta.delta-start).seconds / secs-per-uday.double)
      val secs = ta.offset + (ta.delta * days)      
      Duration( secs.floor.int, secs.fraction )
    }
  }
}

// Convert a TAI instant <= 1972-01-01T00:00:10TAI to a UTC instant.
// There are two discontinuities around the start and end of this
// period:
// 1961-01-01 TAI to 1961-01-01T00:00:01.422818 TAI  and
// 1972-01-01T00:00:09.892 TAI to 1972-01-01T00:00:10 which don't
// map back to a valid UTC time; here we map those periods to a 
// leap 'second' before 1961-01-01Z and 1972-01-01Z respectively. This means
// that the leap 'second' does not have to be 1s but is 1.422818s in 1961 and
// 0.118s in 1972. For example:\
// 1961-01-01T00:00:01.2 TAI maps to 1960-12-31T23:59:61.2Z 
fun tai-inv-offset-pre72( i : instant ) : maybe<instant> {
  if (i > utc1972x9 && i <= utc1972x10) {
    val frac = (i - utc1972x9).seconds.fraction
    // assert(0.0 <= frac && frac < (1.0 - 0.892))
    Just(instant(1971,12,31,23,59,59,1.0 + frac))
  }
  elif (i>=utc1961 && i<utc1961x1) {
    val frac = (i - utc1961).seconds
    // assert(0.0 <= frac && frac <= 1.422818)
    Just(instant(1960,12,31,23,59,59,1.0 + frac))
  }
  else Nothing
}
val utc1972x10 = utc1972 + Duration(10)
val utc1972x9  = utc1972 + Duration(9,0.892)
val utc1961    = Instant(~283996800,0.0)  // 1961-01-01Z
val utc1961x1  = utc1961 + Duration(1,0.422818)



// -----------------------------------------------------------
// Parsing TAI leap second tables
// -----------------------------------------------------------

// [NTP](https://en.wikipedia.org/wiki/Network_Time_Protocol) epoch (1900-01-01Z)
public val ntp-epoch = instant-at(~ntp-epoch-shift)
val ntp-epoch-shift  = 2208988800  // epoch - ntp-epoch

// Parse a NIST leap second table.
// See: <https://www.ietf.org/timezones/data/leap-seconds.list>
fun parse-leap-seconds( leaps : string ) : leaps-table {
  leaps.find-all(rxleap).map fun(cap) {
    val ntpsecs = cap.groups[1].parse-int-default(ntp-epoch-shift)  
    val adjust  = cap.groups[2].parse-int-default(0)
    val ntpi    = instant-at(ntpsecs - ntp-epoch-shift)
    //trace("leap entry: " + adjust.show + " " + ntpi.time.show + " " + (ntpsecs - ntp-epoch-shift).show )
    (instant-at(ntpsecs - ntp-epoch-shift), adjust)
  }.reverse
}
val rxleap = regex(@"^[ \t]*(\d+)[ \t]+(\d+)[ \t]*(?:#.*)?$",multiLine=True)

// NIST leap second data valid until 2017-06-28
val nist-leap-seconds-x20170628 = @"
  # From: https://www.ietf.org/timezones/data/leap-seconds.list
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
val tai-leaps-pre72 : (() -> list<taiadjust>) = once{ parse-tai-utc-dat() }

// Parse the standard TAI adjustment file.\
// See <http://maia.usno.navy.mil/ser7/tai-utc.date>
fun parse-tai-utc-dat( s : string = tai-utc-dat-y1968 ) : list<taiadjust> {
  s.find-all(rxtaiadjust).map( fun(cap) {
    val mjd    = cap.groups[1].parse-double-default(mjd-epoch-shift) - mjd-epoch-shift
    val ofs    = cap.groups[2].parse-double-default(0.0)
    val dmjd   = cap.groups[3].parse-double-default(0.0)
    val delta  = cap.groups[4].parse-double-default(0.0)
    val start  = mjd-epoch + Duration(mjd.int*secs-per-uday)  //instant(1858,11,17 + mjd.int)
    val dstart = mjd-epoch + Duration(dmjd.int*secs-per-uday) //instant(1858,11,17 + dmjd.int)
    trace("start=" + time(start).show )
    Taiadjust( start, ofs, dstart, delta )
  }).reverse
}
val rxtaiadjust = regex(@"^ *\d[^=]*=JD (\d+\.\d+) *TAI-UTC= *(\d+\.\d+)[^\d]+(\d+)[^\d]+(\d+\.\d+) *S *$", multiLine=True)
val mjd-epoch-shift = 2400000.5
val mjd-epoch       = Instant(~3506716800,0.0)  // 1858-11-17Z modified julian date epoch.

// TAI leap second adjustments for dates before 1972-01-01Z
val tai-utc-dat-y1968 = @"
  # from: http://maia.usno.navy.mil/ser7/tai-utc.date
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