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


// The _epoch_ is 1958-01-01Z TAI which equals 1958-01-01Z UTC.\
// Nice properties:
//
// - for any date <  1960-01-01Z, we have UTC == TAI
// - for any date >= 1972-01-01Z, we have UTC == TAI - whole SI second offset
public val epoch = instant-since(0.0,utc)

alias ts-duration = duration

public struct time-scale(
  name      : string,
  from-tai  : duration -> ts-duration,
  to-tai    : ts-duration -> duration,
  from-utc  : ts-duration -> ts-duration,
  to-utc    : ts-duration -> ts-duration,
  allow-leap: bool = False,
  epoch-shift: ts-duration = duration0
)


fun utc-timescale( name : string, epoch-shift : ts-duration = duration0 ) : time-scale {
  fun from-utc(d:ts-duration) { ts-add( d, ~epoch-shift ) }
  fun to-utc(d:ts-duration)   { ts-add( d, epoch-shift) } 
  Time-scale(
    name,
    fun(d) { d.utc-from-tai.from-utc },         
    fun(d) { d.to-utc.tai-from-utc   },
    from-utc,
    to-utc,
    True,
    epoch-shift
  )
}

fun tai-timescale( name : string, epoch-shift : duration = duration0 ) : time-scale {
  fun from-tai(d:duration) { d - epoch-shift }
  fun to-tai(d:duration)   { d + epoch-shift }
  Time-scale(
    name,
    from-tai,
    to-tai,
    fun(d) { d.tai-from-utc.from-tai },
    fun(d) { d.to-tai.utc-from-tai   },
    False,
    epoch-shift
  )
}

public val utc    : time-scale = utc-timescale( "" )

public val unix   : time-scale = utc-timescale( "UNIX", duration(378691200) )

public val tai    : time-scale = tai-timescale( "TAI" )
 
public val gps    : time-scale = tai-timescale( "GPS", duration(694224019) )

public val ts-mjd : time-scale = utc-timescale( "MJD", duration(~3128025600) )

public val ts-jd  : time-scale = utc-timescale( "JD", duration(~207360043200) )

//public val jd  : time-scale = utc-timescale( "JD", duration() )

// Represents an instant in time. 
// Can be arbitrarily far into the future or past and is accurate up 
// to a tenth of a femto-second (10^-16s).
abstract struct instant (
  since: ts-duration,                       
  ts   : time-scale
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
  tz     : timezone = tz-utc
)

/*----------------------------------------------------------------------------
  Durations and instants
----------------------------------------------------------------------------*/

// A zero second duration.
public val duration0 : duration = Duration(0,0.0)

// Create a duration given a number of seconds and fraction of seconds.
public fun duration( secs : int, frac : double = 0.0 ) : duration {
  // maintain 0 <= frac 
  if (!frac.neg?) then
    Duration(secs,frac)
  else {
    val fl = frac.floor
    Duration(secs + fl.int, frac - fl)
  }
}

// Create a duration given a number of seconds as a `:double`.
public fun duration( secs : double ) : duration {
  val fl = secs.floor
  Duration( fl.int, secs - fl )
}

// Create an instant in time given a number of seconds since the `epoch`
// and fraction of seconds (=``0.0``).
public fun instant-since( secs : int, frac : double = 0.0, ts : time-scale = utc ) : instant {
  instant-sincex(secs,frac,ts)
}

private fun instant-sincex( secs : int, frac : double, ts : time-scale ) : instant {
  if (frac.neg? || (frac > 1.0 && !ts.allow-leap)) {
    val fl = frac.floor
    Instant(duration(secs + fl.int, frac - fl), ts)
  }
  else Instant(duration(secs,frac),ts)
}

// Create an instant given a number of seconds since the `epoch`
// given as a `:double`.
public fun instant-since( secs : double, ts : time-scale = utc ) : instant {
  Instant( duration(secs), ts )
}

// Return the duration since the `epoch`.
public fun since-epoch( i : instant ) : duration {
  val j = i.ts-use(tai)
  j.since
}

// Return the instant in time that is `d` duration after the `epoch`.
public fun since-epoch( d : duration ) : instant {
  Instant(d,tai)
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
  val h = j.ts-use(i.ts)
  compare(i.since,h.since)
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
  val f = d.frac + e.frac
  duration(d.secs + e.secs + f.trunc.int, f.fraction)
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

public fun value( d : duration ) : (int,double) {
  (d.secs,d.frac)
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

/*----------------------------------------------------------------------------
  Durations in a another time scale
----------------------------------------------------------------------------*/

fun ts-use( i : instant, tscale : time-scale ) : instant {
  //trace("ts-use: " + tscale.name + ", i.ts=" + i.ts.name)
  if (i.ts.name == tscale.name) then {
    i 
  }
  elif (tscale.allow-leap) {
    Instant( (tscale.from-utc)( (i.ts.to-utc)(i.since)), tscale )
  }
  else {
    Instant( (tscale.from-tai)( (i.ts.to-tai)(i.since) ), tscale)
  }
}

public fun ts-since-epoch( i : instant, ts : time-scale = i.ts ) : ts-duration {
  i.ts-use(ts).since
}

public fun ts-since-epoch( d : ts-duration, ts : time-scale ) : instant {
  Instant(d,ts)
}

public fun ts-add( d : duration, e : ts-duration ) : duration {
  ts-duration(d.secs + e.secs, d.frac + e.frac)
}

public fun ts-add( i : instant, d : ts-duration ) : instant {
  Instant(ts-add(i.since,d), i.ts)
}

public fun ts-diff( i : instant, j : instant ) : ts-duration {
  val d = i.since 
  val e = j.ts-use(i.ts).since
  ts-duration( d.secs + e.secs, d.frac + e.frac )
}

public fun ts-duration( secs : int, frac : double = 0.0 ) : ts-duration {
  duration(secs,frac)
}

public fun ts-duration( secs : double ) : ts-duration {
  duration(secs)
}


/*----------------------------------------------------------------------------
  Show durations and instants
----------------------------------------------------------------------------*/


// Show an instant with an optional precision (=`9`, nano-second resolution).
public fun show( i : instant, prec : int = 9 ) : string {
  show-scale(i.since) + "s " + (if (i.ts.name.empty?) then "" else " " + i.ts.name)
}

// Show a duration with an optional precision (=`9`, nano-second resolution).
public fun show( d : duration, prec : int = 9 ) : string {   
  //trace("show duration: " + i.secs.show + "s, " + i.frac.show)
  show-scale( d, prec ) + "s"
}

private fun show-scale( d : ts-duration, prec : int = 9 ) : string {
  val leap = d.frac.trunc.int  
  // trace("show duration: " + d.secs.show + "s + " + d.frac.show)
  show-frac(d.secs,d.frac,prec,1) 
    + (if (leap.zero?) then "" else " (" + (if (leap.pos?) then "+" else "-") + leap.abs.show + "leap)")
}


// Showing fractions is a bit involved due to rounding and 
// showing precision up to 9 digits in increments of 3 digits
fun show-frac( secs : int, frac : double, prec : int  = 9, width : int = 2 ) : string {
  if (prec==0) return secs.show0(width) 
  val fr   = frac.round-to-prec(prec)
  val fl   = fr.floor
  val (s,f) = if (secs.neg? && !fr.zero?) 
                then (secs + fl.int + 1, (fr - fl) - 1.0)
                else (secs + fl.int, fr - fl)
  val sign = if (s.neg? || s.zero? && fr.neg?) then "-" else "" 
  //trace("show secs: " + s.show + ", " + f.show)
  sign + s.abs.show0(width) + f.show-frac(prec) 
}

// Show a fraction of a second up to an optional maximum precision (=`9`)
fun show-frac( frac : double, prec : int = 9 ) : string {  
  //trace("show frac: " + frac.show)
  val xdigits = frac.abs.fraction.show-fixed(prec).list.drop(2)
  val digits  = xdigits.reverse.drop-while(fun(d){d=='0'}).reverse
  if (digits.nil?) return "" // zero fraction
  // always use a multiple of 3 to display a fraction
  val len3 = min(prec, ((digits.length + 2) / 3) * 3)
  "." + digits.string.pad-right( len3, '0') 
}
val rxdigits = regex(@"^\d+\.\(\d+[1-9]\)0*$")



/*----------------------------------------------------------------------------
  Julian Date
----------------------------------------------------------------------------*/

val jd-epoch-shift     = 2400000.5
val mjd-epoch : instant = instant-since(~mjd-epoch-shift,0.0,utc)  // 1858-11-17Z modified julian date epoch.
val mjd-epoch-shift    = 3128025600

// Create an instant given a [julian day](https://en.wikipedia.org/wiki/Julian_day).
public fun instant-at-jd( julian-date: double ) : instant {
  instant-at-mjd( julian-date - jd-epoch-shift )
}

// Create an instant given a [modified julian day](https://en.wikipedia.org/wiki/Julian_day).\
// modified-julian-date = julian-date - 2400000.5
public fun instant-at-mjd( modified-julian-date : double ) : instant {
  val frac = modified-julian-date.fraction * solar-secs-per-day.double
  val secs = modified-julian-date.trunc.int * solar-secs-per-day + frac.trunc.int 
  ts-since-epoch( duration(secs,frac.fraction), ts-mjd )
}

// Return the [julian day](https://en.wikipedia.org/wiki/Julian_day) of an instant.
public fun jd( i : instant ) : double {
  i.mjd + jd-epoch-shift
}

// Return the [modified julian day](https://en.wikipedia.org/wiki/Julian_day) of an instant.
public fun mjd( i : instant ) : double {
  val d = i.ts-since-epoch(ts-mjd)
  val (days,secs) = divmod(d.secs,solar-secs-per-day)
  days.double + ((secs.double + d.frac) / solar-secs-per-day.double)
}


/*
/*----------------------------------------------------------------------------
  Now
----------------------------------------------------------------------------*/

val unix-epoch-shift = 63072000.0   // 1972-01-01 TAI - 1970-01-01 TAI

// Convert a unix time stamp to an instant in time.
// A unix time stamp measure seconds since the Unix epoch (1970-01-01)
// and ignores any leap seconds.
public fun instant-at-unix( timestamp : double ) : instant {
  tai-from-utc( instant-at( timestamp - unix-epoch-shift ) )
}

// Convert an instant to a unix time stamp in seconds since the
// unix epoch (1970-01-01) ignoring any leap seconds.
public fun unix( i : instant ) : double {
  val d = utc-from-tai( i ).since-epoch.seconds
  d + unix-epoch-shift
}

//val ntp-epoch-shift = 2272060800.0 // 1900-01-01 TAI - 1972-01-01 TAI

public fun instant-at-ntp( timestamp : double ) : instant {
  tai-from-utc( instant-at( timestamp - ntp-epoch-shift.double ) )
}

public fun ntp( i : instant ) : double {
  val d = utc-from-tai( i ).since-epoch.seconds
  d + ntp-epoch-shift.double
}

// The current `:instant` in time.
public fun now() : ndet instant {
  instant-at-unix( unix-now() ) 
}


// Returns a unix time stamp; this still needs
// to be adjusted to our epoch and taking account of leap seconds.
private extern unix-now() : ndet double {
  js "_unix_now"
}

// The resolution in seconds of the system clock.
public fun now-resolution() : ndet double {
  xnow-resolution()
} 

extern xnow-resolution() : ndet double {
  js "_now_resolution"
}

*/

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
   show-frac(t.secs,t.frac,prec,2) + show-tzofs(t.tzofs ) +
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
  tz-name    : string,                      
  utc-offset : (instant) -> ts-duration,
  utc-inverse: (instant) -> maybe<instant> = fun(i) { Nothing }
)

public fun tz-utc?( tz : timezone ) : bool {
  (tz.tz-name == "UTC")
}

// Create a time zone with a fixed offset in seconds from UTC.
public fun timezone-fixed( name : string, secs : int, utc-name : string = "" ) : timezone {
  Timezone( 
    name,            
    fun(i) { ts-duration(secs) }, 
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
      ts-duration(ofs)
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
  val (days,daysecs)   = divmod(i.since.secs, solar-secs-per-day)
  val date             = date-from-days(days)
  val (hours,minsecs)  = divmod(daysecs,3600)
  val (mins,xsecs)     = divmod(minsecs,60)
  val tsecs            = xsecs + i.since.frac.trunc.int  // adjust for leap seconds
  val tfrac            = i.since.frac.fraction
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
      val tzofs1 = (tz.utc-offset)(utc-i)
      val tzi1   = utc-i - tzofs1
      val tzofs2 = (tz.utc-offset)(tzi1)
      val tzi2   = utc-i - tzofs2
      tzi2     
    }
  }
}


// Return a `:time` for a given instant.
fun timez( tzi : instant, tzofs : duration = duration0, tz : timezone = tz-utc, cal : calendar = cal-iso, i : instant = tzi) : time {
  val (d,c) = (cal.dc-from-instant)( tzi )
  Time(d.year,d.month,d.day,c.hours,c.mins,c.secs,c.frac,
       i, cal, tzofs.seconds, tz )
}


// Convert an `:instant` to a `:time` value in a given timezone `tz` (=`utc` by default)
// and calendar (=`iso` by default).
public fun time( i : instant, tz : timezone = tz-utc, cal : calendar = cal-iso ) : time {
  if (tz.tz-utc?) then timez(i,duration0,tz,cal,i) else {
    // use UTC timescale
    val utc-i = i.ts-use(utc)
    val tzofs : ts-duration = (tz.utc-offset)(utc-i)
    val tzi   = utc-i + tzofs
    timez( tzi, tzofs, tz, cal, utc-i )
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


// -----------------------------------------------------------
// UTC calendar leap second calculation
//
// Since the timescales `tai` and `utc` refer to `utc-from-tai`
// and `tai-from-utc` these routines strictly work in durations
// and cannot refer to instants or otherwise the definitions become
// mutually recursive.
// -----------------------------------------------------------

// A leap second table is a list of tuples. 
// Each entry gives the start instant and integer leap second adjustment.
struct leaps-table(
  expire : ts-duration,       // since epoch                   
  adjusts: list<leap-adjust>
)

// Leap second adjustments. For an instant `i` after `start`:\
// ``TAI-offset = offset + (delta * days(i - delta-start))``
struct leap-adjust(
  utc-start  : ts-duration,  // start time in UTC seconds since 1972-01-01Z
  offset     : double,    // base offset
  delta-start: ts-duration = duration0,   // start of delta adjustment
  delta      : double  = 0.0   
)

val zero : leap-adjust = Leap-adjust(duration0,0.0,duration0,0.0)

// Get the leap-second adjustment
fun tai-offset( la : leap-adjust, utc-i : ts-duration ) : duration {
  val secs = if (la.delta.zero?) then la.offset else {
               // pre 1972 is a rubber leap second
               val days = floor((utc-i - la.delta-start).seconds / solar-secs-per-day.double)
               la.offset + (la.delta * days)          
             }
  duration(secs)  
}

// Set up a global leap second table.
// We are going to pretend that this is total even though for future
// dates utc-tai conversion might be off by some future leap seconds.
val global-leaps-table : () -> ref<global,leaps-table> = once{
  unsafe-total{ 
    val pre72  = leap-seconds-pre72()
    val post72 = parse-leap-seconds( default-ietf-leap-seconds )
    trace("parse leap tables: " + pre72.length.show)
    ref(post72(adjusts = post72.adjusts + pre72))  // reverse order
  }
}

// Get the global leap second table
fun get-leapsecs() : st<global> leaps-table {
  !(global-leaps-table())
}


// Return a `:leap-instant` for a UTC instant
fun tai-from-utc( d : ts-duration ) : duration {
  val leaps = unsafe-total(get-leapsecs)
  leaps.adjusts.find-maybe(fun(la) {
    if (la.utc-start > d) then Nothing else {
      val ofs = la.tai-offset(d)
      // trace("found: " + i.show + " >= " + la.utc-start.show + ", ofs:" + ofs.show)
      Just( d + ofs )
    }
  }).default(d)  
}

fun utc-from-tai( i : duration ) : ts-duration {
  trace("utc-from--tai: " + i.show)
  val leaps = unsafe-total(get-leapsecs)
  if (i < duration0) then i else {
    find-utc-from-tai( i, leaps.adjusts )
  }
}
//val tai1961    = duration(~347068800 - 2)  // 1961-01-01Z minus 2 seconds for the initial leap

fun find-utc-from-tai( i : duration, leaps : list<leap-adjust> ) : ts-duration {
  match(leaps) {
    Nil -> i // should never happen
    Cons(la,earlier) -> {
      val ofs   = la.tai-offset(i)
      val utc-i = i - ofs
      val max   = if (ofs.secs.neg?) then utc-i else i
      // trace("check: utc-i: " + utc-i.show + ", start: " + la.utc-start.show + ", ofs: " + ofs.show + ", i: " + i.show)
      if (la.utc-start > max) then find-utc-from-tai(i,earlier) else {
        if (la.utc-start <= utc-i) then {
          // in the time frame
          trace(" utc-from-tai: found: utc: " + utc-i.show + ", tai-ofs: " + ofs.show )
          utc-i
        }
        else {
          // leap-second crosses-over into other time frame            
          // get adjustment using the earlier frame too
          trace("found crossover: " + utc-i.show + ", ofs: " + ofs.show + ", i: " + i.show )
          val la-before = earlier.head.default(zero)
          val ofs-before= la-before.tai-offset(i)
          val utc-prev  = i - ofs-before
          if (la.utc-start > utc-prev) then {
            // use previous leap-second offset            
            // trace(" use previous: " + utc-prev.show)
            utc-prev
          }
          else {
            // this is inside an added leap-second, encode as a fraction `> 1.0`.
            val diff = (ofs - ofs-before).seconds
            val utc-leap = utc-i( frac = utc-i.frac + diff)                            
            // trace(" use leap seconds: " + utc-leap.show + ", diff: " + diff.show)
            utc-leap
          }
        }
      }  
    }
  }
}



// -----------------------------------------------------------
// Parsing UTC leap second tables
// -----------------------------------------------------------

// [NTP](https://en.wikipedia.org/wiki/Network_Time_Protocol) epoch (1900-01-01Z)
public val ntp-epoch = instant-at(~ntp-epoch-shift)
val ntp-epoch-shift  = 1830297600 // (72*365 + 17)*86400 

// Parse a NIST (IETF) leap second table.
// See: <https://www.ietf.org/timezones/data/leap-seconds.list>
fun parse-leap-seconds( leaps : string ) : leaps-table {
  // get leap second adjustments
  val las = leaps.find-all(rxleap).map fun(cap) {
    val ntpsecs = cap.groups[1].parse-int-default(ntp-epoch-shift)  
    val adjust  = cap.groups[2].parse-double-default(0.0)
    //val ntpi    = instant-at(ntpsecs - ntp-epoch-shift)
    //trace("leap entry: " + adjust.show + " " + ntpi.show + " " + (ntpsecs - ntp-epoch-shift).show )
    val utc-i = duration(ntpsecs - ntp-epoch-shift)
    Leap-adjust(utc-i, adjust, duration0, 0.0)
  }.reverse

  // get expiration date
  val la-final   = las.head.default(zero)
  val utc-expire = leaps.find(rxexpire).map fun(cap) {
    val ntpex = cap.groups[1].parse-int-default(ntp-epoch-shift)
    duration(ntpex - ntp-epoch-shift)
  }.default(la-final.utc-start + duration(365*solar-secs-per-day))
  trace("expire: " + utc-expire.show)

  Leaps-table(utc-expire,las)
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
// 3723753600  35  # 1 Jan 2018 # testing

// -----------------------------------------------------------
// Parsing TAI 'fractional' leap second tables from before 1972
// -----------------------------------------------------------

// Cached leap second table for dates before 1972.
val leap-seconds-pre72 : (() -> list<leap-adjust>) = once{ parse-leap-seconds-dat() }


// Parse the standard UTC leap second adjustment file.\
// See <http://maia.usno.navy.mil/ser7/tai-utc.dat>
fun parse-leap-seconds-dat( s : string = leap-seconds-dat ) : list<leap-adjust> {
  s.find-all(rxtaiadjust).map( fun(cap) {
    val mjd    = cap.groups[1].parse-double-default(jd-epoch-shift) - (jd-epoch-shift )
    val ofs    = cap.groups[2].parse-double-default(0.0)
    val dmjd   = cap.groups[3].parse-double-default(0.0)
    val delta  = cap.groups[4].parse-double-default(0.0)
    val start  = duration(mjd.int*solar-secs-per-day - mjd-epoch-shift)  //instant(1858,11,17 + mjd.int)
    val dstart = duration(dmjd.int*solar-secs-per-day - mjd-epoch-shift) //instant(1858,11,17 + dmjd.int)
    // trace("pre72 start=" + start.show )
    Leap-adjust( start, ofs, dstart, delta )
  }).reverse
}
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
    Nil -> (0.0,[])
    Cons(start,rest) -> (start * 60.0, Cons(0.0,rest))
  }
  val population = parts[5].default("0").parse-base60
  if (diffs.length + 1 != indices.length ) error("invalid timezone data:\n  " + s )
  val endings = diffs.foldl((start0,[]), fun(acc:(double,list<maybe<instant>>),diff) {
                  val iu = acc.fst + (diff * 60.0)
                  val i = instant-at-unix(iu)
                  (iu,Cons(Just(i),acc.snd))
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

fun check(name,tst) {
  println(name + ": " + (if (tst()) then "ok" else "failed!"))
}

fun test-jd() {
  check("jd0"){ time(2000,1,1,12,0,0).instant.jd == 2451545.0 }
  check("jd1"){ time(2013,1,1,0,30,0).instant.jd == (2456293.5208333335) }
  check("jd2"){ instant-at-jd(0.0).time.show == "-4713-11-24T12:00:00Z" }
  check("jd3"){ instant-at-jd(0.0).time(cal=cal-julian).show == "-4713-01-01T12:00:00Z JC" }
}

fun test() {
  test-jd()
}