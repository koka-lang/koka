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

extern include {
  // cs file "time-inline.cs"
  js file "xtime-inline.js"
}

val secs-per-uday  = 86400
val nsecs-per-sec  = 1.0e9

abstract struct instant (
  secs : int,         // UTC seconds since epoch.
  frac : double       // fraction of seconds.
)

abstract struct duration (
  secs : int,
  frac : double = 0.0
)

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

public fun compare( i : instant, j : instant ) : order {
  compare(i.since-epoch,j.since-epoch)
}

public fun since-epoch( i : instant ) : duration {
  Duration(i.secs,i.frac)
}

public fun (<)( i : instant, j : instant )  : bool { compare(i,j) == Lt }
public fun (<=)( i : instant, j : instant ) : bool { compare(i,j) != Gt }
public fun (>)( i : instant, j : instant )  : bool { compare(i,j) == Gt }
public fun (>=)( i : instant, j : instant ) : bool { compare(i,j) != Lt }
public fun (==)( i : instant, j : instant ) : bool { compare(i,j) == Eq }
public fun (!=)( i : instant, j : instant ) : bool { compare(i,j) != Eq }

public fun instant-at( secs : int, frac : double = 0.0 ) : instant{
  Instant(secs,frac)
}

public fun instant( t : time ) : instant {
  t.tinstant
}



public val epoch = Instant(0,0.0)

public extern now() : ndet instant {
  js "_now"
}

public fun now-resolution() : ndet double {
  xnow-resolution()
} 

extern xnow-resolution() : ndet double {
  js "_now_resolution"
}

// Show an instant in nano-second resolution.
public fun show( i : instant ) : string {
  i.since-epoch.show  
}

// Show a duration in nano-second resolution.
public fun show( i : duration ) : string {   
  (i.secs).show + "." +    
  i.frac.show-frac + "s +" + i.frac.floor.int.show 
}

fun show-frac( frac : double, prec : int = 9 ) : string {
  frac.fraction.show-fixed(prec).first(2).after.string
}


fun show0( i : int, width : int = 2) : string {
  i.show.pad-left(width,'0')
}

fun show0( i : int32, width : int = 2) : string {
  i.show.pad-left(width,'0')
}

fun clock( t : time ) : (int,int,int,double) {
  (t.hours.int,t.mins.int,t.secs.int,t.frac)
}
/*
  val (hours,minsecs) = divmod(t.secs,3600.int32)
  val (mins,xsecs)    = divmod(minsecs,60.int32)
  val secs            = xsecs.int + t.frac.floor.int  // adjust for leap seconds
  val frac            = t.frac.fraction
  (hours.int,mins.int,secs,frac)
}
*/

fun date( t : time ) : (int,int,int) {
  (t.year,t.month.int,t.day.int)
}

public fun show(t : time ) : string {
  t.year.show + "-" + t.month.show0 + "-" + t.day.show0 + "T" +
  t.hours.show0 + ":" + t.mins.show0 + ":" + t.secs.show0 + "." +
  t.frac.show-frac(9) + show-tzofs(t.tzofs)
}

fun show-tzofs(ofs : double) : string {
  if (ofs.zero?) return "Z"
  val mins = (ofs.abs / 60.0).int32
  val tz = (if (ofs.neg?) then "-" else "+") + (mins/60.int32).show0 + ":" + (mins%60.int32).show0
  val secs = (ofs.abs % 60.0)
  val tzs = if (secs.zero?) then "" else ":" + secs.floor.int32.show0 + "." + secs.fraction.show-frac(3)
  tz + tzs
}

public struct timezone(
  utc-offset : (instant) -> duration,
  utc-inverse: (instant) -> maybe<instant>
)


public fun timezone-fixed( secs : int ) : timezone {
  Timezone( 
    fun(i) { Duration(secs) }, 
    fun(i) { if (secs.zero?) then Just(i) else Nothing }
  )
}

public val utc : timezone = timezone-fixed(0)
public val pst : timezone = timezone-fixed(~25200)


fun leap?(d : double ) {
  d.abs >= 1.0
}

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

public fun (~)( d : duration ) : duration {
  Duration(~d.secs,~d.frac)
}

public fun (-)( i : instant, d : duration ) : instant {
  i + ~d
}

public fun (-)( i : instant, j : instant ) : duration {
  i.since-epoch - j.since-epoch
}

public fun (+) ( d : duration, e : duration ) : duration {
  (Instant(d.secs,d.frac) + e).since-epoch
}

public fun (-)( d : duration, e : duration ) : duration {
  d + ~e
}

public fun seconds( d : duration ) : double {
  d.secs.double + d.frac
}

val epoch-shift = 719468  // shift epoch from 1970-01-01 to 0000-03-01
val days-in-era = 146097  // 365 * 400 + 100 - 3

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

fun instant-udays( i : instant ) : (int,int32) {
  val (udays,secs) = divmod(i.secs,secs-per-uday)
  (udays,secs.int32)
}

public fun time( i : instant, tz : timezone = utc ) : time {
  val tzofs = (tz.utc-offset)(i)
  val tzi   = i + tzofs
  val (udays,daysecs) = tzi.instant-udays
  val (year,month,day)= date-from-udays(udays)
  val (hours,minsecs) = divmod(daysecs,3600.int32)
  val (mins,xsecs)    = divmod(minsecs,60.int32)
  val secs            = xsecs + tzi.frac.floor.int32  // adjust for leap seconds
  val frac            = tzi.frac.fraction
  Time(year,month,day,hours,mins,secs,frac,tzofs.seconds,i,tz)
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

fun udays-from-date( year : int, month : int, day : int ) : int {
  val (xera,doe)  = divmod(day.dec,days-in-era)
  val (xyear,moy) = divmod(month.dec,12)
  udays-from-datex( year + (xera * 400) + xyear, moy.int32.inc, doe.int32.inc )
}

public fun utc-instant( year : int, month : int = 1, day : int = 1, 
                 hours : int = 0, minutes : int = 0, secs : int = 0, frac : double = 0.0) : instant 
{
  val (xdays,xsecs) = divmod( (((hours*60) + minutes)*60) + secs, secs-per-uday )
  val udays = udays-from-date(year, month, day + xdays )
  Instant(udays*secs-per-uday + xsecs, frac)
}

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

public fun time( year : int, month : int = 1, day : int = 1, 
                 hours : int = 0, minutes : int = 0, secs : int = 0, 
                 frac : double = 0.0, tz : timezone = utc ) : time 
{
  time(instant(year,month,day,hours,minutes,secs,frac,tz),tz)
}

val utc1972    = instant(1972,1,1)
val utc1972x10 = utc1972 + Duration(10)
val utc1972x9  = utc1972 + Duration(9,0.892)
val utc1961    = instant(1961,1,1)
val utc1961x1  = utc1961 + Duration(1,0.422818)

alias leaps-table = list<(instant,int)>

fun tai-offset( i : instant, leaps : leaps-table ) : duration {
  val adj = leaps.lookup( fun(utc-start) { utc-start <= i } ).default(0)
  Duration(adj)
}

fun tai-offset-pre72( i : instant ) : duration {
  val leaps = tai-leaps-pre72() // cached, on-demand
  match( leaps.find( fun(ta){ ta.utc-start <= i } ) ) {
    Nothing -> Duration(0)
    Just(ta)-> {
      val days = (i - ta.delta-start).seconds / secs-per-uday.double
      val secs = ta.offset + (ta.delta * days)      
      Duration( secs.floor.int, secs.fraction )
    }
  }
}

fun tai-inv-offset-pre72( i : instant ) : maybe<instant> {
  if (i > utc1972x9) {
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

public val tai : (() -> timezone) = once{ tai-from(nist-leap-seconds-y2016) }

public fun tai-from( leaps-text : string ) : timezone 
{
  val leaps = parse-leap-seconds(leaps-text) // + [(ntp-epoch,0.int32)]
  
  fun utc-offset(i) {
    if (i >= utc1972) 
      then tai-offset(i,leaps) 
      else tai-offset-pre72(i)
  }
  fun utc-inverse(i : instant ) {
    if (i >= utc1972x10) {
      var prev-utc-i := i
      var prev-start := i
      leaps.foreach-while fun(entry) {
        val (utc-start,adj) = entry
        val utc-i = i - Duration(adj)
        if (utc-i >= utc-start) then {
          if (utc-i >= prev-start) {
            // leap second
            prev-utc-i := (utc-i - Duration(1)) + Duration(0,1.0) 
          }
          else {
            prev-utc-i := utc-i
          }
          Just(()) 
        }
        else {
          // regular
          prev-utc-i := utc-i
          prev-start := utc-start
          Nothing
        }      
      }
      Just(prev-utc-i)
    }
    else tai-inv-offset-pre72(i)
  }
  Timezone(utc-offset,utc-inverse)
}

val ntp-epoch-shift  = 2208988800
public val ntp-epoch = instant-at(~ntp-epoch-shift)

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

// NIST leap second data as 2016-10-01Z
// From: https://www.ietf.org/timezones/data/leap-seconds.list
val nist-leap-seconds-y2016 = @"
  # Leap seconds as of 2016-10-14Z
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

struct taiadjust(
  utc-start  : instant,
  offset     : double,
  delta-start: instant,
  delta      : double
)

val tai-adjust0 = Taiadjust(epoch,0.0,epoch,0.0)

val tai-leaps-pre72 : (() -> list<taiadjust>) = once{ parse-tai-utc-dat() }

fun tai-leaps( i : instant, adj : taiadjust ) : double {
  if (i < adj.utc-start) return 0.0
  val days  = (i - adj.delta-start).seconds / secs-per-uday.double
  adj.offset + (adj.delta * days)
}

fun parse-tai-utc-dat( s : string = tai-utc-dat-y1968 ) : list<taiadjust> {
  s.find-all(rxtaiadjust).map( fun(cap) {
    val mjd    = cap.groups[1].parse-double-default(mjd-epoch-shift) - mjd-epoch-shift
    val ofs    = cap.groups[2].parse-double-default(0.0)
    val dmjd   = cap.groups[3].parse-double-default(0.0)
    val delta  = cap.groups[4].parse-double-default(0.0)
    // note: we can convert from modified julian day this way since the date is before the (non-leap) year 2000
    val start  = instant(1858,11,17 + mjd.int)
    val dstart = instant(1858,11,17 + dmjd.int)
    Taiadjust( start, ofs, dstart, delta )
  }).reverse
}
val rxtaiadjust = regex(@"^ *\d[^=]*=JD (\d+\.\d+) *TAI-UTC= *(\d+\.\d+)[^\d]+(\d+)[^\d]+(\d+\.\d+) *S *$", multiLine=True)
val mjd-epoch-shift = 2400000.5

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


/*
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
 1968 FEB  1 =JD 2439887.5  TAI-UTC=   4.2131700 S + (MJD - 39126.) X 0.002592 S
*/
/* 
 1972 JAN  1 =JD 2441317.5  TAI-UTC=  10.0       S + (MJD - 41317.) X 0.0      S
 1972 JUL  1 =JD 2441499.5  TAI-UTC=  11.0       S + (MJD - 41317.) X 0.0      S
 1973 JAN  1 =JD 2441683.5  TAI-UTC=  12.0       S + (MJD - 41317.) X 0.0      S
 1974 JAN  1 =JD 2442048.5  TAI-UTC=  13.0       S + (MJD - 41317.) X 0.0      S
 1975 JAN  1 =JD 2442413.5  TAI-UTC=  14.0       S + (MJD - 41317.) X 0.0      S
 1976 JAN  1 =JD 2442778.5  TAI-UTC=  15.0       S + (MJD - 41317.) X 0.0      S
 1977 JAN  1 =JD 2443144.5  TAI-UTC=  16.0       S + (MJD - 41317.) X 0.0      S
 1978 JAN  1 =JD 2443509.5  TAI-UTC=  17.0       S + (MJD - 41317.) X 0.0      S
 1979 JAN  1 =JD 2443874.5  TAI-UTC=  18.0       S + (MJD - 41317.) X 0.0      S
 1980 JAN  1 =JD 2444239.5  TAI-UTC=  19.0       S + (MJD - 41317.) X 0.0      S
 1981 JUL  1 =JD 2444786.5  TAI-UTC=  20.0       S + (MJD - 41317.) X 0.0      S
 1982 JUL  1 =JD 2445151.5  TAI-UTC=  21.0       S + (MJD - 41317.) X 0.0      S
 1983 JUL  1 =JD 2445516.5  TAI-UTC=  22.0       S + (MJD - 41317.) X 0.0      S
 1985 JUL  1 =JD 2446247.5  TAI-UTC=  23.0       S + (MJD - 41317.) X 0.0      S
 1988 JAN  1 =JD 2447161.5  TAI-UTC=  24.0       S + (MJD - 41317.) X 0.0      S
 1990 JAN  1 =JD 2447892.5  TAI-UTC=  25.0       S + (MJD - 41317.) X 0.0      S
 1991 JAN  1 =JD 2448257.5  TAI-UTC=  26.0       S + (MJD - 41317.) X 0.0      S
 1992 JUL  1 =JD 2448804.5  TAI-UTC=  27.0       S + (MJD - 41317.) X 0.0      S
 1993 JUL  1 =JD 2449169.5  TAI-UTC=  28.0       S + (MJD - 41317.) X 0.0      S
 1994 JUL  1 =JD 2449534.5  TAI-UTC=  29.0       S + (MJD - 41317.) X 0.0      S
 1996 JAN  1 =JD 2450083.5  TAI-UTC=  30.0       S + (MJD - 41317.) X 0.0      S
 1997 JUL  1 =JD 2450630.5  TAI-UTC=  31.0       S + (MJD - 41317.) X 0.0      S
 1999 JAN  1 =JD 2451179.5  TAI-UTC=  32.0       S + (MJD - 41317.) X 0.0      S
 2006 JAN  1 =JD 2453736.5  TAI-UTC=  33.0       S + (MJD - 41317.) X 0.0      S
 2009 JAN  1 =JD 2454832.5  TAI-UTC=  34.0       S + (MJD - 41317.) X 0.0      S
 2012 JUL  1 =JD 2456109.5  TAI-UTC=  35.0       S + (MJD - 41317.) X 0.0      S
 2015 JUL  1 =JD 2457204.5  TAI-UTC=  36.0       S + (MJD - 41317.) X 0.0      S
 2017 JAN  1 =JD 2457754.5  TAI-UTC=  37.0       S + (MJD - 41317.) X 0.0      S
 */

/*
extern include {
  cs file "time-inline.cs"
  js file "time-inline.js"
}

// A `:time` represents a certain time in a gregorian calendar.
// When a time value is created, it is associated with either the local 
// or UTC timezone which is used for displaying the time.\
// Valid times must be between `min-time` (= ``0001-01-01Z`` UTC) and 
// `max-time` (= ``9999-12-31T23:59:59.999Z`` UTC).
// If a time is created outside this range, the ``0001-01-01Z`` time is returned.
public type time

// Return the current time in the local timezone.
public extern now() : ndet time { 
  cs inline "DateTime.Now"
  js "_time_now"
}

// Return the current time in the UTC timezone.
public extern utc-now() : ndet time { 
  cs inline "DateTime.UtcNow"
  js "_time_utcnow"
}

// Convert a time to the UTC timezone.
// i.e. `utc-now() === now().utc`
public extern utc( t : time ) : time {
  cs inline "t.ToUniversalTime()"
  js "_time_to_utc"
}

// Convert a time to the local timezone.
public extern local( t : time ) : ndet time {
  cs inline "t.ToLocalTime()"
  js "_time_to_local"
}

// Is this time in the UTC timezone?
public extern utc?( t : time ) : bool {
  cs inline "(t.Kind == DateTimeKind.Utc)"
  js inline "(t.isutc)"
}

// Get the timezone offset in seconds. This 
// is the difference between the local time and UTC.
// For example, if the timezone is ``UTC+10:00``, the timezone offset is +10\*60\*60.
public extern timezone-offset(t : time) : int {
  cs "_Time.TimezoneOffset"
  js "_time_timezone_offset"
}

extern new-time(year:int, month : int, day:int,hours:int,minutes:int,seconds:int,milliseconds:int, is-utc:bool ) : time {
  cs "_Time.New"  
  js "_time_new"
}

extern new-time( seconds : double, is-utc:bool ) : time {
  cs "_Time.NewFromSeconds"  
  js "_time_new_from_seconds"
}

// The year in the associated timezone.
public extern year( t : time ) : int {
  cs "_Time.Year"
  js "_time_year"
}

// The month in the associated timezone.
public extern month( t : time ) : int {
  cs "_Time.Month"
  js "_time_month"
}

// The day in the associated timezone.
public extern day( t : time ) : int {
  cs "_Time.Day"
  js "_time_day"
}

// The hours in the associated timezone.
public extern hours( t : time ) : int {
  cs "_Time.Hours"
  js "_time_hours"
}

// The minutes in the associated timezone.
public extern minutes( t : time ) : int {
  cs "_Time.Minutes"
  js "_time_minutes"
}

// The seconds in the associated timezone.
public extern seconds( t : time ) : int {
  cs "_Time.Seconds"
  js "_time_seconds"
}

// The milliseconds in the associated timezone.
public extern milliseconds( t : time ) : int {
  cs "_Time.Milliseconds"
  js "_time_milliseconds"
}

// The day of the week in the associated time zone. 
// This is the ISO day of the week, starting at 1 for Monday and ending at 7 for Sunday.
public extern day-of-week(t:time) : int { 
  cs "_Time.DayOfWeek"
  js "_time_dayofweek" 
}

// Number of fractional seconds since  the _epoch_ (``1970-01-01Z``).
public extern epoch-seconds(t:time) : double { 
  cs "_Time.EpochSecs"
  js "_time_epochsecs" 
}

// Number of fractional seconds since  `min-time` (``0001-01-01Z``).
public extern total-seconds(t:time) : double { 
  cs "_Time.TotalSecs"
  js "_time_totalsecs" 
}

// Return the date of a time in the associated time zone. 
// Returns the year, month (starting at 1), and day of the month (starting at 1).
public fun date( t : time ) : (int,int,int) {
  (t.year,t.month,t.day)
}

// Return the clock time in the associated time zone. Returns the hours, minutes, seconds, and milliseconds.
public fun clock( t : time ) : (int,int,int,int) {
  (t.hours,t.minutes,t.seconds,t.milliseconds)
}

// Are two times equal? (regardless of timezone)
public fun (==)( t1: time, t2:time ) : bool {
  t1.total-seconds == t2.total-seconds
}

// Does time `t1` come before time `t2`? (regardless of timezone)
public fun (<)( t1: time, t2:time ) : bool {
  t1.total-seconds < t2.total-seconds
}

// Does time `t1` come after time `t2`? (regardless of timezone)
public fun (>)( t1 : time, t2: time ) : bool {
  t1.total-seconds > t2.total-seconds
}

// Return the fractional seconds difference between two times. (regardless of timezone)
public fun (-)( t1 : time, t2 : time ) : double {
  t1.total-seconds - t2.total-seconds
}

// The _epoch_ time (``1970-01-01Z``)
public val epoch = utc-time(1970,1,1)

// The minimal time value (``0001-01-01Z``).
public val min-time = utc-time(1,1,1)

// The maximal time value (``9999-12-31T23:59:59.999Z``).
public val max-time = utc-time(9999,12,31,23,59,59,999)


// Compare two times. (regardless of timezone)
public fun compare( t1 : time, t2 :time ) : order {
  if (t1==t2) then Eq
  elif (t1<t2) then Lt
  else Gt
}


// Create a new `:time`. Uses the local time zone by default. Use `utc-time` for a new time in the UTC timezone.
public fun time( year:int, month : int, day:int, hours:int = 0,minutes:int = 0,seconds:int = 0,milliseconds:int = 0) : ndet time {
  new-time(year,month,day,hours,minutes,seconds,milliseconds,False)
}

// Create a new `:time` in the local time zone with a given number of fractional seconds since `min-time` (0001-01-01Z).
public fun time( seconds : double ) : ndet time {
  new-time(seconds,False)
}

// Create a new UTC `:time`
public fun utc-time(year:int, month : int, day:int, hours:int = 0,minutes:int = 0,seconds:int = 0,milliseconds:int = 0) : time {
  new-time(year,month,day,hours,minutes,seconds,milliseconds,True)
}

// Create a new UTC `:time` with a given number of fractional seconds since `min-time` (0001-01-01Z).
public fun utc-time( seconds : double ) : time {
  new-time(seconds,True)
}

// Copy a `:time` value keeping it in the same timezone (local or UTC).
public fun copy(t : time, year:int = t.year, month : int = t.month, day:int = t.day, hours:int = t.hours,minutes:int = t.minutes,seconds:int = t.seconds,milliseconds:int = t.milliseconds) : time {
  new-time(year,month,day,hours,minutes,seconds,milliseconds,t.utc?)
}

// Is this time in the local timezone?
public fun local?( t : time ) : bool {
  !t.utc?
}

// Add a specified number of years, months, and days to a time. 
// Takes leap-years into account.
public fun add-date( t : time, years : int, months : int, days : int ) : time {
  val (y,m,d) = t.date
  val (h,min,s,ms) = t.clock
  new-time(y + years, m + months, d + days, h, min, s, ms, t.utc?)
}

// Add a specified number of hours, minutes, seconds, and optional milliseconds to a time.
public fun add-clock( t : time, hours : int, minutes : int, seconds : int, milliseconds : int = 0 ) : time {
  val (y,m,d) = t.date
  val (h,min,s,ms) = t.clock
  new-time(y, m, d, h + hours, min + minutes, s + seconds, ms + milliseconds, t.utc?)
}

// Add `n` days to a time. Takes leap years into account.
public fun add-days( t : time, days : int ) : time {
  t.add-date( 0, 0, days )
}

// -----------------------------------------------------------
// Advanced calendar funs
// -----------------------------------------------------------

// Is this time in a leap year?
public fun leap-year?( year : int ) : bool {
  ((year%4)==0 && ((year%100)!=0 || (year%400)==0))
}

// The number of days in the given year
public fun days-in-year( year : int ) : int {
  if (year.leap-year?) then 366 else 365
}

// Return the day of the year (starting at 1)
public fun day-of-year( t : time ) : int {
  val (year,month,day) = t.date
  val n = day-counts[month - 1].maybe(0) + day
  if (month > 2 && year.leap-year?) then n+1 else n
}
val day-counts = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]


// Return the ISO week number of the year (between 1 and 53).
public fun week-of-year( t : time ) : int {
  val week = ((t.day-of-year - t.day-of-week) + 10) / 7
  if (week<1) then (t.copy(t.year - 1, 1, 1)).last-week
  elif (week>t.last-week) then 1
  else week
}

// Last ISO week number of the year.
fun last-week( t : time ) : int {
  if (t.long-year?) then 53 else 52
}

// Returns true if the year contains 53 weeks (instead of 52)
fun long-year?( t : time ) : bool {
  val firstday = t.copy(t.year,1,1).day-of-week
  (firstday==4 || (t.year.leap-year? && firstday==3))
}

// Return the day of the year given an ISO week number and an ISO week day.
// The returned day maybe smaller than one (belonging to the previous year),
// or larger than the days of the year (belonging to the next year).
// The returned day number can always be passed directly to the `add-days` or
// the `time` / `utc-time` constructors.
public fun week-to-day-of-year( year : int, week : int, weekday : int ) : int {
  val adjust = utc-time(year,1,4).day-of-week + 3
  (((7*week) + weekday) - adjust)
}

// -----------------------------------------------------------
// Formatting
// -----------------------------------------------------------

// Show the time as an [ISO 8601](http://en.wikipedia.org/wiki/ISO_8601) string. 
// For example `now().show` becomes `"2016-09-08T19:55:11.910-07:00"`.
public fun show( t : time ) : string {
  t.show-iso
}

// Show the time as an [ISO 8601](http://en.wikipedia.org/wiki/ISO_8601) string.
// For example `now().show-iso` becomes `"2016-09-08T19:55:11.910-07:00"`.
public fun show-iso(t : time ) : string {
  t.format("YYYY-MM-DD'T'HH:mm:ssfffZ")
}

// Show the date in ISO format, i.e. ``YYYY-MM-DD`` format.
public fun show-iso-date( t : time ) : string {
  t.format("YYYY-MM-DD")
}

// Show the clock time in ISO format, i.e. ``HH:mm:ss`` format.
public fun show-iso-clock( t : time ) : string {
  t.format("HH:mm:ss")
}

// Show time as a standard [Internet Message Format](https://tools.ietf.org/html/rfc2822#section-3.3) date.\
// For example `now().show-imf` returns `"Fri, 9 Oct 2016 11:57:45 -0700"`
public fun show-imf( t : time ) : string {
  t.format("ddd, D MMM Y HH:mm:ss zz")
}

// Show the time as a human readable English string (using ISO clock time)
// For example `now().show-en` -> `"Thu, 8 Oct 2016, 19:48:11"`.
public fun show-en( t : time ) : string {
  t.format("llll")
}

// Show the date in human readable English.
// For example `now().show-en-date` -> `"Thu, 8 Oct 2016"`.
public fun show-en-date( t : time ) : string {
  t.format("ll")
}



// -----------------------------------------------------------
// Ticks
// -----------------------------------------------------------

// Return a high-resolution timestamp in fractional seconds. 
// The timestamp is guaranteed to be monotonically increasing
// and have at least millisecond resolution. 
public extern ticks() : ndet double {
  cs "_Time.Ticks"
  js "_ticks"
}

// Return the smallest time difference in seconds that `ticks` can measure.
public extern ticks-resolution() : ndet double {
  cs inline "_Time.TicksResolution"
  js inline "_ticks_resolution"
}

// Return the number of fractional seconds that it takes to evaluate `action`.
public fun elapsed( action : () -> <ndet|e> a ) : <ndet|e> (double,a) {
  val t0 = ticks()
  val x = action()
  val t1 = ticks()
  (t1 - t0,x)
}

// Measure the number of fractional seconds that it takes to evaluate `action`, and print `msg` postfixed with the
// measured time in millisecond resolution.
public fun print-elapsed( action : () -> <ndet,console|e> a, msg : string = "elapsed" ) : <ndet,console|e> a {
  val (t,x) = elapsed(action)
  println( msg + " " + t.show-fixed(3) + "s" )
  x
} 



// -----------------------------------------------------------
// Generic Formatting
// -----------------------------------------------------------

fun format-weekday( t : time, n : int, locale : time-locale ) : string {
  if (n==1) then t.day-of-week.show else {
    val days = if (n==2) then locale.day-names-min
               elif (n==3) then locale.day-names-short
               else locale.day-names
    days[t.day-of-week - 1].default("?")               
  }
}

fun format-day( t : time, n : int, locale : time-locale ) : string {
  if (n==3) then t.day-of-year.show
  elif (n==4) then t.day-of-year.showl(3)
  else t.day.showl(n)
}

fun format-week( t : time, n : int, locale : time-locale ) : string {
  t.week-of-year.showl(n)
}

fun format-month( t : time, n : int, locale : time-locale ) : string {
  if (n<=2) then t.month.showl(n) else {
    val months = if (n==3) then locale.month-names-short else locale.month-names
    months[t.month - 1].default("?")
  }
}

fun format-year( t : time, n : int, locale : time-locale ) : string {
  if (n==1) then {
    t.year.show
  }
  elif (n<5) then {
    t.year.abs.showl(n).last(n).string
  }
  else {
    val y = t.year.abs.showl(n)
    if (t.year.neg?) then "-" + y else "+" + y
  }
}

fun format-seconds( t : time, n : int, locale : time-locale ) : string {
  t.seconds.showl(n)
}

fun format-minutes( t : time, n : int, locale : time-locale ) : string {
  t.minutes.showl(n)
}

fun format-hours( t : time, n : int, locale : time-locale ) : string {
  t.hours.showl(n)
}

fun format-hours12( t : time, n : int, locale : time-locale ) : string {
  val h = t.hours
  if (h==0 || h==12) then "12"
  elif (h<=11) then h.showl(n) 
  else (h - 12).showl(n)
}

fun format-ampm( t : time, n : int, locale : time-locale ) : string {
  val h = t.hours
  (if (h<=11) then "am" else "pm").first(n).string
}

fun format-upper-ampm( t : time, n : int, locale : time-locale ) : string {
  format-ampm(t,n,locale).to-upper
}

fun format-frac( t : time, n : int, locale : time-locale ) : string {
  val ms   = t.clock.field4
  if (ms==0) return ""
  val frac = ms.double / 1000.0
  frac.show-fixed(n).tail
}

fun format-frac0( t : time, n : int, locale : time-locale ) : string {
  val ms = t.clock.field4
  "." + ms.showl(3).first(n).string.pad-right(n,'0')
}

fun format-tz-offset( t : time, n : int, hmsep : string, utc : string ) : string {
  show-timezone(t,hmsep=hmsep,utc=utc)
}

fun format-timezone( t : time, n : int, locale : time-locale ) : string {
  format-tz-offset(t,n,if (n==2) then "" else ":",if (n==2) then "+0000" else "+00:00")
}

fun format-utc-timezone( t : time, n : int, locale : time-locale ) : string {
  format-tz-offset(t,n,":",if (n==2) then "" else "Z")
}

fun format-timestamp( t : time, n : int, locale : time-locale ) : string {
  t.total-seconds.show-fixed(3)
}


val formats : list<(char,int,(time,int,time-locale) -> string)> = [
  ('Y', 6, format-year),
  ('M', 4, format-month),
  ('D', 4, format-day),
  ('H', 2, format-hours),
  ('m', 2, format-minutes),
  ('s', 2, format-seconds),
  ('z', 2, format-timezone),
  ('Z', 2, format-utc-timezone),
  ('f', 8, format-frac0),
  ('F', 8, format-frac),
  ('d', 4, format-weekday),
  ('w', 2, format-week),
  ('h', 2, format-hours12),
  ('a', 2, format-ampm),
  ('A', 2, format-upper-ampm),
  ('x', 1, format-timestamp), 
]

/* Format a time according to format string `fmt` and using a optional
   time locale (= `time-locale-en-iso`).

Letters (``a`` to ``z``) are always interpreted as a pattern where unknown letter
patterns are ignored. Any literal text should be quote-escaped i.e. use `"'GMT'ZZ"` 
to display as `"GMT-07:00"` (in the PST time zone). Any characters other then
an ascii letter are displayed as is.

Patterns of 2 letters are zero-padded on the left to always
display as 2 digits. Allowed patterns:

* ``Y``: the year as a number (without zero padding) (``1970``,``203``)
* ``YY``, ```YYYY``: the year in upto 4 digits (``70``,``1970``,``0203``).
* ``M``, ``MM``: the month. (``1``,``03``)
* ``MMM``, ``MMMM``: name of the month in English. (``Jan``, ``January``)
* ``D``,``DD``: the day of the month. (``1``,``08``)
* ``DDD``,``DDDD``: the day of the year. (``87``,``087``)
* ``d``: the ISO day of the week, 1 for Monday ending in 7 for Sunday.
* ``dd``,``ddd``,``dddd``: the day of the week in English. (``We``,``Wed``,``Wednesday``)
* ``w``,``ww``, the ISO week number of the year between 1 and 53. (``8``,``08``)
* ``h``,``hh``: the hours using a 12-hour clock (with am/pm). (``9``,``09``)
* ``H``,``HH``: the hours using a 24-hour clock. (``21``,``09``)
* ``a``,``aa``: AM/PM designation. (``a``,``am``)
* ``A``,``AA``: AM/PM designation in upper-case. (``A``,``AM``)
* ``f``,...,``ffffffff``: upto 8 digits of a fraction of a second. Starts with a ``.``. (``.320``,``.000``)
* ``F``,...,``FFFFFFFF``: upto 8 digits of a fraction of a second. If not zero, starts with a ``.``.
    In contrast to the ``f`` patterns displays the minimal number of required digits 
    (and is not right-padded with zeros). (``.32``)
* ``z``: timezone offset in hours and minutes separated by a colon. Use ``+00:00`` for UTC time. (``+01:00``)
* ``zz``: timezone offset in hours and minutes without a separator. Use ``+0000`` for UTC time. (``-0700``)
* ``Z``: timezone offset in hours and minutes separated by a colon, use ``Z`` for UTC time.   
* ``ZZ``: timezone offset in hours and minutes separated by colon, use an empty string for UTC time.   
* ``x``: fractional seconds since `min-time`. (``63610768799.429``)
* ``YYYYYY``: the year in [ECMAscript](http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.1.15.1) 6 digits, prepended with the sign. (``+002016``,``-000023``,``+000000``)
* ``'...'``, ``"..."``: anything between quotes is displayed as is. (``'M'M`` becomes ``M11`` for November)

There are also various forms to display dates and times in a locale specific way.
We give examples in English and Dutch. The lower-case ``l`` variants use short
names for month- and day names.

* ``t``: hours and minutes (``3:21pm, 15:21``)
* ``tt``: hours, minutes, and seconds (``3:21:01pm, 15:21:01``)
* ``L``,``l``: a date (``09/29/2016, 29.09.2016``) and (``9/29/2016, 29.9.2016``)
* ``LL``,``ll``: date with month name (``29 September 2016, 29 september 2016``) and (``29 Sep 2016, 29 sep 2016``)
* ``LLL``,``lll``: date with month name and time (``29 September 2016 1:15pm, 29 september 2016 13:15``)
* ``LLLL``,``llll``: date with day name, month name, and time (``Thursday, 29 September 2016 1:15pm``) and (``Thu, 29 Sep 2016 1:15pm``)

For example, to display a time in the standard [Internet Message Format](https://tools.ietf.org/html/rfc2822#section-3.3)
you can use `now().format("ddd, D MMM Y HH:mm:ss zz")` displayed as `"Tue, 27 Sep 2016 06:36:55 -0700"` for example.
*/
public fun format( t : time, fmt : string, locale : time-locale = time-locale-en-iso ) : string {
  format-list( t, fmt.expand-locales(locale).expand-locales(locale).list, locale )
}

fun format-list( t : time, fmt : list<char>, locale : time-locale ) : string {  
  match(fmt) {
    Nil -> ""
    Cons(c,cs) -> {
      val (formatted,rest) = format-pat(t,c,cs,locale)
      formatted + format-list(t, unsafe-decreasing(rest), locale)
    }
  }
}

fun format-pat( t : time, h : char, fmt : list<char>, locale : time-locale ) : (string,list<char>) {
  if (h=='"' || h=='\'') {
    // extract escaped part
    val (quoted,end) = fmt.span( fun(c) { c!=h } )
    match(end) {
      Nil          -> (h.string, fmt)
      Cons(_,rest) -> (quoted.string, rest)    
    }
  }
  elif(h.alpha?) {
    // match pattern
    val (hs,rest) = fmt.span(fun(c){c==h})
    val n = hs.length + 1
    formats.foreach-while fun(pattern) {
      val (c,m,f) = pattern
      if (c!=h) then Nothing else Just((f(t,min(n,m),locale), rest))
    }.default( ("",rest) ) // ignore if no match
  }
  else (h.string,fmt) // return as-is
}

fun expand-locales( fmt : string, locale : time-locale ) : string {
  fmt.replace-all(rx-locale) fun(cap) {
    val n = cap.matched.count
    if (cap.groups[1] != "") then {
      cap.matched
    }
    elif (cap.groups[2] != "") then {
      if (n>=2) then locale.format-tt else locale.format-t
    }
    else {
      val nfmt = if (n>=4) then locale.format-llll elif (n==3) then locale.format-lll
                 elif (n==2) then locale.format-ll else locale.format-l
      if (cap.groups[3] != "") then {
        nfmt
      }
      else {
        nfmt.replace-all(rx-long, fun(capl) { capl.matched.tail })
      }
    }
  }
}
val rx-locale = regex(@"('[^']*'|""[^""]*"")|(t+)|(L+)|(l+)")
val rx-long   = regex(@"dddd+|MMMM+|DD(?!D)")

// -----------------------------------------------------------
// Format helpers
// -----------------------------------------------------------

// Show the timezone offset (by default in the ISO 8601 format).
// `now().show-timezone` becomes `"-07:00"`.
// The `utc` (`"Z"`) parameter gives the form used for the UTC timezone,
// while the `hmsep` (`":"`) gives the separator used between the hours and minutes of
// other timezone offsets. The `hrwidth` (=`2`) parameter gives the minimal width of the hours part.
fun show-timezone( t : time, utc : string = "Z", hmsep : string = ":", hrwidth : int = 2 ) : string {
  val ofs    = t.timezone-offset / 60  // convert to minutes
  if (ofs==0) return utc
  val absofs = if (ofs >= 0) then ofs else ~ofs
  (if (ofs >= 0) then "+" else "-") + 
   ((absofs / 60).showl(hrwidth)) + hmsep + ((absofs % 60).showl)
}

fun showl( i : int, width : int = 2 ) : string {
  i.show.pad-left(width,'0')
}



// -----------------------------------------------------------
// Parsing 
// -----------------------------------------------------------

/* Parse an [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) time string.
   Always returns a UTC time. Use `local` to convert to the local time.

````plain
2008-12-31               // just date
2009-W01-3               // week number and day
2008-366                 // day of the year

20081231, 2009W013, 2008366  // without dashes

2008-12-31T09            // with time part, use T or space
2008-12-31 09
2008-12-31T09:20:16
2008-12-31T09:20:16.345  // with milliseconds (can be any fraction)
2008-12-30T24:00:00      // next day at midnight
20081231T092016.345      // without separators

2008-12-31T09Z           // UTC time zone (Z)
2008-12-31 09-07:00      // UTC-07:00 time zone
2008-12-31T09:20:16+0830 // UTC+08:30 time zone (without colon)
````
. 
*/
public fun parse-iso( s : string ) : maybe<time> {
  match(s.find(rx-iso-date)) {
    Nothing -> Nothing
    Just(capd) -> {
      val xyear = capd.num(1)
      val (year,month,day) =
        if (capd.groups[2] != "") 
          then (xyear,capd.num(2),capd.num(3)) 
          else {
            val day = if (capd.groups[4] != "") 
                       then week-to-day-of-year(xyear,capd.num(4),capd.num(5,1))
                       else capd.num(6)
            utc-time(xyear,1,day).date
          }
      match(capd.slice.after.string.find(rx-iso-time)) {
        Nothing    -> Nothing
        Just(capt) -> {
          val hours = capt.num(1)
          val mins  = capt.num(2)
          val secs  = capt.num(3)
          val msecs = parse-int-default((capt.groups[4] + "000").first(3).string)
          val tzsign= if (capt.groups[5]=="+") then 1 else ~1
          val tzhours= capt.num(6) 
          val tzmins = capt.num(7)
          val tzofs = tzsign * ((tzhours * 60) + tzmins)
          Just(utc-time(year,month,day,hours,mins - tzofs,secs,msecs))
        }
      }
    }
  }
}
val rx-iso-date = regex(@"^(\d\d\d\d)\-?(?:(\d\d)\-?(\d\d)|W(\d\d)\-?(\d)?|(\d\d\d))(?=$|[T\+\- ]\d|Z)");
val rx-iso-time = regex(@"^(?:[T ](\d\d)(?:\:?(\d\d)(?:\:?(\d\d))?)?(?:[\.,](\d+))?)?(?:Z|([\+\-])(\d\d)(?:\:?(\d\d))?)?$")

fun num( cap : matched, group : int, default : int = 0 ) : int {
  parse-int-default(cap.groups[group], default = default)
}

// Parse an [Internet Message Format](https://tools.ietf.org/html/rfc2822#section-3.3) time string.
// Always returns a UTC time. Use `local` to convert to the local time.
public fun parse-imf( s : string ) : maybe<time> {
  match(s.find(rx-imf)) {
    Nothing -> Nothing
    Just(cap) -> {
      val day   = cap.num(2)
      val month = month-from-name(cap.groups[3]).default(1)
      val xyear = cap.num(4)
      val hours = cap.num(5)
      val mins  = cap.num(6)
      val secs  = cap.num(7)
      val tzsign= if (cap.groups[8]=="+") then 1 else ~1
      val tzhours= cap.num(9) 
      val tzmins = cap.num(10)
      // adjust 2 digit year
      val year  = if (cap.groups[4].count <= 2) 
                   then (if (xyear < 50) then 2000 + xyear else 1900 + xyear)
                   else xyear
      val tzofs = tzsign * ((tzhours * 60) + tzmins)
      Just(utc-time(year,month,day,hours,mins - tzofs,secs,0))
    }
  }
}
val rx-imf = regex(@"^ *(?:(Mon|Tue|Wed|Thu|Fri|Sat|Sun) *, *)? (\d\d?) *(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) *(\d\d(?:\d\d)?) +(\d\d?):(\d\d?)(?::(\d\d))? *(?:([\+\-])(\d\d)(\d\d) *)?$")

fun month-from-name( mname : string ) : maybe<int> {
  val lname = mname.to-lower
  fun search( names : list<string>, month : int ) : maybe<int> {
    match(names) {
      Nil -> Nothing
      Cons(name,rest) -> {
        if (name.to-lower.starts-with(lname).bool) 
         then Just(month) 
         else search(rest,month+1)
      }
    }
  }
  search(time-locale-en-iso.month-names,1)
}


// -----------------------------------------------------------
// Time locale
// -----------------------------------------------------------

// The `:time-locale` describes time and date formats for a specific locale.
public struct time-locale(
  // Language name, for example `en` or `fr-ca`.                          
  lang-name       : string,                            
  day-names       : list<string>,
  month-names     : list<string>,
  month-names-short: list<string> = month-names.map(fun(m) { m.first(3).string }),
  day-names-short : list<string>  = day-names.map(fun(m) { m.first(3).string }),
  day-names-min   : list<string>  = day-names.map(fun(m) { m.first(2).string }),
  format-t        : string = "HH:mm",
  format-tt       : string = "HH:mm:ss",
  format-l        : string = "YYYY-MM-DD",
  format-ll       : string = "D MMMM YYYY",
  format-lll      : string = "D MMMM YYYY t",
  format-llll     : string = "dddd D MMMM YYYY t"
)

// English time locale (`en`)
public val time-locale-en : time-locale = Time-locale( "en",
  ["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"],
  ["January","February","March","April","May","June","July","August","September","October","November","December"],
  format-t    = "h:mmaa",
  format-tt   = "h:mm:ssaa",
  format-l    = "MM/DD/YYYY",
  format-llll = "dddd, D MMMM YYYY t"
)

// ISO English time locale (`en-iso`). Uses English names for
// months and days but displays numeric dates and times using unambigious ISO format. 
public val time-locale-en-iso : time-locale = Time-locale( "en-iso",
  day-names   = time-locale-en.day-names,
  month-names = time-locale-en.month-names,
  format-lll  = "D MMMM YYYY, tt",
  format-llll = "dddd, D MMMM YYYY, tt"
)

// French time locale (`fr`)
public val time-locale-fr : time-locale = Time-locale( "fr",                                                      
  ["lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"],
  ["janvier","février","mars","avril","mai","juin","juillet","août","septembre","octobre","novembre","décembre"],
  ["janv.","févr.","mars","avr.","mai","juin","juil.","août","sept.","oct.","nov.","déc."],
  ["lun.","mar.","mer.","jeu.","ven.","sam.","dim."],
  ["Lu","Ma","Me","Je","Ve","Sa","Di"],
  format-l    = "DD/MM/YYYY"
)

// German time locale (`de`)
public val time-locale-de : time-locale = Time-locale( "de",
  ["Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag","Sonntag"],
  ["Januar","Februar","März","April","Mai","Juni","Juli","August","September","Oktober","November","Dezember"],
  ["Jan.","Febr.","Mrz.","Apr.","Mai","Jun.","Jul.","Aug.","Sept.","Okt.","Nov.","Dez."],
  ["Mo.","Di.","Mi.","Do.","Fr.","Sa.","So."],
  ["Mo","Di","Mi","Do","Fr","Sa","So"],
  format-l    = "DD.MM.YYYY",
  format-ll   = "D. MMMM YYYY",
  format-lll  = "D. MMMM YYYY t",
  format-llll = "dddd, D. MMMM YYYY t"
)

// Spanish time locale (`es`)
public val time-locale-es : time-locale = Time-locale( "es",
  ["lunes","martes","miércoles","jueves","viernes","sábado","domingo"],
  ["enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre"],
  format-t    = "H:mm",
  format-tt   = "H:mm:ss",
  format-l    = "DD/MM/YYYY",
  format-ll   = "D 'de' MMMM 'de' YYYY",
  format-lll  = "D 'de' MMMM 'de' YYYY H:mm",
  format-llll = "dddd, D 'de' MMMM 'de' YYYY H:mm"
)

// Dutch time locale (`nl`)
public val time-locale-nl : time-locale = Time-locale( "nl",
  ["maandag","dinsdag","woensdag","donderdag","vrijdag","zaterdag","zondag"],
  ["januari","februari","maart","april","mei","juni","juli","augustus","september","oktober","november","december"],
  ["jan","feb","mrt","apr","mei","jun","jul","aug","sep","okt","nov","dec"],
  ["ma.","di.","wo.","do.","vr.","za.","zo."],
  ["Ma","Di","Wo","Do","Vr","Za","Zo"],
  format-l    = "DD-MM-YYYY"
)

val locales = [
  // from most specific (``en-GB`` to least specific ``en``)
  time-locale-en-iso,
  time-locale-de,time-locale-en,
  time-locale-es,time-locale-fr,
  time-locale-nl
]

// Return a builtin locale given a locale string (e.g. ``en-GB``, ``es``); 
// Use `time-locale-iso` if no particular match is found.
// Supports ``de``,``en``,``es``,``fr``,``nl`` and ``en-iso``.
public fun get-time-locale( locale : string ) : time-locale {
  locales.find(fun(l) { locale.starts-with(l.lang-name).bool }).default(time-locale-en-iso)
}
*/