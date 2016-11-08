/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Basic date and time funs.

   Years, months, days etc. are always 1-based, while durations are always in (fractional) seconds.
*/
module std/time/leaps

import std/regex
import std/time/duration

// Standard seconds per day (=`86400`). Note that a UTC day can contain extra leap seconds.
public val secs-per-day = 86400

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
  expire : utc-duration,       // since epoch                   
  adjusts: list<leap-adjust>
)

// Leap second adjustments. For an instant `i` after `start`:\
// ``TAI-offset = offset + (delta * days(i - delta-start))``
struct leap-adjust(
  utc-start  : utc-duration,  // start time in UTC seconds since 1972-01-01Z
  offset     : double,    // base offset
  delta-start: utc-duration = duration0,   // start of delta adjustment
  delta      : double  = 0.0   
)

val zero : leap-adjust = Leap-adjust(duration0,0.0,duration0,0.0)

// Get the leap-second adjustment
fun tai-offset( la : leap-adjust, utc-i : utc-duration ) : duration {
  val secs = if (la.delta.zero?) then la.offset else {
               // pre 1972 is a rubber leap second
               val days = floor((utc-i - la.delta-start).seconds /secs-per-day.double)
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


// Convert a UTC duration since `epoch` to a TAI duration since `epoch`.
public fun tai-from-utc( d : utc-duration ) : duration {
  val leaps = unsafe-total(get-leapsecs)
  leaps.adjusts.find-maybe(fun(la) {
    if (la.utc-start > d) then Nothing else {
      val ofs = la.tai-offset(d)
      // trace("found: " + i.show + " >= " + la.utc-start.show + ", ofs:" + ofs.show)
      Just( d + ofs )
    }
  }).default(d)  
}

// Convert a TAI duration since `epoch` to a UTC duration since `epoch`.
public fun utc-from-tai( i : duration ) : utc-duration {
  //trace("utc-from-tai: " + i.show)
  val leaps = unsafe-total(get-leapsecs)
  if (i < duration0) then i else {
    find-utc-from-tai( i, leaps.adjusts )
  }
}
//val tai1961    = duration(~347068800 - 2)  // 1961-01-01Z minus 2 seconds for the initial leap

fun find-utc-from-tai( i : duration, leaps : list<leap-adjust> ) : utc-duration {
  match(leaps) {
    Nil -> i // should never happen
    Cons(la,earlier) -> {
      val ofs   = la.tai-offset(i)
      val utc-i = i - ofs
      val max   = if (ofs.neg?) then utc-i else i
      // trace("check: utc-i: " + utc-i.show + ", start: " + la.utc-start.show + ", ofs: " + ofs.show + ", i: " + i.show)
      if (la.utc-start > max) then find-utc-from-tai(i,earlier) else {
        if (la.utc-start <= utc-i) then {
          // in the time frame
          // trace(" utc-from-tai: found: utc: " + utc-i.show + ", tai-ofs: " + ofs.show )
          utc-i
        }
        else {
          // leap-second crosses-over into other time frame            
          // get adjustment using the earlier frame too
          // trace("found crossover: " + utc-i.show + ", ofs: " + ofs.show + ", i: " + i.show )
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
            val utc-leap = utc-duration( utc-i.secs, utc-i.frac + diff )                            
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

// Parse a NIST (IETF) leap second table.
// See: <https://www.ietf.org/timezones/data/leap-seconds.list>
fun parse-leap-seconds( leaps : string ) : leaps-table {
  // get leap second adjustments
  val las = leaps.find-all(rxleap).map fun(cap) {
    val ntpsecs = cap.groups[1].parse-int-default(0)  
    val adjust  = cap.groups[2].parse-double-default(0.0)
    //trace("leap entry: " + adjust.show + " " + ntpi.show + " " + ntpsecs.show )
    Leap-adjust(utc-duration(ntpsecs), adjust, duration0, 0.0)
  }.reverse

  // get expiration date
  val la-final   = las.head.default(zero)
  val utc-expire = leaps.find(rxexpire).map fun(cap) {
    val ntpex = cap.groups[1].parse-int-default(0)
    utc-duration(ntpex)
  }.default(la-final.utc-start + utc-duration(365*secs-per-day))
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

val jd-epoch-shift     = 2400000.5
val mjd-epoch-shift    = 1297728000 // 1900-01-01Z - 1858-11-17Z modified julian date epoch


// Parse the standard UTC leap second adjustment file.\
// See <http://maia.usno.navy.mil/ser7/tai-utc.dat>
fun parse-leap-seconds-dat( s : string = leap-seconds-dat ) : list<leap-adjust> {
  s.find-all(rxtaiadjust).map( fun(cap) {
    val mjd    = cap.groups[1].parse-double-default(jd-epoch-shift) - jd-epoch-shift 
    val ofs    = cap.groups[2].parse-double-default(0.0)
    val dmjd   = cap.groups[3].parse-double-default(0.0)
    val delta  = cap.groups[4].parse-double-default(0.0)
    val start  = utc-duration(mjd.int*secs-per-day - mjd-epoch-shift)  //instant(1858,11,17 + mjd.int)
    val dstart = utc-duration(dmjd.int*secs-per-day - mjd-epoch-shift) //instant(1858,11,17 + dmjd.int)
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

