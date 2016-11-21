/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* UTC time scales and leap second support.

For proper UTC to TAI conversion, we need to know where leap seconds occur.
The occurrence of leap seconds cannot be reliably predicted though and 
the [IERS announces](https://www.iers.org/SharedDocs/News/EN/BulletinC.html) 
leap seconds at most a year in advance. 
The [IETF leap second](https://www.ietf.org/timezones/data/leap-seconds.list) 
table contains a list of all currently announced leap seconds.

As such, many functions operating on time would be non-deterministic for
any future date as they may be off by a number of, as yet unannounced, leap seconds.
To makes all these function non-deterministic, or to give them all a leap second table
argument seems a bit too cumbersome. So, instead we choose to ignore this form
of non-determinism and make the leap second table a hidden global variable. The 
internal table is valid up to the release of the compiler. 

It is recommended to call [`update-leaps-table`](std_time_download.html#update_leaps_table) at the 
start of a program which will automatically ensure the leap second table is up-to-date
(automatically downloading and caching the IETF leap second file as necessary).

## UTC before 1972-01-01Z

The IERS (and IETF table) defines leap seconds after 1972-01-01Z. Before that the 
leap seconds were defined as a continuous linear interpolation (or 'drift')
(see the [USNO](http://maia.usno.navy.mil/ser7/tai-utc.dat) table). For dates between
1961-01-01Z this table is used for UTC time calculations.

The difference (TAI - UT1) was approximately zero at the TAI epoch 1958-01-01Z (most close at 1958-01-12T15:00:00).
Since 1958-01-01 up to 1961-01-01Z the relation between TAI and UTC was not 
well defined (historically there seems to have been 29 20ms jumps in that time frame).
Starting in 1961-01-01Z, the initial UTC offset was 1.422818s. To avoid having to insert
such an odd initial leap second, we choose to extend the USNO table to linearly interpolate
a 'drift' from 1958-01-01 to 1961-01-01 with one extra entry in the [USNO table](http://maia.usno.navy.mil/ser7/tai-utc.dat):\
``#1958 JAN  12=JD 2436216.125  TAI-UTC=   0.0000000 S + (MJD - 36215.625) X 0.0013121088 S``\
This makes the UTC coincide with TAI up to 1958-01-01, and continuously interpolated up to 1972-01-01,
followed by modern integral leap-seconds.

More fun facts:

* There was a small 0.05s discontinuity in the 'drift' at 1961-08-01Z:
  ```
  instant-at(1961,8,1,0,0,1,0.6,cal=cal-tai).time.show == "1961-07-31T23:59:59.902430Z" 
  instant-at(1961,8,1,0,0,1,0.7,cal=cal-tai).time.show == "1961-08-01T00:00:00.052430Z" 
  ```
* On the transition to modern UTC at 1972-01-01Z there is a mini leap second 0.107758s. 
  ```
  instant-at(1972,1,1,0,0,9,0.99999999,cal=cal-tai).time.show == "1971-12-31T23:59:60.107757999Z" 
  instant-at(1972,1,1,0,0,10,cal=cal-tai).time.show == "1972-01-01T00:00:00Z" 
  ```

*/
module std/time/utc

import std/regex
import std/num/fixed
import std/time/timestamp
import std/time/duration
import std/time/instant

extern include {
  // cs file "time-inline.cs"
  js file "utc-inline.js"
}



/*----------------------------------------------------------------------------
  Unix
----------------------------------------------------------------------------*/

// Given a unix time stamp in seconds and possibly a separate fraction of seconds
// (for increased precision for nanosecond timestamps) return a `:instant`.
public fun unix-instant( secs : double, frac : double = 0.0 ) : instant {
  unsafe-instant-at( timestamp(secs.trunc.int, secs.fraction + frac), ts-unix ) 
}

// The current `:instant` in time as returned by the system clock.
public fun now() : ndet instant {
  val (secs,frac) = unix-now()
  unix-instant(secs,frac)
} 

// Returns a unix time stamp; this still needs
// to be adjusted to our epoch and taking account of leap seconds.
public extern unix-now() : ndet (double,double) {
  js "_unix_now"
}

// The resolution in seconds of the system clock.
extern now-precision() : ndet int {
  js "_now_precision"
}

public fun unix-timestamp( i :instant ) : double {
  i.timestamp(ts-unix).seconds.double
}

/*----------------------------------------------------------------------------
  Timescale creation
----------------------------------------------------------------------------*/

// Create a new time scale based on UTC seconds with a given `name` and a
// `y2k-epoch` (=`timestamp0`) which is the timestamp of the 2001-01-01 date in that timescale
// e.g. for a timescale `ts`:\
// `y2k-epoch = instant-at(2000,1,1,cal=iso-calender(ts)).timestamp(ts)`
public fun utc-timescale( name : string, y2k-epoch : timestamp = timestamp0 ) : timescale {
  val epoch-shift = y2k-epoch.timespan
  fun from-tai(tai:duration) { tai-to-utc(tai) + epoch-shift }
  fun to-tai(t:timestamp)    { utc-to-tai(t - epoch-shift) }
  timescale(
    name,
    from-tai,
    to-tai,
    y2k-epoch,
    "UTC",
    Just(utc-secs-in-day)
  )
}

// The UTC time scale with a 2000-01-01Z UTC epoch. This uses ['UTC seconds'](https://en.wikipedia.org/wiki/Unix_time) where
// one day consists of 86400 'seconds' but where leap seconds are ignored (as in unix timestamps).
// Note that internally to the library the `:timestamp`s will accurately maintain any leap second information.
public val ts-utc : timescale = utc-timescale( "UTC" )

// [Unix](https://en.wikipedia.org/wiki/Unix_time) time scale based on UTC seconds but with 1970-01-01 epoch.
public val ts-unix : timescale = utc-timescale( "UNIX", timestamp(946684800) )

// [NTP](https://en.wikipedia.org/wiki/Network_Time_Protocol) time scale is equal to the UTC time scale (`ts-utc`) but 
// with a 1900-01-01Z epoch.
public val ts-ntp : timescale = utc-timescale( "NTP", timestamp(3155673600) )



// Standard seconds per day (=`86400`). Note that a UTC day can contain extra leap seconds.
val secs-per-day = fixed(86400)

// -----------------------------------------------------------
// UTC calendar leap second calculation
//
// Since the timescales `tai` and `utc` refer to `utc-from-tai`
// and `tai-from-utc` these routines strictly work in durations
// and cannot refer to instants or otherwise the definitions become
// mutually recursive.
// -----------------------------------------------------------


// `:utc-timestamp` is an alias used for timestamp's in UTC seconds.
alias utc-timestamp = timestamp


// A leap second table describes when UTC leap seconds occur.
abstract struct leaps-table(
  public expire : instant,       
  adjusts: list<leap-adjust> // Each entry gives the start instant and integer leap second adjustment.
)

// Leap second adjustments. For an instant `i` after `start`:\
// ``TAI-offset = offset + (delta * days(i - delta-start))``
struct leap-adjust(
  utc-start  : utc-timestamp,  // start time in UTC seconds since 1972-01-01Z
  offset     : timespan,       // base offset
  delta-start: utc-timestamp  = timestamp0,   // start of delta adjustment
  delta      : fixed = zero   
)

val leap-table0 = Leaps-table(epoch,[])
val zero : leap-adjust = Leap-adjust(timestamp0,timespan0,timestamp0,zero)

// Get the leap-second adjustment
fun tai-offset( la : leap-adjust, utc-i : utc-timestamp ) : timespan {
  if (la.delta.zero?) then la.offset else {
    // pre 1972 is a rubber leap second
    val days = ((utc-i.timespan - la.delta-start.timespan) / secs-per-day).floor.fixed
    la.offset + (la.delta * days)
  }
}

// Set up a global leap second table.
// We are going to pretend that this is total even though for future
// dates utc-tai conversion might be off by some future leap seconds.
val global-leaps-table : () -> ref<global,leaps-table> = once{
  unsafe-total{ 
    val pre72  = leap-seconds-pre72()
    val post72 = parse-leaps-table( default-ietf-leap-seconds ).default(leap-table0)
    // trace("parse leap tables: " + pre72.length.show)
    ref(post72(adjusts = post72.adjusts + pre72))  // reverse order
  }
}

// Get the global leap second table.
public fun get-leaps-table() : st<global> leaps-table {
  !(global-leaps-table()) 
}


// Update the leap second table from a leap seconds table.
// Only performs the update if the provided table has a later expiry date.
// The preferred way of updating the leap second table is through 
// [`update-leaps-table`](std_time_download.html#update_leaps_table).
public fun set-leaps-table( post72 : leaps-table ) : io () {
  if (post72.expire > get-leaps-table().expire) then {
    val r = global-leaps-table()
    r := post72( adjusts = post72.adjusts + leap-seconds-pre72() )
  }
}


/*----------------------------------------------------------------------------
  UTC to TAI conversion
----------------------------------------------------------------------------*/

// Convert a UTC duration since `epoch` to a TAI duration since `epoch`.
fun utc-to-tai( t : timestamp ) : duration {
  utc-to-tai-stamp( t ).unsafe-duration
}

fun utc-to-tai-stamp( t : timestamp ) : timestamp {
  val nlsecs = t.without-leap-seconds
  if (nlsecs < utc1958) then return t

  val leaps = unsafe-total(get-leaps-table)
  leaps.adjusts.find-maybe(fun(la) {
    if (la.utc-start > nlsecs) then Nothing else {
      val ofs = la.tai-offset(nlsecs)
      // trace("found: " + i.show + " >= " + la.utc-start.show + ", ofs:" + ofs.show)
      Just( (t + ofs) )
    }
  }).default(t)  
}

// Convert a TAI duration since `epoch` to a UTC duration since `epoch`.
fun tai-to-utc( d : duration ) : timestamp {
  //trace("utc-from-tai: " + i.show)
  if (d < tai1958) then d.timestamp else {
    val leaps = unsafe-total(get-leaps-table)
    find-utc-from-tai( d.timestamp, leaps.adjusts )
  }
}
val tai1958 = duration(~1325376000)  // 1958-01-01Z in seconds since epoch
val utc1958 = tai1958.timestamp

fun find-utc-from-tai( i : timestamp, leaps : list<leap-adjust> ) : timestamp {
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
          // trace(" utc-from-tai: found: utc: " + utc-i.show + ", tai-ofs: " + ofs.ts-show )
          utc-i
        }
        else {
          // leap-second crosses-over into other time frame            
          // get adjustment using the earlier frame too
          // trace("found crossover: " + utc-i.show + ", ofs: " + ofs.ts-show + ", i: " + i.show )
          val la-before = earlier.head.default(zero)
          val ofs-before= la-before.tai-offset(i)
          val utc-prev  = i - ofs-before
          if (la.utc-start > utc-prev) then {
            // use previous leap-second offset            
            // trace(" use previous: " + utc-prev.show + ", ofs-before: " + ofs-before.ts-show )
            utc-prev
          }
          else {
            // this is inside an added leap-second, encode as a fraction `> 1.0`.
            val diff     = ofs - ofs-before
            val utc-leap = utc-i.add-leap-seconds(diff)                            
            // trace(" in a leap second: " + utc-leap.show + ", diff: " + diff.ts-show + ", ofs: " + ofs.ts-show + ", ofs-before: " + ofs-before.ts-show )
            utc-leap
          }
        }
      }  
    }
  }
}


// Return the SI seconds in the day of time `t` (as a UTC timestamp)
public fun utc-secs-in-day( t : timestamp ) : timespan {
  val nlsecs = t.without-leap-seconds
  //trace("search nlsecs: " + nlsecs.ts-show)
  if (nlsecs < utc1972) then return secs-per-day
  val leaps = unsafe-total(get-leaps-table)
  match(leaps.adjusts.long-day?(nlsecs)) {
    Just(leap-secs) -> leap-secs + secs-per-day
    _ -> secs-per-day
  }
}
val utc1972 = timestamp(~883612800)  // instant-at(1972).timestamp(ts-utc)

fun long-day?( leaps : list<leap-adjust>, t : timestamp ) : maybe<fixed> {
  match(leaps) {
    Nil -> Nothing
    Cons(la,rest) -> {
      if (la.utc-start <= t) then return long-day?(rest,t) 
      if (la.utc-start - secs-per-day > t) then return Nothing  
      val delta = match(rest) {
        Nil -> 1.fixed
        Cons(la-before) -> (la.offset - la-before.offset)
      }
      Just(delta)
    }
  }
}



// -----------------------------------------------------------
// Parsing UTC leap second tables
// -----------------------------------------------------------

val ntp-shift = 3155673600  // 2000-01-01 TAI - 1900-01-01 TAI in seconds


// Parse a leap second table from text in the the [IETF leap second](https://www.ietf.org/timezones/data/leap-seconds.list) 
// file format.
public fun parse-leaps-table( leaps : string ) : maybe<leaps-table> {
  // get leap second adjustments
  val las = leaps.find-all(rxleap).map fun(cap) {
    val ntpsecs = cap.groups[1].parse-int-default(ntp-shift)   
    val adjust  = cap.groups[2].parse-int-default(0)
    // trace("leap entry: " + adjust.show + " - " + ntpsecs.show )
    Leap-adjust(timestamp(ntpsecs - ntp-shift), adjust.fixed, timestamp0)
  }.reverse

  // get expiration date
  val la-final   = las.head.default(zero)
  val utc-expire = leaps.find(rxexpire).map fun(cap) {
    val ntpex = cap.groups[1].parse-int-default(ntp-shift)
    timestamp(ntpex - ntp-shift)
  }.default(la-final.utc-start + (365.fixed*secs-per-day))
  // trace("expire: " + utc-expire.ts-show)
  val expire = unsafe-instant-at( utc-expire + la-final.offset, ts-tai ) // interpret as TAI to avoid recursion..

  val table = Leaps-table(expire,las)
  Just(table)
}
val rxleap   = regex(@"^[ \t]*(\d+)[ \t]+(\d+)[ \t]*(?:#.*)?$",multiLine=True)
val rxexpire = regex(@"^[ \t]*#@[ \t]*(\d+)[ \t]*(?:#.*)?$", multiLine=True)

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
// Parsing TAI 'continuous' leap second tables from before 1972
// -----------------------------------------------------------

// Cached leap second table for dates before 1972.
val leap-seconds-pre72 : (() -> list<leap-adjust>) = once{ parse-leap-seconds-dat() }

val jd-epoch-shift     = 2400000.5
val mjd-epoch-shift    = 4453401600.fixed // 2000-01-01 TAI - 1858-11-17 TAI modified julian date epoch


// Parse the standard UTC leap second adjustment file.\
// See <http://maia.usno.navy.mil/ser7/tai-utc.dat>
fun parse-leap-seconds-dat( s : string = leap-seconds-dat ) : list<leap-adjust> {
  s.find-all(rxtaiadjust).map( fun(cap) {
    val mjd    = cap.groups[1].parse-double-default(jd-epoch-shift) - jd-epoch-shift 
    val ofs    = cap.groups[2].parse-double-default(0.0)
    val dmjd   = cap.groups[3].parse-double-default(0.0)
    val delta  = cap.groups[4].parse-double-default(0.0)
    val start  = timestamp(mjd.fixed*secs-per-day - mjd-epoch-shift)  //instant(1858,11,17 + mjd.int)
    val dstart = timestamp(dmjd.fixed*secs-per-day - mjd-epoch-shift) //instant(1858,11,17 + dmjd.int)
    // trace("pre72 start=" + start.show )
    Leap-adjust( start, ofs.fixed, dstart, delta.fixed )
  }).reverse
}
val rxtaiadjust = regex(@"^ *\d[^=]*=JD (\d+\.\d+) *TAI-UTC= *(\d+\.\d+)[^\d]+(\d+\.\d*)[^\d]+(\d+\.\d+) *S *$", multiLine=True)

// TAI leap second adjustments for dates before 1972-01-01Z are linear interpolations.
// Note: (TAI - UT1) was approximately zero at 1958-01-01Z (most close at 1958-01-12T15:00:00)
// We added an initial entry starting at that time such that the UTC offset
// increases linearly up to the first official 1.422818s offset in 1961-01-01Z.
// Historically, there were 29 20ms jumps during that initial time frame.
// #1958 JAN  12=JD 2436216.125  TAI-UTC=   0.0000000 S + (MJD - 36215.625) X 0.0013121088 S
val leap-seconds-dat = @"
  # from: http://maia.usno.navy.mil/ser7/tai-utc.dat
  1958 JAN  1 =JD 2436204.5  TAI-UTC=   0.0000000 S + (MJD - 36204.) X 0.001298191606 S
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

