/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Instants in time.


*/
module std/time/instant

import std/time/duration
import std/time/leaps

extern include {
  // cs file "time-inline.cs"
  js file "instant-inline.js"
}


// The _epoch_ is the NTP epoch, 1900-01-01Z UTC (which equals 1900-01-01Z TAI)\
// Nice properties:
//
// - Epoch for the NTP protocol
// - for any date <  1960-01-01Z, we have UTC == TAI.
// - for any date >= 1972-01-01Z, we have UTC == TAI - whole SI second offset.
// - Any leap second and DST calculations always deal with positive times.
public val epoch = instant-at-timestamp(0.0,ts-utc)


// A `:time-scale` represents a duration relative to a certain epoch,
// and can convert to- and from TAI and UTC seconds. Time scales are
// mostly used internally to minimize conversions for efficiency.
public struct time-scale(
  ts-name    : string,
  from-tai   : duration -> timestamp,
  to-tai     : timestamp -> duration,
  from-utc   : utc-timestamp -> timestamp,
  to-utc     : timestamp -> utc-timestamp,
  allow-leap : bool = False,
  epoch-shift: timestamp = timestamp0
)


// Create a new time scale based on UTC 'seconds' with a given `name` and `epoch-shift`
// duration away from the standard `epoch`.
public fun utc-timescale( name : string, epoch-shift : timestamp = timestamp0 ) : time-scale {
  fun from-utc(d:timestamp) { d - epoch-shift }
  fun to-utc(d:timestamp)   { d + epoch-shift } 
  Time-scale(
    name,
    fun(d) { d.tai-to-utc.from-utc },         
    fun(d) { d.to-utc.utc-to-tai   },
    from-utc,
    to-utc,
    True,
    epoch-shift
  )
}

// Create a new time scale based on SI seconds with a given `name` and `epoch-shift`
// duration away from the standard `epoch`.
public fun tai-timescale( name : string, offset : timestamp = timestamp0, epoch-shift : timestamp = timestamp0 ) : time-scale {
  val total-offset  = epoch-shift + offset
  fun from-tai(d:duration) { d - total-offset }
  fun to-tai(d:duration)   { d + total-offset }
  Time-scale(
    name,
    from-tai,
    to-tai,
    fun(d) { d.utc-to-tai.from-tai },
    fun(d) { d.to-tai.tai-to-utc   },
    False,
    epoch-shift
  )
}

// The TAI time scale based on SI seconds with 1900-01-01 epoch.
public val ts-tai : time-scale = tai-timescale( "TAI" )
 
// The GPS time scale based on SI seconds with 1980-01-06 epoch.\
// GPS = TAI - 19.
public val ts-gps : time-scale = tai-timescale( "GPS", timestamp(19), timestamp(2524953600) )

// The Terrestrial time scale based on SI seconds with 1900-01-01 epoch.\
// TT = TAI + 32.182
public val ts-tt : time-scale = tai-timescale( "TT", timestamp(~32.184) )

// The UTC time scale with a 1900-01-01 epoch. This uses 'UTC seconds' where
// one day consists of 86400 'solar' seconds. Leap seconds are encoded by using a
// fraction `>= 1.0`. 
public val ts-utc : time-scale = utc-timescale( "" )

// Unix time scale based on UTC seconds but with 1970-01-01 epoch.
public val ts-unix: time-scale = utc-timescale( "UNIX", timestamp(2208988800) )

// NTP time scale is equal to the UTC time scale (`ts-utc`) but named `"NTP"`.
public val ts-ntp : time-scale = utc-timescale( "NTP" )



// Represents an instant in time.\
// Can be arbitrarily far into the future or past and is accurate up 
// to a tenth of a femto-second (10^-16s).
abstract struct instant (
  since: timestamp,                       
  ts   : time-scale
)

/*----------------------------------------------------------------------------
  Durations and instants
----------------------------------------------------------------------------*/


public fun instant-at( d : duration ) : instant {
  instant-at-timestamp( d.secs, d.frac, ts-tai )
}

public fun instant-at-timestamp( d : timestamp, ts : time-scale ) : instant {
  instant-at-timestamp(d.secs, d.frac, ts)
}

fun instant-at-timestamp( secs : int, frac : double = 0.0, ts : time-scale ) : instant {
  if (frac.neg? || (frac >= 1.0 && !ts.allow-leap)) {
    val fl = frac.floor
    Instant(timestamp(secs + fl.int, frac - fl), ts)
  } 
  else Instant(timestamp(secs,frac),ts)
}

fun instant-at-timestamp( d : double, ts : time-scale ) : instant {
  Instant(timestamp(d),ts)
}


// Return the duration since the `epoch`.
public fun since-epoch( i : instant ) : duration {
  i.timestamp(ts-tai)
}

// Compare two `:instant`s in time.
public fun compare( i : instant, j : instant ) : order {
  val h = j.ts-use(i.ts)
  compare(i.since,h.since)
}

public fun (<)( i : instant, j : instant )  : bool { compare(i,j) == Lt }
public fun (<=)( i : instant, j : instant ) : bool { compare(i,j) != Gt }
public fun (>)( i : instant, j : instant )  : bool { compare(i,j) == Gt }
public fun (>=)( i : instant, j : instant ) : bool { compare(i,j) != Lt }
public fun (==)( i : instant, j : instant ) : bool { compare(i,j) == Eq }
public fun (!=)( i : instant, j : instant ) : bool { compare(i,j) != Eq }


// Add a duration to an instant in time.
public fun (+)( i : instant, d : duration ) : instant {
  instant-at(i.since-epoch + d)
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

// Internal. Convert to timescale `tscale` to represent this instant.
public fun ts-use( i : instant, tscale : time-scale ) : instant {
  if (i.ts.ts-name == tscale.ts-name) then {
    i 
  }
  elif (tscale.allow-leap) {
    // trace("utc convert: " + i.show)
    Instant( (tscale.from-utc)( (i.ts.to-utc)(i.since)), tscale )
  }
  else {
    // trace("tai convert: " + i.show)
    Instant( (tscale.from-tai)( (i.ts.to-tai)(i.since) ), tscale)
  }
}

// Return a duration in time scale `ts` (=`i`'s timescale) for instant `i` since the time scale epoch.
public fun timestamp( i : instant, ts : time-scale = ts-tai ) : timestamp {
  i.ts-use(ts).since
}



/*--------------------------------------------------------------------------
  Show durations and instants
----------------------------------------------------------------------------*/


// Show an instant with an optional precision (=`9`, nano-second resolution).
public fun show( i : instant, prec : int = 9 ) : string {
  ts-show(i.since) + "s " + (if (i.ts.ts-name.empty?) then "" else " " + i.ts.ts-name)
}


/*----------------------------------------------------------------------------
  Julian Date
----------------------------------------------------------------------------*/

val jd-epoch-shift  = 2400000.5  // relative to the MJD epoch 
val mjd-epoch-shift = 15020.0    // relative to our epoch (1900-01-01)

// Create an instant given a [julian day](https://en.wikipedia.org/wiki/Julian_day).
public fun instant-at-jd( jd: double, ts : time-scale = ts-utc ) : instant {
  instant-at-mjd( jd - jd-epoch-shift, ts )
}

// Create an instant given a [modified julian day](https://en.wikipedia.org/wiki/Julian_day).\
// modified-julian-date = julian-date - 2400000.5
public fun instant-at-mjd( mjd : double, ts : time-scale = ts-utc ) : instant {
  val d    = mjd - mjd-epoch-shift // since 1900-01-01
  val frac = (d.fraction * secs-per-day.double).round-to-prec(6)
  val secs = d.trunc.int * secs-per-day + frac.trunc.int 
  val t    = timestamp(secs,frac.fraction) - ts.epoch-shift // since the time scale epoch
  instant-at-timestamp( t, ts )
}


public fun jd( i : instant, ts : time-scale = ts-utc ) : double {
  i.mjd(ts) + jd-epoch-shift
}

public fun mjd( i : instant, ts : time-scale = ts-utc ) : double {
  val d  = i.timestamp(ts) + ts.epoch-shift
  val (days,secs) = divmod(d.secs,secs-per-day)
  days.double + ((secs.double + d.frac) / secs-per-day.double) + mjd-epoch-shift
}

/*----------------------------------------------------------------------------
  Now
----------------------------------------------------------------------------*/

// Convert a unix time stamp to an instant in time.
// A unix time stamp measure seconds since the Unix epoch (1970-01-01)
// and ignores any leap seconds.
public fun instant-at-unix( d : double ) : instant {
  trace("unix: " + d.show)
  instant-at-timestamp( d, ts-unix)
}

// Convert an instant to a unix time stamp in seconds since the
// unix epoch (1970-01-01) ignoring any leap seconds.
public fun unix( i : instant ) : double {
  i.timestamp(ts-unix).seconds
}

// Convert an NTP time stamp to an instant in time.\
// An NTP time stamp measures seconds since the NTP epoch (1900-01-01)
// and ignores any leap seconds.
public fun instant-at-ntp( d : double ) : instant {
  instant-at-timestamp( d, ts-ntp )
}

// Convert an instant to a NTP time stamp in seconds since the
// NTP epoch (1900-01-01)
public fun ntp( i : instant ) : double {
  i.timestamp(ts-ntp).seconds
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
extern now-precision() : ndet int {
  js "_now_precision"
}

