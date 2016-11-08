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
public val epoch = instant-at(0.0,ts-utc)


// A `:time-scale` represents a duration relative to a certain epoch,
// and can convert to- and from TAI and UTC seconds. Time scales are
// mostly used internally to minimize conversions for efficiency.
public struct time-scale(
  ts-name    : string,
  from-tai   : duration -> ts-duration,
  to-tai     : ts-duration -> duration,
  from-utc   : utc-duration -> ts-duration,
  to-utc     : ts-duration -> utc-duration,
  allow-leap : bool = False,
  epoch-shift: ts-duration = duration0
)


// Create a new time scale based on UTC 'seconds' with a given `name` and `epoch-shift`
// duration away from the standard `epoch`.
public fun utc-timescale( name : string, epoch-shift : ts-duration = duration0 ) : time-scale {
  fun from-utc(d:ts-duration) { d - epoch-shift }
  fun to-utc(d:ts-duration)   { d + epoch-shift } 
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

// Create a new time scale based on SI seconds with a given `name` and `epoch-shift`
// duration away from the standard `epoch`.
public fun tai-timescale( name : string, epoch-shift : duration = duration0 ) : time-scale {
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

// The TAI time scale based on SI seconds with 1900-01-01 epoch.
public val ts-tai : time-scale = tai-timescale( "TAI" )
 
// The GPS time scale based on SI seconds with 1980-01-06 epoch.\
// GPS = TAI - 19.
public val ts-gps : time-scale = tai-timescale( "GPS", duration(2524953619) )

// The UTC time scale with a 1900-01-01 epoch. This uses 'UTC seconds' where
// one day consists of 86400 'solar' seconds. Leap seconds are encoded by using a
// fraction `>= 1.0`. 
public val ts-utc : time-scale = utc-timescale( "" )

// Unix time scale based on UTC seconds but with 1970-01-01 epoch.
public val ts-unix: time-scale = utc-timescale( "UNIX", duration(2208988800) )

// NTP time scale is equal to the UTC time scale (`ts-utc`) but named `"NTP"`.
public val ts-ntp : time-scale = utc-timescale( "NTP" )

// Modified Julian day time scale based on TAI seconds with a 1858-11-17 epoch.
public val ts-mjd : time-scale = tai-timescale( "MJD", duration(~1297728000) )

// Julian day time scale based on TAI seconds with ``-4713-11-24Z`` as epoch.
public val ts-jd  : time-scale = tai-timescale( "JD", duration(~208657814400) )


// Represents an instant in time.\
// Can be arbitrarily far into the future or past and is accurate up 
// to a tenth of a femto-second (10^-16s).
abstract struct instant (
  since: ts-duration,                       
  ts   : time-scale
)

/*----------------------------------------------------------------------------
  Durations and instants
----------------------------------------------------------------------------*/

// Create an instant in time given a number of seconds since the `epoch`
// and fraction of seconds (=``0.0``).
public fun instant-at( secs : int, frac : double = 0.0, ts : time-scale = ts-utc ) : instant {
  instant-sincex(secs,frac,ts)
}

private fun instant-sincex( secs : int, frac : double, ts : time-scale ) : instant {
  if (frac.neg? || (frac >= 1.0 && !ts.allow-leap)) {
    val fl = frac.floor
    Instant(duration(secs + fl.int, frac - fl), ts)
  } 
  else Instant(duration(secs,frac),ts)
}

// Create an instant given a number of seconds since the `epoch`
// given as a `:double`.
public fun instant-at( secs : double, ts : time-scale = ts-utc ) : instant {
  Instant( duration(secs), ts )
}

// Return the duration since the `epoch`.
public fun since-epoch( i : instant ) : duration {
  i.ts-since-epoch(ts-tai)
}

// Return the instant in time that is `d` duration after the `epoch`.
public fun since-epoch( d : duration ) : instant {
  d.ts-since-epoch(ts-tai)
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

// Internal. Return a duration in time scale `ts` (=`i`'s timescale) for instant `i` since the time scale epoch.
public fun ts-since-epoch( i : instant, ts : time-scale = i.ts ) : ts-duration {
  i.ts-use(ts).since
}

// Internal. Create an instant with a given duration in time scale `ts`.
public fun ts-since-epoch( d : ts-duration, ts : time-scale ) : instant {
  Instant(d,ts)
}

// Internal. Add a non SI-seconds duration directly to an instant with normalizing to a TAI time scale.
public fun ts-add( i : instant, d : ts-duration ) : instant {
  Instant(i.since + d, i.ts)
}

// Internal. Return the difference between two instants using the timescale of `i`.
public fun ts-diff( i : instant, j : instant ) : ts-duration {
  val d = i.since 
  val e = j.ts-use(i.ts).since
  d - e
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

val jd-epoch-shift     = 2400000.5
 
// Create an instant given a [julian day](https://en.wikipedia.org/wiki/Julian_day).
public fun instant-at-jd( julian-date: double ) : instant {
  instant-at-mjd( julian-date - jd-epoch-shift )
}

// Create an instant given a [modified julian day](https://en.wikipedia.org/wiki/Julian_day).\
// modified-julian-date = julian-date - 2400000.5
public fun instant-at-mjd( modified-julian-date : double ) : instant {
  val frac = modified-julian-date.fraction * secs-per-day.double
  val secs = modified-julian-date.trunc.int * secs-per-day + frac.trunc.int 
  ts-since-epoch( duration(secs,frac.fraction), ts-mjd )
}

// Return the [julian day](https://en.wikipedia.org/wiki/Julian_day) of an instant.
public fun jd( i : instant ) : double {
  i.mjd + jd-epoch-shift
}

// Return the [modified julian day](https://en.wikipedia.org/wiki/Julian_day) of an instant.
public fun mjd( i : instant ) : double {
  val d = i.ts-since-epoch(ts-mjd)
  val (days,secs) = divmod(d.secs,secs-per-day)
  days.double + ((secs.double + d.frac) / secs-per-day.double)
}


/*----------------------------------------------------------------------------
  Now
----------------------------------------------------------------------------*/

// Convert a unix time stamp to an instant in time.
// A unix time stamp measure seconds since the Unix epoch (1970-01-01)
// and ignores any leap seconds.
public fun instant-at-unix( timestamp : double ) : instant {
  instant-at(timestamp,ts-unix)
  // utc-duration(timestamp).ts-since-epoch(ts-unix )
}

// Convert an instant to a unix time stamp in seconds since the
// unix epoch (1970-01-01) ignoring any leap seconds.
public fun unix( i : instant ) : double {
  i.ts-since-epoch(ts-unix).seconds
}

// Convert an NTP time stamp to an instant in time.\
// An NTP time stamp measures seconds since the NTP epoch (1900-01-01)
// and ignores any leap seconds.
public fun instant-at-ntp( timestamp : double ) : instant {
  instant-at( timestamp, ts-ntp )
  }

// Convert an instant to a NTP time stamp in seconds since the
// NTP epoch (1900-01-01)
public fun ntp( i : instant ) : double {
  i.ts-since-epoch(ts-ntp).seconds
}

// The current `:instant` in time.
public fun now() : ndet instant {
  instant-at-unix( unix-now().round-to-prec(now-precision()) ) 
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

