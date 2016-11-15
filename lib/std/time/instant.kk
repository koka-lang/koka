/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Instants in time.

   Represents instants in time. Instants can be arbitrary far into the past
   and future and are precise up to 10^-15^s (a femto-second). 
   Instants are represented in a certain time scale (`:timescale`) and can
   automatically convert between different time scales when necessary (for 
   example when comparing instants in time). 
   However, durations (`:duration`) are always in (TAI) SI seconds.
*/
module std/time/instant

import std/fixed
import std/time/timespan
import std/time/duration

/*----------------------------------------------------------------------------
  Instant
----------------------------------------------------------------------------*/

// The number of seconds in a day.
public val secs-per-day = 86400

// Represents an instant in time. Instants can be arbitrary far into the past
// and future and are precise up to 10^-15^s (a femto-second). 
// Instants are represented in a certain time scale (`:timescale`) and can
// automatically convert between different time scales when necessary (for 
// example when comparing instants in time). 
abstract struct instant(
  since : timestamp,  // since the time-scale epoch
  ts    : timescale   // the time scale (TAI, UTC, etc)
)

/*----------------------------------------------------------------------------
  Timescale
----------------------------------------------------------------------------*/

// A time scale defines how time is measured, in particular the unit of time, the epoch,
// and how it can be converted to TAI.\
// For time calculations, usually the [TAI](https://en.wikipedia.org/wiki/International_Atomic_Time)
// (international atomic time) time scale is used which is time measured as SI seconds on the Earths geoid.
// Another scale is UTC which also uses SI second time units but contains leap seconds.
public struct timescale(
  ts-name          : string,
  ts-unit          : string,
  private from-tai : duration -> timestamp,
  private to-tai   : timestamp -> duration,
  private epoch-delta : timespan,  // epoch~TS~ = 2000-01-01TAI + epoch-delta
  epoch-jd-delta      : timespan   // epoch~TS~ = 2000-01-01~TS~ + epoch-jd-delta; used for julian date calculation
)


/*----------------------------------------------------------------------------
  Instant operations
----------------------------------------------------------------------------*/

// Change the internal representation of an instant to use another timescale.
// Only used in special cases for efficiency.
public fun use( i : instant, tscale : timescale ) : instant {
  if (i.ts.ts-name == tscale.ts-name) then {
    // already using the right time scale
    i
  }
  else {
    Instant( i.timestamp(tscale), tscale )
  }
}

// Convert a timestamp between time scales
fun convert( t : timestamp, from : timescale, to : timescale ) : timestamp {
  if (from.ts-name == to.ts-name) then {
    // already using the right time scale
    t
  }
  elif (!from.ts-unit.empty? && from.ts-unit == to.ts-unit) then {
    // just an epoch shift
    // important optimization as it allows UNIX timestamp to UTC conversion without going through TAI for example
    // trace( "instant use: shift: " + i.ts.ts-name + " -> " + t.ts-name + ", " + i.ts.epoch-delta.show + " vs. " + t.epoch-delta.show + ", i: " + i.since.show )
    (t + from.epoch-delta) - to.epoch-delta
  }
  else {
    // transform through TAI
    (to.from-tai)( (from.to-tai)(t) ) 
  }
}

// Return the `:duration` since the `epoch` for a timestamp `t` interpreted in time scale `ts`.
public fun to-tai(  ts : timescale, t : timestamp ) : duration {
  t.convert(ts,ts-tai).unsafe-duration
}

// Given a `:duration` since the `epoch`, return a `:timestamp` for that instant in time scale `ts`.
public fun from-tai(ts : timescale, d : duration ) : timestamp {
  d.timestamp.convert(ts-tai,ts)
}

// Return the time scale that instant `i` uses.
public fun timescale( i : instant ) : timescale {
  i.ts
}

// Return a `:timestamp` for instant `i` in a certain time scale `tscale` (= `ts-tai`).
public fun timestamp( i : instant, tscale : timescale = ts-tai ) : timestamp {
  i.since.convert(i.ts,tscale)
}


// Return a `:timespan` for instant `i` in a certain time scale `tscale` (= `ts-tai`) (since the `tscale` epoch).
public fun timespan( i : instant, tscale : timescale = ts-tai ) : timespan {
  i.timestamp(tscale).timespan
}

// Create an instant from a time stamp `t` interpreted in time scale `ts`.\
// Be careful to ensure that `t` should indeed be interepreted in the given time scale.
public fun unsafe-instant-at( t : timestamp, ts : timescale ) : instant {
  Instant(t,ts)
}

// Round an instant to a certain precision (`prec` is number of digits of the fraction of the second).\
// Takes special care for instants that use a UTC timescale to round into leap seconds if appropriate.
public fun round-to-prec(i : instant, prec : int ) : instant {
  if (i.ts.has-leap-seconds && i >= utc-epoch) 
   then Instant( i.use(ts-tai).since.round-to-prec(prec), ts-tai )
   else Instant( i.since.round-to-prec(prec), i.ts )
}
val utc-epoch = Instant(timestamp(~883612789),ts-tai)

fun has-leap-seconds( ts : timescale ) : bool {
  ts.ts-unit == "UTC"
}

// Our epoch is set at 2000-01-01 TAI (= 1999-12-31T23:59:28Z UTC)
public val epoch : instant = Instant(timestamp0,ts-tai)

// Return the (TAI) SI second duration since the `epoch` at this instant.
public fun since-epoch( i : instant ) : duration {
  i.timestamp(ts-tai).unsafe-duration
}

// Return the instant at (TAI) SI second duration from the the `epoch`.
public fun since-epoch( d : duration ) : instant {
  unsafe-instant-at(d.timestamp, ts-tai)
}

// Compare two `:instant`s in time.
public fun compare( i : instant, j : instant ) : order {
  compare(i.since, j.use(i.ts).since)
}

public fun (<)( i : instant, j : instant )  : bool { compare(i,j) == Lt }
public fun (<=)( i : instant, j : instant ) : bool { compare(i,j) != Gt }
public fun (>)( i : instant, j : instant )  : bool { compare(i,j) == Gt }
public fun (>=)( i : instant, j : instant ) : bool { compare(i,j) != Lt }
public fun (==)( i : instant, j : instant ) : bool { compare(i,j) == Eq }
public fun (!=)( i : instant, j : instant ) : bool { compare(i,j) != Eq }


// Add a duration to an instant in time.
public fun (+)( i : instant, d : duration ) : instant {
  if (i.ts.ts-unit=="TAI")
   then Instant( i.since + d.timespan, i.ts ) // directly add
   else Instant( i.timestamp(ts-tai) + d.timespan, ts-tai )
}

// Subtract a duration from an instant in time.
public fun (-)( i : instant, d : duration ) : instant {
  i + (~d)
}

// Return the duration between to instants in time.
public fun (-)( i : instant, j : instant ) : duration {
  i.since-epoch - j.since-epoch
}

// Show an instant as a number of (TAI) SI seconds since the `epoch` in a given precision
public fun show( i : instant, max-prec : int = 9, secs-width : int = 1 ) : string {
  i.use(ts-tai).show-raw(max-prec,secs-width)
}

// Show an instant as a raw timestamp in a given precision
public fun show-raw( i : instant, max-prec : int = 9, secs-width : int = 1 ) : string {
  i.since.show(max-prec,secs-width) 
   + (if (i.ts.ts-unit == "TAI") then "s" else "")
   + (if (i.ts.ts-name.empty? || i.ts.ts-name=="TAI") then "" else " " + i.ts.ts-name)
}


// Create a new time scale based on SI seconds on the Earth's geoid with a given
//  `name`, an `offset` (=`duration0`) from TAI, and `epoch-jd-delta` (= `duration0`:\
// For a new timescale `ts`: `epoch-jd-delta - offset` = `ts.ts-epoch` - `epoch` 
public fun tai-timescale( name : string, offset : duration = duration0, epoch-jd-delta : duration = duration0 ) : timescale {
  val epoch-delta = epoch-jd-delta - offset
  fun from-tai(tai:duration) { (tai - epoch-delta).timestamp }
  fun to-tai(ts:timestamp)   { ts.unsafe-duration + epoch-delta }
  Timescale(
    name,
    "TAI",
    from-tai,
    to-tai,
    epoch-delta.timespan,
    epoch-jd-delta.timespan
  )
}

// The [TAI](https://en.wikipedia.org/wiki/International_Atomic_Time) (International atomic time) 
// time scale is based on SI seconds as measured on the Earth's geoi, with a 2000-01-01 TAI `epoch`.
public val ts-tai : timescale = tai-timescale( "TAI" )

// Return the epoch of a certain time scale.
public fun ts-epoch( ts : timescale ) : instant {
  unsafe-instant-at(timestamp0,ts)
}

// Show a time stamp with an optional maximum precision (`max-prec` (=`9`)) and 
// minimum width for the seconds (=`1`).
public fun show( ts : timestamp, max-prec : int = 9, secs-width : int = 1 ) : string {
  ts.ts-show(max-prec,secs-width)
}
