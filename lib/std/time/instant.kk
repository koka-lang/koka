/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Instants in time.

   Instants can be arbitrary far into the past
   and future and are precise up to 10^-15^s (a femto-second). 
   Instants are represented in a certain time scale (`:timescale`) and can
   automatically convert between different time scales when necessary (for 
   example when comparing instants in time).

   Time durations (`:duration`) are always in SI seconds (as measured on the Earth's geoid).
*/
module std/time/instant

import std/fixed
import std/time/timespan
import std/time/duration

/*----------------------------------------------------------------------------
  Instant
----------------------------------------------------------------------------*/

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

// A time scale defines how time is measured, and defines the unit of time, the epoch,
// and how it can be converted to- and from TAI.\
// For time calculations, usually the [TAI](https://en.wikipedia.org/wiki/International_Atomic_Time)
// (international atomic time) time scale is used which is time measured as SI seconds on the Earths geoid.
// Another scale is UTC which also uses SI second time units but contains leap seconds.
abstract struct timescale(
  public name : string,
  unit        : string,
  from-tai    : duration -> timestamp,
  to-tai      : timestamp -> duration,
  // The y2k-epoch states at what timestamp _in this timescale_ is the date 2001-01-01.
  // It is used for date calculations.
  // y2k-epoch = instant-at(2000,1,1,cal=iso-calendar(<ts>).timestamp(<ts>) with <ts> this timescale
  y2k-epoch   : timestamp,
  epoch-shift : timespan = timespan0
)


/*----------------------------------------------------------------------------
  Instant operations
----------------------------------------------------------------------------*/

// Change the internal representation of an instant to use another timescale.
// Only used in special cases for efficiency.
public fun use( i : instant, tscale : timescale ) : instant {
  if (i.ts.name == tscale.name) then {
    // already using the right time scale
    i
  }
  else {
    Instant( i.timestamp(tscale), tscale )
  }
}

// Convert a timestamp between time scales
fun convert( t : timestamp, from : timescale, to : timescale ) : timestamp {
  if (from.name == to.name) then {
    // already using the right time scale
    t
  }
  elif (!from.unit.empty? && from.unit == to.unit) then {
    // just an epoch shift
    // important optimization as it allows UNIX timestamp to UTC conversion without going through TAI for example
    // trace( "instant use: shift: " + from.name + " -> " + to.name + ", " + from.epoch-shift.ts-show + " vs. " + to.epoch-shift.ts-show + ", t: " + t.show )
    (t - from.epoch-shift) + to.epoch-shift
  }
  else {
    // transform through TAI
    // trace( "convert: " + from.name + " -> " + to.name +  ", t: " + t.show )
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
  if (prec.neg?) then i
  elif (i.ts.has-leap-seconds && i >= utc-epoch) 
   then Instant( i.use(ts-tai).since.round-to-prec(prec), ts-tai ) // TODO: optimize to round first on UTC, and only when necessary convert to TAI
   else Instant( i.since.round-to-prec(prec), i.ts )
}
val utc-epoch = Instant(timestamp(~883612789),ts-tai)

fun has-leap-seconds( ts : timescale ) : bool {
  ts.unit == "UTC"
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
  if (i.ts.unit=="TAI")
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
   + (if (i.ts.unit == "TAI") then "s" else "")
   + (if (i.ts.name.empty? || i.ts.name=="TAI") then "" else " " + i.ts.name)
}

/*----------------------------------------------------------------------------
  Time scales
----------------------------------------------------------------------------*/

// Create a new time scale based on SI seconds (as measured on the Earth's geoid) with a given
//  `name`, a fixed `offset` (=`duration0`) from TAI (e.g. GPS = TAI - 19), and 
// a `y2k-epoch` (= `timestamp0`) which is the timestamp of the 2001-01-01 date in that timescale
// e.g. for a timescale `ts`:\
// `y2k-epoch = instant-at(2000,1,1,cal=iso-calendar(ts)).timestamp(ts)`
public fun tai-timescale( name : string, offset : duration = duration0, y2k-epoch : timestamp = timestamp0 ) : timescale {
  val epoch-shift = y2k-epoch.timespan + offset.timespan
  fun from-tai(tai:duration) { tai.timestamp + epoch-shift }
  fun to-tai(ts:timestamp)   { (ts - epoch-shift).unsafe-duration }
  Timescale(
    name,
    "TAI",
    from-tai,
    to-tai,
    y2k-epoch,
    epoch-shift
  )
}

// Create a new time scale given `name`, two inverse function `from-tai` and `to-tai`,
// and a `y2k-epoch` (=`timestamp0`). The time unit defaults to `name`. 
// The `y2k-epoch` is the timestamp of the 2001-01-01 date in the new timescale
// e.g. for a timescale `ts`:\
// `y2k-epoch = instant-at(2000,1,1,cal=iso-calendar(ts)).timestamp(ts)`
public fun timescale( name : string, 
                      from-tai : duration -> timestamp, 
                      to-tai   : timestamp -> duration, 
                      y2k-epoch : timestamp = timestamp0,
                      unit : string = name
                    ) : timescale {
  Timescale(
    name,
    unit,
    from-tai,
    to-tai,
    y2k-epoch,
    y2k-epoch.timespan
  )
}


// The [TAI](https://en.wikipedia.org/wiki/International_Atomic_Time) (International atomic time) 
// time scale is based on SI seconds measured on the Earth's geoid, with a 2000-01-01 TAI `epoch`.
public val ts-tai : timescale = tai-timescale( "TAI" )

// The [GPS](https://en.wikipedia.org/wiki/Global_Positioning_System#Timekeeping) time scale based 
// on SI seconds with a 1980-01-06 GPS epoch.\
// GPS = TAI - 19s.
public val ts-gps : timescale = tai-timescale( "GPS", duration(~19), timestamp(630720000) )


// Return the epoch of a certain time scale.
public fun timescale-epoch( ts : timescale ) : instant {
  unsafe-instant-at(timestamp0,ts)
}

// Return the instant of the date 2000-01-01 of a particular time scale `ts` (when imposed on a proleptic Gregorian calendar).\
// `timescale-y2k(ts) == instant-at(2000,1,1,cal=iso-calendar(ts))`
public fun timescale-y2k( ts : timescale ) : instant {
  unsafe-instant-at(ts.y2k-epoch,ts)
}

// Show a time stamp with an optional maximum precision (`max-prec` (=`9`)) and 
// minimum width for the seconds (=`1`).
public fun show( ts : timestamp, max-prec : int = 9, secs-width : int = 1 ) : string {
  ts.ts-show(max-prec,secs-width)
}


/*----------------------------------------------------------------------------
  Julian Date
----------------------------------------------------------------------------*/
val jd-epoch-delta  = fixed(24000005,1)  // relative to the MJD epoch 
val mjd-epoch-delta = fixed(51544)       // relative to our epoch (2000-01-01)
val secs-per-day    = fixed(86400)

// Create an instant given a [julian day](https://en.wikipedia.org/wiki/Julian_day).
public fun instant-at-jd( jd: double, ts : timescale ) : instant {
  instant-at-jd(fixed(jd),ts)
}

// Create an instant given a [julian day](https://en.wikipedia.org/wiki/Julian_day)
// and time scale `ts` (=`ts-utc`).
public fun instant-at-jd( jd: fixed, ts : timescale  ) : instant {
  instant-at-mjd( jd - jd-epoch-delta, ts )
}

// Create an instant given a [modified julian day](https://en.wikipedia.org/wiki/Julian_day).
// and time scale `ts` (=`ts-utc`).\
// `modified-julian-day = julian-day - 2400000.5`
public fun instant-at-mjd( mjd : double, ts : timescale ) : instant {
  instant-at-mjd(fixed(mjd),ts)
}

// Create an instant given a [modified julian day](https://en.wikipedia.org/wiki/Julian_day).
// and time scale `ts` (=`ts-utc`).\
// `modified-julian-day = julian-day - 2400000.5`
public fun instant-at-mjd( mjd : fixed, ts : timescale  ) : instant {
  val d    = mjd - mjd-epoch-delta // since our epoch
  val t    = ts.y2k-epoch + timespan(d*secs-per-day) // since the time scale epoch
  unsafe-instant-at(t,ts) 
}

// Return the julian day in a give time scale `ts` (=`ts-utc`) for an instant `i`.
public fun jd( i : instant, ts : timescale, max-prec : int = 9 ) : fixed {
  i.mjd(ts,max-prec) + jd-epoch-delta
}

// Return the modified julian day in a give time scale `ts` (=`ts-utc`) for an instant `i`.
public fun mjd( i : instant, ts : timescale, max-prec : int = 9 ) : fixed {
  val t = i.timespan(ts) - ts.y2k-epoch.timespan
  (div)(t.seconds, secs-per-day, max-prec) + mjd-epoch-delta
}
