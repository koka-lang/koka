/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Instants in time.

Instants use the mighty 128-bit `:ddouble` timestamps to represent a `:duration` since the `epoch`.
This gives very high range and precision. It spans about 10^300^ years
into the past and future, well beyond the expected life span of the universe.
Any time can be expressed up to atto-second (10^-18^) precision up to about 300,000 years 
in the past and future, and can express up to pico-second (10^-12^) precision any time 
since the age of the universe (about 13.8 billion years ago).

```unchecked
> instant(300000,1,1,0,0,0,1.0e-18).show(18) // 1 atto-second after Jan 1, 300000
"9403971696037.000000000000000001s"
> instant((~13.82e9).int,1,1,0,0,0,1.0e-12).show(12) // 1 pico-second after the birth of the universe
"-436117139753903999.999999999999s" 
```

Internally, instants are represented in a certain time scale (`:timescale`) for
efficiency and precision. They automatically convert between different time scales 
when necessary (for example when comparing instants in time, or calculating durations
between UTC calendar times).

Time durations (`:duration`) are always in SI seconds (as measured on the Earth's geoid).
*/
module std/time/instant

import std/num/ddouble
import std/time/timestamp
import std/time/duration

/*----------------------------------------------------------------------------
  Instant
----------------------------------------------------------------------------*/

/* Represents a precise instant in time.

Internally, instants are represented in a certain time scale (`:timescale`) for
efficiency and precision. They automatically convert between different time scales 
when necessary (for example when comparing instants in time, or calculating durations
between UTC calendar times).
*/
abstract struct instant(
  since : timestamp,  // since the time-scale epoch
  ts    : timescale   // the time scale (TAI, UTC, etc)
)

/*----------------------------------------------------------------------------
  Timescale
----------------------------------------------------------------------------*/

// A time scale defines how time is measured, defines the unit of time, the epoch,
// and how it can be converted to- and from TAI.\
// For time calculations, usually the [TAI](https://en.wikipedia.org/wiki/International_Atomic_Time)
// (international atomic time) time scale (`ts-tai`) is used which is time measured as SI seconds on the Earths geoid.
// Another common time scale is UTC (`std/time/utc/ts-utc`) which also uses SI second time units but can contain leap seconds.
abstract struct timescale(
  public name : string,
  unit        : string,
  from-tai    : duration -> timestamp,
  to-tai      : timestamp -> duration,
  // The y2k-epoch states at what timestamp _in this timescale_ is the date 2001-01-01.
  // It is used for date calculations.
  // y2k-epoch = instant(2000,1,1,cal=iso-calendar(<ts>).timestamp(<ts>) with <ts> this timescale
  y2k-epoch   : timestamp,
  epoch-shift : timespan = timespan0,
  secs-in-day : maybe<timestamp -> timespan> = Nothing
)


/*----------------------------------------------------------------------------
  Instant operations
----------------------------------------------------------------------------*/

// Change the internal representation of an instant to use another timescale.
// Only used in special cases for efficiency. For example, when comparing an 
// instant in TAI time to thousands of UTC times, it is more efficient to convert
// the TAI time to UTC first to avoid converting at each comparision.
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

// Returns the 'seconds' in the day of a timestamp with a given timescale.
// Is always 86400 for any timescale except UTC timescales after 1972 where a day
// can be 86399 up to 86401 due to leap seconds.
fun seconds-in-day( ts : timestamp, tscale : timescale ) : timespan {
  match(tscale.secs-in-day) {
    Nothing -> secs-per-day
    Just(f) -> f(ts)
  }
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
// Be careful to ensure that `t` should indeed be interpreted in the given time scale.
public fun unsafe-instant( t : timestamp, ts : timescale ) : instant {
  Instant(t,ts)
}

// Create an instant from a time span in `seconds` interpreted in time scale `ts`.\
// Be careful to ensure that `secs` should indeed be interpreted in the given time scale.
public fun unsafe-instant( secs : int, ts : timescale ) : instant {
  unsafe-instant(secs.timestamp, ts)
}


// Round an instant to a certain precision (`prec` is number of digits of the fraction of the second).\
// Takes special care for instants that use a UTC timescale to round into leap seconds if appropriate.
public fun round-to-prec(i : instant, prec : int ) : instant {
  if (prec.neg?) then i
  elif (i.ts.has-leap-seconds) 
   // Round on TAI representation instead; assumes the leap second time scale has SI seconds (which UTC has)
   // TODO: optimize to round first on UTC, and only when necessary convert to TAI
   then Instant( i.use(ts-tai).since.round-to-prec(prec), ts-tai ) 
   else Instant( i.since.round-to-prec(prec), i.ts )
}

// Does this time scale have leap seconds? (only for UTC time (`ts-utc`, `ts-unix`, and `ts-ntp`))
public fun has-leap-seconds( ts : timescale ) : bool {
  ts.secs-in-day.just?
}

// Our epoch is set at 2000-01-01 TAI (which is equal to 1999-12-31T23:59:28Z UTC).
//
// Another candidate epoch could have been the standard [J2000] epoch ([`epoch-j2000`](std_time_astro.html#epoch_j2000)), 
// which equals 2000-01-01T12:00:00 TT (terrestrial time).
// However, that would mean that for the most common time scales, namely UTC and TAI, there would always be a 
// fractional offset (of 32.184s) for common time stamps. Moreover, by having an epoch at noon there would be
// an extra correction needed for calendar date calculations too. 
//
// Similarly, the standard Unix epoch of 1970-01-01Z UTC is not ideal either since the UTC offset with TAI
// was fractional at that time (namely 8.000082s).
//
// Given these issues, we fixed the epoch at 2000-01-01T00:00:00 TAI instead.
//
// [J2000]: https://en.wikipedia.org/wiki/Equinox_(celestial_coordinates)#J2000.0
public val epoch : instant = Instant(timestamp0,ts-tai)

// Return the (TAI) SI second duration since the `epoch` at this instant.
public fun since-epoch( i : instant ) : duration {
  i.timestamp(ts-tai).unsafe-duration
}

// Return the instant at (TAI) SI seconds duration since the `epoch`.
public fun instant-since-epoch( d : duration ) : instant {
  unsafe-instant(d.timestamp, ts-tai)
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

// The minimum of two instants.
public fun min( i : instant, j : instant ) : instant {
  if (i <= j) then i else j
}

// The maximum of two instants.
public fun max( i : instant, j : instant ) : instant {
  if (i >= j) then i else j
}

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

// Show an instant as a number of (TAI) SI seconds since the `epoch` in a given precision.
// This can be used as an unambigious time stamp.
public fun show( i : instant, max-prec : int = 9, secs-width : int = 1 ) : string {
  i.use(ts-tai).show-raw(max-prec,secs-width)
}

// Internal: show an instant as a raw timestamp in a given precision, postfixed with the time scale name. 
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
// a `y2k-epoch` (= `timestamp0`) which is the timestamp of the 2000-01-01 date in that timescale
// e.g. for a timescale `ts`:\
// `y2k-epoch = instant(2000,1,1,cal=iso-calendar(ts)).timestamp(ts)`
public fun tai-timescale( name : string, offset : duration = zero, y2k-epoch : timestamp = timestamp0 ) : timescale {
  val epoch-shift = y2k-epoch.timespan + offset.timespan
  fun from-tai(tai:duration) { tai.timestamp + epoch-shift }
  fun to-tai(ts:timestamp)   { (ts - epoch-shift).unsafe-duration }
  Timescale(
    name,
    "TAI",
    from-tai,
    to-tai,
    y2k-epoch,
    epoch-shift,
    Nothing
  )
}

// Create a new time scale given `name`, two inverse function `from-tai` and `to-tai`,
// and a `y2k-epoch` (=`timestamp0`). The time unit defaults to `name`. 
// The `y2k-epoch` is the timestamp of the 2000-01-01 date in the new timescale
// e.g. for a timescale `ts`:\
// `y2k-epoch = instant(2000,1,1,cal=iso-calendar(ts)).timestamp(ts)`
public fun timescale( name : string, 
                      from-tai : duration -> timestamp, 
                      to-tai   : timestamp -> duration, 
                      y2k-epoch : timestamp = timestamp0,
                      unit : string = name,
                      secs-in-day : maybe<timestamp -> timespan> = Nothing
                    ) : timescale {
  Timescale(
    name,
    unit,
    from-tai,
    to-tai,
    y2k-epoch,
    y2k-epoch.timespan,
    secs-in-day
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
  unsafe-instant(timestamp0,ts)
}

// Return the instant of the date 2000-01-01 of a particular time scale `ts` (when imposed on a proleptic Gregorian calendar).\
// `timescale-y2k(ts) == instant(2000,1,1,cal=iso-calendar(ts))`
public fun timescale-y2k( ts : timescale ) : instant {
  unsafe-instant(ts.y2k-epoch,ts)
}

// Show a time stamp with an optional maximum precision (`max-prec` (=`9`)) and 
// minimum width for the seconds (=`1`).
public fun show( ts : timestamp, max-prec : int = 9, secs-width : int = 1 ) : string {
  ts.ts-show(max-prec,secs-width)
}


/*----------------------------------------------------------------------------
  Julian Date
----------------------------------------------------------------------------*/
val jd-epoch-delta  = timespan(2400000.5)   // relative to the MJD epoch 
val mjd-epoch-delta = timespan(51544)       // relative to our epoch (2000-01-01)
val secs-per-day    = timespan(86400)

// Create an instant given a [julian day](https://en.wikipedia.org/wiki/Julian_day).
public fun instant-at-jd( jd: double, ts : timescale ) : instant {
  instant-at-jd(ddouble(jd),ts)
}

// Create an instant given a [julian day](https://en.wikipedia.org/wiki/Julian_day)
// and time scale `ts` (=`ts-utc`).
public fun instant-at-jd( jd: ddouble, ts : timescale  ) : instant {
  instant-at-mjd( jd - jd-epoch-delta, ts )
}

// Create an instant given a [modified julian day](https://en.wikipedia.org/wiki/Julian_day).
// and time scale `ts` (=`ts-utc`).\
// `modified-julian-day = julian-day - 2400000.5`
public fun instant-at-mjd( mjd : double, ts : timescale ) : instant {
  instant-at-mjd(ddouble(mjd),ts)
}

// Create an instant given a [modified julian day](https://en.wikipedia.org/wiki/Julian_day).
// and time scale `ts` (=`ts-utc`).\
// `modified-julian-day = julian-day - 2400000.5`
public fun instant-at-mjd( mjd : ddouble, tscale : timescale  ) : instant {
  val d  = mjd - mjd-epoch-delta // since our epoch
  val ts = tscale.y2k-epoch + (d*secs-per-day) // since the time scale epoch
  val daysecs = ts.seconds-in-day(tscale)
  if (daysecs==secs-per-day) then return unsafe-instant(ts,tscale)
  // this is longer/shorter day due to leap seconds; adjust the fraction
  val days = d.trunc
  val sid  = (d - days)*daysecs  // seconds in the day
  val (nlsecs,lsecs) = if (sid >= secs-per-day)
                        then (secs-per-day.dec, sid.inc - secs-per-day)
                        else (sid,zero:ddouble)
  val ts1  = tscale.y2k-epoch + (days*secs-per-day) + nlsecs
  val ts2  = ts1.adjust-leap-seconds(lsecs)
  unsafe-instant(ts2,tscale) 
}

/* Return the julian day in a given time scale `ts` (=`ts-utc`) for an instant `i`.
Properly takes leap seconds into account when calculating the fraction of the day 
in a UTC calendar. For example:
````  
> time(2014,12,31,23,59,59).jd
2457023.499988425925926

> time(2015,12,31,23,59,59).jd
2457388.499988425925926

> time(2016,12,31,23,59,59).jd
2457754.499976852119767

> time(2016,12,31,23,59,60).jd
2457754.499988426059884
````
.
*/
public fun jd( i : instant, ts : timescale ) : ddouble {
  i.mjd(ts) + jd-epoch-delta
}

val femto = 15 // femto-second precision

// Return the modified julian day in a given time scale `ts` (=`ts-utc`) for an instant `i`.
// Can also pass an optional `delta` (=`timespan0`) that is added to the raw timestamp of `i`
// before conversion (used in `std/time/time` to take timezones into account)
public fun mjd( i : instant, tscale : timescale, delta : timespan = timespan0 ) : ddouble {
  val ts0     = i.timestamp(tscale)
  val daysecs = ts0.seconds-in-day(tscale) // total-seconds in the day of the timezone (before delta)
  val ts      = ts0 + delta
  if (daysecs==secs-per-day) then {
    // without leap seconds, just divide. 
    div((ts.unadjusted-seconds - tscale.y2k-epoch.timespan), secs-per-day, femto) + mjd-epoch-delta
  }
  else {
    // otherwise, divide the non-leap-seconds first, but for the seconds in the day
    // divide by the longer day so the day fraction is correct.
    // trace("mjd: day seconds: " + daysecs.show)
    val leaps = ts.leap-adjust
    val secs  = (ts.unadjusted-seconds - tscale.y2k-epoch.timespan)
    val w     = div(secs, secs-per-day, femto).floor
    val r     = secs - (w * secs-per-day)
    val f     = div(r + leaps.timespan, daysecs, femto+3) // see: test/lib/time3, jdleapx2  -- extra precision required on leap seconds
    (w + f + mjd-epoch-delta)
  }
}

