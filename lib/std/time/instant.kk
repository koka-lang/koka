/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Instants in time.

Instants use the mighty 128-bit `:ddouble` timestamps to represent a `:duration`
since the `epoch`.
This gives very high range and precision (up 31 decimal digits).
It spans about 10^300^ years
into the past and future, well beyond the expected life span of the universe.
Any time can be expressed with atto-second (10^-18^) precision
up to about 300,000 years in the past and future, and with pico-second (10^-12^)
precision any time since the age of the universe (about 13.8 billion years ago)
up to 30 billion years into the future. For durations under 300 years, the precision
is in excess of a zepto second (10^-21^). For comparison, it takes light about
500 zepto-seconds to travel the length of an hydrogen atom.

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
  Timescale
----------------------------------------------------------------------------*/

// A time scale defines how time is measured, defines the unit of time, the epoch,
// and how it can be converted to- and from TAI.\
// For time calculations, usually the [TAI](https://en.wikipedia.org/wiki/International_Atomic_Time)
// (international atomic time) time scale (`ts-tai`) is used which is time measured as SI seconds on the Earths geoid.
// Another common time scale is UTC (`std/time/utc/ts-utc`) which also uses SI second time units but can contain leap seconds.
abstract struct timescale(
  // Time scale name, "TAI", "UTC", "UNIX", "TT", etc.
  public name : string,
  // SI seconds: usually "TAI", but could be "TCG", "TCB", "TDB" etc.
  unit        : string,
  // from duration since `epoch` to timestamp since epoch-y2k
  from-tai    : duration -> timestamp,
  // from timestamp since timescale-epoch to duration since `epoch`
  to-tai      : timestamp -> duration,
  // `Nothing` for fixed 86400s days, or
  // a function to return the seconds in the day of the given `:dayspan` (for leap seconds timescales).
  get-seconds-in-day : maybe<(timestamp) -> timespan>
)

// Does this timescale have leap seconds?
public fun has-leap-seconds(ts : timescale) : bool {
  ts.get-seconds-in-day.bool
}


// Convert a timespan between time scales
fun convert( t : timestamp, from : timescale, to : timescale ) : timestamp {
  if (from.name == to.name) then {
    // already using the right time scale
    t
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

// Given a `:duration` since the `epoch`, return a `:timespan` for that instant in time scale `ts`.
public fun from-tai(ts : timescale, d : duration ) : timestamp {
  d.timestamp.convert(ts-tai,ts)
}

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
  since : timestamp,  // time since the 2000-01-01 in the timescale
  ts    : timescale   // the time scale (TAI, UTC, etc)
)

// Return the time scale that instant `i` uses.
public fun timescale( i : instant ) : timescale {
  i.ts
}

// The seconds in the day of instant `i` (in its time scale).
public fun seconds-in-day(i : instant) : timespan {
  match(i.ts.get-seconds-in-day) {
    Nothing -> solar-secs-per-day
    Just(f) -> f(i.since)
  }
}

// Return `:timestamp` since 2000-01-01 in the time scale of the instant
public fun timestamp( i : instant ) : timestamp {
  i.since
}

// Return `:dayspan` since 2000-01-01 in the time scale of the instant
public fun dayspan( i : instant ) : dayspan {
  i.since.dayspan
}


// Create an instant from a time stamp `t` interpreted in time scale `ts`.\
// Be careful to ensure that `t` should indeed be interpreted in the given time scale.
public fun instant( ts : timescale, t : timestamp ) : instant {
  Instant(t, ts)
}

// Create an instant from a dayspan `ds` interpreted in time scale `ts`.\
// Be careful to ensure that `ds` should indeed be interpreted in the given time scale.
public fun instant( ts : timescale, ds : dayspan ) : instant {
  instant(ts, ds.timestamp)
}

// Return the instant at (TAI) SI seconds duration since the `epoch`.
public fun instant( d : duration ) : instant {
  instant(ts-tai, d.timestamp)
}

// Return a `:timestamp` for instant `i` in a certain time scale `tscale` (= `ts-tai`)
public fun timestamp-in( i : instant, tscale : timescale = ts-tai ) : timestamp {
  i.since.convert(i.ts,tscale)
}

// Return a `:dayspan` for instant `i` in a certain time scale `tscale` (= `ts-tai`)
public fun dayspan-in( i : instant, tscale : timescale = ts-tai ) : dayspan {
  i.timestamp-in(tscale).dayspan
}

// Return the (TAI) SI second duration since the `epoch` at this instant.
public fun duration( i : instant ) : duration {
  i.timestamp-in(ts-tai).unsafe-duration // ok, because SI seconds
}


// Change the internal representation of an instant to use another timescale.
// Only used in special cases for efficiency. For example, when comparing an
// instant in TAI time to thousands of UTC times, it is more efficient to convert
// the TAI time to UTC first to avoid converting at each comparision.
public fun use-timescale( i : instant, tscale : timescale ) : instant {
  if (i.ts.name == tscale.name) then {
    // already using the right time scale
    i
  }
  else {
    Instant( i.timestamp-in(tscale), tscale )
  }
}


// Round an instant to a certain precision (`prec` is number of digits of the fraction of the second).\
// Takes special care for instants that use a UTC timescale to round into leap seconds if appropriate.
public fun round-to-prec(i : instant, prec : int ) : instant {
  if (prec.neg?) then i
  elif (i.ts.has-leap-seconds)
   // Round on TAI representation instead; assumes the leap second time scale has TAI SI seconds (which UTC has)
   // TODO: optimize to round first on UTC, and only when necessary convert to TAI
   then Instant( i.use-timescale(ts-tai).since.round-to-prec(prec), ts-tai )
   else Instant( i.since.round-to-prec(prec), i.ts )
}

// Compare two `:instant`s in time.
public fun compare( i : instant, j : instant ) : order {
  compare(i.since, j.use-timescale(i.ts).since)
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
// Note: this generally entails conversion to TAI time (`ts-tai`).
// See also `add-duration-in` and `add-days` to add
// in direct time scale units.
public fun (+)( i : instant, d : duration ) : instant {
  if (i.ts.unit=="TAI")
   then unsafe-add( i, d.timespan )
   else i.use-timescale(ts-tai).unsafe-add( d.timespan )
}

// Add `days` days to the instant.
public fun add-days( i : instant,  days : int) : instant {
  Instant(i.since.add-days(days), i.ts)
}


/* Add a duration of `t` seconds of time scale `tscale`.
This can for example be used to add Unix or NTP seconds where leap seconds
are ignored (allthough it is recommended in that case to use
`:time` and add logical days etc).
```
> instant(2005,12,31).add-duration-in( ts-unix, (24*3600).timespan ).time
2006-01-01T00:00:00Z

> (instant(2005,12,31) + 24.hours).time
2005-12-31T23:59:60Z
```
*/
public fun add-duration-in( i : instant,  tscale : timescale, t : timespan) : instant {
  i.use-timescale(tscale).unsafe-add( t )
}

// Add a time span to an instant in time.
// This is only safe if the time unit of the timespan is the
// same as that of the instant.
public fun unsafe-add(i : instant, tspan : timespan ) : instant {
  Instant( i.since + tspan, i.ts ) // directly add
}

// Subtract a duration from an instant in time.
public fun (-)( i : instant, d : duration ) : instant {
  i + (~d)
}

// Return the duration between to instants in time.
public fun (-)( i : instant, j : instant ) : duration {
  i.duration - j.duration
}

// Show an instant as a number of (TAI) SI seconds since the `epoch` in a given precision.
// This can be used as an unambigious time stamp.
public fun show( i : instant, max-prec : int = 9, secs-width : int = 1 ) : string {
  i.use-timescale(ts-tai).show-raw(max-prec,secs-width,"s")
}

// Internal: show an instant as a raw timestamp in a given precision, postfixed with the time scale name.
public fun show-raw( i : instant, max-prec : int = 9, secs-width : int = 1, unit : string = "" ) : string {
  i.since.ts-show(max-prec,secs-width,unit)
   + (if (i.ts.name.empty? || i.ts.name=="TAI") then "" else " " + i.ts.name)
}

/*----------------------------------------------------------------------------
  Time scales
----------------------------------------------------------------------------*/


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
public val epoch : instant = instant(duration0)


// Create a new time scale based on SI seconds (as measured on the Earth's geoid) with a given
//  `name`, a fixed `offset` (=`duration0`) from TAI (e.g. GPS = TAI - 19), and
// a `epoch-y2k` (= `timestamp0`) which is the timestamp of the 2000-01-01 date in that timescale
// e.g. for a timescale `ts`:\
// `epoch-y2k = instant(2000,1,1,cal=iso-calendar(ts)).since-in(ts)`
public fun tai-timescale( name : string, offset : duration = 0.duration ) : timescale {
  val epoch-shift = offset.timespan
  fun from-tai(tai:duration) { (tai.timespan + epoch-shift).timestamp  }
  fun to-tai(t:timestamp)    { (t - epoch-shift).unsafe-duration }
  timescale(
    name,
    from-tai,
    to-tai,
    "TAI"
  )
}

// Create a new time scale given `name`, two inverse function `from-tai` and `to-tai`,
// and an optional function that returns the seconds in the day of the instant.
// The time unit defaults to `name`.
public fun timescale( name : string,
                      from-tai : duration -> timestamp,
                      to-tai   : timestamp -> duration,
                      unit : string = name,
                      seconds-in-day : maybe<(timestamp) -> timespan> = Nothing
                    ) : timescale {
  Timescale(
    name,
    unit,
    from-tai,
    to-tai,
    seconds-in-day
  )
}



// The [TAI](https://en.wikipedia.org/wiki/International_Atomic_Time) (International atomic time)
// time scale is based on SI seconds measured on the Earth's geoid, with a 2000-01-01 TAI `epoch`.
public val ts-tai : timescale = tai-timescale( "TAI" )

// The [GPS](https://en.wikipedia.org/wiki/Global_Positioning_System#Timekeeping) time scale based
// on SI seconds with a 1980-01-06 GPS epoch.\
// GPS = TAI - 19s.
public val ts-gps : timescale = tai-timescale( "GPS", -19.duration)

val gps2000  = duration(630720000)

// Get the GPS time in SI seconds since the GPS epoch (1980-01-06Z)
public fun gps-instant(i : instant ) {
  gps2000 + i.timestamp-in(ts-gps).unsafe-duration
}

// Create an instant from a raw GPS time since the GPS epoch (1980-01-06Z)
public fun gps-instant(gps : duration) {
  instant( gps - gps2000  )
}

// The [TT](https://en.wikipedia.org/wiki/Terrestrial_Time) (Terrestrial time)
// time scale is based on SI seconds with a 1977-01-01 TAI `epoch`. It is the
// continuation of TDT (Terrestrial dynamic time) and ET (Ephemeris time). TT
// is defined as: TT = TAI + 32.184s.
public val ts-tt : timescale = tai-timescale( "TT", duration(32.184))

val tt2000 = duration(630720000)

// Get the TT time in SI seconds since the GPS epoch (1977-01-01)
public fun tt-instant(i : instant ) {
  tt2000 + i.timestamp-in(ts-tt).unsafe-duration
}

// Create an instant from a raw TT time since the TT epoch (1977-01-01 TAI)
public fun tt-instant(tt : duration) {
  instant( tt - tt2000 )
}



// Show a timestamp with an optional maximum precision (`max-prec` (=`9`)) and
// minimum width for the seconds (=`1`).
public fun show( t : timestamp, max-prec : int = 9, secs-width : int = 1, unit : string = "" ) : string {
  t.ts-show(max-prec,secs-width,unit)
}


/*----------------------------------------------------------------------------
  Julian Date
----------------------------------------------------------------------------*/
val jd-epoch-delta  = timespan(2400000.5)   // relative to the MJD epoch
val mjd-epoch-delta = timespan(51544)       // relative to our epoch (2000-01-01)


// Create an instant given a [julian day](https://en.wikipedia.org/wiki/Julian_day).
public fun instant-at-jd( jd: double, ts : timescale ) : instant {
  instant-at-jd(ddouble(jd),ts)
}

// Create an instant given a [julian day](https://en.wikipedia.org/wiki/Julian_day)
// and time scale `ts` .
public fun instant-at-jd( jd: ddouble, ts : timescale  ) : instant {
  instant-at-mjd( jd - jd-epoch-delta, ts )
}

// Create an instant given a [modified julian day](https://en.wikipedia.org/wiki/Julian_day).
// and time scale `ts`.\
// `modified-julian-day = julian-day - 2400000.5`
public fun instant-at-mjd( mjd : double, ts : timescale ) : instant {
  instant-at-mjd(ddouble(mjd),ts)
}

// Create an instant given a [modified julian day](https://en.wikipedia.org/wiki/Julian_day).
// and time scale `ts`.\
// `modified-julian-day = julian-day - 2400000.5`
public fun instant-at-mjd( mjd : ddouble, ts : timescale  ) : instant {
  val d     = mjd - mjd-epoch-delta // since 2000-01-01
  val days  = d.floor
  val frac  = d - days
  val idays = days.int
  val secs-in-day = ts.instant(Dayspan(idays)).seconds-in-day
  val secs  = frac*secs-in-day
  // if this is a day with a leap second, the seconds may come out
  // larger than 86400, if so, we interpret that as a leap second.
  // TODO: this is wrong for leap seconds occurring on 19:00h (between 1958 and 1960)
  // where it would now show as a leap second at the end of the day.
  val (dsecs,leap) = if (secs < solar-secs-per-day) then (secs,0) else {
                       val leap = secs.floor.int - isolar-secs-per-day + 1
                       (secs - leap.ddouble, leap)
                     }
  ts.instant(Dayspan(idays, dsecs, leap))
}

/* Return the julian day in a given time scale `ts` for an instant `i`.
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


// Return the modified julian day in a given time scale `ts` for an instant `i`.
// Can also pass an optional `delta` (=`timespan0`) that is added to the raw timestamp of `i`
// before conversion (used in `std/time/time` to take timezones into account)
public fun mjd( i : instant, tscale : timescale, tzdelta : timespan = timespan0 ) : ddouble {
  val j = i.use-timescale(tscale)
  // get dayspan and seconds-in-the-day of the original instant
  val ds0 = j.dayspan
  val secs-in-day = j.seconds-in-day
  // then adjust to the dayspan in the timezone, ignoring a possible leap second
  val ds  = solar-dayspan(ds0.days,ds0.secs + tzdelta)
  // calculate fraction in the timezone seconds, with the original leap second and seconds in the day
  val frac= (ds.secs + ds0.leap.ddouble) / secs-in-day
  // and add it together
  (mjd-epoch-delta + ds.days.ddouble + frac)
}
