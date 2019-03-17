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
import std/time/timespan
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
  since : timespan,   // seconds since the time-scale epoch in timescale units
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
  public name : string,   // Time scale name, "TAI", "UTC", "UNIX", "TT", etc.
  unit        : string,   // SI seconds: usually "TAI", but could be "TCG", "TCB", "TDB" etc.
  from-tai    : duration -> timespan,   // from duration since `epoch` to timespan since timescale-epoch
  to-tai      : timespan -> duration,   // from timespan since timescale-epoch to duration since `epoch`
  // from timespan since timescale epoch to dayspan since `epoch-y2k` and total seconds in that day
  to-dayspan  : (timespan) -> (dayspan,timespan),
  // from dayspan since `epoch-y2k` to timespan since timescale epoch
  from-dayspan: (dayspan) -> timespan,
  // The epoch-y2k states at what timespan  _in this timescale_ is the date 2000-01-01.
  // `epoch-y2k == instant(2000,1,1,cal=cal-iso,ts=ts).since-in(ts)` with `ts` this timescale
  epoch-y2k   : timespan,
  // Just if all days have the same number of seconds, Nothing otherwise
  fixed-secs-per-day: maybe<timespan>
)


/*----------------------------------------------------------------------------
  Instant operations
----------------------------------------------------------------------------*/


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
    Instant( i.since-in(tscale), tscale )
  }
}

// Return a `:timespan` for instant `i` in a certain time scale `tscale` (= `ts-tai`)
// since the epoch of that `tscale`.
public fun since-in( i : instant, tscale : timescale = ts-tai ) : timespan {
  i.since.convert(i.ts,tscale)
}


// Convert a timespan between time scales
fun convert( t : timespan, from : timescale, to : timescale ) : timespan {
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
public fun to-tai(  ts : timescale, t : timespan ) : duration {
  t.convert(ts,ts-tai).unsafe-duration
}

// Given a `:duration` since the `epoch`, return a `:timespan` for that instant in time scale `ts`.
public fun from-tai(ts : timescale, d : duration ) : timespan {
  d.timespan.convert(ts-tai,ts)
}

// Return the time scale that instant `i` uses.
public fun timescale( i : instant ) : timescale {
  i.ts
}

// Create an instant from a time stamp `t` interpreted in time scale `ts`.\
// Be careful to ensure that `t` should indeed be interpreted in the given time scale.
public fun unsafe-instant( t : timespan, ts : timescale ) : instant {
  Instant(t,ts)
}

// Create an instant from a time span in `seconds` interpreted in time scale `ts`.\
// Be careful to ensure that `secs` should indeed be interpreted in the given time scale.
public fun unsafe-instant( secs : int, ts : timescale ) : instant {
  Instant(secs.timespan, ts)
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


public fun has-leap-seconds(ts : timescale) : bool {
  !ts.fixed-secs-per-day.bool
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
public val epoch : instant = Instant(timespan0,ts-tai)

// Return the (TAI) SI second duration since the `epoch` at this instant.
public fun since-epoch( i : instant ) : duration {
  i.since-in(ts-tai).unsafe-duration
}

// Return the instant at (TAI) SI seconds duration since the `epoch`.
public fun instant-since-epoch( d : duration ) : instant {
  unsafe-instant(d.timespan, ts-tai)
}

// Return `:dayspan` since 2000-01-01 in the time scale of the instant,
// and the total number of seconds in that day.
public fun dayspan-since-y2k( i : instant ) : (dayspan,timespan) {
  (i.ts.to-dayspan)(i.since - i.ts.epoch-y2k)
}

// Given a `:dayspan` since 2000-01-01 in the given time scale `ts`,
// return the instant in time.
public fun instance-from-dayspan( ts : timescale, ds : dayspan ) : instant {
  Instant( (ts.from-dayspan)(ds) + ts.epoch-y2k, ts )
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
public fun (+)( i : instant, d : duration ) : instant {
  if (i.ts.unit=="TAI")
   then unsafe-add( i, d.timespan )
   else i.use-timescale(ts-tai).unsafe-add( d.timespan )
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
  i.since-epoch - j.since-epoch
}

// Show an instant as a number of (TAI) SI seconds since the `epoch` in a given precision.
// This can be used as an unambigious time stamp.
public fun show( i : instant, max-prec : int = 9, secs-width : int = 1 ) : string {
  i.use-timescale(ts-tai).show-raw(max-prec,secs-width)
}

// Internal: show an instant as a raw timestamp in a given precision, postfixed with the time scale name.
public fun show-raw( i : instant, max-prec : int = 9, secs-width : int = 1 ) : string {
  i.since.show(max-prec,secs-width)
   + "s"
   + (if (i.ts.name.empty? || i.ts.name=="TAI") then "" else " " + i.ts.name)
}

/*----------------------------------------------------------------------------
  Time scales
----------------------------------------------------------------------------*/

// Create a new time scale based on SI seconds (as measured on the Earth's geoid) with a given
//  `name`, a fixed `offset` (=`duration0`) from TAI (e.g. GPS = TAI - 19), and
// a `epoch-y2k` (= `timestamp0`) which is the timestamp of the 2000-01-01 date in that timescale
// e.g. for a timescale `ts`:\
// `epoch-y2k = instant(2000,1,1,cal=iso-calendar(ts)).since-in(ts)`
public fun tai-timescale( name : string, offset : duration = zero, epoch-y2k : timespan = timespan0 ) : timescale {
  val epoch-shift = epoch-y2k + offset.timespan
  fun from-tai(tai:duration)     { tai.timespan + epoch-shift  }
  fun to-tai(tspan:timespan)     { (tspan - epoch-shift).unsafe-duration }
  Timescale(
    name,
    "TAI",
    from-tai,
    to-tai,
    solar-to-dayspan,
    solar-from-dayspan,
    epoch-y2k,
    Just(solar-secs-per-day)
  )
}

fun solar-to-dayspan( t : timespan ) : (dayspan,timespan) {
  val days = (t / solar-secs-per-day).floor
  val secs = t - days
  (Dayspan(days.int, secs), solar-secs-per-day)
}

fun solar-from-dayspan( ds : dayspan ) : timespan {
  (ds.days.ddouble*solar-secs-per-day + ds.secs)
}


// Create a new time scale given `name`, two inverse function `from-tai` and `to-tai`,
// and a `y2k-epoch` (=`timestamp0`). The time unit defaults to `name`.
// The `y2k-epoch` is the timestamp of the 2000-01-01 date in the new timescale
// e.g. for a timescale `ts`:\
// `y2k-epoch = instant(2000,1,1,cal=iso-calendar(ts)).timestamp(ts)`
public fun timescale( name : string,
                      from-tai : duration -> timespan,
                      to-tai   : timespan -> duration,
                      epoch-y2k : timespan = timespan0,
                      unit : string = name,
                      to-dayspan : maybe<(timespan) -> (dayspan,timespan)> = Nothing,
                      from-dayspan : maybe<(dayspan) -> timespan> = Nothing
                    ) : timescale {
  Timescale(
    name,
    unit,
    from-tai,
    to-tai,
    to-dayspan.default(solar-to-dayspan),
    from-dayspan.default(solar-from-dayspan),
    epoch-y2k,
    if (to-dayspan.bool) then Nothing else Just(solar-secs-per-day)
  )
}

// The [TAI](https://en.wikipedia.org/wiki/International_Atomic_Time) (International atomic time)
// time scale is based on SI seconds measured on the Earth's geoid, with a 2000-01-01 TAI `epoch`.
public val ts-tai : timescale = tai-timescale( "TAI" )

// The [GPS](https://en.wikipedia.org/wiki/Global_Positioning_System#Timekeeping) time scale based
// on SI seconds with a 1980-01-06 GPS epoch.\
// GPS = TAI - 19s.
public val ts-gps : timescale = tai-timescale( "GPS", -19.seconds, timespan(630720000) )


// Return the epoch of a certain time scale.
public fun timescale-epoch( ts : timescale ) : instant {
  unsafe-instant(timespan0,ts)
}

// Return the instant of the date 2000-01-01 of a particular time scale `ts` (when imposed on a proleptic Gregorian calendar).\
// `timescale-y2k(ts) == instant(2000,1,1,cal=iso-calendar(ts))`
public fun timescale-y2k( ts : timescale ) : instant {
  unsafe-instant(ts.epoch-y2k,ts)
}

// Show a timespan with an optional maximum precision (`max-prec` (=`9`)) and
// minimum width for the seconds (=`1`).
public fun show( tspan : timespan, max-prec : int = 9, secs-width : int = 1 ) : string {
  tspan.ts-show(max-prec,secs-width)
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
public fun instant-at-mjd( mjd : ddouble, tscale : timescale  ) : instant {
  val d  = mjd - mjd-epoch-delta // since 2000-01-01
  match (tscale.fixed-secs-per-day) {
    Just(secs-per-day) {
      timescale-y2k(tscale).unsafe-add(d*secs-per-day) // since the time scale epoch
    }
    Nothing {
      // Time scale has leap seconds.
      // first add days to the TAI timescale and convert back to the target time scale,
      // and then convert the fraction of the day. This ensures we correctly
      // handle leap seconds in excess of a day.
      // (It also assumes that TAI days are in sync with the timescale days,
      //  which will be the case as this is the reason for leap seconds in the first place.)
      val days = d.floor
      val frac = d - days
      val itai = instant-since-epoch( duration(days*solar-secs-per-day) )
      val i    = itai.use-timescale(tscale)  // start of day in target time scale
      val (_ds,secs-in-day) = i.dayspan-since-y2k
      // assert(ds.days == days && ds.secs == zero)
      val secs = frac * secs-in-day       // fraction of the seconds in this day
      i.unsafe-add(secs)
    }
  }
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
public fun mjd( i : instant, tscale : timescale, delta : timespan = timespan0 ) : ddouble {
  val j   = i.use-timescale(tscale)
  val (ds0,secs-in-day) = j.dayspan-since-y2k
  val ds  = if (delta.zero?) then ds0 else j.unsafe-add(delta).dayspan-since-y2k.fst
  val secs= ds.secs / secs-in-day
  (mjd-epoch-delta + ds.days.ddouble + secs)
}
