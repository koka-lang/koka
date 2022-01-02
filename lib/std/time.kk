/*----------------------------------------------------------------------------
   Copyright 2012-2021, Microsoft Research, Daan Leijen

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/

/* Date and time functionality.

# Dates and time

This is an extensive library for managing time.
By using careful defaults the library offers an easy to use interface for the
common use cases, but uses accurate and precise time representations and algorithms.
In particular, it handles accurate leap seconds, time zones, multiple calendars,
and astronomical time scales.
Internally time is represented by a 128-bit `:ddouble` giving it a very high range
and precision (up to 31 decimal digits, see the `module std/time/instant` module for more
information). As such, we believe this library is one of the most accurate time
libraries around.

Core concepts are:

* A `:duration` is a span of time in SI seconds (as measured on the earth's geoid).

* An `:instant` represents an instant in time. Instants can be substracted to
get the `:duration` between them. Instants are usually represented as the number
of fractional SI seconds since the `epoch` (2000-01-01Z), but can be represented
in different time scales for efficiency. For example, there is the TAI time scale (`ts-tai`),
standard UTC (`ts-ti`), or astronomical time scales like terrestrial time (`module std/time/astro`).

* A `:time` is a human readable `:date` (year, month, day), and `:clock` (hour, minutes, seconds)
in a particular `:calendar` (usually the standard ISO calendar) and time zone.
Instants can be converted to time values and back. We can add logical years, months, or
days to a `:time`, or get the difference between times in days etc. See the
`module std/time/time` module for more information.

The current time, in the UTC timezone (`tz-utc`) using the ISO calendar (`cal-iso`):

```
> time-now().show
"2017-01-30T15:40:25.367000103Z"
```

Or the current time in the local timezone:

```
> local-time-now().show
"2017-01-30T07:59:01.693000078-08:00 (PST)"
```

We can also get the current instant in time as the number of SI seconds since the `epoch`:

```
> now()
539106063.980999947s
```
See `module std/time/chrono` for more information on system time.

Calendar times can be constructed by giving a year, month (1-based), and day of the month (1-based)
and optional hour, minutes, seconds, fraction of the second, timezone (`tz-utc`) and
calendar (`cal-iso`):

```
> time(2000,1,1).show
"2000-01-01T00:00:00Z"
```

Instants can be constructed similarly, and can be converted to and from
`:time` values. Here we construct a time value interpreted in
the TAI time scale (which was 32 seconds ahead of UTC on 2000-01-01Z):

```
> instant(2000,1,1).time(cal=cal-tai).show
"2000-01-01T00:00:32Z TAI"
```

The `cal-tai` calendar is the standard ISO calendar but uses the TAI time scale (`ts-tai`)
instead of UTC time.

## Leap seconds

The library takes much care to correctly handle leap seconds and time zone transitions.
For example, here are some durations between different times:

```
> (time(2016,1,1,0,0,0,0.5) - time(2015,12,31,23,59,59)).show
"1.5s"

> (time(2017,1,1,0,0,0,0.5) - time(2016,12,31,23,59,59)).show
"2.5s" // over a leap second

> (time(2017,1,1,0,0,0,0.5) - time(2017,1,1,2,59,59,tz=tz-fixed(3)).show
"2.5s" // UTC+3, and over a leap second
```

Similarly, adding logical _days_ (or weeks, months, or years) works over leap seconds and
other time irregularties:

```
> time(2016,12,31,12).add-days(1).show
"2017-01-01T12:00:00Z"   // over a leap second

> time(1582,10,4,cal=cal-jg).add-days(1).show
"1582-10-15T00:00:00Z JG"  // transition from Julian (`cal-julian`) to Gregorian (`cal-gregorian`) calendar)
```

See the `std/time/utc` module for more information on leap seconds.

## Time zones

If calculating with timezones besides the UTC (`tz-utc`)
and local timezone (`tz-local`), one needs to use the `module std/time/timezone`
module to load the latest time zone information (`std/time/timezone/load-timezones`).

```
import std/time
import std/time/timezone

fun test-timezones() {
  val tzs = load-timezones()
  val tz1 = tzs.timezone("America/New_York")
  val tz2 = tzs.timezone("Pacific/Kosrae")

  /* A flight of 12.5 hours from New York to Kosrae */
  val t1  = instant(1997,12,31,12,1,0,tz=tz1) + (12.hours + 30.minutes)
  t1.time(tz=tz2).show.println   // "1998-01-01T17:31:00+12:00"

  /* Again, but across DST & leap second */
  val t2  = instant(1998,12,31,12,1,0,tz=tz1) + (12.hours + 30.minutes)
  t2.time(tz=tz2).show.println   // "1999-01-01T16:30:59+11:00"
}
```

## Intervals

Using the `now` or `time-now` for measuring time intervals is not appropriate since these functions
are not guaranteed to be monotonic or of sufficiently high precision. For timing, the `module std/time/timer` module
should be used with functions like `ticks` or `elapsed`:
```koka
import std/time/timer

fun nfib(n : int) : int {
  if (n <= 1) then 1 else 1 + nfib(n - 1) + nfib(n - 2)
}

fun test-nfib() {
  print-elapsed{ nfib(30) }   // "elapsed 0.111s"
}
```

## UTC

The library uses by default the International Time (TI) time scale (`ts-ti`) which
uses UTC time with leap seconds up to 2019, and ignores leap seconds after
this. This is a nice default as it tracks historical leap seconds precisely
while having deterministic future time calculations. Moreover, even if there
are future leap seconds, only durations calculations between times will be
off by the inserted leap seconds; `time-now`, `:time`, and `:instants`
stay valid and designate correct times (as they use a _day + seconds_
internal representation).

There are two other good UTC time scales though:

- `ts-ti-sls` smooths leap seconds over the last 1000s of a day with a leap
  seccond so they never appear in any displayed time or timestamp.
- `ts-utc-create`/`cal-utc-load` creates a proper UTC timescale from a leap
  second table. This ensures future leap seconds are fully accounted for and
  that duration calculations work properly up to 6 months into the future.

.
*/
module std/time

pub import std/time/duration
pub import std/time/instant
pub import std/time/utc
pub import std/time/chrono
pub import std/time/timer

// For now not supported until we move to async
// pub import std/time/download

pub import std/time/date
pub import std/time/calendar
pub import std/time/calendars
pub import std/time/time

pub import std/time/locale
pub import std/time/format
pub import std/time/parse

import std/num/ddouble      // for documentation

