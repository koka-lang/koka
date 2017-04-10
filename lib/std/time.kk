/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Date and time functionality.

# Dates and time

This is an extensive library for managing time and supports advanced features
as multiple calendars, astronomical time scales, accurate leap seconds, and time zones.
Internally time is represented by a `:ddouble` giving it a very high range and precision 
(see the `std/time/instant` module).

Core concepts are:

* A `:duration` is a span of time in fractional SI seconds. 

* An `:instant` represents an instant in time. Instants can be substracted to
get the `:duration` between them. Instants are usually represented as the number
of fractional SI seconds since the `epoch` (2000-01-01Z), but can be represented
in different time scales for efficiency. For example, there is the TAI time scale (`ts-tai`),
standard UTC (`ts-ti`), or astronomical time scales like terrestrial time (`std/time/astro/ts-tt`).

* A `:time` is a human readable `:date` (year, month, day), and `:clock` (hour, minutes, seconds)
in a particular `:calendar` (usually the standard ISO calendar) and time zone. 
Instants can be converted to 
time values and back. We can add years, months, or days to a `:time`, or get the difference
between times in days etc.

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
.
*/
module std/time

public import std/time/duration
public import std/time/instant
public import std/time/utc
public import std/time/chrono
public import std/time/timer

public import std/time/download
public import std/time/date
public import std/time/calendar
public import std/time/calendars
public import std/time/time

public import std/time/locale
public import std/time/format
public import std/time/parse

import std/num/ddouble  // for documentation
