/*----------------------------------------------------------------------------
   Copyright 2012-2021, Microsoft Research, Daan Leijen

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/

/* Low level time spans and time stamps.

These are unit-less and may not necessarily be in (TAI) SI seconds, and
are used internally to do time scale conversions and calendar calculations.
User code should use `:std/time/duration/duration`s and `:std/time/instant/instant`s instead.
*/
module std/time/timestamp

import std/num/ddouble
import std/num/int32
import std/time/date

// Time spans are a time duration in some time scale represented as a `:ddouble`;
// this gives it very high precision and range to represent time very precisely.
// (see the [`instant`](std_time_instant.html) module)
pub alias timespan = ddouble

// A zero-valued timespan.
pub val timespan0 : timespan = zero

pub fun timespan( seconds : int, frac : float64 = 0.0 ) : timespan
  if frac.is-zero then ddouble(seconds) else ddouble(seconds) + ddouble(frac)

pub fun timespan( secs : float64 ) : timespan
  ddouble(secs)

pub fun timespan( secs : float64, frac : float64 ) : timespan
  ddouble(secs,frac)

// Timespan from a `:ddouble`. Just for convenience as `:timespan` is an alias
pub fun timespan( secs : ddouble ) : timespan
  secs

// Divide using `div` to allow for different timespan representations
pub fun div( x : timespan, y : timespan, prec : int = 0) : timespan
  x / y


// Seconds in a solar day, 86400.
pub val isolar-secs-per-day = 86400
pub val solar-secs-per-day = isolar-secs-per-day.timespan

/*----------------------------------------------------------------------------
  Timestamps
----------------------------------------------------------------------------*/

/* A time stamp denotes an instant in time since 2000-01-01 in some time scale.
Time stamps also keep track of possible leap seconds for
UTC based timescales. [Unix/UTC seconds](https://en.wikipedia.org/wiki/Unix_time).

- One way is as a duration in TAI SI seconds
  but that would lead to many unnecessary conversions (and loss of precision),
  for example for unix timestamps that need adjustments for leap seconds.
- To avoid conversions, it is better to represent as days plus the seconds
  into that day. However, that means that adding or subtracting seconds
  always needs normalization.
- To make it easier to add/substract time spans, we can represent it as single
  timespan `since` where `days == floor(since / 86400)` and `seconds == since - days`.
  This is essentially the representation that Unix/Posic timestamps use too.
- Finally, we add a separate `leap` seconds field
  to represent time in a leap step unambigiously (and also at any time during the day).
  The `leap` field is positive if this time is in a leap step.

Here is the representation of a time in the leap second just before 2006-01-01:
```
time(2005,12,31,23,59,60,0.5).instant.timestamp.ts-show == "189388799.500 (+1 leap)"
time(2005,12,31,23,59,60,0.5).instant.timestamp.ts-show-days == "2191d 86399.500s (+1 leap)"
```
The `leap` field also lets us represent leap times that occurred at another
time than the last second of the day:
```
time(1959,1,28,18,59,59,0.01).instant.timestamp.ts-show-days == "-14948d 68399.010s"
time(1959,1,28,18,59,60,0.01).instant.timestamp.ts-show-days == "-14948d 68399.010s (+1 leap)"
time(1959,1,28,19, 0, 0,0.01).instant.timestamp.ts-show-days == "-14948d 68400.010s"
```
See `std/time/utc` for more information on leap seconds.

*/
abstract struct timestamp(
  since : timespan,  // days+seconds since 2000-01-01 in the time scale
  // the extra leap seconds; always zero for any time scale except UTC based ones.
  leap32 : int32
)

pub fun leap( t : timestamp ) : int
  t.leap32.int

// The time stamp at 2000-01-01
pub val timestamp0 : timestamp = Timestamp(ddouble/zero,int32/zero)

// Create a time stamp from a `:timespan` since 2000-01-01 and possible leap seconds.
pub fun timestamp( t : timespan, leap : int = 0 ) : timestamp
  Timestamp(t,leap.int32)

// Create a time stamp from an integer timespan since 2000-01-01 and possible leap seconds.
pub fun timestamp( t : int, frac : float64 = 0.0, leap : int = 0 ) : timestamp
  timestamp(t.ddouble + frac.ddouble,leap)

// Timestamp from days, seconds into the day and possible leap second.
pub fun timestamp-days( days : int, secs : timespan = timespan0, leap : int = 0  ) : timestamp
  timestamp( (days * isolar-secs-per-day).timespan + secs, leap)


// Return days and seconds into the day, disregarding leap seconds.
pub fun days-seconds( ts : timestamp ) : (int,ddouble)
  val secs = ts.since.floor
  val frac = ts.since - secs
  val (days,dsecs) = divmod(secs.int, isolar-secs-per-day)
  (days, dsecs.ddouble + frac)

// Return days and clock into the day, handling possible leap seconds.
pub fun days-clock( ts : timestamp ) : (int,clock)
  val (days,secs) = ts.days-seconds
  (days, clock(secs,ts.leap))

pub fun days( ts : timestamp ) : int
  ts.days-seconds.fst

pub fun seconds-into-day( ts : timestamp ) : ddouble
  ts.days-seconds.snd + ts.leap.ddouble


// The time span since 2000-01-01 including time inside a possible leap second.
pub fun unsafe-timespan-withleap( ts : timestamp ) : timespan
  ts.since + ts.leap.timespan


// The time span since 2000-01-01 for time scales that do not have
// leap seconds and where every day is 86400s. For time scales
// with leap seconds, this effectively ignores any leap seconds.
pub fun timespan-noleap( ts : timestamp ) : timespan
  ts.since

// Round a time stamp to a certain precision (`prec` is number of digits of the fraction of the second).
pub fun round-to-prec(t : timestamp, prec : int ) : timestamp
  Timestamp(t.since.round-to-prec(prec), t.leap32)

// Add `leaps` leap seconds to the timestamp.
pub fun add-leap-seconds( ts : timestamp, leaps : timespan ) : timestamp
  if !leaps.is-pos then ts // + leaps // if negative, add as normal seconds
  elif leaps < 1.timespan && ts.leap.is-zero
   then Timestamp( (ts.since - 1.timespan) + leaps, 1.int32)
   else Timestamp( ts.since + leaps.fraction, ts.leap32 + leaps.truncate.int.int32 )

// Add `days` days to a timestamp
pub fun add-days( ts : timestamp, days : int ) : timestamp
  ts + (days * isolar-secs-per-day).timespan

// Compare two `:timestamp`s.
pub fun compare( i : timestamp, j : timestamp ) : order
  match compare(i.since,j.since)   // compare without leap-seconds first!
    Eq  -> compare(i.leap,j.leap)
    ord -> ord


pub fun (<)( i : timestamp, j : timestamp )  : bool { compare(i,j) == Lt }
pub fun (<=)( i : timestamp, j : timestamp ) : bool { compare(i,j) != Gt }
pub fun (>)( i : timestamp, j : timestamp )  : bool { compare(i,j) == Gt }
pub fun (>=)( i : timestamp, j : timestamp ) : bool { compare(i,j) != Lt }
pub fun (==)( i : timestamp, j : timestamp ) : bool { compare(i,j) == Eq }
pub fun (!=)( i : timestamp, j : timestamp ) : bool { compare(i,j) != Eq }

// The minimum of two timestamps.
pub fun min( i : timestamp, j : timestamp ) : timestamp
  if i <= j then i else j

// The maximum of two timestamps.
pub fun max( i : timestamp, j : timestamp ) : timestamp
  if i >= j then i else j

// Add a time span to a time stamp.
pub fun (+)( ts : timestamp, t : timespan ) : timestamp
  Timestamp(ts.since + t, ts.leap32)

// Subtract a time span from a time stamp.
pub fun (-)( ts : timestamp, t : timespan ) : timestamp
  Timestamp(ts.since - t, ts.leap32)

// Show a time stamp.
pub fun ts-show( ts : timestamp, max-prec : int = 9, secs-width : int = 1, unit : string = "" ) : string
  val l = if ts.leap.is-zero then "" else " (+" ++ ts.leap.show ++ " leap)"
  ts.since.show-seconds(max-prec,secs-width,unit) ++ l

// Show a day stamp
pub fun ts-show-days( ts : timestamp, prec : int = 9 ) : string
  val (days,secs) = ts.days-seconds
  days.show ++ "d " ++ ts-show(timestamp(secs,ts.leap),prec,unit="s")

