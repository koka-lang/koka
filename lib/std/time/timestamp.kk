/*----------------------------------------------------------------------------
   Copyright (C) 2012-2019 Microsoft Corporation

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Low level time stamps.

These are unit-less and may not necessarily be in (TAI) SI seconds, and
are used internally to do time scale conversions and calendar calculations.
User code should use `:std/time/duration/duration`s and `:std/time/instant/instant`s instead.
*/
module std/time/timestamp

import std/text/regex
import std/num/ddouble

// Time spans are a time duration represented as a `:ddouble`;
// this gives it very high precision
// and range to represent time very precisely. (see the [`instant`](std_time_instant.html) module)
public alias timespan = ddouble

// A zero-valued timespan.
public val timespan0 : timespan = zero

public fun timespan( seconds : int, frac : double = 0.0 ) : timespan {
  if (frac.zero?) then ddouble(seconds) else ddouble(seconds) + ddouble(frac)
}

public fun timespan( secs : double ) : timespan {
  ddouble(secs)
}

// Timespan from a `:ddouble`. Just for convenience as `:timespan` is an alias
public fun timespan( secs : ddouble ) : timespan {
  secs
}

// Divide using `div` to allow for different timespan representations
public fun div( x : timespan, y : timespan, prec : int = 0) : timespan {
  x / y
}

/*----------------------------------------------------------------------------
  Day span
----------------------------------------------------------------------------*/

// A _day span_ is a time given as days, seconds, and leap seconds
// from 2000-01-01 (in some timescale). It takes the `days` since
// 2000-01-01, the fractional `secs` seconds into that day, and
// potential (whole) `leap` seconds. The `leap` seconds are used
// for times that are _in_ a leap second in that timescale and are
// only used by UTC based timescales.
//
// We need a separate field `leap`
// since some leap steps between 1958 and 1960 occurred at 19:00h
// instead of the last second of the day. For example,
// ```
// time(1959,1,28,18,59,60,0.01).instant.dayspan-since-y2k == "-14948d 68399.010s (+1 leap)"
// ```
// See `std/time/utc` for more information on leap seconds.
public struct dayspan(
  days: int,                  // days since 2000-01-01
  secs: timespan = timespan0, // seconds into this day
  leap: int = 0               // and extra leap seconds (to provide for leap seconds at other times than the end of day)
)

public val isolar-secs-per-day = 86400
public val solar-secs-per-day = isolar-secs-per-day.timespan

// Create a normalized dayspan assuming 86400 seconds in a day.
public fun solar-dayspan( days : int, secs : timespan = timespan0, leap : int = 0 ) {
  val fsecs = secs.floor
  val frac  = secs - fsecs
  val (ddays,dsecs) = divmod(fsecs.int, isolar-secs-per-day)
  Dayspan(days + ddays, dsecs.ddouble + frac, leap)
}

// Show a day span
public fun show( ds : dayspan, prec : int = 9 ) : string {
  ds.days.show + "d " + ts-show(timestamp(ds.secs,ds.leap),prec,unit="s")
}

/*----------------------------------------------------------------------------
  Timestamps
----------------------------------------------------------------------------*/

/* A time stamp denotes an instant in time since 2000-01-01 in some time scale.
Time stamps also keep track of possible leap seconds for UTC based timescales. [Unix/UTC seconds](https://en.wikipedia.org/wiki/Unix_time).

There are various representation possible. One way is as a duration TAI SI seconds
but that would lead to many conversions (and loss of precision), for example for
unix timestamps that need adjustments for leap seconds. To avoid conversions,
it is better to represent as days plus the seconds into that day. Furthermore,
to avoid normalization and to make it easier to add/substract time spans,
we write this as a single timespan `since` where `days == floor(since / 86400)`
and `seconds == since - days`. This is essentially the representation that
unix timestamps use too. However, we also need a separate `leap` seconds field
too that is positive if this time is in a leap step. This allows us also to
represent time in a leap step unambigiously, and also at any time during the day.

Here is the representation of the leap second just before 2006-01-01:
```
time(2005,12,31,23,59,60,0.5).instant.since-y2k.ts-show == "189388799.500 (+1 leap)"
time(2005,12,31,23,59,60,0.5).instant.dayspan-since-y2k.show == "2191d 86399.500s (+1 leap)"
```
*/
abstract struct timestamp(
  since : timespan,  // days+seconds since 2000-01-01 in the time scale
  // the extra leap seconds; always zero for any time scale except UTC based ones.
  public leap : int
)

// The time stamp at 2000-01-01
public val timestamp0 : timestamp = Timestamp(zero,0)


// Create a time stamp from a `:timespan` since 2000-01-01 and possible leap seconds.
public fun timestamp( t : timespan, leap : int = 0 ) : timestamp {
  Timestamp(t,leap)
}

// Create a time stamp from an integer timespan since 2000-01-01 and possible leap seconds.
public fun timestamp( t : int, frac : double = 0.0, leap : int = 0 ) : timestamp {
  timestamp(t.ddouble + frac.ddouble,leap)
}

// Timestamp from dayspan.
public fun timestamp( ds : dayspan ) : timestamp {
  Timestamp( (ds.days * isolar-secs-per-day).timespan + ds.secs, ds.leap)
}

// Dayspan from a timestamp
public fun dayspan( ts : timestamp ) : dayspan {
  val secs = ts.since.floor
  val frac = ts.since - secs
  val (days,dsecs) = divmod(secs.int, isolar-secs-per-day)
  Dayspan(days, dsecs.ddouble + frac, ts.leap)
}

// The time span since 2000-01-01. (including leap seconds).
// Note: this loses information about the leap seconds.
public fun timespan-withleap( ts : timestamp ) : timespan {
  ts.since + ts.leap.timespan
}


// The time span since 2000-01-01 ignoring leap seconds.
public fun timespan-noleap( ts : timestamp ) : timespan {
  ts.since
}

// Round a time stamp to a certain precision (`prec` is number of digits of the fraction of the second).
public fun round-to-prec(t : timestamp, prec : int ) : timestamp {
  Timestamp(t.since.round-to-prec(prec), t.leap)
}

// Add `leaps` leap seconds to the timestamp.
public fun add-leap-seconds( ts : timestamp, leaps : timespan ) : timestamp {
  if (!leaps.pos?) then ts
  elif (leaps < 1.timespan && ts.leap.zero?)
   then Timestamp( (ts.since - 1.timespan) + leaps, 1)
   else Timestamp( ts.since + leaps.fraction, ts.leap + leaps.truncate.int )
}

// Compare two `:timestamp`s.
public fun compare( i : timestamp, j : timestamp ) : order {
  match(compare(i.since,j.since)) {  // compare without leap-seconds first!
    Eq  -> compare(i.leap,j.leap)
    ord -> ord
  }
}

public fun (<)( i : timestamp, j : timestamp )  : bool { compare(i,j) == Lt }
public fun (<=)( i : timestamp, j : timestamp ) : bool { compare(i,j) != Gt }
public fun (>)( i : timestamp, j : timestamp )  : bool { compare(i,j) == Gt }
public fun (>=)( i : timestamp, j : timestamp ) : bool { compare(i,j) != Lt }
public fun (==)( i : timestamp, j : timestamp ) : bool { compare(i,j) == Eq }
public fun (!=)( i : timestamp, j : timestamp ) : bool { compare(i,j) != Eq }

// The minimum of two timestamps.
public fun min( i : timestamp, j : timestamp ) : timestamp {
  if (i <= j) then i else j
}

// The maximum of two timestamps.
public fun max( i : timestamp, j : timestamp ) : timestamp {
  if (i >= j) then i else j
}

// Add a time span to a time stamp.
public fun (+)( ts : timestamp, t : timespan ) : timestamp {
  Timestamp(ts.since + t, ts.leap)
}

// Subtract a time span from a time stamp.
public fun (-)( ts : timestamp, t : timespan ) : timestamp {
  Timestamp(ts.since - t, ts.leap)
}

// Show a time stamp (without the `s` postfix).
public fun ts-show( ts : timestamp, max-prec : int = 9, secs-width : int = 1, unit : string = "" ) : string {
  val s = ts.since.show-fixed(~max-prec.abs)
  val l = if (ts.leap.zero?) then "" else " (+" + ts.leap.show + " leap)"
  match(s.find(".")) {
    Nothing -> s.pad-left(secs-width,'0') + l
    Just(dot) -> {
      val f = dot.after.string
      val len3 = ((f.count + 2)/3)*3
      //trace("ts-show: " + s + ", " + len3.show + ", prec: " + max-prec.show + ", " + ts.since.show(-9))
      dot.before.string.pad-left(secs-width,'0') + "." + f.pad-right(len3,'0') + unit + l
    }
  }
}
