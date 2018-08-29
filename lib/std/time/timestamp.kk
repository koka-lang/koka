/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation

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

// Time spans are just a `:ddouble`; this gives it very high precision
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

public fun div( x : ddouble, y : ddouble, prec : int = 0) : ddouble {
  x / y
}

/*----------------------------------------------------------------------------
  Timestamps are timespan's since some epoch in some time unit.
  Timestamps expose leap seconds (when appropiate for the time scale)
----------------------------------------------------------------------------*/

// A time stamp is a `:timespan` since some epoch. Time stamps also keep
// track of possible leap seconds when the unit is in [Unix/UTC seconds](https://en.wikipedia.org/wiki/Unix_time).
//
// When using Unix/UTC seconds, the `leap-adjust` field is non-zero if this `:timestamp` is inside a leap
// second. It is _not_ the total number of leap seconds that have occurred up to that time, but is used
// as an adjustment to signify the time is in a particular leap second. For example, for a regular
// leap second we have:\
// ``2016-12-31T23:59:59Z`` = 536543999 with a `leap-adjust` of 0.\
// ``2016-12-31T23:59:60Z`` = 536543999 with a `leap-adjust` of 1.\
// ``2017-01-01Z`` = 536544000 with a `leap-adjust` of 0 again.
abstract struct timestamp(
  since : timespan,
  // the extra leap seconds; zero for any time scale except non-monotonic ones like Unix time.
  public leap-adjust : int
)

// The time stamp at its epoch.
public val timestamp0 : timestamp = Timestamp(zero,0)


// Create a time stamp from a `:timespan` since some epoch.
public fun timestamp( t : timespan, leap-adjust : int = 0 ) : timestamp {
  Timestamp(t,leap-adjust)
}

// Create a time stamp in seconds and fraction of seconds since its epoch.
// Can also pass possible `leap-adjust` (=`0`). If the unit is in Unix seconds
// (where a day is always 86400 seconds) this is used to indicate extra leap seconds in that day.
public fun timestamp( secs : int, frac : double = 0.0, leap-adjust : int = 0 ) : timestamp {
  timestamp(timespan(secs,frac), leap-adjust)
}

// Create a time stamp in seconds since its epoch
public fun timestamp( secs : double, leap-adjust : int = 0 ) : timestamp {
  timestamp(timespan(secs),leap-adjust)
}

// The time span since the epoch of the time stamp. (including leap seconds)
public fun timespan( ts : timestamp ) : timespan {
  ts.seconds
}

// The total seconds (in some time unit) since the epoch of the time stamp.
// This includes a possible leap adjustment.
public fun seconds( ts : timestamp ) : timespan {
  ts.since + ts.leap-adjust.timespan
}

// Round a time stamp to a certain precision (`prec` is number of digits of the fraction of the second).
public fun round-to-prec(t : timestamp, prec : int ) : timestamp {
  Timestamp(t.since.round-to-prec(prec), t.leap-adjust)
}

// Return the non-leap seconds since its epoch.
// `t.seconds == t.unadjusted-seconds + t.leap-adjust.timespan`.
public fun unadjusted-seconds( ts : timestamp ) : timespan {
  ts.since
}

// Add `leaps` leap seconds to the leap adjustment of the timestamp.
public fun adjust-leap-seconds( ts : timestamp, leaps : timespan ) : timestamp {
  if (!leaps.pos?) then ts
  elif (leaps < timespan(1.0) && ts.leap-adjust.zero?)
   then Timestamp( (ts.since - 1.timespan) + leaps, 1)
   else Timestamp( ts.since + leaps.fraction, ts.leap-adjust + leaps.truncate.int )
}



// Return the total non-leap seconds, the positive fraction (in the range [0,1.0>), and the leap adjustment.
// This is used when displaying calendar time such that leap seconds can be displayed correctly.
public fun calendar-seconds( ts : timestamp ) : (int,double,int) {
  (ts.since.floor.int, ts.since.ffraction.double, ts.leap-adjust)
}

// Compare two `:timestamp`s.
public fun compare( i : timestamp, j : timestamp ) : order {
  match(compare(i.since,j.since)) {  // compare without leap-seconds first!
    Eq  -> compare(i.leap-adjust,j.leap-adjust)
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
  Timestamp(ts.since + t, ts.leap-adjust)
}

// Subtract a time span from a time stamp.
public fun (-)( ts : timestamp, t : timespan ) : timestamp {
  Timestamp(ts.since - t, ts.leap-adjust)
}

// Difference between two time stamps.
//public fun (-)( ts1 : timestamp, ts2 : timestamp ) : timespan {
//  ts1.since - ts2.since
//}

// Show a time stamp (without the `s` postfix).
public fun ts-show( ts : timestamp, max-prec : int = 9, secs-width : int = 1 ) : string {
  val s = ts.since.show-fixed(~max-prec.abs)
  val l = if (ts.leap-adjust.zero?) then "" else " (+" + ts.leap-adjust.show + " leap)"
  match(s.find(".")) {
    Nothing -> s.pad-left(secs-width,'0') + l
    Just(dot) -> {
      val f = dot.after.string
      val len3 = ((f.count + 2)/3)*3
      //trace("ts-show: " + s + ", " + len3.show + ", prec: " + max-prec.show + ", " + ts.since.show(-9))
      dot.before.string.pad-left(secs-width,'0') + "." + f.pad-right(len3,'0') + l
    }
  }
}
