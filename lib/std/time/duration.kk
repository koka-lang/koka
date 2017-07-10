/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Time durations in SI seconds.

   Durations use SI seconds as measured on the Earth's geoid (i.e. [TAI](https://en.wikipedia.org/wiki/International_Atomic_Time) SI seconds).
*/
module std/time/duration

import std/num/ddouble
import std/time/timestamp

/*----------------------------------------------------------------------------
  Durations
----------------------------------------------------------------------------*/

// A duration in time in SI seconds (as measured on the Earth's geoid).\
// A duration is represented by a `:ddouble` giving it a high range and precision (see the [`instant`](std_time_instant.html) module)
abstract struct duration(
  secs : timespan                     
)

// A zero duration.
public val zero : duration = Duration(zero:timespan)

// Convert a `:timespan` to a `:duration`. Be careful to only use
// use this on timespan's that are in SI time units!
public fun unsafe-duration( t : timespan ) : duration {
  Duration(t)
}

public fun duration( t : timespan ) : duration { 
  Duration(t) 
}

// Duration since the epoch of the time stamp. 
// Be careful to only use this on time stamps in SI seconds!
public fun unsafe-duration( ts : timestamp ) : duration {
  Duration(ts.seconds)
}

// Convert a duration to a `:timespan`.
public fun timespan( d : duration ) : timespan {
  d.secs
}

// Create a time stamp from a `:duration` since some epoch.
public fun timestamp( d : duration ) : timestamp {
  timestamp(d.timespan)
}

// Create a duration from whole seconds `secs` and a fraction of seconds `frac`.
public fun duration( secs : int, frac : double = 0.0 ) : duration {
  Duration(timespan(secs,frac))
}

// Create a duration from seconds as a `:double`.
public fun duration( secs : double ) : duration {
  Duration(timespan(secs))
}

// Return the duration in SI seconds.
public fun seconds( d : duration ) : timespan {
  d.secs
}

// Return the duration in rounded SI milli-seconds.
public fun milli-seconds( d : duration ) : int {
  (d.secs * 1000.timespan).int
}

// Return the duration in rounded SI nano-seconds.
public fun nano-seconds( d : duration ) : int {
  (d.secs * 1000000000.timespan).int
}

// The whole seconds (in some time unit) of the duration as `:int``.
public fun trunc( d : duration ) : int {
  d.secs.trunc.int
}

// The fractional seconds of a duration as a `:double`.
// `d.seconds == d.trunc.fixed + d.fraction.fixed
public fun fraction( d : duration ) : double {
  d.secs.fraction.double
}

// Is this a negative duration?
public fun neg?( d : duration ) : bool {
  d.secs.neg?
}

// Is this a zero duration?
public fun zero?(  d : duration  ) : bool {
  d.secs.zero?
}

// Is this a positive duration?
public fun pos?( d : duration ) : bool {
  d.secs.pos?
}

// Compare two `:duration`s.
public fun compare( i : duration, j : duration ) : order {
  compare(i.secs,j.secs)
}

public fun (<)( i : duration, j : duration )  : bool { compare(i,j) == Lt }
public fun (<=)( i : duration, j : duration ) : bool { compare(i,j) != Gt }
public fun (>)( i : duration, j : duration )  : bool { compare(i,j) == Gt }
public fun (>=)( i : duration, j : duration ) : bool { compare(i,j) != Lt }
public fun (==)( i : duration, j : duration ) : bool { compare(i,j) == Eq }
public fun (!=)( i : duration, j : duration ) : bool { compare(i,j) != Eq }

// The minimum of two durations.
public fun min( i : duration, j : duration ) : duration {
  if (i <= j) then i else j
}

// The maximum of two durations.
public fun max( i : duration, j : duration ) : duration {
  if (i >= j) then i else j
}

// Add two durations.
public fun (+)( d : duration, e : duration ) : duration {
  duration(d.secs + e.secs)
}

// Negate a duration.
public fun (~)( d : duration ) : duration {
  duration(~d.secs)
}

// Subtract a duration from a duration.
public fun (-)( d : duration, e : duration ) : duration {
  d + ~e
}

// Show a duration in SI seconds.
public fun show( d : duration, max-prec : int = 9 ) : string {
  d.secs.show-fixed(~max-prec.abs) + "s"
}

// Create a `:duration` of `n` milli-seconds.
public fun milli-seconds( n : int ) : duration { duration(n.timespan / 1000.timespan) }

// Create a `:duration` of `n` seconds.
public fun seconds( n : int ) : duration { duration(n) }

// Create a `:duration` of `n` minutes.
public fun minutes( n : int ) : duration { duration(n*60) }

// Create a `:duration` of `n` hours.
public fun hours( n : int )   : duration { duration(n*3600) }

// Create a `:duration` of `n` days (assuming 86400s in a day).
public fun days( n : int )    : duration { duration(n*86400) }

// Create a `:duration` of `n` weeks (assuming 7 days of 86400s).
public fun weeks( n : int )   : duration { duration(n*7*86400) }

// Create a `:duration` of `n` years (assuming 365 days of 86400s).
public fun years( n : int )   : duration { duration(n*365*86400) }
