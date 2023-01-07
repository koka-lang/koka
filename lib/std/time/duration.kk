/*----------------------------------------------------------------------------
   Copyright 2012-2021, Microsoft Research, Daan Leijen

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/

/* Time durations in SI seconds.

   Durations use SI seconds.
*/
module std/time/duration

import std/num/ddouble
import std/time/timestamp

/*----------------------------------------------------------------------------
  Durations
----------------------------------------------------------------------------*/

// A duration in time in (TAI) SI seconds (as measured on the earth's geoid).\
// A duration is represented by a `:ddouble` giving it a high range and precision (see the [`instant`](std_time_instant.html) module)
abstract struct duration(
  secs : timespan
)

// A zero duration.
pub val zero : duration = Duration(timespan0)
pub val duration0 : duration = Duration(timespan0)

// Convert a `:timespan` to a `:duration`. Be careful to only use
// use this on timespan's that are in TAI SI seconds!
pub fun duration( t : timespan ) : duration
  Duration(t)


// Convert a `:timestamp` to a `:duration`. Be careful to only use
// use this on timestamp's that are in TAI SI seconds and do not
// contain leap seconds!
pub fun unsafe-duration( t : timestamp ) : duration
  Duration(t.unsafe-timespan-withleap)


// Create a duration from whole seconds `secs` and a fraction of seconds `frac`.
pub fun duration( secs : int, frac : float64 = 0.0 ) : duration
  Duration(timespan(secs,frac))

// Create a duration from seconds as a `:float64`.
pub fun duration( secs : float64 ) : duration
  Duration(timespan(secs))

// Create a duration from seconds and a fraction as a `:float64`'s.
pub fun duration( secs : float64, frac : float64 ) : duration
  Duration(timespan(secs,frac))

// Convert a duration to a `:timespan`.
pub fun timespan( d : duration ) : timespan
  d.seconds

// Convert a duration to a `:timespan`.
pub fun timestamp( d : duration ) : timestamp
  d.seconds.timestamp

// Return the duration in SI seconds.
pub fun seconds( d : duration ) : timespan
  d.secs

// Return the duration in rounded SI milli-seconds.
pub fun milli-seconds( d : duration ) : int
  (d.secs * 1000.timespan).int

// Return the duration in rounded SI nano-seconds.
pub fun nano-seconds( d : duration ) : int
  (d.secs * 1000000000.timespan).int

// The whole seconds (in some time unit) of the duration as `:int``. Rounds towards zero.
pub fun truncate( d : duration ) : int
  d.secs.truncate.int

// The fractional seconds of a duration as a `:float64`.
// `d.seconds == d.truncate.fixed + d.fraction.fixed
pub fun fraction( d : duration ) : float64
  d.secs.fraction.float64

// Is this a negative duration?
pub fun is-neg( d : duration ) : bool
  d.secs.is-neg

// Is this a zero duration?
pub fun is-zero(  d : duration  ) : bool
  d.secs.is-zero

// Is this a positive duration?
pub fun is-pos( d : duration ) : bool
  d.secs.is-pos

// Compare two `:duration`s.
pub fun compare( i : duration, j : duration ) : order
  compare(i.secs,j.secs)

pub fun (<)( i : duration, j : duration )  : bool { compare(i,j) == Lt }
pub fun (<=)( i : duration, j : duration ) : bool { compare(i,j) != Gt }
pub fun (>)( i : duration, j : duration )  : bool { compare(i,j) == Gt }
pub fun (>=)( i : duration, j : duration ) : bool { compare(i,j) != Lt }
pub fun (==)( i : duration, j : duration ) : bool { compare(i,j) == Eq }
pub fun (!=)( i : duration, j : duration ) : bool { compare(i,j) != Eq }

// The minimum of two durations.
pub fun min( i : duration, j : duration ) : duration
  if i <= j then i else j

// The maximum of two durations.
pub fun max( i : duration, j : duration ) : duration
  if i >= j then i else j

// Add two durations.
pub fun (+)( d : duration, e : duration ) : duration
  Duration(d.secs + e.secs)

// Negate a duration.
pub fun (~)( d : duration ) : duration
  Duration(~d.secs)

// Subtract a duration from a duration.
pub fun (-)( d : duration, e : duration ) : duration
  d + ~e

// Show a duration in SI seconds.
pub fun show( d : duration, max-prec : int = 9 ) : string
  d.secs.show-fixed(~max-prec.abs) ++ "s"

// Create a `:duration` of `n` milli-seconds.
pub fun milli-seconds( n : int ) : duration { Duration(n.timespan / 1000.timespan) }

// Create a `:duration` of `n` seconds.
pub fun seconds( n : int ) : duration { duration(n) }

// Create a `:duration` of `n` minutes.
pub fun minutes( n : int ) : duration { duration(n*60) }

// Create a `:duration` of `n` hours.
pub fun hours( n : int )   : duration { duration(n*3600) }

// Create a `:duration` of `n` "days" (assuming 86400s in a day).
pub fun days( n : int )    : duration { duration(n*isolar-secs-per-day) }

// Create a `:duration` of `n` "weeks" (assuming 7 days of 86400s).
pub fun weeks( n : int )   : duration { duration(n*7*isolar-secs-per-day) }

// Create a `:duration` of `n` "years" (assuming 365 days of 86400s).
pub fun years( n : int )   : duration { duration(n*365*isolar-secs-per-day) }
