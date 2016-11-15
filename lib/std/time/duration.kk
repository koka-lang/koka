/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Time durations in (TAI) [SI seconds](https://en.wikipedia.org/wiki/Second).
*/
module std/time/duration

import std/fixed
import std/time/timespan

/*----------------------------------------------------------------------------
  Durations
----------------------------------------------------------------------------*/

// A duration in time in (TAI) SI seconds.\
// A duration can be arbitrarly long and is precise up to 10^-15^s (1 femto-second).
abstract struct duration(
  secs : timespan                     
)

// A zero duration.
public val duration0 : duration = Duration(timespan0)

// Convert a `:timespan` to a `:duration`. Be careful to only use
// use this on timespan's that are in SI time units!
public fun unsafe-duration( t : timespan ) : duration {
  Duration(t)
}

fun duration( t : timespan ) : duration { 
  Duration(t) 
}

// Duration since the epoch of the time stamp. 
// Be careful to only use this on time stamps in SI seconds!
public fun unsafe-duration( ts : timestamp ) : duration {
  Duration(ts.timespan)
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

// Create a duration from SI seconds as a `:fixed` point number.
public fun duration( secs : fixed ) : duration {
  Duration(timespan(secs))
}

// Create a duration from seconds as a `:double`.
public fun duration( secs : double ) : duration {
  Duration(timespan(secs))
}

// Return the duration in SI seconds.
public fun seconds( d : duration ) : fixed {
  d.secs.seconds
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
  d.secs.ts-show(max-prec) + "s"
}
