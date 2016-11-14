/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Time durations.
*/
module std/time/duration

import std/regex
import std/fixed

// Represents duration in time in (TAI) SI seconds
abstract struct duration(
  public seconds : fixed                          
)

abstract struct timestamp(
  // total seconds, includes the leap-seconds                          
  public seconds      : fixed,
  // possible leap seconds
  public leap-seconds : int
)

public function timestamp( d : duration, leap-seconds : int = 0 ) : timestamp {
  Timestamp(d.seconds,leap-seconds)
}

public function duration( t : timestamp ) : duration {
  Duration(t.seconds)
}


/*----------------------------------------------------------------------------
  Durations and instants
----------------------------------------------------------------------------*/

// A zero second duration.
public val duration0 : duration = Duration(zero)

// Create a duration given a number of seconds and fraction of seconds.
public fun duration( secs : int, frac : double = 0.0 ) : duration {
  if (frac.zero?) 
   then duration(fixed(secs))
   else duration(fixed(secs) + fixed(frac))
}

// Create a duration given a number of seconds as a `:double`.
public fun duration( secs : double ) : duration {
  duration(fixed(secs))
}

public function duration( seconds : fixed ) : duration {
  Duration(seconds)
}


// Is this a negative duration?
public fun neg?( d : duration ) : bool {
  d.seconds.neg?
}

// Is this a zero duration?
public fun zero?( d : duration ) : bool {
  d.seconds.zero?
}

// Is this a positive duration?
public fun pos?( d : duration ) : bool {
  d.seconds.pos?
}

// Compare two `:duration`s.
public fun compare( i : duration, j : duration ) : order {
  compare(i.seconds,j.seconds)
}

public fun (<)( i : duration, j : duration )  : bool { compare(i,j) == Lt }
public fun (<=)( i : duration, j : duration ) : bool { compare(i,j) != Gt }
public fun (>)( i : duration, j : duration )  : bool { compare(i,j) == Gt }
public fun (>=)( i : duration, j : duration ) : bool { compare(i,j) != Lt }
public fun (==)( i : duration, j : duration ) : bool { compare(i,j) == Eq }
public fun (!=)( i : duration, j : duration ) : bool { compare(i,j) != Eq }

// Add two durations.
public fun (+)( d : duration, e : duration ) : duration {
  Duration(d.seconds + e.seconds)
}

// Negate a duration.
public fun (~)( d : duration ) : duration {
  Duration(~d.seconds)
}

// Subtract a duration from a duration.
public fun (-)( d : duration, e : duration ) : duration {
  Duration(d.seconds - e.seconds)
}



/*----------------------------------------------------------------------------
  Durations in a another time scale
----------------------------------------------------------------------------*/

public val timestamp0 : timestamp = timestamp(duration0)


public fun timestamp( seconds : int, leap-seconds : int = 0 ) : timestamp {
  timestamp(fixed(seconds),leap-seconds)
}


public fun timestamp( seconds : fixed, leap-seconds : int = 0 ) : timestamp {
  Timestamp(seconds,leap-seconds)
}

public fun add-leap-seconds( t : timestamp, leaps : fixed ) : timestamp {
  //trace("add leaps: " + t.show + ", leaps: " + leaps.show)
  if (leaps.zero?) then t else {
    val leap = max(leaps.trunc.int,1)
    Timestamp(t.seconds + leaps, t.leap-seconds + leap)
  }
}

public fun non-leap-seconds( t : timestamp ) : fixed {
  t.seconds - fixed(t.leap-seconds)
}

public fun clock-seconds( t : timestamp ) : (int,fixed,int) {
  //trace("clock seconds: " + t.show)
  val secs = t.seconds.trunc.int
  val frac = t.seconds.fraction
  if (frac.neg?) then {
    (secs.dec - t.leap-seconds, fixed(1) + frac, t.leap-seconds)
  }
  else {
    (secs - t.leap-seconds, frac, t.leap-seconds)
  }
}


// Is this a negative timestamp?
public fun neg?( t : timestamp ) : bool {
  t.seconds.neg?
}

// Is this a zero timestamp?
public fun zero?( t : timestamp ) : bool {
  t.seconds.zero?
}

// Is this a positive timestamp?
public fun pos?( t : timestamp ) : bool {
  t.seconds.pos?
}

// Compare two `:timestamp`s.
public fun compare( i : timestamp, j : timestamp ) : order {
  compare(i.seconds,j.seconds)
}

public fun (<)( i : timestamp, j : timestamp )  : bool { compare(i,j) == Lt }
public fun (<=)( i : timestamp, j : timestamp ) : bool { compare(i,j) != Gt }
public fun (>)( i : timestamp, j : timestamp )  : bool { compare(i,j) == Gt }
public fun (>=)( i : timestamp, j : timestamp ) : bool { compare(i,j) != Lt }
public fun (==)( i : timestamp, j : timestamp ) : bool { compare(i,j) == Eq }
public fun (!=)( i : timestamp, j : timestamp ) : bool { compare(i,j) != Eq }

// Add two timestamps.
public fun (+)( d : timestamp, e : timestamp ) : timestamp {
  timestamp(d.seconds + e.seconds, d.leap-seconds + e.leap-seconds)
}

// Subtract a timestamp from a timestamp.
public fun (-)( d : timestamp, e : timestamp ) : timestamp {
  timestamp(d.seconds - e.seconds, max(0, d.leap-seconds - e.leap-seconds))
}



/*----------------------------------------------------------------------------
  Show durations and instants
----------------------------------------------------------------------------*/


// Show a duration with an optional maximum precision (=`9`, nano-second resolution).
public fun show( d : duration, prec : int = 9 ) : string {   
  //trace("show duration: " + i.secs.show + "s, " + i.frac.show)
  d.seconds.round-to-prec(prec).show + "s"
}

// Show a duration without the `s` (SI seconds) postfix.
public fun show( t : timestamp, prec : int = 9, secs-width : int = 1 ) : string {
  val secs = t.seconds.round-to-prec(prec)
  val s    = secs.trunc.int.show.pad-left(secs-width,'0')
  val l    = if (t.leap-seconds.zero?) then "" else " (+" + t.leap-seconds.show + " leap)"
  val frac = secs.fraction
  if (frac.zero?) then s + l else {
    val f = frac.abs.show.first(2).after.string
    val len3 = ((f.count + 2)/3)*3
    s + "." + f.pad-right(len3,'0')  + l
  }
}
