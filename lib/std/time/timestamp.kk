/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Time spans and Time stamps that are unit-less
*/
module std/time/timestamp

import std/regex
import std/fixed

// Represents timespan in time in seconds in some time unit.
abstract struct timespan(
  secs : int,
  frac : double  // 0 <= frac; for non-leap encoding, frac < 1.0
)

public val timespan0 = Timespan(0,0.0)

public fun timespan( secs : int, frac : double = 0.0 ) : timespan {
  if (frac.neg? || frac >= 1.0) then {
    val fl = frac.floor
    Timespan( secs + fl.int, frac - fl )
  }
  else Timespan(secs,frac)
}

public fun timespan( secs : fixed ) : timespan {
  timespan( secs.trunc.int, secs.fraction.double )
}

public fun timespan( secs : double ) : timespan {
  timespan( secs.trunc.int, secs.fraction )
}

public fun seconds( t : timespan ) : fixed {
  if (t.frac.zero?) then fixed(t.secs) else fixed(t.secs) + fixed(t.frac)
}

fun without-leap-seconds( t : timespan ) : timespan {
  Timespan(t.secs,t.frac.fraction)
}

// Is this a negative timespan?
public fun neg?( t : timespan ) : bool {
  t.secs.neg?
}

// Is this a zero timespan?
public fun zero?(  t : timespan  ) : bool {
  t.secs.zero?
}

// Is this a positive timespan?
public fun pos?( t : timespan ) : bool {
  t.secs.pos?
}

// Compare two `:timespan`s.
public fun compare( i : timespan, j : timespan ) : order {
  match(compare(i.secs,j.secs)) {
    Eq  -> compare(i.frac,j.frac)
    ord -> ord
  }
}

public fun (<)( i : timespan, j : timespan )  : bool { compare(i,j) == Lt }
public fun (<=)( i : timespan, j : timespan ) : bool { compare(i,j) != Gt }
public fun (>)( i : timespan, j : timespan )  : bool { compare(i,j) == Gt }
public fun (>=)( i : timespan, j : timespan ) : bool { compare(i,j) != Lt }
public fun (==)( i : timespan, j : timespan ) : bool { compare(i,j) == Eq }
public fun (!=)( i : timespan, j : timespan ) : bool { compare(i,j) != Eq }

// Add two timespans.
public fun (+)( t : timespan, u : timespan ) : timespan {
  if (t.frac.zero? || u.frac.zero?) 
   then Timespan(t.secs + u.secs, t.frac + u.frac) // if either frac is zero, maintain leap seconds
   else timespan(t.secs + u.secs, t.frac + u.frac)
}

// Negate a timespan.
public fun (~)( t : timespan ) : timespan {
  timespan(~t.secs, ~t.frac)
}

// Subtract a timespan from a timespan.
public fun (-)( t : timespan, u : timespan ) : timespan {
  t + ~u
}

// Show a timespan with an optional maximum precision (=`9`, nano-second resolution).
public fun show( t0 : timespan, max-prec : int = 9 ) : string {   
  // trace("show timespan: " + t0.secs.show + "s, " + t0.frac.show + ", " + t0.frac.fraction.show)
  val t = t0.round-to-prec(max-prec)
  val fr = t.frac.fraction
  (t.secs + t.frac.trunc.int).show + 
    (if (fr.zero?) then "" else fr.show-fixed(max-prec).tail)
}

// Round a time span to a certain precision.
public fun round-to-prec(t : timespan, prec : int ) : timespan {
  val leap = t.frac.trunc
  val fr   = t.frac.fraction
  val fp   = fr.round-to-prec(prec)
  if (fp==1.0) 
   then Timespan(t.secs + 1, leap )
   else Timespan(t.secs, leap + fp)   
}

// ------------------------------------------------------------------
// Internal leap second handling; used by `:timestamp`.
// ------------------------------------------------------------------

fun add-leap-seconds( t : timespan, leaps : double ) : timespan {
  // trace("add leap secs: " + t.show + ", leaps: " + leaps.show)
  if (leaps <= 0.0) then t 
  elif (leaps < 1.0) then Timespan(t.secs - 1, t.frac + 1.0 + leaps)
  else Timespan(t.secs, t.frac + leaps) 
}

fun leap-seconds( t : timespan ) : int {
  t.frac.trunc.int
}

// Return the total non-leap seconds, the positive fraction (in the range [0,1.0>) and the leap seconds
fun calendar-seconds( t : timespan ) : (int,double,int) {
  (t.secs, t.frac.fraction, t.leap-seconds)
}

// Return the total seconds, a fraction with the same sign as the seconds (in the range <-1.0,1.0>), and the leap seconds.
fun timestamp-seconds( t : timespan ) : (int,double,int) {
  val (secs0,frac0,leaps) = t.calendar-seconds
  val secs1 = secs0 + leaps
  if (secs1.neg? && frac0.pos?) then (secs1 + 1, 1.0 - frac0, leaps) else (secs1,frac0,leaps)
}


/*----------------------------------------------------------------------------
  Timestamps are timespan's since some epoch in some time unit.
  Timestamps expose leap seconds (when appropiate for the time scale)
----------------------------------------------------------------------------*/

// A time stamp is a `:timespan` since some epoch.
abstract struct timestamp(
  since : timespan                          
)

// The time stamp at its epoch.
public val timestamp0 : timestamp = Timestamp(timespan0)


// Create a time stamp from a `:timespan` since some epoch.
public fun timestamp( t : timespan ) : timestamp {
  Timestamp(t)
}

// Create a time stamp in seconds and fraction of seconds since its epoch.
public fun timestamp( secs : int, frac : double = 0.0 ) : timestamp {
  Timestamp(timespan(secs,frac))
}

// Create a time stamp in seconds since its epoch
public fun timestamp( secs : fixed ) : timestamp {
  Timestamp(timespan(secs))
}

// Create a time stamp in seconds since its epoch
public fun timestamp( secs : double ) : timestamp {
  Timestamp(timespan(secs))
}

// The time span since the epoch of the time stamp.
public fun timespan( ts : timestamp ) : timespan {
  ts.since
}

// The seconds (in some time unit) since the epoch of the time stamp
public fun seconds( ts : timestamp ) : fixed {
  ts.since.seconds
}

// Round a time stamp to a certain precision (`prec` is number of digits of the fraction of the second).
public fun round-to-prec(t : timestamp, prec : int ) : timestamp {
  Timestamp(t.since.round-to-prec(prec))
}


// Return the non-leap seconds since its epoch.
public fun without-leap-seconds( ts : timestamp ) : timestamp {
  Timestamp(ts.since.without-leap-seconds)
}

// Add `leaps` leap seconds to the time stamp.
public fun add-leap-seconds( ts : timestamp, leaps : double ) : timestamp {
  if (leaps.pos?) 
   then Timestamp( ts.since.add-leap-seconds(leaps) )
   else ts
}

// Return the total non-leap seconds, the positive fraction (in the range [0,1.0>), and the leap seconds
public fun calendar-seconds( ts : timestamp ) : (int,double,int) {
  ts.since.calendar-seconds
}

// Compare two `:timestamp`s.
public fun compare( i : timestamp, j : timestamp ) : order {
  compare(i.since,j.since)
}

public fun (<)( i : timestamp, j : timestamp )  : bool { compare(i,j) == Lt }
public fun (<=)( i : timestamp, j : timestamp ) : bool { compare(i,j) != Gt }
public fun (>)( i : timestamp, j : timestamp )  : bool { compare(i,j) == Gt }
public fun (>=)( i : timestamp, j : timestamp ) : bool { compare(i,j) != Lt }
public fun (==)( i : timestamp, j : timestamp ) : bool { compare(i,j) == Eq }
public fun (!=)( i : timestamp, j : timestamp ) : bool { compare(i,j) != Eq }

// Add a time span to a time stamp.
public fun (+)( ts : timestamp, t : timespan ) : timestamp {
  Timestamp(ts.since + t)
}

// Subtract a time span from a time stamp.
public fun (-)( ts : timestamp, t : timespan ) : timestamp {
  Timestamp(ts.since - t)
}

// Difference between two time stamps.
public fun (-)( ts1 : timestamp, ts2 : timestamp ) : timespan {
  ts1.since - ts2.since
}

// Show a time span (without the `s` (SI seconds) postfix).
public fun show( ts : timestamp, prec : int = 9, secs-width : int = 1 ) : string {
  val (secs,frac,leaps) = ts.since.round-to-prec(prec).timestamp-seconds
  val s    = secs.show.pad-left(secs-width,'0')
  val l    = if (leaps.zero?) then "" else " (+" + leaps.show + " leap)"
  if (frac.zero?) then s + l else {
    val f    = frac.show.first(2).after.string
    val len3 = ((f.count + 2)/3)*3
    s + "." + f.pad-right(len3,'0')  + l
  }
}


