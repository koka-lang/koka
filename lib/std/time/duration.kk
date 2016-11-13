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

// `:timestamp` is an alias used for durations that may not be in SI seconds
// and are generally durations after a certain epoch.
public alias timestamp = duration


// Represents duration in time. 
// Can be arbitrarily long and is accurate up to a tenth of a femto-second (1.0&times;10^-16^s).
// The `secs` field contains the number of SI seconds, while `frac` holds the
// positive fraction of a second. The total duration `d` is always `d.secs + d.frac`.
abstract struct duration (
  secs : int,
  frac : double = 0.0  // fraction of a second always 0.0 <= frac; for non-leap seconds frac < 1.0
)



/*----------------------------------------------------------------------------
  Durations and instants
----------------------------------------------------------------------------*/

// A zero second duration.
public val duration0 : duration = Duration(0,0.0)

// Create a duration given a number of seconds and fraction of seconds.
public fun duration( secs : int, frac : double = 0.0 ) : duration {
  // maintain 0 <= frac 
  if (!frac.neg?) then
    Duration(secs,frac)
  else {
    val fl = frac.floor
    Duration(secs + fl.int, frac - fl)
  }
}

// Create a duration given a number of seconds as a `:double`.
public fun duration( secs : double ) : duration {
  if (!secs.neg?) then Duration( secs.trunc.int, secs.fraction ) else {
    val fl = secs.floor
    Duration( fl.int, secs - fl)
  }
}

public fun duration( secs : fixed ) : duration {
  duration(secs.trunc.int, secs.fraction.double )
}

// Is this a negative duration?
public fun neg?( d : duration ) : bool {
  d.secs.neg?
}

// Is this a zero duration?
public fun zero?( d : duration ) : bool {
  d.secs.zero? && d.frac.zero?
}

// Is this a positive duration?
public fun pos?( d : duration ) : bool {
  d.secs.pos?
}



// Compare two `:duration`s.
public fun compare( i : duration, j : duration ) : order {
  match(compare(i.secs,j.secs)) {
    Eq  -> compare(i.frac,j.frac) 
    ord -> ord
  }
}


public fun (<)( i : duration, j : duration )  : bool { compare(i,j) == Lt }
public fun (<=)( i : duration, j : duration ) : bool { compare(i,j) != Gt }
public fun (>)( i : duration, j : duration )  : bool { compare(i,j) == Gt }
public fun (>=)( i : duration, j : duration ) : bool { compare(i,j) != Lt }
public fun (==)( i : duration, j : duration ) : bool { compare(i,j) == Eq }
public fun (!=)( i : duration, j : duration ) : bool { compare(i,j) != Eq }

// Add two durations.
public fun (+)( d : duration, e : duration ) : duration {
  // for duration with a leap second encoding (i.e. `frac >= 1.0`), we maintain
  // the leap second if the other duration has a zero fraction. This allows for
  // epoch shifts.
  if (d.frac.zero? || e.frac.zero?) then duration(d.secs + e.secs, d.frac + e.frac)
  else {
    // in all other cases, we normalize the fraction to be in the range ``[0,1.0>``.
    val f = d.frac + e.frac
    duration(d.secs + e.secs + f.trunc.int, f.fraction)
  }
}

// Negate a duration.
public fun (~)( d : duration ) : duration {
  duration(~d.secs,~d.frac)
}

// Subtract a duration from a duration.
public fun (-)( d : duration, e : duration ) : duration {
  d + ~e
}


// Return a duration in seconds. This may lose precision for large durations
// as the result is returned as a `:double`.
public fun double( d : duration ) : double {
  d.secs.double + d.frac
}


public fun seconds( d : duration ) : fixed {
  fixed(d.secs) + fixed(d.frac)
}

/*----------------------------------------------------------------------------
  Durations in a another time scale
----------------------------------------------------------------------------*/

public val timestamp0 : timestamp = duration0

public fun timestamp( seconds : double ) : timestamp {
  duration(seconds)
}


public fun timestamp( seconds : fixed ) : timestamp {
  duration(seconds)
}

public fun timestamp( seconds : int, frac : double = 0.0 ) : timestamp {
  duration(seconds,frac)
}

public fun non-leap-seconds( t : timestamp ) : (timestamp) {
  if (t.frac < 1.0) then t else {
    Duration( t.secs, t.frac.fraction )
  }
}


public fun clock-seconds( t : timestamp ) : (int,double,double) {
  if (t.frac < 1.0) then (t.secs,t.frac,0.0) else {
    (t.secs, t.frac.fraction, t.frac.trunc)
  }
}

public fun timestamp-leap( t : timestamp, leaps : double ) : timestamp {
  Duration( t.secs, t.frac + leaps )
}

/*----------------------------------------------------------------------------
  Show durations and instants
----------------------------------------------------------------------------*/


// Show a duration with an optional maximum precision (=`9`, nano-second resolution).
public fun show( d : duration, prec : int = 9 ) : string {   
  //trace("show duration: " + i.secs.show + "s, " + i.frac.show)
  ts-show( d, prec ) + "s"
}

// Show a duration without the `s` (SI seconds) postfix.
public fun ts-show( d : timestamp, prec : int = 9, secs-width : int = 1 ) : string {
  val (secs,frac,leap) = if (d.frac < 1.0) then (d.secs,d.frac,0) else {
                           val leap = d.frac.trunc.int 
                           (d.secs + leap, d.frac.fraction, leap)
                         }
  show-frac(secs, frac, prec, secs-width ) 
    + (if (leap.zero?) then "" else " (+" + leap.show + " leap)")
}


// Showing fractions is a bit involved due to rounding and 
// showing precision up to 9 digits in increments of 3 digits
fun show-frac( secs0 : int, frac0 : double, prec : int  = 9, secs-width : int = 2 ) : string {
  val (secs,frac) = if (secs0.neg?) then {
                      val adj = frac0.trunc.int + 1
                      (secs0 + adj, 1.0 - frac0.fraction)
                    } else (secs0,frac0)
  val s  = secs.show.pad-left(secs-width,'0')
  val f  = frac.show-fixed(prec).first(2).after.string.replace(rxzeros,"")
  if (f.empty?) then s else {
    val len3 = ((f.count + 2)/3)*3
    s + "." + f.pad-right(len3,'0')
  }
}
val rxzeros = regex("0+$")