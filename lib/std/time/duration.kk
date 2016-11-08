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

// `:timestamp` is an alias used for durations that may not be in SI seconds
// and are generally durations after a certain epoch.
public alias timestamp  = duration

// `:utc-timestamp` is an alias used for timestamp's in UTC seconds.
public alias utc-timestamp = timestamp

// Represents duration in time. 
// Can be arbitrarily long and is accurate up to a tenth of a femto-second (10^-16s).
// The `secs` field contains the number of SI seconds, while `frac` holds the
// positive fraction of a second. The total duration `d` is always `d.secs + d.frac`.
abstract struct duration (
  public secs : int,          // SI seconds
  public frac : double = 0.0  // fraction of seconds, always in range [0, 1.0>
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
  val fl = secs.floor
  Duration( fl.int, secs - fl )
}

// Is this a negative duration?
public fun neg?( d : duration ) : bool {
  d.secs.neg?
}

// Is this a zero duration?
public fun zero?( d : duration ) : bool {
  d.secs.zero? && d.frac.zero?
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
public fun seconds( d : duration ) : double {
  d.secs.double + d.frac
}


/*----------------------------------------------------------------------------
  Durations in a another time scale
----------------------------------------------------------------------------*/

// Internal: create a duration in UTC 'seconds'
public fun timestamp( secs : int, frac : double = 0.0 ) : timestamp {
  duration(secs,frac)
}

// Internal: create a duration in UTC 'seconds'
public fun timestamp( secs : double ) : timestamp {
  duration(secs)
}

// Internal: create a duration in UTC 'seconds'
public fun utc-timestamp( secs : int, frac : double = 0.0 ) : utc-timestamp {
  duration(secs,frac)
}

// Internal: create a duration in UTC 'seconds'
public fun utc-timestamp( secs : double ) : utc-timestamp {
  duration(secs)
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
  val leap = d.frac.trunc.int  
  // trace("show duration: " + d.secs.show + "s + " + d.frac.show)
  show-frac(d.secs,d.frac,prec,secs-width) 
    + (if (leap.zero?) then "" else " (" + (if (leap.pos?) then "+" else "-") + leap.abs.show + "leap)")
}


// Showing fractions is a bit involved due to rounding and 
// showing precision up to 9 digits in increments of 3 digits
fun show-frac( secs : int, frac : double, prec : int  = 9, width : int = 2 ) : string {
  if (prec==0) return secs.show0(width) 
  val fr   = frac.round-to-prec(prec)
  val fl   = fr.floor
  val (s,f) = if (secs.neg? && !fr.zero?) 
                then (secs + fl.int + 1, (fr - fl) - 1.0)
                else (secs + fl.int, fr - fl)
  val sign = if (s.neg? || s.zero? && fr.neg?) then "-" else "" 
  //trace("show secs: " + s.show + ", " + f.show)
  sign + s.abs.show0(width) + f.show-frac(prec) 
}

// Show a fraction of a second up to an optional maximum precision (=`9`)
fun show-frac( frac : double, prec : int = 9 ) : string {  
  //trace("show frac: " + frac.show)
  val xdigits = frac.abs.fraction.show-fixed(prec).list.drop(2)
  val digits  = xdigits.reverse.drop-while(fun(d){d=='0'}).reverse
  if (digits.nil?) return "" // zero fraction
  // always use a multiple of 3 to display a fraction
  val len3 = min(prec, ((digits.length + 2) / 3) * 3)
  "." + digits.string.pad-right( len3, '0') 
}
val rxdigits = regex(@"^\d+\.\(\d+[1-9]\)0*$")

// pad with zeros
fun show0( i : int, width : int = 2) : string {
  i.show.pad-left(width,'0')
}

