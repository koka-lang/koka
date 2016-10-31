/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Basic date and time funs.

   Years, months, days etc. are always 1-based, while durations are always in (fractional) seconds.
*/
module std/time/duration

import std/regex

public alias ts-duration  = duration
public alias utc-duration = ts-duration

// Represents duration between `:instant`s in time. 
// Can be arbitrarily long and is accurate up 
// to a tenth of a femto-second (10^-16s).
abstract struct duration (
  secs : int,          // SI seconds
  frac : double = 0.0  // fraction of seconds, always in range <-1.0, 1.0>
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

public fun neg?( d : duration ) : bool {
  d.secs.neg?
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

// Add two durations
public fun (+)( d : duration, e : duration ) : duration {
  val f = d.frac + e.frac
  duration(d.secs + e.secs + f.trunc.int, f.fraction)
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

public fun value( d : duration ) : (int,double) {
  (d.secs,d.frac)
}


/*----------------------------------------------------------------------------
  Durations in a another time scale
----------------------------------------------------------------------------*/

public fun ts-add( d : duration, e : ts-duration ) : duration {
  ts-duration(d.secs + e.secs, d.frac + e.frac)
}

public fun ts-duration( secs : int, frac : double = 0.0 ) : ts-duration {
  duration(secs,frac)
}

public fun ts-duration( secs : double ) : ts-duration {
  duration(secs)
}

public fun utc-duration( secs : int, frac : double = 0.0 ) : ts-duration {
  ts-duration(secs,frac)
}

public fun utc-duration( secs : double ) : ts-duration {
  ts-duration(secs)
}


/*----------------------------------------------------------------------------
  Show durations and instants
----------------------------------------------------------------------------*/

// Show a duration with an optional precision (=`9`, nano-second resolution).
public fun show( d : duration, prec : int = 9 ) : string {   
  //trace("show duration: " + i.secs.show + "s, " + i.frac.show)
  ts-show( d, prec ) + "s"
}

public fun ts-show( d : ts-duration, prec : int = 9, secs-width : int = 1 ) : string {
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

