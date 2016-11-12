/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Instants in time.


*/
module std/time/instant

import std/fixed


/*----------------------------------------------------------------------------
  Durations 
----------------------------------------------------------------------------*/


// Represents duration in time. 
// Can be arbitrarily long and is accurate up to a tenth of a femto-second (10^-16s).
// The `secs` field contains the number of SI seconds, while `frac` holds the
// positive fraction of a second. The total duration `d` is always `d.secs + d.frac`.
abstract struct duration (
  public seconds : fixed     // SI seconds
)


// A zero second duration.
public val duration0 : duration = Duration(zero)

// Create a duration given a number of seconds and nano-seconds
public fun duration-ns( seconds : int, nano-seconds : int ) : duration {
  duration( fixed-frac(seconds,nano-seconds,9) )
}

// Create a duration given a number of seconds and milli-seconds
public fun duration-ms( seconds : int, milli-seconds : int ) : duration {
  duration( fixed-frac(seconds,milli-seconds,3) )
}

// Create a duration given a number of seconds as a `:double` using `max-prec` precision (=`9` (nanosecond))
public fun duration( seconds : double, max-prec : int = 9 ) : duration {
  duration(fixed(seconds,max-prec))
}

public fun duration( seconds : fixed ) : duration {
  Duration(seconds)
}


// Is this a negative duration?
public fun neg?( d : duration ) : bool {
  d.seconds.neg?
}


// Is this a positive duration?
public fun pos?( d : duration ) : bool {
  d.seconds.pos?
}

// Is this a zero duration?
public fun zero?( d : duration ) : bool {
  d.seconds.zero? 
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
  Show durations
----------------------------------------------------------------------------*/

// Show a duration with an optional maximum precision (=`9`, nano-second resolution).
public fun show( d : duration, prec : int = 9 ) : string {   
  //trace("show duration: " + i.secs.show + "s, " + i.frac.show)
  ts-show( d, prec ) + "s"
}

// Show a duration without the `s` (SI seconds) postfix.
public fun ts-show( d : duration, prec : int = 9, secs-width : int = 1 ) : string {
  val s = d.seconds.show(prec)
  //trace("ts-show: " + d.seconds.show + ", prec=" + prec.show + ", s=" + s)
  match(s.find(".")) {
    Nothing   -> s.pad-left(secs-width,'0')
    Just(dot) -> {
      val frac = dot.after.string
      val len3 = ((frac.count + 2)/3)*3
      dot.before.string.pad-left(secs-width,'0') + "." + frac.pad-right(len3,'0')
    }
  }
}



/*----------------------------------------------------------------------------
  Instant
----------------------------------------------------------------------------*/

abstract struct instant(
  public seconds : fixed
)

public val epoch : instant = Instant(zero)

public fun instant-at( t : timestamp ) : instant {
  Instant(t.seconds)
}

public fun instant-at( d : duration ) : instant {
  Instant(d.seconds)
}

public fun since-epoch( i : instant ) : duration {
  Duration(i.seconds)
}

// Compare two `:instant`s in time.
public fun compare( i : instant, j : instant ) : order {
  compare(i.seconds,j.seconds)
}

public fun (<)( i : instant, j : instant )  : bool { compare(i,j) == Lt }
public fun (<=)( i : instant, j : instant ) : bool { compare(i,j) != Gt }
public fun (>)( i : instant, j : instant )  : bool { compare(i,j) == Gt }
public fun (>=)( i : instant, j : instant ) : bool { compare(i,j) != Lt }
public fun (==)( i : instant, j : instant ) : bool { compare(i,j) == Eq }
public fun (!=)( i : instant, j : instant ) : bool { compare(i,j) != Eq }


// Add a duration to an instant in time.
public fun (+)( i : instant, d : duration ) : instant {
  Instant(i.seconds + d.seconds)
}

// Subtract a duration from an instant in time.
public fun (-)( i : instant, d : duration ) : instant {
  Instant(i.seconds - d.seconds)
}

// Return the difference between to instants in time.
public fun (-)( i : instant, j : instant ) : duration {
  i.since-epoch - j.since-epoch
}


// `:timestamp` is an alias used for durations that may not be in SI seconds
// and are generally durations after a certain epoch.
abstract struct timestamp(
  public seconds     : fixed,
  public leap-seconds: fixed = zero
)

public fun non-leap-seconds( t : timestamp ) : fixed {
  t.seconds - t.leap-seconds
}

public fun timestamp( i : instant ) : timestamp {
  timestamp(i.seconds)
}

public fun timestamp( seconds : fixed, leap-seconds : fixed = zero ) : timestamp {
  Timestamp(seconds + leap-seconds, leap-seconds)
}

public fun (+)( t : timestamp, secs : fixed ) : timestamp {
  Timestamp(t.seconds + secs, t.leap-seconds)
}

public fun (-)( t : timestamp, secs : fixed ) : timestamp {
  Timestamp(t.seconds - secs, t.leap-seconds)
}

public fun duration( t : timestamp ) : duration {
  duration(t.seconds)
}

public fun show( t : timestamp ) : string {
  t.seconds.show + (match(t.leap-seconds.sign) {
    Eq -> ""
    Lt -> t.leap-seconds.show
    Gt -> "+" + t.leap-seconds.show
  })
}