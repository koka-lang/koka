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
import std/time/duration

/*----------------------------------------------------------------------------
  Instant
----------------------------------------------------------------------------*/

public val secs-per-day = 86400

abstract struct instant(
  since : timestamp,
  ts    : timescale
)

/*----------------------------------------------------------------------------
  Timescale
----------------------------------------------------------------------------*/

public struct timescale(
  ts-name     : string,
  ts-unit     : string,
  private from-tai    : duration -> timestamp,
  private to-tai      : timestamp -> duration,
  epoch-delta    : timestamp,         // ts-epoch = 2000-01-01TS + epoch-delta
  epoch-jd-delta : timestamp   // epoch-delta minus the offset; used for julian date calculation
)


/*----------------------------------------------------------------------------
  Instant operations
----------------------------------------------------------------------------*/

public fun use( i : instant, tscale : timescale ) : instant {
  if (i.ts.ts-name == tscale.ts-name) then {
    // already using the right time scale
    i
  }
  else {
    Instant( i.timestamp(tscale), tscale )
  }
}

public fun convert( t : timestamp, from : timescale, to : timescale ) : timestamp {
  if (from.ts-name == to.ts-name) then {
    // already using the right time scale
    t
  }
  elif (!from.ts-unit.empty? && from.ts-unit == to.ts-unit) then {
    // just an epoch shift
    // important optimization as it allows UNIX timestamp to UTC conversion without going through TAI
    // trace( "instant use: shift: " + i.ts.ts-name + " -> " + t.ts-name + ", " + i.ts.epoch-delta.show + " vs. " + t.epoch-delta.show + ", i: " + i.since.show )
    (t + from.epoch-delta) - to.epoch-delta
  }
  else {
    // transform through TAI
    (to.from-tai)( (from.to-tai)(t) ) 
  }
}

public fun to-tai(  ts : timescale, t : timestamp ) : duration {
  t.convert(ts,ts-tai).duration
}

public fun from-tai(ts : timescale, d : duration ) : timestamp {
  d.timestamp.convert(ts-tai,ts)
}

public fun timescale( i : instant ) : timescale {
  i.ts
}

public fun timestamp( i : instant, tscale : timescale = ts-tai ) : timestamp {
  i.since.convert(i.ts,tscale)
}

public fun instant-at( t : timestamp, ts : timescale ) : instant {
  Instant(t,ts)
}


// Our epoch is set at 2000-01-01 TAI (= 1999-12-31T23:59:28Z)
public val epoch : instant = Instant(timestamp0,ts-tai)

public fun since-epoch( i : instant ) : duration {
  duration( i.timestamp(ts-tai) )
}

public fun since-epoch( d : duration ) : instant {
  instant-at(d.timestamp, ts-tai)
}

// Compare two `:instant`s in time.
public fun compare( i : instant, j : instant ) : order {
  compare(i.since, j.use(i.ts).since)
}

public fun (<)( i : instant, j : instant )  : bool { compare(i,j) == Lt }
public fun (<=)( i : instant, j : instant ) : bool { compare(i,j) != Gt }
public fun (>)( i : instant, j : instant )  : bool { compare(i,j) == Gt }
public fun (>=)( i : instant, j : instant ) : bool { compare(i,j) != Lt }
public fun (==)( i : instant, j : instant ) : bool { compare(i,j) == Eq }
public fun (!=)( i : instant, j : instant ) : bool { compare(i,j) != Eq }


// Add a duration to an instant in time.
public fun (+)( i : instant, d : duration ) : instant {
  if (i.ts.ts-unit=="TAI")
   then Instant( i.since + d.timestamp, i.ts ) // directly add
   else Instant( i.timestamp(ts-tai) + d.timestamp, ts-tai )
}

// Subtract a duration from an instant in time.
public fun (-)( i : instant, d : duration ) : instant {
  i + (~d)
}

// Return the difference between to instants in time.
public fun (-)( i : instant, j : instant ) : duration {
  i.since-epoch - j.since-epoch
}


public fun show( i : instant, max-prec : int = 9, secs-width : int = 1 ) : string {
  i.since.show(max-prec,secs-width) + "s"
   + (if (i.ts.ts-name.empty?) then "" else " " + i.ts.ts-name)
}



// Create a new time scale based on SI seconds with a given `name`, `offset` and `epoch-jd-delta`:\
// `time-scale-epoch == epoch + epoch-jd-delta`.
public fun tai-timescale( name : string, offset : timestamp = timestamp0, epoch-jd-delta : timestamp = timestamp0 ) : timescale {
  val epoch-delta = epoch-jd-delta - offset
  fun from-tai(tai:duration) { tai.timestamp - epoch-delta }
  fun to-tai(ts:timestamp)   { (ts + epoch-delta).duration }
  Timescale(
    name,
    "TAI",
    from-tai,
    to-tai,
    epoch-delta,
    epoch-jd-delta
  )
}

// The [TAI](https://en.wikipedia.org/wiki/International_Atomic_Time) (International atomic time) 
// time scale is based on SI seconds with a 2000-01-01 TAI `epoch`.
public val ts-tai : timescale = tai-timescale( "TAI" )
