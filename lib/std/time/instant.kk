/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Basic date and time funs.

   Years, months, days etc. are always 1-based, while durations are always in (fractional) seconds.
*/
module std/time/instant

import std/time/duration
import std/time/leaps

extern include {
  // cs file "time-inline.cs"
  js file "instant-inline.js"
}


// The _epoch_ is 1958-01-01Z TAI which equals 1958-01-01Z UTC.\
// Nice properties:
//
// - for any date <  1960-01-01Z, we have UTC == TAI
// - for any date >= 1972-01-01Z, we have UTC == TAI - whole SI second offset
public val epoch = instant-since(0.0,utc)


public struct time-scale(
  name      : string,
  from-tai  : duration -> ts-duration,
  to-tai    : ts-duration -> duration,
  from-utc  : ts-duration -> ts-duration,
  to-utc    : ts-duration -> ts-duration,
  allow-leap: bool = False,
  epoch-shift: ts-duration = duration0
)


fun utc-timescale( name : string, epoch-shift : ts-duration = duration0 ) : time-scale {
  fun from-utc(d:ts-duration) { ts-add( d, ~epoch-shift ) }
  fun to-utc(d:ts-duration)   { ts-add( d, epoch-shift) } 
  Time-scale(
    name,
    fun(d) { d.utc-from-tai.from-utc },         
    fun(d) { d.to-utc.tai-from-utc   },
    from-utc,
    to-utc,
    True,
    epoch-shift
  )
}

fun tai-timescale( name : string, epoch-shift : duration = duration0 ) : time-scale {
  fun from-tai(d:duration) { d - epoch-shift }
  fun to-tai(d:duration)   { d + epoch-shift }
  Time-scale(
    name,
    from-tai,
    to-tai,
    fun(d) { d.tai-from-utc.from-tai },
    fun(d) { d.to-tai.utc-from-tai   },
    False,
    epoch-shift
  )
}


public val tai    : time-scale = tai-timescale( "TAI" )
 
public val gps    : time-scale = tai-timescale( "GPS", duration(694224019) )

public val utc    : time-scale = utc-timescale( "" )

public val ts-unix: time-scale = utc-timescale( "UNIX", duration(378691200) )

public val ts-ntp : time-scale = utc-timescale( "NTP", duration(~2272060800) )

public val ts-mjd : time-scale = utc-timescale( "MJD", duration(~3128025600) )

public val ts-jd  : time-scale = utc-timescale( "JD", duration(~207360043200) )


// Represents an instant in time. 
// Can be arbitrarily far into the future or past and is accurate up 
// to a tenth of a femto-second (10^-16s).
abstract struct instant (
  since: ts-duration,                       
  ts   : time-scale
)

/*----------------------------------------------------------------------------
  Durations and instants
----------------------------------------------------------------------------*/

// Create an instant in time given a number of seconds since the `epoch`
// and fraction of seconds (=``0.0``).
public fun instant-since( secs : int, frac : double = 0.0, ts : time-scale = utc ) : instant {
  instant-sincex(secs,frac,ts)
}

private fun instant-sincex( secs : int, frac : double, ts : time-scale ) : instant {
  if (frac.neg? || (frac > 1.0 && !ts.allow-leap)) {
    val fl = frac.floor
    Instant(duration(secs + fl.int, frac - fl), ts)
  }
  else Instant(duration(secs,frac),ts)
}

// Create an instant given a number of seconds since the `epoch`
// given as a `:double`.
public fun instant-since( secs : double, ts : time-scale = utc ) : instant {
  Instant( duration(secs), ts )
}

// Return the duration since the `epoch`.
public fun since-epoch( i : instant ) : duration {
  val j = i.ts-use(tai)
  j.since
}

// Return the instant in time that is `d` duration after the `epoch`.
public fun since-epoch( d : duration ) : instant {
  Instant(d,tai)
}

// Compare two `:instant`s in time.
public fun compare( i : instant, j : instant ) : order {
  val h = j.ts-use(i.ts)
  compare(i.since,h.since)
}

public fun (<)( i : instant, j : instant )  : bool { compare(i,j) == Lt }
public fun (<=)( i : instant, j : instant ) : bool { compare(i,j) != Gt }
public fun (>)( i : instant, j : instant )  : bool { compare(i,j) == Gt }
public fun (>=)( i : instant, j : instant ) : bool { compare(i,j) != Lt }
public fun (==)( i : instant, j : instant ) : bool { compare(i,j) == Eq }
public fun (!=)( i : instant, j : instant ) : bool { compare(i,j) != Eq }


// Add a duration to an instant in time.
public fun (+)( i : instant, d : duration ) : instant {
  (i.since-epoch + d).since-epoch
}

// Subtract a duration from an instant in time.
public fun (-)( i : instant, d : duration ) : instant {
  i + ~d
}

// Return the difference between to instants in time.
public fun (-)( i : instant, j : instant ) : duration {
  i.since-epoch - j.since-epoch
}

/*----------------------------------------------------------------------------
  Durations in a another time scale
----------------------------------------------------------------------------*/

public fun ts-use( i : instant, tscale : time-scale ) : instant {
  if (i.ts.name == tscale.name) then {
    i 
  }
  elif (tscale.allow-leap) {
    //trace("utc convert: " + i.show)
    Instant( (tscale.from-utc)( (i.ts.to-utc)(i.since)), tscale )
  }
  else {
    //trace("tai convert: " + i.show)
    Instant( (tscale.from-tai)( (i.ts.to-tai)(i.since) ), tscale)
  }
}

public fun ts-since-epoch( i : instant, ts : time-scale = i.ts ) : ts-duration {
  i.ts-use(ts).since
}

public fun ts-since-epoch( d : ts-duration, ts : time-scale ) : instant {
  Instant(d,ts)
}

public fun ts-add( i : instant, d : ts-duration ) : instant {
  Instant(ts-add(i.since,d), i.ts)
}

public fun ts-diff( i : instant, j : instant ) : ts-duration {
  val d = i.since 
  val e = j.ts-use(i.ts).since
  ts-add(d,e)
}



/*----------------------------------------------------------------------------
  Show durations and instants
----------------------------------------------------------------------------*/


// Show an instant with an optional precision (=`9`, nano-second resolution).
public fun show( i : instant, prec : int = 9 ) : string {
  ts-show(i.since) + "s " + (if (i.ts.name.empty?) then "" else " " + i.ts.name)
}


/*----------------------------------------------------------------------------
  Julian Date
----------------------------------------------------------------------------*/

val jd-epoch-shift     = 2400000.5
val mjd-epoch : instant = instant-since(~mjd-epoch-shift,0.0,utc)  // 1858-11-17Z modified julian date epoch.
val mjd-epoch-shift    = 3128025600

// Create an instant given a [julian day](https://en.wikipedia.org/wiki/Julian_day).
public fun instant-at-jd( julian-date: double ) : instant {
  instant-at-mjd( julian-date - jd-epoch-shift )
}

// Create an instant given a [modified julian day](https://en.wikipedia.org/wiki/Julian_day).\
// modified-julian-date = julian-date - 2400000.5
public fun instant-at-mjd( modified-julian-date : double ) : instant {
  val frac = modified-julian-date.fraction * secs-per-day.double
  val secs = modified-julian-date.trunc.int * secs-per-day + frac.trunc.int 
  ts-since-epoch( duration(secs,frac.fraction), ts-mjd )
}

// Return the [julian day](https://en.wikipedia.org/wiki/Julian_day) of an instant.
public fun jd( i : instant ) : double {
  i.mjd + jd-epoch-shift
}

// Return the [modified julian day](https://en.wikipedia.org/wiki/Julian_day) of an instant.
public fun mjd( i : instant ) : double {
  val (dsecs,dfrac) = i.ts-since-epoch(ts-mjd).value
  val (days,secs) = divmod(dsecs,secs-per-day)
  days.double + ((secs.double + dfrac) / secs-per-day.double)
}


/*----------------------------------------------------------------------------
  Now
----------------------------------------------------------------------------*/

val unix-epoch-shift = 63072000.0   // 1972-01-01 TAI - 1970-01-01 TAI

// Convert a unix time stamp to an instant in time.
// A unix time stamp measure seconds since the Unix epoch (1970-01-01)
// and ignores any leap seconds.
public fun instant-at-unix( timestamp : double ) : instant {
  ts-since-epoch( duration(timestamp), ts-unix )
}

// Convert an instant to a unix time stamp in seconds since the
// unix epoch (1970-01-01) ignoring any leap seconds.
public fun unix( i : instant ) : double {
  i.ts-since-epoch(ts-unix).seconds
}

//val ntp-epoch-shift = 2272060800.0 // 1900-01-01 TAI - 1972-01-01 TAI

public fun instant-at-ntp( timestamp : double ) : instant {
  ts-since-epoch( duration(timestamp), ts-ntp )
}

public fun ntp( i : instant ) : double {
  i.ts-since-epoch(ts-ntp).seconds
}

// The current `:instant` in time.
public fun now() : ndet instant {
  instant-at-unix( unix-now().round-to-prec(now-precision()) ) 
}


// Returns a unix time stamp; this still needs
// to be adjusted to our epoch and taking account of leap seconds.
private extern unix-now() : ndet double {
  js "_unix_now"
}

// The resolution in seconds of the system clock.
extern now-precision() : ndet int {
  js "_now_precision"
}

