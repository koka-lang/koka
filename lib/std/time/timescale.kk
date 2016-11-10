/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Time scales


*/
module std/time/timescale

import std/fixed
import std/time/xinstant
import std/time/xleaps

extern include {
  // cs file "time-inline.cs"
  js file "instant-inline.js"
}


// A `:time-scale` represents a duration relative to a certain epoch,
// and can convert to- and from TAI seconds.
public struct timescale(
  ts-name         : string,
  from-tai        : instant -> timestamp,
  to-tai          : timestamp -> instant,
  has-leap-seconds: bool = False,
  epoch-shift     : fixed = zero
)

// Create a new time scale based on SI seconds with a given `name` and `epoch-shift`
// duration away from the standard `epoch`.
public fun tai-timescale( name : string, offset : fixed = zero, epoch-shift : fixed = zero ) : timescale {
  val total-offset  = epoch-shift + offset
  fun from-tai(i:instant) { timestamp(i.since-epoch.seconds - total-offset) }
  fun to-tai(t:timestamp) { instant-at(duration(t.seconds + total-offset)) }
  Timescale(
    name,
    from-tai,
    to-tai,
    False,
    epoch-shift
  )
}

// Create a new time scale based on SI seconds with a given `name` and `epoch-shift`
// duration away from the standard `epoch`.
public fun utc-timescale( name : string, offset : fixed = zero, epoch-shift : fixed = zero ) : timescale {
  val total-offset  = epoch-shift + offset
  fun from-tai(i:instant) { tai-to-utc(i) - total-offset }
  fun to-tai(t:timestamp) { utc-to-tai(t + total-offset) }
  Timescale(
    name,
    from-tai,
    to-tai,
    False,
    epoch-shift
  )
}

// The [TAI](https://en.wikipedia.org/wiki/International_Atomic_Time) (international atomic time) 
// time scale is based on SI seconds with a 1900-01-01 epoch.
public val ts-tai : timescale = tai-timescale( "TAI" )
 
// The GPS time scale based on SI seconds with a 1980-01-06 epoch.\
// GPS = TAI - 19s.
public val ts-gps : timescale = tai-timescale( "GPS", fixed(19), fixed(2524953600) )

// The Terrestrial (Terrestrial dynamic time) time scale is based on SI seconds with a 1900-01-01 epoch.\
// TT = TAI + 32.184s
public val ts-tt : timescale = tai-timescale( "TT", fixed(~32184,3), zero )

// The UTC time scale with a 1900-01-01 epoch. This uses 'UTC seconds' where
// one day consists of 86400 'seconds' where leap seconds are ignored (as in unix timestamps)
// (but internally the `:timestamp`s will maintain any leap seconds).
public val ts-utc : timescale = utc-timescale( "" )

// Unix time scale based on UTC seconds but with 1970-01-01 epoch.
public val ts-unix : timescale = utc-timescale( "UNIX", zero, fixed(2208988800) )

// NTP time scale is equal to the UTC time scale (`ts-utc`) but named `"NTP"`.
public val ts-ntp : timescale = utc-timescale( "NTP" )


// Convert an instant to a timestamp in a specific timescale.
public fun timestamp( i : instant, ts : timescale ) : timestamp {
  (ts.from-tai)(i)
}

// Create an instant from a timestamp in a specific timescale.
public fun instant-at( t : timestamp, ts : timescale ) : instant {
  (ts.to-tai)(t)
}


/*----------------------------------------------------------------------------
  Julian Date
----------------------------------------------------------------------------*/
public val secs-per-day = fixed(86400)

val jd-epoch-shift  = fixed(24000005,1)  // relative to the MJD epoch 
val mjd-epoch-shift = fixed(15020)       // relative to our epoch (1900-01-01)

// Create an instant given a [julian day](https://en.wikipedia.org/wiki/Julian_day).
public fun instant-at-jd( jd: double, ts : timescale = ts-utc ) : instant {
  instant-at-jd(fixed(jd),ts)
}

// Create an instant given a [julian day](https://en.wikipedia.org/wiki/Julian_day).
public fun instant-at-jd( jd: fixed, ts : timescale = ts-utc ) : instant {
  instant-at-mjd( jd - jd-epoch-shift, ts )
}

// Create an instant given a [modified julian day](https://en.wikipedia.org/wiki/Julian_day).\
// modified-julian-date = julian-date - 2400000.5
public fun instant-at-mjd( mjd : double, ts : timescale = ts-utc ) : instant {
  instant-at-mjd(fixed(mjd),ts)
}

// Create an instant given a [modified julian day](https://en.wikipedia.org/wiki/Julian_day).\
// modified-julian-date = julian-date - 2400000.5
public fun instant-at-mjd( mjd : fixed, ts : timescale = ts-utc ) : instant {
  val d    = mjd - mjd-epoch-shift // since 1900-01-01
  val t    = d*secs-per-day - ts.epoch-shift // since the time scale epoch
  instant-at(timestamp(t),ts) 
}


public fun jd( i : instant, ts : timescale = ts-utc, max-prec : int = 9 ) : fixed {
  i.mjd(ts,max-prec) + jd-epoch-shift
}

public fun mjd( i : instant, ts : timescale = ts-utc, max-prec : int = 9 ) : fixed {
  val t = i.timestamp(ts).seconds + ts.epoch-shift
  (/)(t,secs-per-day,max-prec) + mjd-epoch-shift
}


/*----------------------------------------------------------------------------
  TCG: Geocentric coordinate time
----------------------------------------------------------------------------*/

public val lg = fixed(6969291,16)
public val tcg-epoch = fixed(2429913632184,3)

// USNO circular 179, Section 2.6
// dTT/dTCG = (1-Lg)
// =>
// dTT = dTCG*(1 - Lg)
// =>
// dTCG = dTT / (1-Lg)
// ==
// TCG - t0 = (TT - t0) * (1 + Lg)
// ==
// TCG = (TT - t0)*(1 + Lg) + t0
//     = TT + TT*Lg - t0 - t0*Lg + t0
//     = TT + Lg*(TT - t0)
fun tcg-from-tai( i : instant ) : timestamp {
  val tt = i.timestamp(ts-tt).seconds
  timestamp(tt + lg * (tt - tcg-epoch))
}

// USNO circular 179, Section 2.6
// dTT/dTCG = (1-Lg)
// =>
// dTT = dTCG*(1-Lg)
// ==
// TT - t0 = (TCG - t0)*(1-Lg)
// ==
// TT = (TCG - t0)*(1-Lg) + t0
//    = TCG - TCG*Lg - t0 + t0*Lg + t0
//    = TCG - Lg*(TCG - t0)
fun tcg-to-tai( t : timestamp ) : instant {
  instant-at( timestamp(t.seconds - lg*(t.seconds - tcg-epoch)), ts-tt )
}

// The [Geocentric coordinate time](https://en.wikipedia.org/wiki/Geocentric_Coordinate_Time)(TCG).
// This is the coordinate counter part to Terrestrial time (TT, `ts-tt`). TT and TCG where
// equal on 1977-01-01Z and differ in rate by about 0.7 parts per billion.\
// Computed using the formulas in USNO circular 179 ([2005](http://aa.usno.navy.mil/publications/docs/Circular_179.pdf), section 2.6).
public val ts-tcg : timescale = Timescale( "TCG", tcg-from-tai, tcg-to-tai )


/*----------------------------------------------------------------------------
  TDB: Barycentric dynamical time
----------------------------------------------------------------------------*/
val j2k-epoch = fixed(3155716800)
val secs-per-julian-year = 31557600.0

// USNO circular 179 ([2005](http://aa.usno.navy.mil/publications/docs/Circular_179.pdf), section 2.6).
// The error in this approximation is a maximum of 10 micro-seconds in the time period 1600 to 2200.
fun tdb-shift( tt : fixed ) : fixed {
  val t = (tt - j2k-epoch).double / secs-per-julian-year  
  fixed(  0.001657*sin( 628.3076*t + 6.2401)
        + 0.000022*sin( 575.3385*t + 4.2970)
        + 0.000014*sin(1256.6152*t + 6.1969)
        + 0.000005*sin( 606.9777*t + 4.0212)
        + 0.000005*sin(  52.9691*t + 0.4444)
        + 0.000002*sin(  21.3299*t + 5.5431)
        + 0.000010*sin( 628.3076*t + 4.2490) * t
       ,9 /* nano-second precision */)  
}

fun tai-to-tdb( i : instant ) : timestamp {
  val tt = i.timestamp(ts-tt)
  tt + tdb-shift(tt.seconds)
}

fun tdb-to-tai( tdb : timestamp ) : instant {
  instant-at( tdb - tdb-shift(tdb.seconds), ts-tt )
}

// The [Barycentric dynamical time](https://en.wikipedia.org/wiki/Barycentric_Coordinate_Time)(TDB).
// This is the same as Terrestrial time (TT, `ts-tt`) but with relativistic corrections to move the
// origin to the solar system barycenter. These corrections are less then 2 milli-seconds and are
// periodic with an average of zero.\
// Computes TDB using the expansion given in USNO circular 179 ([2005](http://aa.usno.navy.mil/publications/docs/Circular_179.pdf), section 2.6).
// (which is within 10 micro-seconds precision in the time span 1600 to 2200). 
public val ts-tdb : timescale = Timescale( "TDB", tai-to-tdb, tdb-to-tai )


/*----------------------------------------------------------------------------
  TCB: Barycentric coordinate time
----------------------------------------------------------------------------*/

val lb : fixed = fixed(155051976772,19)
val p0 : fixed = fixed(655,7)

fun tai-to-tcb( i : instant ) : timestamp {
  val tdb = i.timestamp(ts-tdb)
  tdb + lb*(tdb.seconds - tcg-epoch) + p0   
}

fun tcb-to-tai( tcb : timestamp ) : instant {
  val tdb = tcb - lb*(tcb.seconds - tcg-epoch) - p0  
  instant-at(tdb,ts-tdb)
}

// The [Barycentric coordinate time](https://en.wikipedia.org/wiki/Geocentric_Coordinate_Time)(TCB).\
// This is the coordinate counterpart to TDB (`tt-tbd`) which differs in rate by about 15 parts per billion.
// On 1977-01-01 they differed by P0 (=`6.55e-7`).
// Derived from the TDB (`ts-tdb`) using the formula of _Denis et al_ ([2004](https://www.iers.org/SharedDocs/Publikationen/EN/IERS/Publications/tn/TechnNote32/tn32_104.pdf?__blob=publicationFile&v=1), Section 10.1):\
// (TCB - TDB) = Lb * (MJD - 413144.0)*86400 + P0
public val ts-tcb : timescale = Timescale( "TCB", tai-to-tcb, tcb-to-tai )



// The current `:instant` in time.
public fun now() : ndet instant {
  instant-at( timestamp(fixed(unix-now())), ts-unix ) 
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
