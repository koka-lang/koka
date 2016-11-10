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
import std/time/instant
import std/time/leaps

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

// The [TAI](https://en.wikipedia.org/wiki/International_Atomic_Time) (International atomic time) 
// time scale is based on SI seconds with a 1900-01-01 epoch.
public val ts-tai : timescale = tai-timescale( "TAI" )
 
// The [GPS](https://en.wikipedia.org/wiki/Global_Positioning_System#Timekeeping) time scale based 
// on SI seconds with a 1980-01-06 epoch.\
// GPS = TAI - 19s.
public val ts-gps : timescale = tai-timescale( "GPS", fixed(19), fixed(2524953600) )

// The [TT](https://en.wikipedia.org/wiki/Terrestrial_Time) (Terrestrial time) time scale is based on SI seconds with a 1900-01-01 epoch.
// It is the continuation of TDT (Terrestrial dynamic time) and ET (Ephemeris time).
// TT is defined as: TT = TAI + 32.184s, as by IAU recommendation IV ([1991](https://www.iers.org/IERS/EN/Science/Recommendations/recommendation4.html?nn=12932))
public val ts-tt : timescale = tai-timescale( "TT", fixed(~32184,3), zero )

// The UTC time scale with a 1900-01-01 epoch. This uses ['UTC seconds'](https://en.wikipedia.org/wiki/Unix_time) where
// one day consists of 86400 'seconds' but where leap seconds are ignored (as in unix timestamps)
// Note that internally to the library the `:timestamp`s will maintain any leap second information.
public val ts-utc : timescale = utc-timescale( "" )

// [Unix](https://en.wikipedia.org/wiki/Unix_time) time scale based on UTC seconds but with 1970-01-01 epoch.
public val ts-unix : timescale = utc-timescale( "UNIX", zero, fixed(2208988800) )

// [NTP](https://en.wikipedia.org/wiki/Network_Time_Protocol) time scale is equal to the UTC time scale (`ts-utc`) but named `"NTP"`.
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

// The [TCG](https://en.wikipedia.org/wiki/Geocentric_Coordinate_Time) (Geocentric coordinate time) time scale.
// The unit of TCG is the SI second in a coordinate reference frame at the center of the Earth. It is in a linear
// relationship with TT (`ts-tt`) where TCG runs faster than TT at a constant rate. TCG and TT were equal on 1977-01-01Z.
// Computed from TT using IAU recommendation IV ([1991](https://www.iers.org/IERS/EN/Science/Recommendations/recommendation4.html?nn=12932)):\
// TCG - TT = L~g~&times;(JD~TT~ - 2443144.5)&times;86400, with L~g~ = 6.969291&times;10^-10^.
public val ts-tcg : timescale = Timescale( "TCG", tcg-from-tai, tcg-to-tai )


/*----------------------------------------------------------------------------
  TDB: Barycentric dynamical time
----------------------------------------------------------------------------*/
val j2k-epoch = fixed(3155716800)
val secs-per-julian-year = 31557600.0

// Fairhead & Bretagnon (1990) initial coefficents.
// See also USNO circular 179 ([2005](http://aa.usno.navy.mil/publications/docs/Circular_179.pdf), section 2.6).
// The error in this approximation is a maximum of 10 micro-seconds in the time period 1600 to 2200.
fun tdb-shift( tt : fixed ) : fixed {
  val t = (tt - j2k-epoch).double / secs-per-julian-year  
  fixed( 1.0e-6 * (   1656.6746*sin( 628.3076*t + 6.2401)
                    +   22.4175*sin( 575.3385*t + 4.2970)
                    +   13.8398*sin(1256.6152*t + 6.1969)
                    +    4.7701*sin( 606.9777*t + 4.0212)
                    +    4.6767*sin(  52.9691*t + 0.4444)
                    +    2.2567*sin(  21.3299*t + 5.5431)
                    +    1.6942*sin(  ~0.3523*t + 5.0251)
                    +    1.5549*sin(7771.3773*t + 5.1985)
                    +    1.2768*sin( 768.0419*t + 5.9889)
                    +    1.1934*sin( 522.3694*t + 2.6498)
                    + ( (  10.2157*sin( 628.3076*t + 4.2490)
                         +  0.1707*sin(1256.1517*t + 4.2059)
                        ) * t)
                  )
       ,9 /* nano-second precision */)  
}

fun tai-to-tdb( i : instant ) : timestamp {
  val tt = i.timestamp(ts-tt)
  tt + tdb-shift(tt.seconds)
}

fun tdb-to-tai( tdb : timestamp ) : instant {
  instant-at( tdb - tdb-shift(tdb.seconds), ts-tt )
}

// The [TDB](https://en.wikipedia.org/wiki/Barycentric_Coordinate_Time) (Barycentric dynamical time) time scale
// (which can be considered equivalent T~eph~).
// Runs slower than TCB (`ts-tcb`) at a constant rate in order to remain approximately equal to TT (`ts-tt`).
// These relativistic corrections ensure a difference to TT of less then 2 milli-seconds and are
// periodic with an average of zero.\
// TDB is approximated from TT using an initial set of coefficients by
// Fairhead &amp; Bretagnon (1990) assuming the geo center (longitude and latitude of 0 degrees).
// See also USNO circular 179 ([2005](http://aa.usno.navy.mil/publications/docs/Circular_179.pdf), equation 2.6).
// The approximation is within 10&mu;s precision in the time span 1600 to 2200.
// For other locations than the geo-center, the approximation is within 200&mu;s.
public val ts-tdb : timescale = Timescale( "TDB", tai-to-tdb, tdb-to-tai )


/*----------------------------------------------------------------------------
  TCB: Barycentric coordinate time
----------------------------------------------------------------------------*/

val lb : fixed = fixed(155051976772,19)
val tdb0 : fixed = fixed(~655,7)

fun tai-to-tcb( i : instant ) : timestamp {
  val tdb = i.timestamp(ts-tdb)
  tdb + lb*(tdb.seconds - tcg-epoch) - tdb0   
}

fun tcb-to-tai( tcb : timestamp ) : instant {
  val tdb = tcb - lb*(tcb.seconds - tcg-epoch) + tdb0  
  instant-at(tdb,ts-tdb)
}

// The [TCB](https://en.wikipedia.org/wiki/Geocentric_Coordinate_Time) (Barycentric coordinate time) time scale.\
// The unit of TCB is the SI second, but in a coordinate reference frame at the barycenter of the solar system.
// TCB and TCG (`tt-tcg`) are related by a coordinate transformation taking into account relativistic effects (see 
// IAU 2000 [Resolution B1.5](http://syrte.obspm.fr/IAU_resolutions/Resol-UAIEng.ps)).
// Instead of transforming TCG, we compute TCB from (the approximated) TDB (`ts-tdb`) using the IAU 2006 [Resolution B3](http://www.iau.org/static/resolutions/IAU2006_Resol3.pdf):\
// TDB = TCB - L~b~ &times; (JD~TCB~ - T~0~) &times; 86400 + TDB~0~,
// with T~0~ = 2443144.5003725, L~b~ = 1.55051976772&times;10^-8^, TDB~0~ = -6.55&times;10^-5^. 
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
