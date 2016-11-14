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
import std/time/timestamp
import std/time/duration
import std/time/instant
import std/time/leaps

extern include {
  // cs file "time-inline.cs"
  js file "instant-inline.js"
}

/*----------------------------------------------------------------------------
  Timescale creation
----------------------------------------------------------------------------*/

// Create a new time scale based on SI seconds with a given `name` and `epoch-jd-delta`:\
// `time-scale-epoch == epoch + epoch-jd-delta`.
public fun utc-timescale( name : string, offset : timespan = timespan0, epoch-jd-delta : timespan = timespan0 ) : timescale {
  val epoch-delta = epoch-jd-delta - offset
  fun from-tai(tai:duration) { tai-to-utc(tai) - epoch-delta }
  fun to-tai(t:timestamp)    { utc-to-tai(t + epoch-delta) }
  Timescale(
    name,
    "UTC",
    from-tai,
    to-tai,
    epoch-delta,
    epoch-jd-delta
  )
}

// Create a new time scale based on SI seconds with a given `name` and `epoch-jd-delta`:\
// `time-scale-epoch == epoch + epoch-jd-delta`.
public fun timescale( name : string, 
                      from-tai : duration -> timestamp, 
                      to-tai   : timestamp -> duration, 
                      epoch-jd-delta : timespan = timespan0,
                      unit  : string = "" ) : timescale {
  Timescale(
    name,
    unit,
    from-tai,
    to-tai,
    timespan0,
    epoch-jd-delta
  )
}


// The [GPS](https://en.wikipedia.org/wiki/Global_Positioning_System#Timekeeping) time scale based 
// on SI seconds with a 1980-01-06 GPS epoch.\
// GPS = TAI - 19s.
// Note that internally to the library the `:timestamp`s will maintain any leap second information.
public val ts-gps : timescale = tai-timescale( "GPS", duration(~19), duration(~630720000) )

// The [TT](https://en.wikipedia.org/wiki/Terrestrial_Time) (Terrestrial time) time scale is based on SI seconds with a 2000-01-01 TAI `epoch`.
// It is the continuation of TDT (Terrestrial dynamic time) and ET (Ephemeris time).
// TT is defined as: TT = TAI + 32.184s, as by IAU recommendation IV ([1991](https://www.iers.org/IERS/EN/Science/Recommendations/recommendation4.html?nn=12932))
public val ts-tt : timescale = tai-timescale( "TT", duration(32.184))

// The UTC time scale with a 2000-01-01Z UTC epoch. This uses ['UTC seconds'](https://en.wikipedia.org/wiki/Unix_time) where
// one day consists of 86400 'seconds' but where leap seconds are ignored (as in unix timestamps)
public val ts-utc : timescale = utc-timescale( "UTC" )

// [Unix](https://en.wikipedia.org/wiki/Unix_time) time scale based on UTC seconds but with 1970-01-01 epoch.
public val ts-unix : timescale = utc-timescale( "UNIX", timespan0, timespan(~946684800) )

// [NTP](https://en.wikipedia.org/wiki/Network_Time_Protocol) time scale is equal to the UTC time scale (`ts-utc`) but 
// with a 1900-01-01Z epoch.
public val ts-ntp : timescale = utc-timescale( "NTP", timespan0, timespan(~3155673600) )



/*----------------------------------------------------------------------------
  Julian Date
----------------------------------------------------------------------------*/
val jd-epoch-delta  = fixed(24000005,1)  // relative to the MJD epoch 
val mjd-epoch-delta = fixed(51544)       // relative to our epoch (2000-01-01)

// Create an instant given a [julian day](https://en.wikipedia.org/wiki/Julian_day).
public fun instant-at-jd( jd: double, ts : timescale = ts-utc ) : instant {
  instant-at-jd(fixed(jd),ts)
}

// Create an instant given a [julian day](https://en.wikipedia.org/wiki/Julian_day).
public fun instant-at-jd( jd: fixed, ts : timescale = ts-utc ) : instant {
  instant-at-mjd( jd - jd-epoch-delta, ts )
}

// Create an instant given a [modified julian day](https://en.wikipedia.org/wiki/Julian_day).\
// modified-julian-date = julian-date - 2400000.5
public fun instant-at-mjd( mjd : double, ts : timescale = ts-utc ) : instant {
  instant-at-mjd(fixed(mjd),ts)
}

// Create an instant given a [modified julian day](https://en.wikipedia.org/wiki/Julian_day).\
// modified-julian-date = julian-date - 2400000.5
public fun instant-at-mjd( mjd : fixed, ts : timescale = ts-utc ) : instant {
  val d    = mjd - mjd-epoch-delta // since our epoch
  val t    = timestamp(d*secs-per-day.fixed) - ts.epoch-jd-delta // since the time scale epoch
  unsafe-instant-at(t,ts) 
}


public fun jd( i : instant, ts : timescale = ts-utc, max-prec : int = 9 ) : fixed {
  i.mjd(ts,max-prec) + jd-epoch-delta
}

public fun mjd( i : instant, ts : timescale = ts-utc, max-prec : int = 9 ) : fixed {
  val t = i.timestamp(ts) + ts.epoch-jd-delta
  (div)(t.timespan.seconds, secs-per-day.fixed, max-prec) + mjd-epoch-delta
}

/*----------------------------------------------------------------------------
  TCG: Geocentric coordinate time
----------------------------------------------------------------------------*/

val lg : fixed = fixed(6969290134,19)// 6.969290134e-10  // 1991: 6.969291e-10  // now: 6.969290134
val tcg-epoch-delta = timestamp(fixed(~725759967816,3)) // 1977-01-01T00:00:32.184 TT, 2443144.5003725 JD~TT~

// IAU recommendation IV ([1991](https://www.iers.org/IERS/EN/Science/Recommendations/recommendation4.html?nn=12932)):\
// TCG - TT = L~g~&times;(JD~TT~ - TCG~epoch~)
// =>
// TCG = TT + Lg*(TT - TCG~epoch~)
fun tcg-from-tai( tai : duration ) : timestamp {
  val tt = ts-tt.from-tai(tai)
  tt + (lg*(tt - tcg-epoch-delta).seconds).timespan
}

// IAU recommendation IV ([1991](https://www.iers.org/IERS/EN/Science/Recommendations/recommendation4.html?nn=12932)):\
// TCG - TT = L~g~&times;(JD~TT~ - TCG~epoch~)
// =>
// TT = TCG - Lg*(TCG - TCG~epoch~)
fun tcg-to-tai( t : timestamp ) : duration {
  val tt = t - (lg*(t - tcg-epoch-delta).seconds).timespan
  ts-tt.to-tai(tt)
}

// The [TCG](https://en.wikipedia.org/wiki/Geocentric_Coordinate_Time) (Geocentric coordinate time) time scale.
// The unit of TCG is the SI second in a coordinate reference frame at the center of the Earth. It is in a linear
// relationship with TT (`ts-tt`) where TCG runs faster than TT at a constant rate. TCG and TT were equal on 1977-01-01Z.
// Computed from TT using IAU recommendation IV ([1991](https://www.iers.org/IERS/EN/Science/Recommendations/recommendation4.html?nn=12932))
// (and updated to currently known [precision](https://en.wikipedia.org/wiki/Terrestrial_Time)):\
// TCG - TT = L~g~&times;(JD~TCG~ - 2443144.5003725)&times;86400, with L~g~ = 6.969290134&times;10^-10^.
public val ts-tcg : timescale = timescale( "TCG", tcg-from-tai, tcg-to-tai )


/*----------------------------------------------------------------------------
  TDB: Barycentric dynamical time
----------------------------------------------------------------------------*/
val j2k-epoch = timestamp(43200)       // 2000-01-01T12:00:00 TT
val secs-per-julian-year = 31557600.0  // 365.25 * 86400.0

// Compute tdb-delta(TT) = TDB - TT
// Fairhead & Bretagnon (1990) initial coefficents.
// See also USNO circular 179 ([2005](http://aa.usno.navy.mil/publications/docs/Circular_179.pdf), section 2.6).
// The error in this approximation is a maximum of 10 micro-seconds in the time period 1600 to 2200.
fun tdb-delta( tt : timestamp ) : timespan {
  val t = (tt - j2k-epoch).seconds.double / (secs-per-julian-year * 1000.0) 
  // First terms of Fairhead et al. model 
  val wf =  1656.6746e-6*sin( 6283.0758*t + 6.2401)
          +   22.4175e-6*sin( 5753.3859*t + 4.2970)
          +   13.8398e-6*sin(12566.1527*t + 6.1969)
          +    4.7701e-6*sin(  529.6910*t + 0.4444)
          +    4.6767e-6*sin( 6069.7768*t + 4.0212)
          +    2.2567e-6*sin(  213.2991*t + 5.5431)
          +    1.6942e-6*sin(   ~3.5232*t + 5.0251)
          +    1.5549e-6*sin(77713.7715*t + 5.1985)
          +    1.2768e-6*sin( 7860.4194*t + 5.9888)
          +    1.1934e-6*sin( 5223.6939*t + 3.6498)            
          + t*(102.1567e-6*sin( 6283.0758*t + 4.2490))
           
  // Adjustments to use JPL planetary masses instead of IAU.
  val wj =  0.00065e-6*sin(6069.776754*t + 4.021194) +
            0.00033e-6*sin( 213.299095*t + 5.543132) +
          (~0.00196e-6*sin(6208.294251*t + 5.696701)) +
          (~0.00173e-6*sin(  74.781599*t + 2.435900)) +
            0.03638e-6*t*t;
  timespan( (wf + wj).fixed ) // + tdb-topocentric(t,0.5,~155.933222,10000.0,5000.0))            
}

val pi2   = 6.283185307179586476925287      // 2pi
val dd2r  = 1.745329251994329576923691e-2   // degree to radians

// From the ERFA routines
fun tdb-topocentric(  t : double,        // fractional julian millennia
                      ut : double = 0.0, // UT1 (fraction of one day)
                      elong : double = 0.0, // east positive degrees 
                      u : double = 0.0, // distance earth spin axis in km
                      v : double = 0.0  // distance north of eq. plane in km 
                      ) {
  // Convert UT to local solar time in radians. 
  val tsol = ut.fraction*pi2 + elong*dd2r

  // Combine time argument (millennia) with deg/arcsec factor. 
  val w = t / 3600.0

  // Sun Mean Longitude. 
  val elsun = ((280.46645683 + 1296027711.03429 * w) % 360.0) * dd2r

  // Sun Mean Anomaly. 
  val emsun = ((357.52910918 + 1295965810.481 * w) % 360.0) * dd2r

  // Mean Elongation of Moon from Sun. 
  val d = ((297.85019547 + 16029616012.090 * w) % 360.0) * dd2r

  // Mean Longitude of Jupiter. 
  val elj = ((34.35151874 + 109306899.89453 * w) % 360.0) * dd2r

  // Mean Longitude of Saturn.
  val els = ((50.07744430 + 44046398.47038 * w) % 360.0) * dd2r

  // Moyer 1981 and Murray 1983. 
  val wt =   0.00029e-10 * u * sin(tsol + elsun - els)
          +  0.00100e-10 * u * sin(tsol - 2.0 * emsun)
          +  0.00133e-10 * u * sin(tsol - d)
          +  0.00133e-10 * u * sin(tsol + (elsun - elj))
          -  0.00229e-10 * u * sin(tsol + (2.0 * elsun) + emsun)
          -  0.02200e-10 * v * cos(elsun + emsun)
          +  0.05312e-10 * u * sin(tsol - emsun)
          -  0.13677e-10 * u * sin(tsol + 2.0 * elsun)
          -  1.31840e-10 * v * cos(elsun)
          +  3.17679e-10 * u * sin(tsol);
  wt          
}

fun tai-to-tdb( tai : duration ) : timestamp {
  val tt = ts-tt.from-tai(tai)
  tt + tdb-delta(tt)
}

fun tdb-to-tai( tdb : timestamp ) : duration {
  ts-tt.to-tai( tdb - tdb-delta(tdb) )
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
public val ts-tdb : timescale = timescale( "TDB", tai-to-tdb, tdb-to-tai )


/*----------------------------------------------------------------------------
  TCB: Barycentric coordinate time
----------------------------------------------------------------------------*/

val lb   : fixed = fixed(155051976772,19) // 1.55051976772e-8
val tdb0 : fixed = fixed(655,7) // ~6.55e-5

fun tai-to-tcb( tai : duration ) : timestamp {
  val tdb = ts-tdb.from-tai(tai)
  val adj = lb*(tdb - tcg-epoch-delta).seconds
  tdb + (adj - tdb0).timespan
}

fun tcb-to-tai( tcb : timestamp ) : duration {
  val adj = lb*(tcb - tcg-epoch-delta).seconds
  val tdb = tcb - (adj - tdb0).timespan 
  ts-tdb.to-tai(tdb)
}

// The [TCB](https://en.wikipedia.org/wiki/Geocentric_Coordinate_Time) (Barycentric coordinate time) time scale.\
// The unit of TCB is the SI second, but in a coordinate reference frame at the barycenter of the solar system.
// TCB and TCG (`tt-tcg`) are related by a coordinate transformation taking into account relativistic effects (see 
// IAU 2000 [Resolution B1.5](http://syrte.obspm.fr/IAU_resolutions/Resol-UAIEng.ps)).
// Instead of transforming TCG, we compute TCB from (the approximated) TDB (`ts-tdb`) using the IAU 2006 [Resolution B3](http://www.iau.org/static/resolutions/IAU2006_Resol3.pdf):\
// TDB = TCB - L~b~ &times; (JD~TCB~ - T~0~) &times; 86400 + TDB~0~,
// with T~0~ = 2443144.5003725, L~b~ = 1.55051976772&times;10^-8^, TDB~0~ = -6.55&times;10^-5^. 
public val ts-tcb : timescale = timescale( "TCB", tai-to-tcb, tcb-to-tai )



/*----------------------------------------------------------------------------
  Unix
----------------------------------------------------------------------------*/

// The current `:instant` in time.
public fun now() : ndet instant {
  unsafe-instant-at( timestamp(unix-now()), ts-unix ) 
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
