/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Time scales for astronomy (TT, TCG, TDB, and TCB).

Internally all computations use high precision `:ddouble` numbers. 
The different time scales relate as follows:

* **TAI**, `ts-tai`, `cal-tai`: International Atomic Time. The atomic time standard
  measuring SI seconds on the Earth's geoid.
  
* **GPS**, `ts-gps`, `cal-gps`: Global Positioning System time. Runs at the same rate as
  TAI but with a 19 second offset: GPS = TAI - 19s.
  
* **UT1**, [`cal-ut1-load`](std_time_ut1.html): Universal Time: A time scale based
  on the rotation of the earth where one rotation is 86400 (solar) seconds. 
  Defined to be a linear function of the rotation angle of the Earth. 
  See the [``std/time/ut1``](std_time_ut1.html) module for more information.
  
* **UTC**, `ts-utc-create`, `cal-utc-load`: 
  Coordinated Universal Time: A hybrid of UT1 and TAI: uses
  SI seconds as its unit, but inserts (or deletes) periodically leap
  seconds in order to stay within 0.9s of UT1. See the [`std/time/utc`](std_time_utc.html)
  module for more information.

* **TI**, `ts-ti`, `cal-iso`: International Time: this was a
  [proposed][TIpropose] time scale at the 2004 ITU-R meeting as a replacement of UTC
  without future leap seconds. In this library, we define TI to match
  exactly UTC up to the compiler release date (currently 2017) but ignore any
  possible future leap seconds after that date. This is the preferred time scale
  in this library as it guarantees deterministic time calculations for any
  future date, i.e. before 2017-01-01Z, TI == UTC, while after that, TI == TAI - 37s.
  
* **[TT]**, `ts-tt`, `cal-tt`: Terrestrial time. This is an ideal time scale defined to
  run slower at a constant rate from TCG such that TT runs at the same rate as a
  time scale based on SI seconds on the Earth's geoid. Since TCG
  is a theoretical time scale, TT can be considered an idealized form
  of TAI and is currently realized by defining:\
  TT = TAI + 32.184s.\
  Terrestrial time replaces (and preserves continuity with) the older Terrestrial
  Dynamic Time (TDT) and Ephemeris Time (ET).
  
* **TCG**, `ts-tcg`, `cal-tcg`: Geocentric Coordinate Time. Theoretical clock using
  SI seconds in a coordinate reference frame at the center of the Earth.
  It is in a linear relationship with TT where TCG runs faster
  than TT at a constant rate. By definition, TCG and TT were equal on 1977-01-01Z.
  Computed from TT using IAU 1991 recommendation [IV][IAU-IV]
   (and updated to currently known [precision][IERS-2010])):\
  TCG = TT + L~g~&times;(JD~TCG~ - 2443144.5003725)&times;86400, with L~g~ = 6.969290134&times;10^-10^.

* **TCB**, `ts-tcb`, `cal-tcb`: Barycentric Coordinate Time. Theoretical clock using SI
   seconds in a coordinate reference frame at the gravitational center of the solar
   system. TCB and TCG are related by a coordinate transformation taking
   into account relativistic effects (see IAU 2000 resolution
   [B1.5](http://syrte.obspm.fr/IAU_resolutions/Resol-UAIEng.ps)). In the
   library, instead of transforming TCG, we compute TCB from (the
   approximated) TDB (`ts-tdb`) using the IAU 2006 resolution [B3][IAU-B3]:\
   TDB = TCB - L~b~ &times; (JD~TCB~ - T~0~) &times; 86400 + TDB~0~,
   with T~0~ = 2443144.5003725, L~b~ = 1.55051976772&times;10^-8^, TDB~0~
   = -6.55&times;10^-5^.

* **TDB**, `ts-tdb`, `cal-tdb`: Barycentric Dynamical Time. Defined to run slower at a
   constant rate from TCB in order to be approximately equal to TT These
   relativistic corrections ensure a difference to TT of less then 2
   milli-seconds and are periodic with an average of zero. In the
   library, TDB is approximated from TT using an initial set of
   coefficients by Fairhead &amp; Bretagnon [@Fairhead:tt] assuming the geo
   center (longitude and latitude of 0 degrees), and corrected for JPL
   planetary masses. See also USNO circular 179 [@Kaplan:circ],
   equation 2.6). The approximation is within 10&mu;s precision in the
   time span 1600 to 2200 (and probably a bit less).

Note that theoretically the definition of TT is based on TCG which is
based on TCB, but in practice we define TT on the basis of an actual clock (TAI) 
and derive TCG from TT. Moreover, TDB is approximated from TT, and TCB 
is derived from the approximated TDB. 

Since TDB (and in turn TCB) are approximated, it is possible to create
your own time scale which uses a potentially more precise approximation using
`ts-tdb-create` and `ts-tcb-create`.

~ Figure { #fig-timescales; caption:"Relation between different time scales relative to TAI between 1950 and 2000. \
    The periodic terms of TCB and TDB are magnified by 100 to make them visible. \
    Figure from Seidelmann and Fukushima [@Seidelmann:why]." }
![timescales]
~

[timescales]: timescales.png { width:70%; }


## References {-}

~ Bibliography { caption:"0" }

~~ BibItem { #Fairhead:tt; bibitem-label:"[1]"; searchterm:"Fairhead+Bretagnon+An+analytical+formula+for+the+time+transformation" }
L.\ Fairhead and P.\ Bretagnon.
_An analytical formula for the time transformation TB-TT_.
Astronomy and Astrophysics, vol. 229, no. 1, March 1990, p. 240--247.
[ADS](http://articles.adsabs.harvard.edu/cgi-bin/nph-iarticle_query?1990A%26A...229..240F)
~~

~~ BibItem { #Kaplan:circ; bibitem-label:"[2]"; searchterm:"Kaplan+The+IAU+Resolutions+on+Astronomical+Reference+Systems" }
George H.\ Kaplan.
_The IAU Resolutions on Astronomical Reference Systems,
Time Scales, and Earth Rotation Models -- Explanation and Implementation_.
USNO Circular [179](http://aa.usno.navy.mil/publications/docs/Circular_179.pdf)
~~

~~ BibItem { #Seidelmann:why; bibitem-label:"[3]"; searchterm:"Seidelmann+Fukushima+Why+new+time+scales" }
P.K.\ Seidelmann and T.\ Fukushima.
_Why new time scales?_ 
Astronomy and Astrophysics, vol. 265, no. 2, p. 833--838.
[ADS](http://adsabs.harvard.edu/full/1992A%26A...265..833S)
~~

~

[IAU-B3]: http://www.iau.org/static/resolutions/IAU2006_Resol3.pdf
[IAU-IV]: https://www.iers.org/IERS/EN/Science/Recommendations/recommendation4.html?nn=12932
[IERS-2010]: https://www.iers.org/SharedDocs/Publikationen/EN/IERS/Publications/tn/TechnNote36/tn36.pdf?__blob=publicationFile&v=1
[TIpropose]: https://syrte.obspm.fr/journees2004/pdf/McCarthy2.pdf#page=4
[Java time]: https://docs.oracle.com/javase/8/docs/api/java/time/Instant.html
[UTC]: https://en.wikipedia.org/wiki/Coordinated_Universal_Time
[TAI]: https://en.wikipedia.org/wiki/International_Atomic_Time
[UT1]: https://en.wikipedia.org/wiki/Universal_Time
[TT]: https://en.wikipedia.org/wiki/Terrestrial_Time
\/

*/
module std/time/astro

import std/num/double
import std/num/ddouble
import std/time/timestamp
import std/time/duration
import std/time/instant
import std/time/calendar

// for documentation
import std/time/utc        // ts-ti
import std/time/calendars  // cal-tai, cal-gps
import std/time/download   // cal-utc-load

// The Terrestrial (or Ephemeris) time calendar, i.e. an ISO calendar but using the `ts-tt` time scale.
// TT is always `TAI + 32.184`
public val cal-tt  = iso-calendar( ts-tt )//, fixed(32184,3) )

// The TCG (Geometric coordinate time) calendar, i.e. an ISO calendar but using the `ts-tcg` time scale.
public val cal-tcg  = iso-calendar( ts-tcg )//, fixed(32184,3) )

// The TDB (Barycentric dynamical time) calendar, i.e. an ISO calendar but using the `ts-tdb` time scale. Always within 2ms of TT (`cal-tt`).
public val cal-tdb  = iso-calendar( ts-tdb )//, fixed(32184,3) )

// The TCB (Barycentric coordinate time) calendar, i.e. an ISO calendar but using the `ts-tcb` time scale.
public val cal-tcb  = iso-calendar( ts-tcb )//, fixed(32184,3) )


// The [TT](https://en.wikipedia.org/wiki/Terrestrial_Time) (Terrestrial time)
// time scale is based on SI seconds with a 2000-01-01 TAI `epoch`. It is the
// continuation of TDT (Terrestrial dynamic time) and ET (Ephemeris time). TT
// is defined as: TT = TAI + 32.184s.
public val ts-tt : timescale = tai-timescale( "TT", duration(32.184))


val secs-per-day    = ddouble(86400)
val offset-j2000    = ddouble(43200)

// The [J2000] epoch is defined as 2000-01-01T12:00:00 TT (Terrestrial time).
//
// [J2000]: https://en.wikipedia.org/wiki/Equinox_(celestial_coordinates)#J2000.0
public val epoch-j2000 = instant-at-j2000(zero)

// Return the J2000 julian date for an instant. This is the number of days, and
// the fraction of the day since J2000 (`epoch-j2000`) in terrestrial time (`ts-tt`).\
// `instant(2000,1,1,12,0,0).j2000.show == "0.00074287037037"`\
// `instant(2000,1,1,12,0,0,cal=cal-tt).j2000.show == "0"`
public fun j2000( i : instant ) : ddouble {
  div( (i.timespan(ts-tt) - offset-j2000), secs-per-day, 18 )  // atto-second precision
}

// Return the instant corresponding to a J2000 julian date: this is the number of days, and
// the fraction of the day since J2000 (`epoch-j2000`) in terrestrial time (`ts-tt`).\
// `instant-at-j2000(zero).time.show == "2000-01-01T11:58:55.816Z"`\
// `instant-at-j2000(zero).time(cal=cal-tt).show == "2000-01-01T12:00:00Z TT"`
public fun instant-at-j2000( jd : ddouble ) : instant {
  unsafe-instant( timestamp( (jd * secs-per-day) + offset-j2000 ), ts-tt )
}


/*----------------------------------------------------------------------------
  TCG: Geocentric coordinate time
----------------------------------------------------------------------------*/

val lg : ddouble = ddouble-exp(6969290134,~19) // 6.969290134e-10  // 1991: 6.969291e-10  // now: 6.969290134
val tcg-epoch-delta = ddouble-exp(~725759967816,~3) // 1977-01-01T00:00:32.184 TT, 2443144.5003725 JD~TT~

// IAU 1991 recommendation IV :\
// TCG - TT = L~g~&times;(JD~TT~ - TCG~epoch~)
// =>
// TCG = TT + Lg*(TT - TCG~epoch~)
fun tcg-from-tai( tai : duration ) : timestamp {
  val tt = ts-tt.from-tai(tai)
  tt + (lg*(tt - tcg-epoch-delta).timespan)
}

// IAU 1991 recommendation [IV](https://www.iers.org/IERS/EN/Science/Recommendations/recommendation4.html?nn=12932):\
// TCG - TT = L~g~&times;(JD~TT~ - TCG~epoch~)
// =>
// TT = TCG - Lg*(TCG - TCG~epoch~)
fun tcg-to-tai( t : timestamp ) : duration {
  val tt = t - (lg*(t - tcg-epoch-delta).timespan)
  ts-tt.to-tai(tt)
}

// The [TCG](https://en.wikipedia.org/wiki/Geocentric_Coordinate_Time) (Geocentric coordinate time) time scale.
// The unit of TCG is the SI second in a coordinate reference frame at the center of the Earth. 
public val ts-tcg : timescale = timescale( "TCG", tcg-from-tai, tcg-to-tai )


/*----------------------------------------------------------------------------
  TDB: Barycentric dynamical time
----------------------------------------------------------------------------*/
val j2k-epoch = ddouble(43200)       // 2000-01-01T12:00:00 TT
val secs-per-julian-year = 31557600.0  // 365.25 * 86400.0

// Compute tdb-delta(TT) = TDB - TT. This is the default function used for `ts-tdb`.
// Fairhead & Bretagnon (1990) initial coefficents.
// See also USNO circular 179 ([@Kaplan:circ], section 2.6).
// The error in this approximation is a maximum of 10 micro-seconds in the time period 1600 to 2200.
// (and probably less as we expanded to more terms and do JPL planetary mass adjustment)
fun tdb-delta( tt : timestamp ) : timespan {
  val t = (tt - j2k-epoch).seconds.double / (secs-per-julian-year * 1000.0) // fraction of Julian millenia since J2000

  // First terms of Fairhead et al. model 
  val wf : double
         =(1656.6746e-6*sin( 6283.0758*t + 6.2401)
          + ( 22.4175e-6*sin( 5753.3859*t + 4.2970)
          +   13.8398e-6*sin(12566.1527*t + 6.1969)
          +    4.7701e-6*sin(  529.6910*t + 0.4444)
          +    4.6767e-6*sin( 6069.7768*t + 4.0212))
          )
          +
          (    2.2567e-6*sin(  213.2991*t + 5.5431)
          +    1.6942e-6*sin(   ~3.5232*t + 5.0251)
          +    1.5549e-6*sin(77713.7715*t + 5.1985)
          +    1.2768e-6*sin( 7860.4194*t + 5.9888)
          +    1.1934e-6*sin( 5223.6939*t + 3.6498)
          )            
          + t*(102.1567e-6*sin( 6283.0758*t + 4.2490))
           
  // Adjustments to use JPL planetary masses instead of IAU.
  val wj : double
         =  0.00065e-6*sin(6069.776754*t + 4.021194) +
            0.00033e-6*sin( 213.299095*t + 5.543132) +
          (~0.00196e-6*sin(6208.294251*t + 5.696701)) +
          (~0.00173e-6*sin(  74.781599*t + 2.435900)) +
            0.03638e-6*t*t;

  // Topo centric adjustments
  val wt = 0.0 // tdb-topocentric(t,0.5,~155.933222,10000.0,5000.0)
  
  ddouble( wf+wj+wt )
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
  val wt : double 
         =(  0.00029e-10 * u * sin(tsol + elsun - els)
          + (0.00100e-10 * u * sin(tsol - 2.0 * emsun)
          +  0.00133e-10 * u * sin(tsol - d)
          +  0.00133e-10 * u * sin(tsol + (elsun - elj))
          -  0.00229e-10 * u * sin(tsol + (2.0 * elsun) + emsun)
          -  0.02200e-10 * v * cos(elsun + emsun))
          )
          +  
          (  0.05312e-10 * u * sin(tsol - emsun)
          -  0.13677e-10 * u * sin(tsol + 2.0 * elsun)
          -  1.31840e-10 * v * cos(elsun)
          +  3.17679e-10 * u * sin(tsol)
          );
  wt          
}

fun tai-to-tdb( tai : duration, delta : timestamp -> timespan = tdb-delta ) : timestamp {
  val tt = ts-tt.from-tai(tai)
  tt + delta(tt)
}

fun tdb-to-tai( tdb : timestamp, delta : timestamp -> timespan = tdb-delta ) : duration {
  ts-tt.to-tai( tdb - delta(tdb) )
}

// The [TDB](https://en.wikipedia.org/wiki/Barycentric_Coordinate_Time) (Barycentric dynamical time) time scale
// (which can be considered equivalent T~eph~).
//
// TDB is approximated from TT using an initial set of coefficients by
// Fairhead &amp; Bretagnon [@Fairhead:tt] assuming the geo center (longitude and latitude of 0 degrees).
// See also USNO circular 179 ([@Kaplan:circ], equation 2.6).
// The approximation is within 10&mu;s precision in the time span 1600 to 2200 (and probably a bit less).
public val ts-tdb : timescale = ts-tdb-create(tdb-delta)

// Creates a new TDB timescale given a user provided function to calculate TDB-TT.
// The default function used for `ts-tdb` uses the Fairhead &amp; Bretagnon approximation
// corrected for JPL planetary masses but assumes the geo center. Use this function 
// to create a new TDB timescale if you need more precision.
public fun ts-tdb-create( tdb-minus-tt : timestamp -> timespan ) : timescale {
  timescale("TDB", 
    fun(tai:duration)  { tai-to-tdb(tai,tdb-minus-tt) },
    fun(tdb:timestamp) { tdb-to-tai(tdb,tdb-minus-tt) }
  )
}


/*----------------------------------------------------------------------------
  TCB: Barycentric coordinate time
----------------------------------------------------------------------------*/

val lb   : ddouble = ddouble-exp(155051976772,~19) // 1.55051976772e-8
val tdb0 : ddouble = ddouble-exp(~655,~7) // ~6.55e-5

fun tai-to-tcb( tai : duration, tstdb : timescale ) : timestamp {
  val tdb = tstdb.from-tai(tai)
  val adj = lb*(tdb - tcg-epoch-delta).timespan
  tdb + (adj - tdb0)
}

fun tcb-to-tai( tcb : timestamp, tstdb : timescale ) : duration {
  val adj = lb*(tcb - tcg-epoch-delta).timespan
  val tdb = tcb - (adj - tdb0)
  tstdb.to-tai(tdb)
}

// The [TCB](https://en.wikipedia.org/wiki/Geocentric_Coordinate_Time)
// (Barycentric coordinate time) time scale.\ The unit of TCB is the SI
// second, but in a coordinate reference frame at the barycenter of the solar
// system. 
public val ts-tcb : timescale = ts-tcb-create(ts-tdb)

// Create a new TCB timescale given a TDB timescale (`tstdb`). The default
// `ts-tcb` uses the default `ts-tdb` timescale, but if you created
// a different TCB time scale (using `ts-tdb-create`) you can use
// this function to create a TCB time scale from it. It computes
// TCB from the provided TDB using the IAU 2006 resolution 
// [B3](http://www.iau.org/static/resolutions/IAU2006_Resol3.pdf):\ TDB = TCB -
// L~b~ &times; (JD~TCB~ - T~0~) &times; 86400 + TDB~0~, with T~0~ =
// 2443144.5003725, L~b~ = 1.55051976772&times;10^-8^, TDB~0~ =
// -6.55&times;10^-5^.
public fun ts-tcb-create( tstdb : timescale ) : timescale {
  timescale( "TCB", 
    fun(tai:duration)  { tai-to-tcb(tai, tstdb) },
    fun(tcb:timestamp) { tcb-to-tai(tcb, tstdb) }
  )
}

