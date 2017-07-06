/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* UTC time scales and leap second support.

# UTC time calculation 

The world's standard time scale is Universal Coordinated Time ([UTC]). 
UTC is directly based on International Atomic Time ([TAI])
(`ts-tai`) and uses standard SI seconds. The UTC time scale differs from
TAI in that UTC stays within 1 second of [UT1](std_time_ut1.html); the
time scale based on the rotation of the Earth. To keep UTC close to UT1
about every 1.5 year a _leap second_ is added to the day (appearing as
a 60th second in the last minute of the day, e.g. 23:59:60h).

In order to calculate SI second durations between time instants, we need to 
know when a leap second is inserted. For example, there was a leap second
inserted on 2017-01-01Z, so:
```
time(2017,1,1,0,0,0) - time(2016,12,31,23,0,0) 
```
equals 3601 SI seconds to account for the extra leap second. We also need to
know the inserted leap seconds to convert UTC time to TAI which forms the
basis of many other time scales (see [``std/time/astro``](std_time_astro.html)).

The occurrence of leap seconds cannot be reliably predicted though and
the IERS
usually [announces](https://www.iers.org/SharedDocs/News/EN/BulletinC.html)
leap seconds about half a year in advance. The [IETF leap second][ietfl]
table contains a list of all currently announced leap seconds. _As such,
any future duration calculation in UTC is essentially non-deterministic since
it may be off by a number of (as yet unannounced) leap seconds_. This means
in practice:

* You cannot reliably tell how many SI seconds in the future a certain 
  UTC time occurs (since there may be leap seconds inserted in the future).
  If you need to store a future date, say ``2020-01-01T12:00:00Z``, then
  store it as an ISO calendar date (`:std/time/time/time`), not as a time stamp.

* Do not use Unix time stamps as those may be ambigious (see `unix-instant`); 
  this library
  already defaults to time stamps as SI seconds since `epoch` which are
  monotonic and unambigious. If no interaction with the user is needed, 
  consider using TAI time and calendar instead.


## Effect types and future leap seconds

Since UTC time calculations depend on a leap second table (`:leaps-table`),
the UTC time scale can only be created from such table (`ts-utc-create`).
For most precise UTC time, you can use [`ts-utc-load`](std_time_download.html#ts_utc_load)
to automatically download and cache the latest IERS leap second information.
This can be somewhat cumbersome though as this is a function with an `:io` effect.

International Time ([TI]) (or _Temps International_) is a proposed time scale
that matches exactly UTC up to some date but with no further leap seconds
added  after that -- which makes time calculations robust and deterministic
with regard to future dates. For this reason, this library defaults to using
TI instead of UTC. The TI time scale (`ts-ti`) is defined
to exactly match UTC before the compiler release date (currently 2017) but
ignores any future leap seconds.


## UTC between 1961 and 1972

UTC is only well defined after 1972-01-01Z where it got integral leap
seconds. Before 1972, the UTC time was broadcast using a combination of
fixed time steps (i.e. _mini leap seconds_) and an offset from the atomic
clock frequency. By adjusting the frequency, UTC time was effectively
running at a linearly adjusted rate relative to TAI. We call this
adjustment _drift_. For example, between 1963-11-01Z and 1964-01-01Z
the frequency offset was -130&times;10^-10^ which means UTC was running
ahead of TAI by 0.0011232 seconds per day. The drift was then defined as:

&quad;(TAI-UTC) = 1.9458580 + 0.0011232&times;&Delta;(1962-01-01) 

where the function &Delta;(_date_) returns the number of days since
_date_. Therefore, the difference started as 2.6972788s (as there were
669 days between 1962-01-01 and 1963-11-01) and increased linearly up to
2.7657940s on 1964-01-01Z. For dates between 1961-01-01Z and 1972-01-01Z
the library calculates the UTC time based on the historical [USNO][dat]
drift data (see Table [#tab-utc]).

Here are some interesting time steps that occurred in this time frame: 

* There was a negative time step of -0.1s inserted on 1968-02-01Z:
  ```
  instant(1968,1,31,23,59,59,0.9).time(cal=cal-tai) == "1968-02-01T00:00:06.185682Z TAI"
  instant(1968,2,1).time(cal=cal-tai) == "1968-02-01T00:00:06.185682Z TAI"
  ```
  The only other negative time step was inserted on 1961-08-01Z with a
  duration of -0.05s.

* On the transition to modern UTC at 1972-01-01Z there is a mini leap second of 0.107758s.
  (because the final drift just before 1971-01-01 is 9.892242s) 
  ```
  instant(1972,1,1,0,0,9,0.99999999,cal=cal-tai).time.show(6) == "1971-12-31T23:59:60.107758Z" 
  instant(1972,1,1,0,0,10,cal=cal-tai).time.show == "1972-01-01T00:00:00Z" 
  ```

## UTC before 1961

The TAI timescale was officially established on 1958-01-01Z, and at that
time the difference (TAI - UT1) was approximately zero.
In the time frame 1958 up to 1961 the relation between TAI and UTC is not
well defined, as UTC was only officially established in 1960 (Arias
and Guinot [@Arias:utc]).

We can however use the time steps and frequency adjustments as broadcast by
the NIST WWV radio station (see Explanatory Supplement to the Astronomical
Almanac [@Almanac, pages [86--87][astroa]]).  Historically there were
18 20ms time steps from 1958. The frequency offset in the year 1958 is
not clear, and is described in the Supplement as  ".. an offset of _about_
-100&times;10^-10^ during 1958".  We fixed the offset at -85&times;10^-10^ in
order to have a zero difference from TAI at exactly 1958-01-01Z.

In the library, this makes UTC coincide with TAI up to 1958-01-01,
interpolated with 'drift' up to 1972-01-01, and using integral leap-seconds
after that.

Table [#tab-utc] shows all leap second time steps from 1958 up to 2017. This table
is calculated and uses historical data for time steps before 1972.

~ TableFigure { #tab-utc; caption:"Leap second time steps since 1958. The function \
  &Delta;(_date_) returns the number of days since _date_. Entries after \
  1972 are based on [IETF data][ietfl]. The entries before 1972 are  \
  derived from the _Explanatory Supplement to the Astronomical Almanac_, by \
  P. Kenneth Seidelmann [@Almanac, pages [86--87][astroa]]. Entries before 1961 are based on the \
  time steps broadcast by the NIST [WWV] radio station."}

|-------------------|--------------------------|----------------|----------------|-------------------------------------------------|-----------|
| Date (UTC) &quad; | Time steps &quad; &quad; | (TAI--UTC)     | (TAI--UTC)     | Drift                                           | &quad; Frequency |
|                   |                          | Just before\ \ | On the date&quad; |                                                 | Offset    |
+:------------------|--------------------------|:---------------|:---------------|-------------------------------------------------|----------:+
| 1958-01-01        | +0                       | 0              | 0              | 0.000s + 0.00073458&times;&Delta;(1958-01-01)       | -85&times;10^-10^ |
| 1958-01-15, 19:00 | +0.020                   | 0.0108657      | 0.0308657      | 0.020s + 0.00073458&times;&Delta;(&quad;&quad;"&quad;&quad;) | -85&times;10^-10^ |
| 1958-02-05, 19:00 | +0.020                   | 0.0462918      | 0.0662918      | 0.040s + 0.00073458&times;&Delta;(&quad;&quad;"&quad;&quad;) | -85&times;10^-10^ |
| 1958-02-19, 19:00 | +0.020                   | 0.0765760      | 0.0965760      | 0.060s + 0.00073458&times;&Delta;(&quad;&quad;"&quad;&quad;) | -85&times;10^-10^ |
| 1958-04-09, 19:00 | +0.020                   | 0.1325704      | 0.1525704      | 0.080s + 0.00073458&times;&Delta;(&quad;&quad;"&quad;&quad;) | -85&times;10^-10^ |
| 1958-06-11, 19:00 | +0.020                   | 0.1988489      | 0.2188489      | 0.100s + 0.00073458&times;&Delta;(&quad;&quad;"&quad;&quad;) | -85&times;10^-10^ |
| 1958-07-02, 19:00 | +0.020                   | 0.2342751      | 0.2542751      | 0.120s + 0.00073458&times;&Delta;(&quad;&quad;"&quad;&quad;) | -85&times;10^-10^ |
| 1958-07-16, 19:00 | +0.020                   | 0.2645592      | 0.2845592      | 0.140s + 0.00073458&times;&Delta;(&quad;&quad;"&quad;&quad;) | -85&times;10^-10^ |
| 1958-10-22, 19:00 | +0.020                   | 0.3565481      | 0.3765481      | 0.160s + 0.00073458&times;&Delta;(&quad;&quad;"&quad;&quad;) | -85&times;10^-10^ |
| 1958-11-26, 19:00 | +0.020                   | 0.4022584      | 0.4222584      | 0.180s + 0.00073458&times;&Delta;(&quad;&quad;"&quad;&quad;) | -85&times;10^-10^ |
| 1958-12-24, 19:00 | +0.020                   | 0.4428266      | 0.4628266      | 0.200s + 0.00073458&times;&Delta;(&quad;&quad;"&quad;&quad;) | -85&times;10^-10^ |
| 1959-01-01        | +0                       | 0.4681217      | 0.4681220      | 0.468122s + 0.0008640&times;&Delta;(1959-01-01) | -100&times;10^-10^ |
| 1959-01-28, 19:00 | +0.020                   | 0.4921340      | 0.5121340      | 0.488122s + 0.0008640&times;&Delta;(&quad;&quad;"&quad;&quad;) | -100&times;10^-10^ |
| 1959-02-25, 19:00 | +0.020                   | 0.5363260      | 0.5563260      | 0.508122s + 0.0008640&times;&Delta;(&quad;&quad;"&quad;&quad;) | -100&times;10^-10^ |
| 1959-04-05, 19:00 | +0.020                   | 0.5900220      | 0.6100220      | 0.528122s + 0.0008640&times;&Delta;(&quad;&quad;"&quad;&quad;) | -100&times;10^-10^ |
| 1959-08-26, 19:00 | +0.020                   | 0.7335740      | 0.7535740      | 0.548122s + 0.0008640&times;&Delta;(&quad;&quad;"&quad;&quad;) | -100&times;10^-10^ |
| 1959-09-30, 19:00 | +0.020                   | 0.7838140      | 0.8038140      | 0.568122s + 0.0008640&times;&Delta;(&quad;&quad;"&quad;&quad;) | -100&times;10^-10^ |
| 1959-11-04, 19:00 | +0.020                   | 0.8340540      | 0.8540540      | 0.588122s + 0.0008640&times;&Delta;(&quad;&quad;"&quad;&quad;) | -100&times;10^-10^ |
| 1959-11-18, 19:00 | +0.020                   | 0.8661500      | 0.8861500      | 0.608122s + 0.0008640&times;&Delta;(&quad;&quad;"&quad;&quad;) | -100&times;10^-10^ |
| 1959-12-16, 19:00 | +0.020                   | 0.9103420      | 0.9303420      | 0.628122s + 0.0008640&times;&Delta;(&quad;&quad;"&quad;&quad;) | -100&times;10^-10^ |
| \                 |                          |                |                   |                                                                 |                    |
| 1960-01-01        | +0                       | 0.9434820      | 0.9434820      | 0.943482s + 0.0012960&times;&Delta;(1960-01-01) | -150&times;10^-10^ |
| 1961-01-01        | +0.005                   | 1.4178180      | 1.4228180      | 1.422818s + 0.0012960&times;&Delta;(1961-01-01) | -150&times;10^-10^ |
| 1961-08-01        | --0.050                  | 1.6975700      | 1.6475700      | 1.372818s + 0.0012960&times;&Delta;(&quad;&quad;"&quad;&quad;) | -150&times;10^-10^ |
| 1962-01-01        | +0                       | 1.8458580      | 1.8458580      | 1.845858s + 0.0011232&times;&Delta;(1962-01-01) | -130&times;10^-10^ |
| 1963-11-01        | +0.100                   | 2.5972788      | 2.6972788      | 1.945858s + 0.0011232&times;&Delta;(&quad;&quad;"&quad;&quad;) | -130&times;10^-10^ |
| 1964-01-01        | +0                       | 2.7657940      | 2.7657940      | 3.240130s + 0.0012960&times;&Delta;(1965-01-01) | -150&times;10^-10^ |
| 1964-04-01        | +0.100                   | 2.8837300      | 2.9837300      | 3.340130s + 0.0012960&times;&Delta;(&quad;&quad;"&quad;&quad;) | -150&times;10^-10^ |
| 1964-09-01        | +0.100                   | 3.1820180      | 3.2820180      | 3.440130s + 0.0012960&times;&Delta;(&quad;&quad;"&quad;&quad;) | -150&times;10^-10^ |
| 1965-01-01        | +0.100                   | 3.4401300      | 3.5401300      | 3.540130s + 0.0012960&times;&Delta;(&quad;&quad;"&quad;&quad;) | -150&times;10^-10^ |
| 1965-03-01        | +0.100                   | 3.6165940      | 3.7165940      | 3.640130s + 0.0012960&times;&Delta;(&quad;&quad;"&quad;&quad;) | -150&times;10^-10^ |
| 1965-07-01        | +0.100                   | 3.8747060      | 3.9747060      | 3.740130s + 0.0012960&times;&Delta;(&quad;&quad;"&quad;&quad;) | -150&times;10^-10^ |
| 1965-09-01        | +0.100                   | 4.0550580      | 4.1550580      | 3.840130s + 0.0012960&times;&Delta;(&quad;&quad;"&quad;&quad;) | -150&times;10^-10^ |
| 1966-01-01        | +0                       | 4.3131700      | 4.3131700      | 4.313170s + 0.0025920&times;&Delta;(1966-01-01) | -300&times;10^-10^ |
| 1968-02-01        | --0.100                  | 6.2856820      | 6.1856820      | 4.213170s + 0.0025920&times;&Delta;(&quad;&quad;"&quad;&quad;) | -300&times;10^-10^ |
| \                 |                          |                |                   |                                                                 |                    |
| 1972-01-01        | +0.107758                | 9.8922420      | 10             |                                                 |            |
| 1972-07-01        | +1                       | 10             | 11             |                                                 |            |
| 1973-01-01        | +1                       | 11             | 12             |                                                 |            |
| 1974-01-01        | +1                       | 12             | 13             |                                                 |            |
| 1975-01-01        | +1                       | 13             | 14             |                                                 |            |
| 1976-01-01        | +1                       | 14             | 15             |                                                 |            |
| 1977-01-01        | +1                       | 15             | 16             |                                                 |            |
| 1978-01-01        | +1                       | 16             | 17             |                                                 |            |
| 1979-01-01        | +1                       | 17             | 18             |                                                 |            |
| 1980-01-01        | +1                       | 18             | 19             |                                                 |            |
| 1981-07-01        | +1                       | 19             | 20             |                                                 |            |
| 1982-07-01        | +1                       | 20             | 21             |                                                 |            |
| 1983-07-01        | +1                       | 21             | 22             |                                                 |            |
| 1985-07-01        | +1                       | 22             | 23             |                                                 |            |
| 1988-01-01        | +1                       | 23             | 24             |                                                 |            |
| 1990-01-01        | +1                       | 24             | 25             |                                                 |            |
| 1991-01-01        | +1                       | 25             | 26             |                                                 |            |
| 1992-07-01        | +1                       | 26             | 27             |                                                 |            |
| 1993-07-01        | +1                       | 27             | 28             |                                                 |            |
| 1994-07-01        | +1                       | 28             | 29             |                                                 |            |
| 1996-01-01        | +1                       | 29             | 30             |                                                 |            |
| 1997-07-01        | +1                       | 30             | 31             |                                                 |            |
| 1999-01-01        | +1                       | 31             | 32             |                                                 |            |
| 2006-01-01        | +1                       | 32             | 33             |                                                 |            |
| 2009-01-01        | +1                       | 33             | 34             |                                                 |            |
| 2012-07-01        | +1                       | 34             | 35             |                                                 |            |
| 2015-07-01        | +1                       | 35             | 36             |                                                 |            |
| 2017-01-01        | +1                       | 36             | 37             |                                                 |            |
|-------------------|--------------------------|----------------|----------------|-------------------------------------------------|-----------|
{white-space:nowrap; col-2-padding-left:1em; .sans-serif; }

~

-- Daan Leijen, 2016.

## References { - }

~ Bibliography { caption:"9" }

~~ BibItem { #Arias:utc; bibitem-label:"[1]"; searchterm:"Arias+Coordinated+universal+time+UTC+historical+background+perspectives" }
E.F.\ Arias and B.\ Guinot.
_Coordinated universal time UTC: historical background and perspectives_. 
Journ&eacute;es Syst&egrave;mes de R&eacute;f&eacute;rence Spatio-Temporels. 2004.
[pdf][utchistory].
~~

~~ BibItem { #Almanac; bibitem-label:"[2]"; searchterm:"Explanatory+Supplement+to+the+Astronomical+Almanac" }
P.K.\ Seidelmann.
_Explanatory Supplement to the Astronomical Almanac_. 
University Science Books, 1992. [pdf][astroa]
~~
~

[ietfl]: https://www.ietf.org/timezones/data/leap-seconds.list
[dat]: http://maia.usno.navy.mil/ser7/tai-utc.dat
[astroa]: https://books.google.com/books?id=uJ4JhGJANb4C&lpg=PA87&vq=wwv&pg=PA87#v=onepage&q=wwv&f=false
[utchistory]: https://syrte.obspm.fr/journees2004/pdf/Arias2.pdf
[WWV]: https://www.nist.gov/time-and-frequency-services/nist-radio-stations/wwv
[UTC]: std_time_astro.html
[TAI]: std_time_astro.html
[TI]: std_time_astro.html
[TIpropose]: https://syrte.obspm.fr/journees2004/pdf/McCarthy2.pdf#page=4
\/
*/
module std/time/utc

import std/text/regex
import std/num/double
import std/num/ddouble
import std/time/timestamp
import std/time/duration
import std/time/instant

/*----------------------------------------------------------------------------
  Unix
----------------------------------------------------------------------------*/

/* Given a Unix time stamp in (fractional) seconds (`secs`) and an optional separate 
fraction of seconds `frac` (for increased precision for nanosecond timestamps), return an `:instant`.
that is `secs + frac` seconds after the Unix epoch (``1970-01-01Z``).

Unfortunately, Unix time stamps are [ambigious](https://en.wikipedia.org/wiki/Unix_time). 
The seconds `secs` are interpreted as: `val days = secs / 86400` and `val dsecs = secs % 86400`,
where `days` is the number of days since ``1970-01-01Z`` and `dsecs` is the SI seconds into
the day. This means that one cannot represent a possible extra leap second since it
will look as the first second of the next day. For example, here is how the time stamps look
around the leap second of ``1973-01-01Z``:
````
> instant(1972,12,31,23,59,59).unix-timestamp
94694399.0

> instant(1972,12,31,23,59,60).unix-timestamp
94694400.0

> instant(1973,1,1).unix-timestamp
94694400.0
````

Internally, this library uses proper `:timestamp`s that _can_ keep track of leap seconds. 
To indicate a time in a leap second, you can use a fraction `frac` that is larger than `1.0`. For example:
````
> unix-instant(94694399.0).time
1972-12-31T23:59:59Z

> unix-instant(94694399.0,1.0).time
1972-12-31T23:59:60Z

> unix-instant(94694400.0).time
1973-01-01T00:00:00Z
````

This works well for systems that support [``CLOCK_UTC``](http://www.madore.org/~david/computers/unix-leap-seconds.html).
*/
public fun unix-instant( secs : double, frac : double = 0.0 ) : instant {
  unsafe-instant( timestamp( timespan(secs.trunc.int, secs.fraction + frac.fraction), frac.trunc.int), ts-unix ) 
}

// Get a standard Unix timestamp in fractional seconds since the Unix epoch (1970-01-01Z).
public fun unix-timestamp( i :instant ) : double {
  i.timestamp(ts-unix).seconds.double
}

/*----------------------------------------------------------------------------
  Timescale creation
----------------------------------------------------------------------------*/

// Create a new time scale based on UTC seconds with a given `name` and a
// `y2k-epoch` (=`timestamp0`) which is the timestamp of the 2001-01-01 date in that timescale
// e.g. for a timescale `ts`:\
// `y2k-epoch = instant-at(2000,1,1,cal=iso-calender(ts)).timestamp(ts)`.
public fun utc-timescale( name : string, leaps : leaps-table, y2k-epoch : timestamp = timestamp0 ) : timescale {
  val epoch-shift = y2k-epoch.timespan
  fun from-tai(tai:duration)  { tai-to-utc(leaps, tai) + epoch-shift }
  fun to-tai(t:timestamp)     { utc-to-tai(leaps, t - epoch-shift) }
  fun secs-in-day(t:timestamp){ utc-secs-in-day(leaps, t) }
  timescale(
    name,
    from-tai,
    to-tai,
    y2k-epoch,
    "UTC",
    Just(secs-in-day)
  )
}

// The UTC time scale with a 2000-01-01Z UTC epoch. This uses [Unix seconds](https://en.wikipedia.org/wiki/Unix_time)
// but in contrast to Unix time stamps the library will accurately maintain any leap second information
// (through `:timestamp`s). 
public fun ts-utc-create( leaps : leaps-table ) : timescale {
  utc-timescale( "UTC", leaps )
}

// [Unix](https://en.wikipedia.org/wiki/Unix_time) time scale based on Unix seconds but with 1970-01-01 epoch.
public fun ts-unix-create( leaps : leaps-table ) : timescale {
  utc-timescale( "UNIX", leaps, timestamp(946684800) )
}

// [NTP](https://en.wikipedia.org/wiki/Network_Time_Protocol) time scale is equal to the UTC time scale (`ts-utc`) but 
// with a 1900-01-01Z epoch.
public fun ts-ntp-create( leaps : leaps-table ) : timescale {
  utc-timescale( "NTP", leaps, timestamp(3155673600) )
}

/* The [TI] (_International Time_) time scale with a 2000-01-01Z UTC epoch. 
This is the default time scale used in this library. It was a
[proposed][TIpropose] time scale at the 2004 ITU-R meeting as a replacement of UTC
without future leap seconds. In this library, we define TI to match
exactly UTC up to the compiler release date (currently 2017) but ignore any
possible future leap seconds after that date. This is the preferred time scale
in this library as it guarantees deterministic time calculations for any
future date, i.e. before 2017-01-01Z, TI == UTC, while after that, TI == TAI - 37s.
*/
public val ts-ti : timescale = utc-timescale( "TI", leaps-table-ti )

// [Unix](https://en.wikipedia.org/wiki/Unix_time) time scale based on Unix seconds. 
// It equals the `ts-ti` time scale except with a 1970-01-01 epoch.
public val ts-unix : timescale = ts-unix-create( leaps-table-ti )


// Standard seconds per day (=`86400`). Note that a UTC day can contain extra leap seconds.
val secs-per-day = timespan(86400)

// -----------------------------------------------------------
// UTC calendar leap second calculation
//
// Since the timescales `tai` and `utc` refer to `utc-from-tai`
// and `tai-from-utc` these routines strictly work in durations
// and cannot refer to instants or otherwise the definitions become
// mutually recursive.
// -----------------------------------------------------------


// `:utc-timestamp` is an alias used for timestamp's in UTC seconds.
alias utc-timestamp = timestamp


// A leap second table describes when UTC leap seconds occur.
abstract struct leaps-table(
  public expire : instant,       
  adjusts: list<leap-adjust> // Each entry gives the start instant and integer leap second adjustment.
)

// Leap second adjustments. For an instant `i` after `start`:\
// ``TAI-offset = offset + (delta * days(i - delta-start))``
struct leap-adjust(
  utc-start  : utc-timestamp,  // start time in UTC seconds since 1972-01-01Z
  offset     : timespan,       // base offset
  delta-start: utc-timestamp  = timestamp0,   // start of delta adjustment
  delta      : ddouble = zero   
)

val leaps-table0 = Leaps-table(epoch,[])
val zero : leap-adjust = Leap-adjust(timestamp0,timespan0,timestamp0,zero)

fun zero?( la : leap-adjust ) : bool {
  la.offset.zero? && la.delta.zero?
}

// Get the leap-second adjustment
fun tai-offset( la : leap-adjust, utc-i : utc-timestamp ) : timespan {
  if (la.delta.zero?) then la.offset else {
    // pre 1972 is a rubber leap second
    val days = div((utc-i.timespan - la.delta-start.timespan), secs-per-day, 3) 
    la.offset + (la.delta * days)
  }
}


// Default TI leaps table has leap second information up to the compiler release (currently `leaps-table-y2017`).
public val leaps-table-ti : leaps-table = parse-leaps-table( default-ietf-leap-seconds ).default(leaps-table0)

// Leap second table up to 2017-01-01Z.
public val leaps-table-y2017 : leaps-table = leaps-table-ti.upto(536544000.timestamp)  // == instant(2017,1,1).timestamp(ts-ti)

private fun upto( lt : leaps-table, end : timestamp ) : leaps-table {
  lt( adjusts = lt.adjusts.drop-while(fun(la) { la.utc-start > end }),
      expire  = unsafe-instant(end,ts-ti))
}


// Get a list of leap second steps in a triple, UTC time, offset just before, and the new offset at that time,
// the base offset, the delta start date and the delta rate.
public fun get-leap-steps( table : leaps-table = leaps-table-ti ) : list<(timestamp,timespan,timespan,(timespan,timestamp,ddouble))> {
  val adjusts = table.adjusts.reverse
  zip(Cons(zero,adjusts),adjusts).map( fun(las) {
    val ts = las.snd.utc-start
    val ofs1 = las.fst.tai-offset(ts)
    val ofs2 = las.snd.tai-offset(ts)
    (ts,ofs1,ofs2,(las.snd.offset,las.snd.delta-start,las.snd.delta))
  })
}


/*----------------------------------------------------------------------------
  UTC-SLS: smoothed leap seconds
----------------------------------------------------------------------------*/

// TI time scale with smoothed leap seconds.\
// Implements a TI time scale (`ts-ti`) except without ever showing leap seconds.
// TI-SLS is equivalent to TI except for the last 1000 seconds of a day where
// a leap second occurs. On such day, the leap second time step (positive or negative)
// is distributed over the last 1000 seconds of the day. On the full hour, TI
// and TI-SLS are equal again. 
public val ts-ti-sls : timescale = utc-sls-create( "TI-SLS", leaps-table-ti )


// Create a new UTC-SLS time scale from a provided leap second table `leaps`.
// Implements a UTC time scale except without ever showing leap seconds.
// UTC-SLS is equivalent to UTC except for the last 1000 seconds of a day where
// a leap second occurs. On such day, the leap second time step (positive or negative)
// is distributed over the last 1000 seconds of the day. On the full hour, UTC
// and UTC-SLS are equal again. 
//
// This is a recommended time scale to use for
// time stamps or communication with other services since it avoids any potential trouble
// with leap seconds while still being quite precise.
// See also: <https://www.cl.cam.ac.uk/~mgk25/time/utc-sls>.
//
// You can create a UTC-SLS time scale based on the latest IETF leap second
// data using [`cal-utc-sls-load`](std_time_download.html#cal_utc_sls_load).
public fun ts-utc-sls-create( leaps : leaps-table ) : timescale {
  utc-sls-create( "UTC-SLS", leaps )
}

// Create a new smooted leap second time scale with an optional period during
// which smoothing takes place. This is 1000s for `ts-utc-sls`.
fun utc-sls-create( name : string, leaps : leaps-table, smooth-period : duration = 1000.seconds ) : timescale {
  val sp = smooth-period.seconds
  timescale(
    name,
    fun(tai:duration) { tai-to-utc-sls(leaps, tai, sp) },
    fun(utc:timestamp) { utc-sls-to-tai(leaps, utc, sp) },
    timestamp0,
    "UTC-SLS",
    Nothing
  )
}

fun utc-sls-to-tai( leaps : leaps-table, t : timestamp, smooth-period : ddouble ) : duration {
  val (ofs,mbsmooth) = find-utc-smooth(leaps, t, smooth-period) 
  // convert directly to tai as we have all info
  match(mbsmooth) { 
    Nothing -> {
      unsafe-duration( t + ofs )
    }
    Just((step,start)) -> {
      unsafe-duration( div( ofs + start + (t.timespan - start), (1.ddouble - div(step,smooth-period)) ) )
    }
  }
}

fun tai-to-utc-sls( leaps : leaps-table, tai :duration, smooth-period : ddouble ) : timestamp {
  val t = tai-to-utc(leaps,tai)
  val tsls = match(find-utc-smooth(leaps, t, smooth-period).snd) {
    Nothing -> t
    Just((step,start)) -> t - step*(div(t.timespan - start, smooth-period))
  }
  timestamp(tsls.seconds) // remove leap seconds
}


fun find-utc-smooth( leaps : leaps-table, t : timestamp, smooth-period : ddouble ) : (timespan,maybe<(timespan,timespan)>) {
  if (t < utc1958) then return (zero:timespan,Nothing)

  match(leaps.adjusts.find-adjust(t)) {
    Just((ladj,next)) -> {
      val ofs   = ladj.tai-offset(t)
      val step  = next.tai-offset(next.utc-start) - ofs
      val start = next.utc-start + step - smooth-period
      (ofs, if (next.zero? || t < start) then Nothing else Just((step,start.timespan)))
    }
    Nothing -> (zero:timespan,Nothing)
  } 
}

fun find-adjust( leaps : list<leap-adjust>, t : timestamp, previous : leap-adjust = zero ) : maybe<(leap-adjust,leap-adjust)> {
  match(leaps) {
    Nil -> Nothing
    Cons(la,earlier) -> {
      if (la.utc-start <= t) 
       then Just((la,previous))
       else find-adjust(earlier, t, la)
    }
  }
}



/*----------------------------------------------------------------------------
  UTC to TAI conversion
----------------------------------------------------------------------------*/

// Convert a UTC duration since `epoch` to a TAI duration since `epoch`.
fun utc-to-tai( leaps : leaps-table, t : timestamp ) : duration {
  utc-to-tai-stamp( leaps, t ).unsafe-duration
}

fun utc-to-tai-stamp( leaps : leaps-table, t : timestamp ) : timestamp {
  if (t < utc1958) then return t
  leaps.adjusts.find-maybe(fun(la) {
    if (la.utc-start > t) then Nothing else {
      val ofs = la.tai-offset(t)
      // trace("found: " + t.show + " >= " + la.utc-start.show + ", ofs:" + ofs.show)
      Just( (t + ofs) )
    }
  }).default(t)  
}

// Convert a TAI duration since `epoch` to a UTC duration since `epoch`.
fun tai-to-utc( leaps : leaps-table, d : duration ) : timestamp {
  //trace("utc-from-tai: " + i.show)
  if (d < tai1958) then d.timestamp else find-utc-from-tai( d.timestamp, leaps.adjusts )
}
val tai1958 = duration(~1325376000)  // 1958-01-01Z in seconds since epoch
val utc1958 = tai1958.timestamp

fun find-utc-from-tai( i : timestamp, leaps : list<leap-adjust> ) : timestamp {
  match(leaps) {
    Nil -> i // should never happen
    Cons(la,earlier) -> {
      val ofs   = la.tai-offset(i)
      val utc-i = i - ofs
      val max   = if (ofs.neg?) then utc-i else i
      // trace("check: utc-i: " + utc-i.show + ", start: " + la.utc-start.show + ", ofs: " + ofs.show + ", i: " + i.show)
      if (la.utc-start > max) then find-utc-from-tai(i,earlier) else {
        if (la.utc-start <= utc-i) then {
          // in the time frame
          // trace(" utc-from-tai: found: utc: " + utc-i.show + ", tai-ofs: " + ofs.ts-show )
          utc-i
        }
        else {
          // leap-second crosses-over into other time frame            
          // get adjustment using the earlier frame too
          // trace("found crossover: " + utc-i.show + ", ofs: " + ofs.ts-show + ", i: " + i.show )
          val la-before = earlier.head.default(zero)
          val ofs-before= la-before.tai-offset(i)
          val utc-prev  = i - ofs-before
          if (la.utc-start > utc-prev) then {
            // use previous leap-second offset            
            // trace(" use previous: " + utc-prev.show + ", ofs-before: " + ofs-before.ts-show )
            utc-prev
          }
          else {
            // this is inside an added leap-second, use `add-leap-seconds`.
            val diff     = ofs - ofs-before
            val utc-leap = utc-i.adjust-leap-seconds(diff)                            
            // trace(" in a leap second: " + utc-leap.show + ", diff: " + diff.ts-show + ", ofs: " + ofs.ts-show + ", ofs-before: " + ofs-before.ts-show )
            utc-leap
          }
        }
      }  
    }
  }
}


// Return the SI seconds in the day of time `t` (as a UTC timestamp)
public fun utc-secs-in-day( leaps : leaps-table, t : timestamp ) : timespan {
  //trace("search nlsecs: " + nlsecs.ts-show)
  if (t < utc1972) then return secs-per-day
  match(leaps.adjusts.long-day?(t)) {
    Just(leap-secs) -> leap-secs + secs-per-day
    _ -> secs-per-day
  }
}
val utc1972 = timestamp(~883612800)  // instant-at(1972).timestamp(ts-utc)

fun long-day?( leaps : list<leap-adjust>, t : timestamp ) : maybe<ddouble> {
  match(leaps) {
    Nil -> Nothing
    Cons(la,rest) -> {
      if (la.utc-start <= t) then return long-day?(rest,t) 
      if (la.utc-start - secs-per-day > t) then return Nothing  
      val delta = match(rest) {
        Nil -> 1.timespan
        Cons(la-before) -> (la.offset - la-before.offset)
      }
      Just(delta)
    }
  }
}



// -----------------------------------------------------------
// Parsing UTC leap second tables
// -----------------------------------------------------------

val ntp-shift = 3155673600  // 2000-01-01 TAI - 1900-01-01 TAI in seconds; = (100*365 + 24)*(24*3600)  (24 leap days)


// Parse a leap second table from text `leaps` in the  [IETF leap second](https://www.ietf.org/timezones/data/leap-seconds.list) 
// file format. Returns a full leap second table extended with historical leap second adjustments from before 1972.
public fun parse-leaps-table( leaps : string ) : maybe<leaps-table> {
  // get leap second adjustments
  val adjusts72 = leaps.find-all(rxleap).map fun(cap) {
    val ntpsecs = cap.groups[1].parse-int-default(ntp-shift)   
    val adjust  = cap.groups[2].parse-int-default(0)
    // trace("leap entry: " + adjust.show + " - " + ntpsecs.show )
    Leap-adjust(timestamp(ntpsecs - ntp-shift), adjust.timespan, timestamp0)
  }.reverse

  // get expiration date
  val la-final   = adjusts72.head.default(zero)
  val utc-expire = leaps.find(rxexpire).map fun(cap) {
    val ntpex = cap.groups[1].parse-int-default(ntp-shift)
    timestamp(ntpex - ntp-shift)
  }.default(la-final.utc-start + (182.timespan*secs-per-day))  // default: 6 months after last leap second
  // trace("expire: " + utc-expire.ts-show)
  val expire = unsafe-instant( utc-expire + la-final.offset, ts-tai ) // interpret as TAI to avoid recursion..

  val adjusts = adjusts72 + leap-seconds-pre72()  // add pre 1972 adjustments
  val table = Leaps-table(expire,adjusts)
  Just(table)
}
val rxleap   = regex(@"^[ \t]*(\d+)[ \t]+(\d+)[ \t]*(?:#.*)?$",multiLine=True)
val rxexpire = regex(@"^[ \t]*#@[ \t]*(\d+)[ \t]*(?:#.*)?$", multiLine=True)

// IETF leap second data valid until 2017-06-28
val default-ietf-leap-seconds = @"
  # From: https://www.ietf.org/timezones/data/leap-seconds.list
  # Updated through IERS Bulletin C52
  # File expires on:  28 June 2017
  #
  #@  3707596800
  #
  2272060800  10  # 1 Jan 1972
  2287785600  11  # 1 Jul 1972
  2303683200  12  # 1 Jan 1973
  2335219200  13  # 1 Jan 1974
  2366755200  14  # 1 Jan 1975
  2398291200  15  # 1 Jan 1976
  2429913600  16  # 1 Jan 1977
  2461449600  17  # 1 Jan 1978
  2492985600  18  # 1 Jan 1979
  2524521600  19  # 1 Jan 1980
  2571782400  20  # 1 Jul 1981
  2603318400  21  # 1 Jul 1982
  2634854400  22  # 1 Jul 1983
  2698012800  23  # 1 Jul 1985
  2776982400  24  # 1 Jan 1988
  2840140800  25  # 1 Jan 1990
  2871676800  26  # 1 Jan 1991
  2918937600  27  # 1 Jul 1992
  2950473600  28  # 1 Jul 1993
  2982009600  29  # 1 Jul 1994
  3029443200  30  # 1 Jan 1996
  3076704000  31  # 1 Jul 1997
  3124137600  32  # 1 Jan 1999
  3345062400  33  # 1 Jan 2006
  3439756800  34  # 1 Jan 2009
  3550089600  35  # 1 Jul 2012
  3644697600  36  # 1 Jul 2015
  3692217600  37  # 1 Jan 2017"
  // 3723753600  35  # 1 Jan 2018" //testing negative 2 seconds

// -----------------------------------------------------------
// Parsing TAI 'continuous' leap second tables from before 1972
// -----------------------------------------------------------

// Cached leap second table for dates before 1972.
val leap-seconds-pre72 : (() -> list<leap-adjust>) = once{ parse-leap-seconds-dat() }

val jd-epoch-shift     = 2400000.5.timespan
val mjd-epoch-shift    = 4453401600.timespan // 2000-01-01 TAI - 1858-11-17 TAI modified julian date epoch


// Parse the standard UTC leap second adjustment file.\
// See <http://maia.usno.navy.mil/ser7/tai-utc.dat>
fun parse-leap-seconds-dat( s : string = leap-seconds-dat ) : list<leap-adjust> {
  s.find-all(rxtaiadjust).map( fun(cap) {
    val mjd    = cap.groups[1].parse-ddouble.default(jd-epoch-shift) - jd-epoch-shift 
    val ofs    = cap.groups[2].parse-ddouble.default(zero)
    val dmjd   = cap.groups[3].parse-ddouble.default(zero)
    val delta  = cap.groups[4].parse-ddouble.default(zero)
    val start  = timestamp(mjd*secs-per-day - mjd-epoch-shift)  //instant(1858,11,17 + mjd.int)
    val dstart = timestamp(dmjd*secs-per-day - mjd-epoch-shift) //instant(1858,11,17 + dmjd.int)
    // trace("pre72 start=" + start.show + ", ofs: " + ofs.show + ", delta: " + delta.show )
    Leap-adjust( start, ofs, dstart, delta )
  }).reverse
}
val rxtaiadjust = regex(@"^ *\d[^=]*=JD (\d+\.\d+) *TAI-UTC= *(-?\d+\.\d+)[^\d]+(\d+\.\d*)[^\d]+(\d+\.\d+) *S *$", multiLine=True)

// TAI leap second adjustments for dates before 1972-01-01Z are linear interpolations.
// TAI started in 1958-01-01Z. The initial official UTC time step in 1961-01-01Z was 1.422818s and before that there 
// were small steps of 20ms. See Explanatory Supplement to the Astronomical Almanac, 1992 edition, pages 86--87.
// In 1958, the supplement remarks that WWC operated at an offset of _about_ -100e-10, we 
// change it to -85e-10 to end up with TAI-UTC == 0 at 1958-01-01. 
// (without a rate change it is a negative -0.0272380s). 
// Note the JD dates are at 0.29167 as the time steps were usually at 19:00h instead of midnight.
val leap-seconds-dat = @"
  # from: Explanatory Supplement to the Astronomical Almanac, 1992 edition, pages 86--87.
  1958 JAN  1 =JD 2436204.5     TAI-UTC= 0.0  S + (MJD - 36204.) X 0.00073458 S
  1958 JAN 15 =JD 2436219.29167 TAI-UTC= 0.02 S + (MJD - 36204.) X 0.00073458 S
  1958 FEB  5 =JD 2436240.29167 TAI-UTC= 0.04 S + (MJD - 36204.) X 0.00073458 S
  1958 FEB 19 =JD 2436254.29167 TAI-UTC= 0.06 S + (MJD - 36204.) X 0.00073458 S
  1958 APR  9 =JD 2436303.29167 TAI-UTC= 0.08 S + (MJD - 36204.) X 0.00073458 S
  1958 JUN 11 =JD 2436366.29167 TAI-UTC= 0.10 S + (MJD - 36204.) X 0.00073458 S
  1958 JUL  2 =JD 2436387.29167 TAI-UTC= 0.12 S + (MJD - 36204.) X 0.00073458 S
  1958 JUL 16 =JD 2436401.29167 TAI-UTC= 0.14 S + (MJD - 36204.) X 0.00073458 S
  1958 OCT 22 =JD 2436499.29167 TAI-UTC= 0.16 S + (MJD - 36204.) X 0.00073458 S
  1958 NOV 26 =JD 2436534.29167 TAI-UTC= 0.18 S + (MJD - 36204.) X 0.00073458 S
  1958 DEC 24 =JD 2436562.29167 TAI-UTC= 0.20 S + (MJD - 36204.) X 0.00073458 S
  
  1959 JAN  1 =JD 2436569.5     TAI-UTC= 0.4681220 S + (MJD - 36569.) X 0.000864 S
  1959 JAN 28 =JD 2436597.29167 TAI-UTC= 0.4881220 S + (MJD - 36569.) X 0.000864 S
  1959 FEB 25 =JD 2436625.29167 TAI-UTC= 0.5081220 S + (MJD - 36569.) X 0.000864 S
  1959 APR  5 =JD 2436664.29167 TAI-UTC= 0.5281220 S + (MJD - 36569.) X 0.000864 S
  1959 AUG 26 =JD 2436807.29167 TAI-UTC= 0.5481220 S + (MJD - 36569.) X 0.000864 S
  1959 SEP 30 =JD 2436842.29167 TAI-UTC= 0.5681220 S + (MJD - 36569.) X 0.000864 S
  1959 NOV  4 =JD 2436877.29167 TAI-UTC= 0.5881220 S + (MJD - 36569.) X 0.000864 S
  1959 NOV 18 =JD 2436891.29167 TAI-UTC= 0.6081220 S + (MJD - 36569.) X 0.000864 S
  1959 DEC 16 =JD 2436919.29167 TAI-UTC= 0.6281220 S + (MJD - 36569.) X 0.000864 S
  1960 JAN  1 =JD 2436934.5     TAI-UTC= 0.9434820 S + (MJD - 36934.) X 0.001296 S

  # from: http://maia.usno.navy.mil/ser7/tai-utc.dat
  1961 JAN  1 =JD 2437300.5  TAI-UTC=   1.4228180 S + (MJD - 37300.) X 0.001296 S
  1961 AUG  1 =JD 2437512.5  TAI-UTC=   1.3728180 S + (MJD - 37300.) X 0.001296 S
  1962 JAN  1 =JD 2437665.5  TAI-UTC=   1.8458580 S + (MJD - 37665.) X 0.0011232S
  1963 NOV  1 =JD 2438334.5  TAI-UTC=   1.9458580 S + (MJD - 37665.) X 0.0011232S
  1964 JAN  1 =JD 2438395.5  TAI-UTC=   3.2401300 S + (MJD - 38761.) X 0.001296 S
  1964 APR  1 =JD 2438486.5  TAI-UTC=   3.3401300 S + (MJD - 38761.) X 0.001296 S
  1964 SEP  1 =JD 2438639.5  TAI-UTC=   3.4401300 S + (MJD - 38761.) X 0.001296 S
  1965 JAN  1 =JD 2438761.5  TAI-UTC=   3.5401300 S + (MJD - 38761.) X 0.001296 S
  1965 MAR  1 =JD 2438820.5  TAI-UTC=   3.6401300 S + (MJD - 38761.) X 0.001296 S
  1965 JUL  1 =JD 2438942.5  TAI-UTC=   3.7401300 S + (MJD - 38761.) X 0.001296 S
  1965 SEP  1 =JD 2439004.5  TAI-UTC=   3.8401300 S + (MJD - 38761.) X 0.001296 S
  1966 JAN  1 =JD 2439126.5  TAI-UTC=   4.3131700 S + (MJD - 39126.) X 0.002592 S
  1968 FEB  1 =JD 2439887.5  TAI-UTC=   4.2131700 S + (MJD - 39126.) X 0.002592 S"

