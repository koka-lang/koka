/*----------------------------------------------------------------------------
   Copyright 2012-2021, Microsoft Research, Daan Leijen.

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
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
to exactly match UTC before the compiler release date (currently 2019) but
ignores any future leap seconds.

## UTC between 1961 and 1972

UTC only got integral leap seconds after 1972-01-01Z.
Before 1972, the UTC time was broadcast using a combination of
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

UTC was only established in 1960 (Arias and Guinot [@Arias:utc]), and the TAI
instant 1961-01-01 00:00:01.422818Z TAI was set to be UTC instant
1961-01-01Z exactly.

## UTC before 1961

The TAI timescale was established with a 1958-01-01Z epoch, where
the difference (UT2 - TAI) was set to be approximately zero[^fn-taiepoch].
In the time frame 1958 up to 1961 the relation between TAI and UTC
is not formally defined.

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

Interestingly, in the time before 1961, most leap steps occurred at 19:00h
instead of at the last second of the day, for example, on 1959-01-28
there was a 0.02s leap step:
```
instant(1959,1,28,19,0,0,0.50,cal=cal-tai).time.show == "1959-01-28T18:59:60.007866Z"
instant(1959,1,28,19,0,0,0.52,cal=cal-tai).time.show == "1959-01-28T19:00:00.007866Z"
```
showing the seconds at 60 just before 19:00h.

## All historical leap second adjustments

Table [#tab-utc] shows all leap second time steps from 1958 up to 2017. This table
is calculated and uses historical data for time steps before 1972.

~ Begin TableFigure { #tab-utc; caption:"Leap second time steps since 1958. The function \
  &Delta;(_date_) returns the number of days since _date_. Entries after \
  1972 are based on [IETF data][ietfl]. The entries before 1972 are  \
  derived from the _Explanatory Supplement to the Astronomical Almanac_, by \
  P. Kenneth Seidelmann [@Almanac, pages [86--87][astroa]]. Entries before 1961 are based on the \
  time steps broadcast by the NIST [WWV] radio station."}

|-------------------|--------------------------|----------------|----------------|-------------------------------------------------|-----------|
| Date (UTC) &quad; | Time steps &quad; &quad; | (TAI--UTC)     | (TAI--UTC)     | Drift                                           | &quad; Frequency |
|                   |                          | Just before\ \ | On the date&quad; |                                                 | Offset    |
+:------------------|--------------------------|:---------------|:---------------|-------------------------------------------------|----------:+
| 1958-01-01        | +0                       | 0.000000       | 0.000000       | 0.000000s + 0.00073458&times;&Delta;(1958-01-01) | -85&times;10^-10^ |
| 1958-01-15, 19:00 | +0.020                   | 0.010866       | 0.030866       | 0.020000s + 0.00073458&times;&Delta;(&quad;&quad;"&quad;&quad;) | -85&times;10^-10^ |
| 1958-02-05, 19:00 | +0.020                   | 0.046292       | 0.066292       | 0.040000s + 0.00073458&times;&Delta;(&quad;&quad;"&quad;&quad;) | -85&times;10^-10^ |
| 1958-02-19, 19:00 | +0.020                   | 0.076576       | 0.096576       | 0.060000s + 0.00073458&times;&Delta;(&quad;&quad;"&quad;&quad;) | -85&times;10^-10^ |
| 1958-04-09, 19:00 | +0.020                   | 0.132570       | 0.152570       | 0.080000s + 0.00073458&times;&Delta;(&quad;&quad;"&quad;&quad;) | -85&times;10^-10^ |
| 1958-06-11, 19:00 | +0.020                   | 0.198849       | 0.218849       | 0.100000s + 0.00073458&times;&Delta;(&quad;&quad;"&quad;&quad;) | -85&times;10^-10^ |
| 1958-07-02, 19:00 | +0.020                   | 0.234275       | 0.254275       | 0.120000s + 0.00073458&times;&Delta;(&quad;&quad;"&quad;&quad;) | -85&times;10^-10^ |
| 1958-07-16, 19:00 | +0.020                   | 0.264559       | 0.284559       | 0.140000s + 0.00073458&times;&Delta;(&quad;&quad;"&quad;&quad;) | -85&times;10^-10^ |
| 1958-10-22, 19:00 | +0.020                   | 0.356548       | 0.376548       | 0.160000s + 0.00073458&times;&Delta;(&quad;&quad;"&quad;&quad;) | -85&times;10^-10^ |
| 1958-11-26, 19:00 | +0.020                   | 0.402258       | 0.422258       | 0.180000s + 0.00073458&times;&Delta;(&quad;&quad;"&quad;&quad;) | -85&times;10^-10^ |
| 1958-12-24, 19:00 | +0.020                   | 0.442827       | 0.462827       | 0.200000s + 0.00073458&times;&Delta;(&quad;&quad;"&quad;&quad;) | -85&times;10^-10^ |
| 1959-01-01        | +0                       | 0.468122       | 0.468122       | 0.468122s + 0.0008640&times;&Delta;(1959-01-01) | -100&times;10^-10^ |
| 1959-01-28, 19:00 | +0.020                   | 0.492134       | 0.512134       | 0.488122s + 0.0008640&times;&Delta;(&quad;&quad;"&quad;&quad;) | -100&times;10^-10^ |
| 1959-02-25, 19:00 | +0.020                   | 0.536326       | 0.556326       | 0.508122s + 0.0008640&times;&Delta;(&quad;&quad;"&quad;&quad;) | -100&times;10^-10^ |
| 1959-04-05, 19:00 | +0.020                   | 0.590022       | 0.610022       | 0.528122s + 0.0008640&times;&Delta;(&quad;&quad;"&quad;&quad;) | -100&times;10^-10^ |
| 1959-08-26, 19:00 | +0.020                   | 0.733574       | 0.753574       | 0.548122s + 0.0008640&times;&Delta;(&quad;&quad;"&quad;&quad;) | -100&times;10^-10^ |
| 1959-09-30, 19:00 | +0.020                   | 0.783814       | 0.803814       | 0.568122s + 0.0008640&times;&Delta;(&quad;&quad;"&quad;&quad;) | -100&times;10^-10^ |
| 1959-11-04, 19:00 | +0.020                   | 0.834054       | 0.854054       | 0.588122s + 0.0008640&times;&Delta;(&quad;&quad;"&quad;&quad;) | -100&times;10^-10^ |
| 1959-11-18, 19:00 | +0.020                   | 0.866150       | 0.886150       | 0.608122s + 0.0008640&times;&Delta;(&quad;&quad;"&quad;&quad;) | -100&times;10^-10^ |
| 1959-12-16, 19:00 | +0.020                   | 0.910342       | 0.930342       | 0.628122s + 0.0008640&times;&Delta;(&quad;&quad;"&quad;&quad;) | -100&times;10^-10^ |
| \                 |                          |                |                   |                                                                 |                    |
| 1960-01-01        | +0                       | 0.943482       | 0.943482       | 0.943482s + 0.0012960&times;&Delta;(1960-01-01) | -150&times;10^-10^ |
| 1961-01-01        | +0.005                   | 1.417818       | 1.422818       | 1.422818s + 0.0012960&times;&Delta;(1961-01-01) | -150&times;10^-10^ |
| 1961-08-01        | -0.050                   | 1.697570       | 1.647570       | 1.372818s + 0.0012960&times;&Delta;(&quad;&quad;"&quad;&quad;) | -150&times;10^-10^ |
| 1962-01-01        | +0                       | 1.845858       | 1.845858       | 1.845858s + 0.0011232&times;&Delta;(1962-01-01) | -130&times;10^-10^ |
| 1963-11-01        | +0.100                   | 2.597279       | 2.697279       | 1.945858s + 0.0011232&times;&Delta;(&quad;&quad;"&quad;&quad;) | -130&times;10^-10^ |
| 1964-01-01        | +0                       | 2.765794       | 2.765794       | 3.240130s + 0.0012960&times;&Delta;(1965-01-01) | -150&times;10^-10^ |
| 1964-04-01        | +0.100                   | 2.883730       | 2.983730       | 3.340130s + 0.0012960&times;&Delta;(&quad;&quad;"&quad;&quad;) | -150&times;10^-10^ |
| 1964-09-01        | +0.100                   | 3.182018       | 3.282018       | 3.440130s + 0.0012960&times;&Delta;(&quad;&quad;"&quad;&quad;) | -150&times;10^-10^ |
| 1965-01-01        | +0.100                   | 3.440130       | 3.540130       | 3.540130s + 0.0012960&times;&Delta;(&quad;&quad;"&quad;&quad;) | -150&times;10^-10^ |
| 1965-03-01        | +0.100                   | 3.616594       | 3.716594       | 3.640130s + 0.0012960&times;&Delta;(&quad;&quad;"&quad;&quad;) | -150&times;10^-10^ |
| 1965-07-01        | +0.100                   | 3.874706       | 3.974706       | 3.740130s + 0.0012960&times;&Delta;(&quad;&quad;"&quad;&quad;) | -150&times;10^-10^ |
| 1965-09-01        | +0.100                   | 4.055058       | 4.155058       | 3.840130s + 0.0012960&times;&Delta;(&quad;&quad;"&quad;&quad;) | -150&times;10^-10^ |
| 1966-01-01        | +0                       | 4.313170       | 4.313170       | 4.313170s + 0.0025920&times;&Delta;(1966-01-01) | -300&times;10^-10^ |
| 1968-02-01        | -0.100                   | 6.285682       | 6.185682       | 4.213170s + 0.0025920&times;&Delta;(&quad;&quad;"&quad;&quad;) | -300&times;10^-10^ |
| \                 |                          |                |                   |                                                                 |                    |
| 1972-01-01        | +0.107758                | 9.892242       | 10             |                                                 |            |
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

~ End TableFigure

-- Daan Leijen, 2016.

## References { - }

~ Begin Bibliography { caption:"9" }

~~ BibItem { #Arias:utc; bibitem-label:"[1]"; searchterm:"Arias+Coordinated+universal+time+UTC+historical+background+perspectives" }
E.F.\ Arias and B.\ Guinot.
_Coordinated universal time UTC: historical background and perspectives_.
Journ&eacute;es Syst&egrave;mes de R&eacute;f&eacute;rence Spatio-Temporels. 2004.
[pdf][utchistory].
~~

~~ BibItem { #Almanac; bibitem-label:"[2]"; searchterm:"Explanatory+Supplement+to+the+Astronomical+Almanac" }
P.K.\ Seidelmann.
_Explanatory Supplement to the Astronomical Almanac_.
University Science Books, 1992. [book][astroa]
~~

~~ BibItem { #taiepoch; bibitem-label:"[3]"; searchterm:"History+of+the+Bureau+International+lHeure+Guinot" }
B.\ Guinot.
_	History of the Bureau International de l'Heure_.
In "Polar Motion: Historical and Scientific Problems", ASP Conference Series, Vol. 208,
Edited by Steven Dick, Dennis McCarthy, and Brian Luzum.
ISBN: 1-58381-039-0, 2000., p.175.
[pdf][taiepoch]
~~

~~ BibItem { #byear; bibitem-label:"[4]"; searchterm:"Besselian+Year+Precession+Matrix+Based+on+IAU+Lieske" }
Jay\ Lieske.
_Precession Matrix Based on IAU_ (1976).
System of Astronomical Constants, Astronomy \& Astrophysics, Vol. 73, pages 282--284. 1979.
[wikipedia](https://en.wikipedia.org/wiki/Year#Besselian_year)
~~

~ End Bibliography

[^fn-taiepoch]: The difference TAI-UT2 was supposed to be zero on 1958-01-01Z TAI but different
    observatories used their own versions of UT2 leading to longitude errors
    of a "few 0.01s" [@taiepoch]. At 1958-01-01 TAI the &Delta;T = TT - UT1 was 31.166s &plusmn; 0.003s
    (see [historical &Delta;T](http://maia.usno.navy.mil/ser7/historic_deltat.data)),
    which equals 1958-01-01T00:00:00.018 UT1 time.
    When we calculate UT2 using the current definition [@Almanac, page 85]:

    > UT2 = UT1 + 0.022&centerdot;sin(2&pi;&tau;) - 0.012&centerdot;cos(2&pi;&tau;) - 0.006&centerdot;sin(4&pi;&tau;) + 0.007&centerdot;cos(4&pi;&tau;)

    where &tau; is the fraction of the Besselian year [@byear]:

    > &tau; = 1900.0 + (JD~TT~ - 2415020.31352) / 365.242198781

    we get 1958-01-01T00:00:00.013023922Z UT2. A difference of about 0.013s with TAI.

[ietfl]: https://www.ietf.org/timezones/data/leap-seconds.list
[dat]: http://maia.usno.navy.mil/ser7/tai-utc.dat
[astroa]: https://books.google.com/books?id=uJ4JhGJANb4C&lpg=PA87&vq=wwv&pg=PA87#v=onepage&q=wwv&f=false
[utchistory]: https://syrte.obspm.fr/journees2004/pdf/Arias2.pdf
[taiepoch]:http://articles.adsabs.harvard.edu/cgi-bin/nph-iarticle_query?2000ASPC..208..175G&amp;data_type=PDF_HIGH&amp;whole_paper=YES&amp;type=PRINTER&amp;filetype=.pdf
[WWV]: https://www.nist.gov/time-and-frequency-services/nist-radio-stations/wwv
[UTC]: std_time_astro.html
[TAI]: std_time_astro.html
[TI]: std_time_astro.html
[TIpropose]: https://syrte.obspm.fr/journees2004/pdf/McCarthy2.pdf#page=4
\/
*/
module std/time/utc

import std/num/float64
import std/num/ddouble
import std/text/parse
import std/time/timestamp
import std/time/duration
import std/time/instant

// Return the UTC timescale with the latest leap second information.
pub effect fun utc() : timescale

pub fun ".default-utc"( action : () -> <utc|e> a) : e a
  with fun utc() { ts-ti }
  action()

/*----------------------------------------------------------------------------
  Unix and NTP timestamps
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
94694399

> instant(1972,12,31,23,59,60).unix-timestamp
94694400

> instant(1973,1,1).unix-timestamp
94694400
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
pub fun unix-instant( u : float64, frac : float64 = 0.0, ts : timescale = ts-ti ) : instant
  val t    = timespan(u,frac.fraction)
  val leap = frac.truncate.int
  unix-instant(t,leap,ts)

// Create an instant from raw unix seconds since the unix epoch (1970-01-01T00:00:10 TAI)
// Use a fraction `> 1` to indicate a time inside a leap second.
pub fun unix-instant( u : int, frac : float64 = 0.0, ts : timescale = ts-ti ) : instant
  val t    = timespan(u.ddouble + frac.fraction.ddouble)
  val leap = frac.truncate.int
  unix-instant(t,leap,ts)

// Create an instant from raw unix seconds since the unix epoch (1970-01-01T00:00:10 TAI)
// and optional leap seconds to designate instants inside a leap seconds.
pub fun unix-instant( t : timespan, leap : int = 0, ts : timescale = ts-ti ) : instant
  ts-ti.instant( timestamp(t - unix2000, leap) ).use-timescale(ts)


// Get a standard Unix timestamp in fractional seconds since the Unix epoch (1970-01-01Z).
// Since Unix time stamps are ambigioous,
// instants inside a leap seconds show as occurring in the second after that.
pub fun unix-timestamp( i :instant ) : ddouble
  (unix2000 + i.timestamp-in(ts-ti).unsafe-timespan-withleap)

val unix2000 = timespan(946684800)

// Convert an NTP time in seconds since the NTP epoch (1900-01-01Z) to an instant.
// Also takes an optional `leap` argument if the NTP time is inside a leap second.
pub fun ntp-instant( ntp : ddouble, leap : int = 0 ) : instant
  ts-ntp.instant( timestamp(ntp,leap) - ntp2000 )

// Return the NTP time of an instant since the NTP epoch (1900-01-01)
// Since NTP time stamps are ambigious, times inside a leap second show
// as occurring in the second after the leap second.
pub fun ntp-timestamp( i : instant ) : ddouble
  ntp2000 + i.timestamp-in(ts-ntp).unsafe-timespan-withleap

val ntp2000 = timespan(3155673600)   // 2000-01-01 - 1900-01-01 in NTP seconds; = (100*365 + 24)*(24*3600)  (24 leap days)

/*----------------------------------------------------------------------------
  Timescale creation
----------------------------------------------------------------------------*/

// Create a new time scale based on UTC seconds with a given `name`
// and a leap second table.
pub fun utc-timescale( name : string, leaps : leaps-table ) : timescale
  fun from-tai(tai:duration)       { utc-from-tai(leaps,tai) }
  fun to-tai(utc:timestamp)        { utc-to-tai(leaps,utc) }
  fun seconds-in-day(utc:timestamp){ utc-seconds-in-day(leaps,utc) }
  fun to-mjd(utc:timestamp,tzdelta:timespan){ utc-to-mjd(leaps,utc,tzdelta) }
  fun from-mjd(days:int,frac:ddouble){ utc-from-mjd(leaps,days,frac) }
  timescale(
    name,
    from-tai,
    to-tai,
    "UTC",
    Just(seconds-in-day),
    Just(to-mjd),
    Just(from-mjd)
  )

// The UTC time scale.
pub fun ts-utc-create( leaps : leaps-table ) : timescale
  utc-timescale( "UTC", leaps )

// [Unix](https://en.wikipedia.org/wiki/Unix_time) time scale is equal
// to the UTC time scale (`ts-utc`).
pub fun ts-unix-create( leaps : leaps-table ) : timescale
  ts-utc-create(leaps) // utc-timescale( "UNIX", leaps )


// [NTP](https://en.wikipedia.org/wiki/Network_Time_Protocol) time scale is equal
// to the UTC time scale (`ts-utc`).
pub fun ts-ntp-create( leaps : leaps-table ) : timescale
  ts-utc-create(leaps)


/* The [TI] (_International Time_) time scale with a 2000-01-01Z UTC epoch.
This is the default time scale used in this library. It was a
[proposed][TIpropose] time scale at the 2004 ITU-R meeting as a replacement of UTC
without future leap seconds. In this library, we define TI to match
exactly UTC up to the compiler release date (currently 2017) but ignore any
possible future leap seconds after that date. This is the preferred time scale
in this library as it guarantees deterministic time calculations for any
future date, i.e. before 2017-01-01Z, TI == UTC, while after that, TI == TAI - 37s.
*/
pub val ts-ti : timescale = utc-timescale( "", leaps-table-ti )

// [Unix](https://en.wikipedia.org/wiki/Unix_time) time scale based on Unix seconds.
// It equals the `ts-ti` time scale.
pub val ts-unix : timescale = ts-ti // utc-timescale( "UNIX-TI", leaps-table-ti )

// [NTP](https://en.wikipedia.org/wiki/Network_Time_Protocol) time scale.
// It equals the `ts-ti` time scale.
pub val ts-ntp : timescale = ts-ti // utc-timescale( "NTP-TI", leaps-table-ti )


// Create a new smooted leap second time scale with an optional period during
// which smoothing takes place. This is 1000s for `ts-utc-sls`.
fun utc-sls-timescale( name : string, leaps : leaps-table, smooth : timespan = 1000.timespan ) : timescale
  fun from-tai(tai:duration) { utc-sls-from-tai(leaps,smooth,tai) }
  fun to-tai(utc:timestamp)  { utc-sls-to-tai(leaps,smooth,utc) }
  timescale(
    name,
    from-tai,
    to-tai,
    "UTC-SLS"
  )


// Create a new UTC-SLS time scale from a provided leap second table `leaps`.
// Implements a UTC time scale except without ever showing leap seconds.
// UTC-SLS is equivalent to UTC except for the last 1000 seconds of a day where
// a leap second occurs. On such day, the leap second time step (positive or negative)
// is distributed over the last 1000 seconds of the day. On the full hour, UTC
// and UTC-SLS are equal again.
//
// This is a recommended time scale to use for time stamps or communication
// with other services since it avoids any potential trouble
// with leap seconds while still being quite precise.
// See also: <https://www.cl.cam.ac.uk/~mgk25/time/utc-sls>.
//
// You can create a UTC-SLS time scale based on the latest IETF leap second
// data using [`cal-utc-sls-load`](std_time_download.html#cal_utc_sls_load).
pub fun ts-utc-sls-create( leaps : leaps-table ) : timescale
  utc-sls-timescale( "UTC-SLS", leaps )

// TI time scale with smoothed leap seconds.\
// Implements a TI time scale (`ts-ti`) except without ever showing leap seconds.
// TI-SLS is equivalent to TI except for the last 1000 seconds of a day where
// a leap second occurs. On such day, the leap second time step (positive or negative)
// is distributed over the last 1000 seconds of the day. On the full hour, TI
// and TI-SLS are equal again.
pub val ts-ti-sls : timescale = utc-sls-timescale( "TI-SLS", leaps-table-ti )

/*----------------------------------------------------------------------------
  UTC to TAI conversion
----------------------------------------------------------------------------*/

fun utc-to-tai( leaps : leaps-table, utc : timestamp ) : duration
  val dtai = utc-to-delta-tai(leaps,utc)
  (utc + dtai).unsafe-duration  // ok, because TAI seconds now


/* Converting TAI time back to UTC is not straigtforward as
we need to estimate UTC at first as the leap table goes from UTC-to-TAI.
Moreover, we need to detect if we crossed over a leap second, or are
inside a leap step. Take for example the leap second of 2006-01-01
to +33. This looks like:

UTC-to-TAI-delta:                        ... +32   |   +33 ...

UTC timestamp          189388799   189388799+1  189388800
UTC 2005-12-31T23:59:     59          60   leap    00
                 ---------|-----------|xxxxxxxxxxxx|-------------
                          |           |            |
TAI 2006-01-01T00:00:     31          32           33
TAI timestamp:         189388831   189388832    189388833

In the code below, suppose `tai` is `189388832.5`.
Then we estimate at first the delta `dtai0` to +33, so our
estimate `utc0` is `189388799.5` (just before the leap step!).
We then use `utc0` to get delta-TAI at that time, +32 and
set the difference `diff` to `(33-32) == 1` -- the time of the
leap second we crossed. (usually, `diff` is zero of course).
If the difference is positive, we then check if `utc0` is in the
leap period itself (instead of before it): that is the case if
the delta-TAI at `utc0+diff` equals `dtai0` again.
If we are not in a leap second, the final utc time is the
estimate plus the time of the leap period if we crossed over it, `utc0+diff`.
Otherwise, the same holds but we need to add `diff` as leap seconds,
in the example ending up as `189388799.5+1`.
*/
fun utc-from-tai( leaps : leaps-table, tai-since : duration ) : timestamp
  val tai   = tai-since.timestamp
  // take tai==utc for an initial estimate
  val dtai0 = utc-to-delta-tai(leaps,tai)
  val utc0  = tai - dtai0
  // get delta-tai at the estimate
  val dtai1 = utc-to-delta-tai(leaps,utc0)
  // check if we crossed over a leap-second moment
  val diff  = (dtai0 - dtai1)
  // `inleap` is `True` if this time falls in the leap second gap itself
  val hasgap = (diff.round-to-prec(3).is-pos)  // round to disregard a difference due to `drift`
  val inleap = (hasgap && utc-to-delta-tai(leaps,utc0 + diff) != dtai1)

  if !inleap then utc0 + diff else utc0.add-leap-seconds(diff)

// Return `Just(start,diff)` if a leap second occurred in the given day
// (`days` after 2000-01-01) with the start time and leap second gap (`diff`)
fun utc-leap-in-day( leaps : leaps-table, days : int ) : maybe<(timestamp,timespan)>
  // check if there is a leap second in the day
  val utc0  = timestamp-days(days)
  val utc1  = timestamp-days(days + 1)
  val la1   = utc-to-leap-adjust(leaps,utc1)
  if la1.utc-start < utc0 then return Nothing

  // if so, find the start and the timespan of the leap second
  val dtai0 = utc-to-delta-tai(leaps,utc0)
  val dtai1 = la1.delta-tai(utc1)
  val diff  = (dtai1 - dtai0).round-to-prec(3)  // round to not take drift into account
  Just((la1.utc-start, diff))

// The UTC seconds in a day
fun utc-seconds-in-day( leaps : leaps-table, utc : timestamp ) : timespan
  match(utc-leap-in-day(leaps,utc.days))
    Nothing -> solar-secs-per-day
    Just((_,diff)) -> solar-secs-per-day + diff


// Return the modified julian day since 2000-01-01 taking leap seconds into
// account that happen any time during the day
fun utc-to-mjd( leaps : leaps-table, utc : utc-timestamp, tzdelta : timespan ) : ddouble
  val (days1,secs1) = (utc + tzdelta).days-seconds
  val frac = match(utc-leap-in-day(leaps,utc.days))
               Nothing -> (secs1 + utc.leap.ddouble) / solar-secs-per-day
               Just((start,diff)) ->
                 val secs-in-day = solar-secs-per-day + diff
                 val secs = if utc < start then secs1 + utc.leap.ddouble
                                             else secs1 + diff
                 (secs / secs-in-day)

  (days1.ddouble + frac)

// Return UTC from the modified julian day since 2000-01-01 taking leap seconds into
// account that happen any time during the day
fun utc-from-mjd( leaps : leaps-table, days : int, frac : ddouble ) : utc-timestamp
  match(utc-leap-in-day(leaps,days))
    Nothing -> timestamp-days( days, frac*solar-secs-per-day)
    Just((start,diff)) ->
      val secs = frac*(solar-secs-per-day + diff)
      val utc  = timestamp-days(days,secs)
      if utc < start then utc
      elif utc > start + diff then utc - diff
      else (utc - diff).add-leap-seconds(diff)



/*----------------------------------------------------------------------------
  UTC-SLS to TAI conversion
----------------------------------------------------------------------------*/
fun utc-sls-leap-in-day( leaps : leaps-table, smooth : timespan, utc : utc-timestamp ) : maybe<(timestamp,timestamp,timespan,timespan)>
  match(utc-leap-in-day(leaps, utc.days))
    Nothing -> Nothing // on non-leap days, utc-sls == utc
    Just((start,diff)) ->
      val smooth-start = (start - smooth) + diff
      val smooth-end   = start.add-leap-seconds(diff)
      if utc <= smooth-start then Nothing // before the smoothing
      elif utc > smooth-end then Nothing // after the leap second
      else
        val smooth-total = smooth-end.timespan-noleap - smooth-start.timespan-noleap
        val dt = (utc.unsafe-timespan-withleap - smooth-start.timespan-noleap)
        Just((start,smooth-start,smooth-total,dt))



fun utc-sls-from-tai( leaps : leaps-table, smooth : timespan, tai-since : duration ) : timestamp
  val utc = utc-from-tai(leaps,tai-since)
  match(utc-sls-leap-in-day(leaps, smooth, utc))
    Nothing -> utc // outside smooth zone: utc-sls == utc
    Just((_start,smooth-start,smooth-total,dt)) ->
      val frac = dt / smooth
      smooth-start + (frac * smooth-total)


fun utc-sls-to-tai( leaps : leaps-table, smooth : timespan, sls : utc-timestamp ) : duration
  val utc = match(utc-sls-leap-in-day(leaps,smooth,sls))  // sls ~ utc
    Nothing -> sls  // utc == utc-sls outside smooth zone
    Just((start,smooth-start,smooth-total,dt)) ->
      val frac  = dt / smooth-total
      val utc0  = smooth-start + (frac * smooth)
      if utc0 <= start then utc0 else
        val ldiff = utc0.timespan-noleap - start.timespan-noleap
        start.add-leap-seconds(ldiff)


  utc-to-tai(leaps,utc)


// -----------------------------------------------------------
// Leap second table
// -----------------------------------------------------------

// `:utc-timestamp` is UTC time since 2000-01-01
pub alias utc-timestamp = timestamp

// A leap second table describes when UTC leap seconds occur.
abstract struct leaps-table(
  pub expire : instant,
  // List of adjustments, ordered from most recent to oldest (decreasing `utc-start`)
  // Each entry gives the start instant and integer leap second adjustment.
  adjusts: list<leap-adjust>
)

// Leap second adjustments. For an instant `i` after `start`:\
// ``TAI-offset = offset + (drift * days(i - drift-start))``
abstract struct leap-adjust(
  utc-start  : utc-timestamp,  // start time in NTP seconds since epoch (2000-01-01)
  offset     : timespan,       // base offset
  drift-start: utc-timestamp = timestamp0,   // start of drift adjustment
  drift      : ddouble = zero
)

val leaps-table0 = Leaps-table(epoch,[])
val zero : leap-adjust = Leap-adjust(timestamp0,timespan0,timestamp0,zero)

fun is-zero( la : leap-adjust ) : bool
  la.offset.is-zero && la.drift.is-zero

pub fun show( l : leap-adjust ) : string
  [l.utc-start.show,": offset: ",l.offset.show,", drift-start: ",l.drift-start.show,", drift: ",l.drift.show].join

pub fun show( t : leaps-table ) : string
  t.adjusts.map(show).unlines

/*----------------------------------------------------------------------------
  delta-TAI == TAI - UTC
----------------------------------------------------------------------------*/

val utc1958 = timestamp(-1325376000) // 2000-01-01 - 1958-01-01 in NTP seconds

fun utc-to-delta-tai( leaps : leaps-table, utc : utc-timestamp ) : timespan
  utc-to-leap-adjust(leaps,utc).delta-tai(utc)

fun utc-to-leap-adjust( leaps : leaps-table, utc : utc-timestamp ) : leap-adjust
  if utc < utc1958
   then zero
   else find-leap-adjust(utc,leaps.adjusts)

fun find-leap-adjust( utc : utc-timestamp, leaps : list<leap-adjust> ) : leap-adjust
  match leaps
    Nil -> zero // should never happen
    Cons(la,earlier) ->
      if la.utc-start > utc
       then find-leap-adjust(utc,earlier)
       else la


// Get the leap-second adjustment _delta-tai_ (= TAI - UTC).
// Needs the timestamp to handle _drift_.
fun delta-tai( la : leap-adjust, utc : utc-timestamp ) : timespan
  if la.drift.is-zero then la.offset else
    // pre 1972 is a rubber leap second
    val days = (utc.timespan-noleap - la.drift-start.timespan-noleap) / solar-secs-per-day
    la.offset + (la.drift * days)


/*----------------------------------------------------------------------------
  Default leap tables
----------------------------------------------------------------------------*/

// Leap second table upto (but not including) 1972-01-01 UTC
pub val leaps-table-pre1972 : leaps-table =
  parse-leap-seconds-dat( default-leap-seconds-pre72 )

// Default TI leaps table has leap second information up to the compiler release (currently `leaps-table-y2017`).
pub val leaps-table-ti : leaps-table =
  val post72 = parse-leap-seconds( default-ietf-leap-seconds )
  post72.extend(leaps-table-pre1972)

// Leap second table up to 2017-01-01Z.
pub val leaps-table-y2017 : leaps-table = leaps-table-ti.upto(536544000.timestamp)  // == instant(2017,1,1).timestamp(ts-ti)

pub fun extend(leap1 : leaps-table, leap2 : leaps-table) : leaps-table
  match leap1.adjusts.reverse
    Nil -> leap2
    Cons(la,_) ->
      leap1( adjusts = leap1.adjusts ++ leap2.upto(la.utc-start - 1.timespan).adjusts )


fun upto( lt : leaps-table, end : utc-timestamp ) : leaps-table
  lt( adjusts = lt.adjusts.drop-while(fn(la) { la.utc-start > end }) )

// Get a list of leap second steps in a triple, NTP start time, offset just before, and the new offset at that time,
// the base offset, the drift start date and the drift rate.
pub fun get-leap-steps( table : leaps-table = leaps-table-ti ) : list<(utc-timestamp,timespan,timespan,(timespan,utc-timestamp,ddouble))>
  val adjusts = table.adjusts.reverse
  zip(Cons(zero,adjusts),adjusts).map fn(las)
    val start = las.snd.utc-start
    val ofs1 = las.fst.delta-tai(start)
    val ofs2 = las.snd.delta-tai(start)
    (start,ofs1,ofs2,(las.snd.offset,las.snd.drift-start,las.snd.drift))
  

// -----------------------------------------------------------
// Parsing UTC leap second tables
// -----------------------------------------------------------

// Parse a leap second table from text `leaps` in the  [IETF leap second](https://www.ietf.org/timezones/data/leap-seconds.list)
// file format.

pub fun parse-leap-seconds( leaps : string ) : leaps-table
  val adjusts = leaps.lines.flatmap-maybe(parse-leap).reverse
  val expire = parse-leap-expire(leaps,adjusts)
  Leaps-table(expire,adjusts)

fun parse-leap( line : string ) : maybe<leap-adjust>
  if (line.trim-left(" ").starts-with("#").bool)
   then Nothing
   else line.slice.parse(pleap).maybe

fun pleap() : parse leap-adjust
  whitespace0()
  val ntpsecs = pint()
  whitespace()
  val adjust = pint()
  // trace("leap entry: " ++ adjust.show ++ " - " ++ ntpsecs.show )
  Leap-adjust(timestamp(ntpsecs.timespan - ntp2000), adjust.timespan, timestamp0, timespan0)

/*
pub fun parse-leap-seconds( leaps : string ) : leaps-table
  // get leap second adjustments
  val adjusts = leaps.find-all(rxleap).map fn(cap)
    val ntpsecs = cap.groups[1].parse-int-default(0)
    val adjust  = cap.groups[2].parse-int-default(0)
    // trace("leap entry: " ++ adjust.show ++ " - " ++ ntpsecs.show )
    Leap-adjust(timestamp(ntpsecs.timespan - ntp2000), adjust.timespan, timestamp0, timespan0)
  }.reverse

  val expire = parse-leap-expire(leaps,adjusts)
  val table = Leaps-table(expire,adjusts)
  table

val rxleap   = regex(r"^[ \t]*(\d+)[ \t]+(\d+)[ \t]*(?:#.*)?$",multiLine=True)
*/

// IETF leap second data valid until 2017-06-28
val default-ietf-leap-seconds = r"
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

val jd-epoch-shift     = 2400000.5.timespan  // JD to MJD
val mjd-epoch-shift    = 51544.timespan // 2000-01-01Z - 1858-11-17Z modified julian date epoch

// Parse the standard UTC leap second adjustment file in the "old" .dat format as
// in <http://maia.usno.navy.mil/ser7/tai-utc.dat>, where entries have the shape
// ````
// 1961 JAN  1 =JD 2437300.5  TAI-UTC=   1.4228180 S + (MJD - 37300.) X 0.001296 S
// ````
// which specifies the start time (`JD 2437300.5`), new TAI-UTC offset
// (`1.4228180`s), and the drift, starting at `37300` MJD of 0.001296s per day.
// Lines that start with ``#`` are comments. As an extension you can have an
// expiration date on a line that starts with ``#@`` followed by seconds since
// the NTP epoch (1900-01-01). Just as in a standard IETF leap second file.
pub fun parse-leap-seconds-dat( leaps : string  ) : leaps-table
  val adjusts = leaps.lines.flatmap-maybe(parse-taiadjust).reverse
  val expire = parse-leap-expire(leaps,adjusts)
  Leaps-table(expire,adjusts)

fun parse-leap-expire( leaps : string, adjusts : list<leap-adjust>) : instant
  // get expiration date
  val la-final    = adjusts.head.default(zero)
  val utc-expires = leaps.lines.flatmap-maybe(parse-expire)
  val utc-expire  = utc-expires.head.default(la-final.utc-start + (182*isolar-secs-per-day).timespan)  // default: 6 months after last leap second
  ts-tai.instant( utc-expire - ntp2000 + la-final.offset ) // interpret as TAI to avoid recursion.

// val rxexpire = regex(r"^[ \t]*#@[ \t]*(\d+)[ \t]*(?:#.*)?$", multiLine=True)
fun parse-expire( line : string ) : maybe<timestamp>
  if (line.trim-left(" ").starts-with("#@").bool)
   then line.slice.parse(pexpire).maybe
   else Nothing

fun pexpire() : parse timestamp
  whitespace0()
  pstring("#@")
  whitespace0()
  val ntpex = pddouble()
  timestamp(ntpex.timespan - ntp2000)

fun parse-taiadjust( line : string ) : maybe<leap-adjust>
  line.slice.parse-eof(ptaiadjust).maybe

fun ptaiadjust() : parse leap-adjust
  whitespace0()
  many1{ none-of("=") }
  pstring("=JD")
  whitespace0()
  val mjd = pddouble() - jd-epoch-shift
  whitespace0()
  pstring("TAI-UTC=")
  whitespace0()
  val ofs = pddouble()
  many1{ no-digit() }
  val dmjd = pddouble()
  many1{ no-digit() }
  val drift = pddouble()
  whitespace0()
  pstring("S")
  whitespace0()
  // round to always start on a whole second
  val start  = ((mjd - mjd-epoch-shift)*solar-secs-per-day).round.timestamp
  val dstart = ((dmjd - mjd-epoch-shift)*solar-secs-per-day).round.timestamp
  // trace("pre72 start=" + start.show + ", ofs: " + ofs.show + ", drift: " + drift.show )
  Leap-adjust( start, ofs, dstart, drift )

// TAI leap second adjustments for dates before 1972-01-01Z are linear interpolations.
// TAI started in 1958-01-01Z. The initial official UTC time step in 1961-01-01Z was 1.422818s and before that there
// were small steps of 20ms. See Explanatory Supplement to the Astronomical Almanac, 1992 edition, pages 86--87.
// In 1958, the supplement remarks that WWC operated at an offset of _about_ -100e-10, we
// change it to -85e-10 to end up with TAI-UTC == 0 at 1958-01-01.
// (without a rate change it is a negative -0.0472380s).
// Note the JD dates are at 0.29167 as the time steps were usually at 19:00h instead of midnight.
val default-leap-seconds-pre72 = r"
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

/*
fun parse-leap-expire( leaps : string, adjusts : list<leap-adjust>) : instant
val la-final    = adjusts.head.default(zero)
val utc-expire  = leaps.find(rxexpire).map fn(cap)
    val ntpex = cap.groups[1].parse-int-default(ntp2000.int)
    timestamp(ntpex.timespan - ntp2000)
  }.default(la-final.utc-start + (182*isolar-secs-per-day).timespan)  // default: 6 months after last leap second
  // trace("expire: " + utc-expire.ts-show)
  ts-tai.instant( utc-expire - ntp2000 + la-final.offset ) // interpret as TAI to avoid recursion.

val rxexpire = regex(r"^[ \t]*#@[ \t]*(\d+)[ \t]*(?:#.*)?$", multiLine=True)
*/

/*
pub fun parse-leap-seconds-dat( leaps : string  ) : leaps-table
  val adjusts = leaps.find-all(rxtaiadjust).map( fn(cap)
    val mjd    = cap.groups[1].parse-ddouble.default(jd-epoch-shift) - jd-epoch-shift
    val ofs    = cap.groups[2].parse-ddouble.default(zero)
    val dmjd   = cap.groups[3].parse-ddouble.default(zero)
    val drift  = cap.groups[4].parse-ddouble.default(zero)
    // round to always start on a whole second
    val start  = ((mjd - mjd-epoch-shift)*solar-secs-per-day).round.timestamp
    val dstart = ((dmjd - mjd-epoch-shift)*solar-secs-per-day).round.timestamp
    // trace("pre72 start=" + start.show + ", ofs: " + ofs.show + ", drift: " + drift.show )
    Leap-adjust( start, ofs, dstart, drift )
  }).reverse
  val expire = parse-leap-expire(leaps,adjusts)
  Leaps-table(expire,adjusts)

val rxtaiadjust = regex(r"^ *\d[^=]*=JD (\d+\.\d+) *TAI-UTC= *(-?\d+(?:\.\d*)?)[^\d]+(\d+(?:\.\d*)?)[^\d]+(\d+\.\d+) *S *$", multiLine=True)
*/
