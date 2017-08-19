  /*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* The UT1 time scale and calendar.

The UT1 time scale is based on the rotation of the earth where one rotation is 86400 seconds.
The exact UT1 time can only be determined by measuring the Earth's rotation angle very precisely.
The International Earth Rotation Service
(IERS) issues [bulletin A](https://www.iers.org/IERS/EN/Publications/Bulletins/bulletins.html)
containing accurate (UT1 - UTC) deltas.

The library provides the `cal-ut1-load` function to automatically cache and download the latest USNO
bulletin A information (``finals2000A`` ``all`` and ``daily``) to calculate UT1 very accurately from
1972 until about 3 months from now (using predicted values). By default it downloads daily data
every week and full data about every 8 weeks. For dates outside this range, the library uses its builtin
tables and approximations:

* monthly determined tables for any observed date (usually > 1973 up to the compiler release (2016))
* quarterly prediction tables for dates after that (2016) up to about 10 years (2025)
* yearly historic tables for dates >= 1750 and before 1973
* fitted parabolic approximations for dates between -500 and 2150; (Espenak & Meeus 2006)
* generic parabolic approximation for dates < -500 and dates > 2150 (Morrison and Stephenson, 2004.)

You can also use `cal-ut1-approx` to not download any recent data and always use approximated values.

Usage:
```
val cal-ut1 = cal-ut1-load()
val i = now()
i.time().show.println              // in UTC
i.time(cal=cal-ut1).show.println   // in UT1
```
*/
module std/time/ut1

import std/num/double
import std/async
import std/os/path
import std/num/ddouble
import std/text/regex

import std/time/timestamp
import std/time/duration
import std/time/instant
import std/time/utc
import std/time/download

import std/time/astro
import std/time/calendar
import std/time/time

// The default UT1 (Universal time 1) calendar.
// Uses builtin approximation of UT1 time as described in `deltat`.
// To calculate UT1 more precisely, use `cal-ut1-load` (or `cal-ut1-create`).
public val cal-ut1-approx = cal-ut1-create()

// Create a UT1 (Universal time 1) calendar.
// Takes an optional `override` parameter to calculate &Delta;T (=TT - UT1)
// more precisely. The `override` functions should return `Nothing` for instants
// outside its range so the builtin methods can be used in that case (see `deltat`).
// (Note, you can get the UT1 `:timescale` from the returned calendar using `std/time/calendar/timescale`)
public fun cal-ut1-create( override : (i : instant) -> maybe<duration> = deltat-none ) {
  iso-calendar( ts-ut1(override) )
}

val usno-url = "http://maia.usno.navy.mil/ser7/"  // "http://toshi.nofs.navy.mil/ser7/"

// Create a new UT1 calendar using [IERS bulletin A](http://www.usno.navy.mil/USNO/earth-orientation/eo-products/weekly)
// data for precise UT1 calculation. Uses linear interpolation between daily UT1-UTC values.
// This is a convenience function that composes `load-iers-ut1` and `cal-ut1-create`.
//
// By default loads the daily and full data from the files `basename` (= `"config/finals2000A"` in the application directory).
// If the files are not present, they will be downloaded from `url`. The default `url` is:\
// <http://maia.usno.navy.mil/ser7>\
// Set the `url` to the empty string to suppress automatic downloading.
// If the ``.daily`` file is older than `download-delay` (=`1.weeks`) the file is automatically refreshed from `url`.
// The `download-timeout` (=`5.seconds`) gives the maximum time for downloading to finish (daily file is about 40kb).
// If the ``.all`` file is older than `8*download-delay` the file is automatically refreshed from `url`.
// The `4*download-timeout` (=`20.seconds`) gives the maximum time for downloading to finish (``.all`` file is about 3mb).
// Set the optional parameter `verbose` to `False` to suppress trace messages on downloads or errors.
public fun cal-ut1-load( basename : string = "finals2000A",
                         baseurl  : string = usno-url,
                         download-delay: duration = 1.weeks,
                         download-timeout: duration = 5.seconds,
                         verbose : bool = True
                        ) : <io,async> calendar
{
  cal-ut1-create( load-iersA( basename, baseurl, download-delay, download-timeout, verbose ) )
}

// Load [IERS bulletin A](http://www.usno.navy.mil/USNO/earth-orientation/eo-products/weekly)
// information for precise UT1 determination. Loads and caches both the _daily_ and _all_
// data. Downloads a fresh daily file about every week (`download-delay`) with a given `download-timeout` (=`5`s).
// The full data is refreshed only every `8*download-delay` (=`8.weeks`). The `basename` gives
// the basic file name `finals2000A` and the `baseurl` the download URL
// (= <http://maia.usno.navy.mil/ser7/>).
// Set the `baseurl` to the empty string to suppress automatic downloading. The files are cached
// in the `config/` directory of the application installation directory.
// Set the optional parameter `verbose` to `False` to suppress trace messages on downloads or errors.
public fun load-iersA( basename : string = "finals2000A",
                       baseurl  : string = usno-url,
                       download-delay: duration = 1.weeks,
                       download-timeout: duration = 5.seconds,
                       verbose : bool = True
                     )
{
  val fname-all    = basename + ".all"
  val finals-all   = load-iersA-all( fname-all, baseurl + fname-all,
                                     duration(download-delay.seconds * 8.ddouble),
                                     duration(download-timeout.seconds * 4.ddouble), verbose )
  val fname-daily  = basename + ".daily"
  val finals-daily = load-iersA-daily( fname-daily, baseurl + fname-daily,
                                       download-delay, download-timeout, verbose )
  (fun(i:instant) {
    match(finals-daily(i)) {
      Nothing -> finals-all(i)
      just    -> just
    }
  })
}

// Load [IERS bulletin A](http://www.usno.navy.mil/USNO/earth-orientation/eo-products/weekly)
// information for precise UT1 determination. Loads the full bulletin A data since 1973
// with about 1 year of predictions. Uses linear interpolation between daily UT1-UTC values.
// By default loads the file `fname` (= `"config/finals2000A.all"` in the application directory).
// If the file is not present, it will be downloaded from `url`. The default `url` is:\
// <http://maia.usno.navy.mil/ser7/finals2000A.all>\
// Set the `url` to the empty string to suppress automatic downloading.
// If the file is older than `download-delay` (=`4.weeks`) the file is automatically refreshed from `url`.
// The `download-timeout` (=`20.seconds`) gives the maximum time for downloading to finish (the
// full data is about 3mb in size).
// Set the optional parameter `verbose` to `False` to suppress trace messages on downloads or errors.
public fun load-iersA-all( fname : string = "",
                          url   : string = "auto",
                          download-delay : duration   = 8.weeks,
                          download-timeout : duration = 20.seconds,  // 3mb file!
                          verbose : bool = True
                         ) : <io,async> ((i:instant) -> maybe<duration>)
{
  load-latest-iersA( (fname || "finals2000A.all").path, url, download-delay, download-timeout, verbose )
}


// Load the daily [IERS bulletin A](http://www.usno.navy.mil/USNO/earth-orientation/eo-products/weekly)
// information for precise UT1 determination. The daily file contains information for the past 90 days
// with 90 days of predictions. This file is about 40kb and much smaller than the full data.
// Uses linear interpolation between daily UT1-UTC values.
// By default loads the file `fname` (= `"config/finals2000A.daily"` in the application directory).
// If the file is not present, it will be downloaded from `url`. The default `url` is:\
// <http://maia.usno.navy.mil/ser7/finals2000A.daily>\
// Set the `url` to the empty string to suppress automatic downloading.
// If the file is older than `download-delay` (=`1.weeks`) the file is automatically refreshed from `url`.
// The `download-timeout` (=`5.seconds`) gives the maximum time for downloading to finish.
// Set the optional parameter `verbose` to `False` to suppress trace messages on downloads or errors.
public fun load-iersA-daily( fname : string = "",
                              url   : string = "auto",
                              download-delay : duration   = 1.weeks,
                              download-timeout : duration = 5.seconds, // ~37kb file
                              verbose : bool = True
                             ) : <io,async> ((i:instant) -> maybe<duration>)
{
  load-latest-iersA( (fname || "finals2000A.daily").path, url, download-delay, download-timeout, verbose )
}

fun load-latest-iersA( fname : path, url   : string,
                        download-delay : duration, download-timeout : duration,
                        verbose : bool = True
                       ) : <io,async> ((i:instant) -> maybe<duration>)
{
  val xfname = if (fname.dirname.empty?) then appdir() + path("config") + fname else fname
  val xurl   = if (url=="auto") then usno-url + fname.basename else url
  val dtt    = load-latest( xfname, xurl, parse-iersA, fun(dtt:dut-table){ Just(dtt.expire) },
                            download-timeout=download-timeout,download-delay=download-delay,
                            error-prefix="load IERS A data (" + fname.basename + ")",
                            verbose=verbose
                          )
  val ts-utc = ts-utc-load(verbose=verbose)
  (fun(i:instant){ dut-deltat(ts-utc, dtt,i) })
}


//-----------------------------------------------------------------
// Data from a finals2000A file.
//-----------------------------------------------------------------

// A table of DUT (= UT1 - UTC) entries parsed from a IERS finals2000A file.
struct dut-table(
  expire: instant,
  start : double,   // start mjd
  end   : double,   // end mjd
  duts  : vector<double> // one entry per day
)

// Calculate &Delta;T from a DUT table.
// Interpolates linearly between daily entries.
fun dut-deltat( ts-utc : timescale, dtt : dut-table, i : instant ) : maybe<duration> {
  try(Nothing) {
    val utci = i.use(ts-utc)
    val mjd = utci.mjd(ts-utc).double
    if (mjd < dtt.start || mjd >= dtt.end) then Nothing else {
      val idx = (mjd - dtt.start).trunc.int
      // interpolate
      val dut1= dtt.duts[idx]
      val dut2= if (idx + 1 >= dtt.duts.length) then dut1 else dtt.duts[idx+1]
      val dut3= if (dut1 - 0.5 < dut2 && dut1 + 0.5 > dut2) then dut2
                elif (dut1 + 0.5 < dut2 && dut1 + 1.5 > dut2) then dut2 - 1.0 // a leap second was added
                elif (dut1 - 0.5 > dut2 && dut1 - 1.5 < dut2) then dut2 + 1.0 // a leap second was subtracted
                else dut1 // a jump outside a reasonable range; don't interpolate
      val dut = dut1 + ((dut3 - dut1) * mjd.fraction)
      // trace("dut interpolate: " + dut1.show + ", and " + dut3.show + ", to " + dut.show)
      // calculate deltaT (TT - UT1) from DUT (=UT1 - UTC)
      val dt  = (i.timestamp(ts-tt) - dut.ddouble) - utci.timestamp(ts-utc).seconds // include leap seconds!
      // trace("dut found: mjd: " + mjd.show + ", dut: " + dut.show + ", dt: " + dt.show)
      Just( unsafe-duration(dt) )
    }
  }
}


// Parse a ``finals2000A.all`` file into a DUT table for precise &Delta;T calculation (using `dut-deltat`)
// See <http://maia.usno.navy.mil/ser7/finals2000A.all>, or <http://toshi.nofs.navy.mil/ser7/finals2000A.all>
fun parse-iersA( s : string ) : maybe<dut-table> {
  val start   = match( s.find(rx-finals-start) ) {
                  Nothing   -> 41684.0
                  Just(cap) -> cap.groups[1].parse-double.maybe(41684.0)
                }
  val mbexp   = s.find(rx-finals-expire) // first prediction date
  val caps    = s.find-all(rx-finals-dut)
  val entries = caps.map-acc(fun(cap){ cap.groups[1].parse-double.maybe(0.0) }).vector

  val end     = start + entries.length.double
  val expire  = (match(mbexp) {
                  Just(cap) -> cap.groups[1].parse-double.maybe(end - 365.0)
                  Nothing   -> end - 365.0  // guess one year early
                }).instant-at-mjd(ts-ti)
  // trace("finals200A expire: " + expire.time.show)
  // trace("entries: " + entries.list.map(fun(d){ show(d) }).join("\n"))
  // trace("finals: " + entries.length.show + ", start: " + start.show + ", end: " + end.show )
  Just(Dut-table(expire,start,end,entries))
}
val rx-finals-dut = regex(@"^.{57}[IP]([\-\+ ]\d+(?:\.\d+))", multiline=True)
val rx-finals-start = regex(@"^(?:\d\d| \d).{4} (\d+(?:\.\d+)?)", multiline=True)
val rx-finals-expire= regex(@"^(?:\d\d| \d).{4} (\d+(?:\.\d+)?) P", multiline=True)

// recurse using an accumulator using constant heap space
private fun map-acc(xs : list<a>, f : a -> b, acc : list<b> = []) : list<b> {
  match(xs) {
    Cons(x,xx) -> map-acc(xx,f,Cons(f(x),acc))
    Nil -> reverse(acc)
  }
}



// -----------------------------------------------------------
// UT1 timescale; UT1 = TT - DeltaT
// -----------------------------------------------------------

fun tai-to-ut( tai : duration, override : (i:instant) -> maybe<duration> ) : timestamp {
  val i  = tai.instant-since-epoch
  val dt = deltat(i,override)
  // trace("tai-to-ut: dt: " + dt.show )
  (i - dt).timestamp(ts-tt)
}

fun ut-to-tai( t : timestamp, override : (i : instant) -> maybe<duration> ) : duration {
  val i   = unsafe-instant(t,ts-tt)   // approximately tt
  val i1  = i + deltat(i,override)
  val i2  = i + deltat(i1,override)
  // trace("ut-to-tai: dt1: " + deltat(i0).show + ", dt2: " + deltat(i1).show)
  i2.since-epoch
}

// Takes an optional `override` parameter to calculate &Delta;T (=TT - UT1)
// more precisely. The `override` functions should return `Nothing` for instants
// outside its range so the builtin methods can be used in that case (see `deltat`)
fun ts-ut1( override : (i : instant) -> maybe<duration> = deltat-none ) : timescale {
  timescale("UT1",
    fun(i) { tai-to-ut(i,override) },
    fun(t) { ut-to-tai(t,override) }
  )
}


// -----------------------------------------------------------
// Calculate delta-t
// -----------------------------------------------------------

fun deltat-none(i : instant) : maybe<duration> {
  Nothing
}

/* Calculate &Delta;T (= TT - UT1);
If the `override` function returns `Nothing`, it tries in order:

* monthly determined tables for any observed date (usually > 1973 up to now (2016))
* quarterly prediction tables for dates > now (2016) up to about 10 years (2025)
* yearly historic tables for dates >= 1750
* fitted parabolic approximations for dates between -500 and 2150; (Espenak & Meeus 2006)
* generic parabolic approximation for dates < -500 and dates > 2150 (Morrison and Stephenson, 2004.)

*/
public fun deltat( i : instant, override : (i : instant) -> maybe<duration> = deltat-none ) : duration {
  val dt = match(override(i)) {
            Just(dt) -> dt
            Nothing  -> {
              val y = i.time(cal=cal-tt).year-frac.double.round-to-prec(4)
              deltat-builtin(y).duration
            }
          }
  // trace("deltat: year: " + y.show-ddouble + ", dt: " + dt.show)
  dt
}

fun deltat-builtin( y : double,
                   determined : deltat-table = deltat-table-determined(),
                   predicted  : deltat-table = deltat-table-predicted() ) : double
{
  // A date after (about) 1986 ?  (determined starts usually at 1973.0833333)
  if (y >= determined.start || y >= 1986.0) {
    // observed Delta-T is best
    match(determined.interpolate(y)) {
      Just(dt) -> dt
      Nothing -> {
        // otherwise use predicted values
        match(predicted.interpolate(y)) {
          Just(dt) -> dt
          Nothing  -> deltat-future-approx(y)
        }
      }
    }
  }
  // Use historic data; goes up to 1987 (excluded)
  else {
    match(deltat-table-historic().interpolate(y)) {
      Just(dt) -> dt
      Nothing  -> deltat-historic-approx(y)
    }
  }
}

// Approximate delta-t before 1800 using parabolic fitted curves (Espenak & Meeus 2006).
// See <https://eclipse.gsfc.nasa.gov/SEcat5/deltatpoly.html>
fun deltat-historic-approx( y : double ) : double {
  if (y > 1800.0) then {
    deltat-future-approx(y)
  }
  elif (y >= 1700.0) {
    val t = y - 1700.0
    8.83 + t*(0.1603 + t*(~0.0059285 + t*(0.00013336 - (t/1174000.0))))
  }
  elif (y >= 1600.0) {
    val t = y - 1600.0
    120.0 + t*(~0.9808 + t*(~0.01532 + (t/7129.0)))
  }
  elif (y >= 500.0) {
    val u = (y - 1000.0)/100.0
    1574.2 + u*(~556.01 + u*(71.23472 + u*(0.319781 + u*(~0.8503463 + u*(~0.005050998 + u*0.0083572073)))))
  }
  elif (y >= ~500.0) {
    val u = y/100.0
    10583.6 + u*(~1014.41 + u*(33.78311 + u*(~5.952053 + u*(~0.1798452 + u*(0.022174192 + u*0.0090316521)))))
  }
  else deltat-generic(y)
}

// Approximate delta-t after 1986 using parabolic estimates (from Espenak & Meeus 2006).
// See <https://eclipse.gsfc.nasa.gov/SEcat5/deltatpoly.html>
fun deltat-future-approx( y : double) : double {
  if (y < 1986.0) then {
    deltat-generic(y)
  }
  elif (y <= 2005.0) {
    val t = y - 2000.0
    63.86 + t*(0.3345 + t*(~0.060374 + t*(0.0017275 + t*(0.000651814 + t*0.00002373599))))
  }
  elif (y <= 2050.0) {
    val t = y - 2000.0
    62.92 + t*(0.32217 + t*0.005589)
  }
  elif (y <= 2150.0) {
    val u = (y - 1820.0)/100.0
    ~20.0 + 32.0*u*u - 0.5628*(2150.0 - y)
  }
  else deltat-generic(y)
}


// Generic delta-t from Morrison and Stephenson, 2004.
fun deltat-generic( y : double ) : double {
  val u = (y - 1820.0)/100.0
   ~20.0 + 32.0*u*u
}



// -----------------------------------------------------------
// Parsing for less recent UT1 tables:
// historic data, recent determined data, and 10 year predictions.
// -----------------------------------------------------------

// A deltat-table has a `start` date (fractional year) and a number of evenly
// spaced entries per year. Usually 2 for historic data (every half year),
// 12 for determined data (monthly), and 4 for predicted data (quarterly).
// The entries contain the delta-t at the start of that period.
struct deltat-table(
  start: double,            // fractional year
  entries-per-year: double,
  entries: vector<double>
)

val deltat-table-empty = Deltat-table(0.0,0.0,vector(0,0.0))

// Given a `deltat-table` and a fractional year, return the delta-t by
// linear interpolation between two known delta-t instants. If the
// year `y` is outside the bounds of the table, return `Nothing`.
fun interpolate( dtable : deltat-table, y : double )  : maybe<double> {
  if (y < dtable.start || dtable.end <= y) then Nothing else {
    try(Nothing) {
      val i   = (y - dtable.start)*dtable.entries-per-year
      val idx = i.trunc.int
      val d0  = dtable.entries[idx]
      val d1  = dtable.entries[idx+1]
      Just( d0 + ((d1 - d0)*i.fraction) )
    }
  }
}

// The end fractional year until which this table contains valid data.
fun end( tb : deltat-table ) : double {
  tb.start + (tb.entries.length.double / tb.entries-per-year)
}


fun parse-deltat( determined : string = "", predicted : string = "") : ((year:double) -> maybe<double>) {
  val det  = parse-determined(determined)
  val pred = parse-predicted(predicted)
  (fun(year:double) {
    match(det.interpolate(year)) {
      Nothing -> pred.interpolate(year)
      jdt     -> jdt
    }
  })
}


fun parse-predicted( s : string ) : deltat-table {
  val caps = s.find-all(rx-predict)
  if (caps.nil?) return deltat-table-empty
  val start = caps.head.map(fun(cap){ cap.groups[1] }).default("").parse-double.maybe(2000.0)
  val entries = caps.map(fun(cap){ cap.groups[2].parse-double.maybe(0.0) }).vector
  Deltat-table(start,4.0,entries)
}
val rx-predict = regex(@"^\s*(\d+\.\d+)\s*(\d+(?:\.\d*)?)", multiline=True)

fun parse-determined( s : string ) : deltat-table {
  val caps  = s.find-all(rx-determined)
  if (caps.nil?) then return deltat-table-empty
  val (y,m) = caps.head.map(fun(cap){ (cap.groups[1],cap.groups[2]) }).default(("",""))
  val start = y.parse-double.maybe(2000.0) + ((m.parse-double.maybe(1.0) - 1.0)/12.0)
  val entries = caps.map(fun(cap){ cap.groups[4].parse-double.maybe(0.0) }).vector
  Deltat-table(start,12.0,entries)
}
val rx-determined = regex(@"^\s*(\d+)\s+(\d+)\s+(\d+)\s+(\d+(?:\.\d*)?)", multiline=True)


fun parse-historic( start: double, s : string ) : deltat-table {
  val caps = s.replace-all(rxcomment,"").find-all(rxdouble)
  val entries = caps.map(fun(cap){ cap.matched.parse-double.maybe(0.0) }).vector
  Deltat-table(start,2.0,entries)
}
val rxdouble = regex(@"-?\d+(?:\.\d+)?")
val rxcomment = regex(@"//.*")




//-----------------------------------------------------------------
// Historic data (half-yearly from 1750 up to 1987)
//-----------------------------------------------------------------

// Half-yearly history delta-t values from 1750 up to 1987
// From: http://maia.usno.navy.mil/ser7/historic_deltat.data
// Note: values before 1750 are excluded as the Espenak & Meeus (2006) estimates are more
// precise. The values in the original table are from McCarthy and Babcock (1986).
// Table runs from 1750 to 1987 (excluded) for each half year.
val deltat-table-historic : () -> deltat-table = once{ parse-historic(1750.0,@"
[13.7,13.8,13.9,14.0,14.0,14.1,14.1,14.1,14.1,14.2,14.3,14.4,14.4,14.5,14.6,14.6,14.7,14.7,14.7,
14.8,14.8,14.9,14.9,15.0,15.0,15.1,15.2,15.3,15.4,15.5,15.6,15.6,15.6,15.8,15.9,15.9,15.9,15.8,
15.7,15.8,15.7,15.7,15.7,15.8,15.9,16.1,16.1,16.0,15.9,15.9,15.7,15.4,15.3,15.4,15.5,15.6,15.6,
15.6,15.6,15.6,15.6,15.6,15.5,15.5,15.4,15.3,15.2,15.1,14.9,14.8,14.6,14.4,14.3,14.2,14.1,14.2,
14.2,13.9,13.7,13.5,13.3,13.1,13.0,13.2,13.2,13.1,13.1,13.2,13.3,13.5,13.5,13.4,13.2,13.2,13.1,
13.1,13.0,12.8,12.6,12.7,12.6,12.3,12.0,11.9,11.8,11.6,11.4,11.2,11.1,11.1,11.1,11.1,11.1,11.2,
11.1,11.1,11.2,11.4,11.5,11.3,11.2,11.4,11.7,11.9,11.9,11.9,11.8,11.7,11.8,11.8,11.8,11.7,11.6,
11.6,11.5,11.5,11.4,11.4,11.3,11.3,11.13,11.16,10.94,10.72,10.29,10.04,9.94,9.91,9.88,9.86,9.72,
9.67,9.66,9.64,9.51,9.40,9.21,9.00,8.60,8.29,7.95,7.73,7.59,7.49,7.36,7.26,7.10,7.00,6.89,6.82,
6.73,6.64,6.39,6.28,6.25,6.27,6.25,6.27,6.22,6.24,6.22,6.27,6.30,6.36,6.35,6.37,6.32,6.33,6.33,
6.37,6.37,6.41,6.40,6.44,6.46,6.51,6.48,6.51,6.53,6.58,6.55,6.61,6.69,6.80,6.84,6.94,7.03,7.13,
7.15,7.22,7.26,7.30,7.23,7.22,7.21,7.20,6.99,6.98,7.19,7.36,7.35,7.39,7.41,7.45,7.36,7.18,6.95,
6.72,6.45,6.24,5.92,5.59,5.15,4.67,4.11,3.52,2.94,2.47,1.97,1.52,1.04,0.60,0.11,~0.34,~0.82,~1.25,
~1.70,~2.08,~2.48,~2.82,~3.19,~3.50,~3.84,~4.14,~4.43,~4.59,~4.79,~4.92,~5.09,~5.24,~5.36,~5.34,
~5.37,~5.32,~5.34,~5.33,~5.40,~5.47,~5.58,~5.66,~5.74,~5.68,~5.69,~5.65,~5.67,~5.68,~5.73,~5.72,
~5.78,~5.79,~5.86,~5.89,~6.01,~6.13,~6.28,~6.41,~6.53,~6.49,~6.50,~6.45,~6.41,~6.26,~6.11,~5.90,
~5.63,~5.13,~4.68,~4.19,~3.72,~3.21,~2.70,~2.09,~1.48,~0.75,~0.08,0.62,1.26,1.95,2.59,3.28,3.92,
4.61,5.20,5.73,6.29,7.00,7.68,8.45,9.13,9.78,10.38,10.99,11.64,12.47,13.23,14.00,14.69,15.38,16.00,
16.64,17.19,17.72,18.19,18.67,19.13,19.69,20.14,20.54,20.86,21.14,21.41,21.78,22.06,22.30,22.51,
22.79,23.01,23.29,23.46,23.55,23.63,23.80,23.95,24.25,24.39,24.42,24.34,24.22,24.10,24.08,24.02,
24.04,23.98,23.91,23.89,23.95,23.93,23.92,23.88,23.94,23.91,23.82,23.76,23.87,23.91,23.95,23.96,
24.00,24.04,24.20,24.35,24.61,24.82,25.09,25.30,25.56,25.77,26.05,26.27,26.54,26.76,27.04,27.27,
27.55,27.77,28.03,28.25,28.50,28.70,28.95,29.15,29.38,29.57,29.80,29.97,30.19,30.36,30.57,30.72,
30.93,31.07,31.24,31.349,31.516,31.677,31.923,32.166,32.449,32.671,32.919,33.150,33.397,33.584,
33.804,33.992,34.240,34.466,34.731,35.030,35.400,35.738,36.147,36.546,36.995,37.429,37.879,38.291,
38.753,39.204,39.707,40.182,40.706,41.170,41.686,42.227,42.825,43.373,43.959,
// 1974
44.486,44.997,
45.477,45.983,
46.458,46.997,
47.521,48.034,
48.535,49.099,
49.589,50.102,
// 1980
50.540,50.975,
51.382,51.810,
52.168,52.572,
52.957,53.434,
53.789,54.087  //1984: end of historic data file
54.343,54.636  //1985: from determined
54.872,55.113  //1986: from determined
]"
)}


//-----------------------------------------------------------------
// The following two tables should be updated yearly to contain
// the latest builtin information Delta-T.
//
// determined data: <http://maia.usno.navy.mil/ser7/deltat.data>
// predicted  data: <http://maia.usno.navy.mil/ser7/deltat.preds>
//-----------------------------------------------------------------

// Monthly observed delta-t data from 1973 up to 2017
// See: <http://maia.usno.navy.mil/ser7/deltat.data>
val deltat-table-determined = once{ parse-determined( @"
 1973  2  1  43.4724
 1973  3  1  43.5648
 1973  4  1  43.6737
 1973  5  1  43.7782
 1973  6  1  43.8763
 1973  7  1  43.9562
 1973  8  1  44.0315
 1973  9  1  44.1132
 1973 10  1  44.1982
 1973 11  1  44.2952
 1973 12  1  44.3936
 1974  1  1  44.4841
 1974  2  1  44.5646
 1974  3  1  44.6425
 1974  4  1  44.7386
 1974  5  1  44.8370
 1974  6  1  44.9302
 1974  7  1  44.9986
 1974  8  1  45.0584
 1974  9  1  45.1284
 1974 10  1  45.2064
 1974 11  1  45.2980
 1974 12  1  45.3897
 1975  1  1  45.4761
 1975  2  1  45.5633
 1975  3  1  45.6450
 1975  4  1  45.7375
 1975  5  1  45.8284
 1975  6  1  45.9133
 1975  7  1  45.9820
 1975  8  1  46.0408
 1975  9  1  46.1067
 1975 10  1  46.1825
 1975 11  1  46.2789
 1975 12  1  46.3713
 1976  1  1  46.4567
 1976  2  1  46.5445
 1976  3  1  46.6311
 1976  4  1  46.7302
 1976  5  1  46.8284
 1976  6  1  46.9247
 1976  7  1  46.9970
 1976  8  1  47.0709
 1976  9  1  47.1451
 1976 10  1  47.2362
 1976 11  1  47.3413
 1976 12  1  47.4319
 1977  1  1  47.5214
 1977  2  1  47.6049
 1977  3  1  47.6837
 1977  4  1  47.7781
 1977  5  1  47.8771
 1977  6  1  47.9687
 1977  7  1  48.0348
 1977  8  1  48.0942
 1977  9  1  48.1608
 1977 10  1  48.2460
 1977 11  1  48.3439
 1977 12  1  48.4355
 1978  1  1  48.5344
 1978  2  1  48.6325
 1978  3  1  48.7294
 1978  4  1  48.8365
 1978  5  1  48.9353
 1978  6  1  49.0319
 1978  7  1  49.1013
 1978  8  1  49.1591
 1978  9  1  49.2286
 1978 10  1  49.3070
 1978 11  1  49.4018
 1978 12  1  49.4945
 1979  1  1  49.5862
 1979  2  1  49.6805
 1979  3  1  49.7602
 1979  4  1  49.8556
 1979  5  1  49.9489
 1979  6  1  50.0347
 1979  7  1  50.1019
 1979  8  1  50.1622
 1979  9  1  50.2260
 1979 10  1  50.2968
 1979 11  1  50.3831
 1979 12  1  50.4599
 1980  1  1  50.5387
 1980  2  1  50.6161
 1980  3  1  50.6866
 1980  4  1  50.7658
 1980  5  1  50.8454
 1980  6  1  50.9187
 1980  7  1  50.9761
 1980  8  1  51.0278
 1980  9  1  51.0843
 1980 10  1  51.1538
 1980 11  1  51.2319
 1980 12  1  51.3063
 1981  1  1  51.3808
 1981  2  1  51.4526
 1981  3  1  51.5160
 1981  4  1  51.5985
 1981  5  1  51.6809
 1981  6  1  51.7573
 1981  7  1  51.8133
 1981  8  1  51.8532
 1981  9  1  51.9014
 1981 10  1  51.9603
 1981 11  1  52.0328
 1981 12  1  52.0985
 1982  1  1  52.1668
 1982  2  1  52.2316
 1982  3  1  52.2938
 1982  4  1  52.3680
 1982  5  1  52.4465
 1982  6  1  52.5180
 1982  7  1  52.5752
 1982  8  1  52.6178
 1982  9  1  52.6668
 1982 10  1  52.7340
 1982 11  1  52.8056
 1982 12  1  52.8792
 1983  1  1  52.9565
 1983  2  1  53.0445
 1983  3  1  53.1268
 1983  4  1  53.2197
 1983  5  1  53.3024
 1983  6  1  53.3747
 1983  7  1  53.4335
 1983  8  1  53.4778
 1983  9  1  53.5300
 1983 10  1  53.5845
 1983 11  1  53.6523
 1983 12  1  53.7256
 1984  1  1  53.7882
 1984  2  1  53.8367
 1984  3  1  53.8830
 1984  4  1  53.9443
 1984  5  1  54.0042
 1984  6  1  54.0536
 1984  7  1  54.0856
 1984  8  1  54.1084
 1984  9  1  54.1463
 1984 10  1  54.1914
 1984 11  1  54.2452
 1984 12  1  54.2958
 1985  1  1  54.3427
 1985  2  1  54.3911
 1985  3  1  54.4320
 1985  4  1  54.4898
 1985  5  1  54.5456
 1985  6  1  54.5977
 1985  7  1  54.6355
 1985  8  1  54.6532
 1985  9  1  54.6776
 1985 10  1  54.7174
 1985 11  1  54.7741
 1985 12  1  54.8253
 1986  1  1  54.8713
 1986  2  1  54.9161
 1986  3  1  54.9581
 1986  4  1  54.9997
 1986  5  1  55.0476
 1986  6  1  55.0912
 1986  7  1  55.1132
 1986  8  1  55.1328
 1986  9  1  55.1532
 1986 10  1  55.1898
 1986 11  1  55.2416
 1986 12  1  55.2838
 1987  1  1  55.3222
 1987  2  1  55.3613
 1987  3  1  55.4063
 1987  4  1  55.4629
 1987  5  1  55.5111
 1987  6  1  55.5524
 1987  7  1  55.5812
 1987  8  1  55.6004
 1987  9  1  55.6262
 1987 10  1  55.6656
 1987 11  1  55.7168
 1987 12  1  55.7698
 1988  1  1  55.8197
 1988  2  1  55.8615
 1988  3  1  55.9130
 1988  4  1  55.9663
 1988  5  1  56.0220
 1988  6  1  56.0700
 1988  7  1  56.0939
 1988  8  1  56.1105
 1988  9  1  56.1314
 1988 10  1  56.1611
 1988 11  1  56.2068
 1988 12  1  56.2583
 1989  1  1  56.3000
 1989  2  1  56.3399
 1989  3  1  56.3790
 1989  4  1  56.4283
 1989  5  1  56.4804
 1989  6  1  56.5352
 1989  7  1  56.5697
 1989  8  1  56.5983
 1989  9  1  56.6328
 1989 10  1  56.6739
 1989 11  1  56.7332
 1989 12  1  56.7972
 1990  1  1  56.8553
 1990  2  1  56.9111
 1990  3  1  56.9755
 1990  4  1  57.0471
 1990  5  1  57.1136
 1990  6  1  57.1738
 1990  7  1  57.2226
 1990  8  1  57.2597
 1990  9  1  57.3073
 1990 10  1  57.3643
 1990 11  1  57.4334
 1990 12  1  57.5016
 1991  1  1  57.5653
 1991  2  1  57.6333
 1991  3  1  57.6973
 1991  4  1  57.7711
 1991  5  1  57.8407
 1991  6  1  57.9058
 1991  7  1  57.9576
 1991  8  1  57.9975
 1991  9  1  58.0426
 1991 10  1  58.1043
 1991 11  1  58.1679
 1991 12  1  58.2389
 1992  1  1  58.3092
 1992  2  1  58.3833
 1992  3  1  58.4537
 1992  4  1  58.5401
 1992  5  1  58.6228
 1992  6  1  58.6917
 1992  7  1  58.7410
 1992  8  1  58.7836
 1992  9  1  58.8406
 1992 10  1  58.8986
 1992 11  1  58.9714
 1992 12  1  59.0438
 1993  1  1  59.1218
 1993  2  1  59.2003
 1993  3  1  59.2747
 1993  4  1  59.3574
 1993  5  1  59.4434
 1993  6  1  59.5242
 1993  7  1  59.5850
 1993  8  1  59.6344
 1993  9  1  59.6928
 1993 10  1  59.7588
 1993 11  1  59.8386
 1993 12  1  59.9111
 1994  1  1  59.9845
 1994  2  1  60.0564
 1994  3  1  60.1231
 1994  4  1  60.2042
 1994  5  1  60.2804
 1994  6  1  60.3530
 1994  7  1  60.4012
 1994  8  1  60.4440
 1994  9  1  60.4900
 1994 10  1  60.5578
 1994 11  1  60.6324
 1994 12  1  60.7059
 1995  1  1  60.7853
 1995  2  1  60.8664
 1995  3  1  60.9387
 1995  4  1  61.0277
 1995  5  1  61.1103
 1995  6  1  61.1870
 1995  7  1  61.2454
 1995  8  1  61.2881
 1995  9  1  61.3378
 1995 10  1  61.4036
 1995 11  1  61.4760
 1995 12  1  61.5525
 1996  1  1  61.6287
 1996  2  1  61.6846
 1996  3  1  61.7433
 1996  4  1  61.8132
 1996  5  1  61.8823
 1996  6  1  61.9497
 1996  7  1  61.9969
 1996  8  1  62.0343
 1996  9  1  62.0714
 1996 10  1  62.1202
 1996 11  1  62.1810
 1996 12  1  62.2382
 1997  1  1  62.2950
 1997  2  1  62.3506
 1997  3  1  62.3995
 1997  4  1  62.4754
 1997  5  1  62.5463
 1997  6  1  62.6136
 1997  7  1  62.6571
 1997  8  1  62.6942
 1997  9  1  62.7383
 1997 10  1  62.7926
 1997 11  1  62.8567
 1997 12  1  62.9146
 1998  1  1  62.9659
 1998  2  1  63.0217
 1998  3  1  63.0807
 1998  4  1  63.1462
 1998  5  1  63.2053
 1998  6  1  63.2599
 1998  7  1  63.2844
 1998  8  1  63.2961
 1998  9  1  63.3126
 1998 10  1  63.3422
 1998 11  1  63.3871
 1998 12  1  63.4339
 1999  1  1  63.4673
 1999  2  1  63.4979
 1999  3  1  63.5319
 1999  4  1  63.5679
 1999  5  1  63.6104
 1999  6  1  63.6444
 1999  7  1  63.6642
 1999  8  1  63.6739
 1999  9  1  63.6926
 1999 10  1  63.7147
 1999 11  1  63.7518
 1999 12  1  63.7927
 2000  1  1  63.8285
 2000  2  1  63.8557
 2000  3  1  63.8804
 2000  4  1  63.9075
 2000  5  1  63.9393
 2000  6  1  63.9691
 2000  7  1  63.9799
 2000  8  1  63.9833
 2000  9  1  63.9938
 2000 10  1  64.0093
 2000 11  1  64.0400
 2000 12  1  64.0670
 2001  1  1  64.0908
 2001  2  1  64.1068
 2001  3  1  64.1282
 2001  4  1  64.1584
 2001  5  1  64.1833
 2001  6  1  64.2094
 2001  7  1  64.2117
 2001  8  1  64.2073
 2001  9  1  64.2116
 2001 10  1  64.2223
 2001 11  1  64.2500
 2001 12  1  64.2761
 2002  1  1  64.2998
 2002  2  1  64.3192
 2002  3  1  64.3450
 2002  4  1  64.3735
 2002  5  1  64.3943
 2002  6  1  64.4151
 2002  7  1  64.4132
 2002  8  1  64.4118
 2002  9  1  64.4097
 2002 10  1  64.4168
 2002 11  1  64.4329
 2002 12  1  64.4511
 2003  1  1  64.4734
 2003  2  1  64.4893
 2003  3  1  64.5053
 2003  4  1  64.5269
 2003  5  1  64.5471
 2003  6  1  64.5597
 2003  7  1  64.5512
 2003  8  1  64.5371
 2003  9  1  64.5359
 2003 10  1  64.5415
 2003 11  1  64.5544
 2003 12  1  64.5654
 2004  1  1  64.5736
 2004  2  1  64.5891
 2004  3  1  64.6015
 2004  4  1  64.6176
 2004  5  1  64.6374
 2004  6  1  64.6549
 2004  7  1  64.6530
 2004  8  1  64.6379
 2004  9  1  64.6372
 2004 10  1  64.6400
 2004 11  1  64.6543
 2004 12  1  64.6723
 2005  1  1  64.6876
 2005  2  1  64.7052
 2005  3  1  64.7313
 2005  4  1  64.7575
 2005  5  1  64.7811
 2005  6  1  64.8001
 2005  7  1  64.7995
 2005  8  1  64.7876
 2005  9  1  64.7831
 2005 10  1  64.7921
 2005 11  1  64.8096
 2005 12  1  64.8311
 2006  1  1  64.8452
 2006  2  1  64.8597
 2006  3  1  64.8850
 2006  4  1  64.9175
 2006  5  1  64.9480
 2006  6  1  64.9794
 2006  7  1  64.9895
 2006  8  1  65.0028
 2006  9  1  65.0138
 2006 10  1  65.0371
 2006 11  1  65.0773
 2006 12  1  65.1122
 2007  1  1  65.1464
 2007  2  1  65.1833
 2007  3  1  65.2145
 2007  4  1  65.2494
 2007  5  1  65.2921
 2007  6  1  65.3279
 2007  7  1  65.3413
 2007  8  1  65.3452
 2007  9  1  65.3496
 2007 10  1  65.3711
 2007 11  1  65.3972
 2007 12  1  65.4296
 2008  1  1  65.4573
 2008  2  1  65.4868
 2008  3  1  65.5152
 2008  4  1  65.5450
 2008  5  1  65.5781
 2008  6  1  65.6127
 2008  7  1  65.6288
 2008  8  1  65.6370
 2008  9  1  65.6493
 2008 10  1  65.6760
 2008 11  1  65.7097
 2008 12  1  65.7461
 2009  1  1  65.7768
 2009  2  1  65.8025
 2009  3  1  65.8237
 2009  4  1  65.8595
 2009  5  1  65.8973
 2009  6  1  65.9323
 2009  7  1  65.9509
 2009  8  1  65.9534
 2009  9  1  65.9628
 2009 10  1  65.9839
 2009 11  1  66.0147
 2009 12  1  66.0420
 2010  1  1  66.0699
 2010  2  1  66.0961
 2010  3  1  66.1310
 2010  4  1  66.1683
 2010  5  1  66.2072
 2010  6  1  66.2356
 2010  7  1  66.2409
 2010  8  1  66.2335
 2010  9  1  66.2349
 2010 10  1  66.2441
 2010 11  1  66.2751
 2010 12  1  66.3054
 2011  1  1  66.3246
 2011  2  1  66.3406
 2011  3  1  66.3624
 2011  4  1  66.3957
 2011  5  1  66.4289
 2011  6  1  66.4619
 2011  7  1  66.4749
 2011  8  1  66.4751
 2011  9  1  66.4829
 2011 10  1  66.5056
 2011 11  1  66.5383
 2011 12  1  66.5706
 2012  1  1  66.6030
 2012  2  1  66.6340
 2012  3  1  66.6569
 2012  4  1  66.6925
 2012  5  1  66.7289
 2012  6  1  66.7579
 2012  7  1  66.7708
 2012  8  1  66.7740
 2012  9  1  66.7846
 2012 10  1  66.8103
 2012 11  1  66.8400
 2012 12  1  66.8779
 2013  1  1  66.9069
 2013  2  1  66.9443
 2013  3  1  66.9763
 2013  4  1  67.0258
 2013  5  1  67.0716
 2013  6  1  67.1100
 2013  7  1  67.1266
 2013  8  1  67.1331
 2013  9  1  67.1458
 2013 10  1  67.1717
 2013 11  1  67.2091
 2013 12  1  67.2460
 2014  1  1  67.2810
 2014  2  1  67.3136
 2014  3  1  67.3457
 2014  4  1  67.3890
 2014  5  1  67.4318
 2014  6  1  67.4666
 2014  7  1  67.4858
 2014  8  1  67.4989
 2014  9  1  67.5111
 2014 10  1  67.5353
 2014 11  1  67.5711
 2014 12  1  67.6070
 2015  1  1  67.6439
 2015  2  1  67.6765
 2015  3  1  67.7117
 2015  4  1  67.7591
 2015  5  1  67.8012
 2015  6  1  67.8402
 2015  7  1  67.8606
 2015  8  1  67.8822
 2015  9  1  67.9120
 2015 10  1  67.9546
 2015 11  1  68.0055
 2015 12  1  68.0514
 2016  1  1  68.1024
 2016  2  1  68.1577
 2016  3  1  68.2044
 2016  4  1  68.2665
 2016  5  1  68.3188
 2016  6  1  68.3703
 2016  7  1  68.3964
 2016  8  1  68.4094
 2016  9  1  68.4305
 2016 10  1  68.4630
 2016 11  1  68.5078
 2016 12  1  68.5537
 2017  1  1  68.5928
")}


// Quarterly Predicted delta-t values, from 2017 upto 2026
// from <http://maia.usno.navy.mil/ser7/deltat.preds>.
val deltat-table-predicted = once{ parse-predicted(@"
  YEAR    TT-UT PREDICTION  UT1-UTC PREDICTION  ERROR

 2017.00      68.591              0.593         0.000
 2017.25      68.72               0.464         0.02
 2017.50      68.86                             0.06
 2017.75      69.0                              0.1
 2018.00      69.1                              0.2
 2018.25      69.2                              0.2
 2018.50      69.3                              0.3
 2018.75      69.4                              0.4
 2019.00      69.6                              0.4
 2019.25      69.7                              0.5
 2019.50      69.9                              0.6
 2019.75      70.0                              0.8
 2020.00      70.2                              0.9
 2020.25      70.                               1.
 2020.50      70.                               1.
 2020.75      71.                               1.
 2021.00      71.                               1.
 2021.25      71.                               2.
 2021.50      71.                               2.
 2021.75      71.                               2.
 2022.00      71.                               2.
 2022.25      71.                               2.
 2022.50      71.                               2.
 2022.75      72.                               2.
 2023.00      72.                               3.
 2023.25      72.                               3.
 2023.50      72.                               3.
 2023.75      72.                               3.
 2024.00      72.                               3.
 2024.25      72.                               3.
 2024.50      72.                               3.
 2024.75      73.                               4.
 2025.00      73.                               4.
 2025.25      73.                               4.
 2025.50      73.                               4.
 2025.75      73.                               4.
 2026.00      73.                               5.
")}
