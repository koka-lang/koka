/*----------------------------------------------------------------------------
   Copyright 2012-2021, Microsoft Research, Daan Leijen

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/

/* Calendars.  

  A Calendar determines how a `:date` and `:clock` relates to an `:instant` in time
  with respect to a `:timezone`. These functions turn `:instant`'s into "human" dates.
  Months, days etc. are always 1-based.
*/
module std/time/calendar

import std/num/ddouble
import std/time/timestamp
import std/time/duration
import std/time/instant
import std/time/utc
import std/time/date

extern import
  c  file "calendar-inline.c"
  cs file "calendar-inline.cs"
  js file "calendar-inline.js"

// A Calendar determines how a `:date` and `:clock` relates to an `:instant` in time.
abstract struct calendar
  pub name        : string
  pub long-name   : string
  //timescale   : timescale
  pub month-prefix: string
  pub show-era    : (date) -> string
  instant-to-dc : (i:instant,tzdelta:duration) -> (date,clock)
  dc-to-instant : (date,clock,timezone,timescale) -> instant
  days-to-date  : (days:int) -> date
  date-to-days  : (date:date) -> int


// Check if two calendars use the same date calculations. (Display of era names etc. may differ)
pub fun (==)( c1 : calendar, c2 : calendar ) : bool
  (c1.long-name == c2.long-name)

/*----------------------------------------------------------------------------
  Time zone
----------------------------------------------------------------------------*/

// A `:timezone` determines a time offset with respect to the UTC / GMT timezone. For
// using general time zones, see the [`std/time/timezone`](std_time_timezone.html) module.\
// The `tz-utc` and `tz-local` time zones are used for UTC/GMT time and the local system time.\
// The `name` field contains the time zone name as IANA timezone identifier.
// The `utc-delta` field returns the time zone offset
// relative to UTC (for GMT-08:00 it returns a duration of `-8*3600` seconds) and the abbreviated
// time zone name (``PDT`` for example).\
// The optional `utc-inverse` field returns for an instant in the time zone, the associated UTC time.
// By default it returns `Nothing` in which case a generic algorithm is used to determine the
// inverse.
abstract struct timezone
  pub name    : string
  utc-delta   : (instant) -> (duration,string)
  utc-inverse : (instant) -> maybe<instant> = fn(i) { Nothing }


// Same timezone?
pub fun (==)( tz1 : timezone, tz2 : timezone ) : bool
  (tz1.name == tz2.name)

// Is this the UTC timezone?
pub fun is-tz-utc( tz : timezone ) : bool
  tz == tz-utc

// Create a time zone with a fixed number of hours and optional minutes (=`0`) difference from UTC/GMT.
// The time zone name and abbreviation is optional, and the default name
// will reflect the offset from UTC, for example `"UTC+1:20"`, or `"UTC-8"`.
// The default abbreviation is the empty string.
// The sign of the minutes will be matched to the hours (unless `hours` is `0`).
// For example, `tz-fixed(-1,30)` gives a time zone offset of -90 minutes, "UTC-1:30".
// Returns `tz-utc` if both `hours` and `mins` are zero.
pub fun tz-fixed( hours: int, mins : int = 0, name : string = "", abbrv : string = "", hourwidth : int = 1 ) : timezone
  if hours.is-zero && mins.is-zero then return tz-utc
  val xmins = if hours.is-pos then mins.abs elif hours.is-neg then (mins.abs).negate else mins
  val secs  = hours*3600 + xmins*60
  val tzonename = name || ("UTC" ++ (if secs.is-neg then "-" else "+")
                            ++ hours.abs.show.pad-left(hourwidth,'0')
                            ++ (if mins.is-zero then "" else ":" ++ mins.abs.show.pad-left(2,'0')))
  val delta  = duration(secs)
  tz-fixed(tzonename,delta,abbrv)

fun tz-fixed( name : string, delta : duration, abbrv : string = "" ) : timezone
  Timezone( name,
    fn(i) { (delta,abbrv) },
    fn(i) { if delta.is-zero then Just(i) else Nothing }
  )

// The standard UTC time zone with a 0 delta.
pub val tz-utc : timezone = tz-fixed("UTC",zero)

// Return the local timezone on the current system.
pub fun tz-local() : ndet timezone
  val tz = local-get-timezone()  // get it once so the `Timezone` itself is immutable!
  Timezone(
    "",
    fn(i)
      val (ofs,abbrv) = local-utc-delta(tz, i.unix-timestamp.float64)
      (duration(ofs),abbrv)

  )

// A local timezone structure provided by the host system
type local-timezone

// Get the current local timezone structure.
// The timezone name is allowed to be the empty string.
extern local-get-timezone() : ndet local-timezone
  c  "kk_local_get_timezone"
  cs "_Calendar.LocalTimeZone"
  js "_local_get_timezone"

// Return the utc delta in fractional seconds given a local timezone structure
// and time `i` in fractional seconds since the UNIX epoch (1970-01-01).
// Also returns time zone abbreviation.
extern local-utc-delta( tz : local-timezone, i : float64 ) : (float64,string)
  c  "kk_local_get_utc_delta_tuple"
  cs "_Calendar.LocalUtcDelta"
  js "_local_utc_delta"


/*----------------------------------------------------------------------------
  Time and instant conversion using a calendar
----------------------------------------------------------------------------*/

// Return the instant in time for a given `:date` and `:clock` (= `clock0`) interpreted by
// calendar `cal` (=`cal-iso`) in a timezone `tz` (=`tz-utc` by default).
pub fun instant( ts : timescale, d : date, c : clock = clock0, tz : timezone = tz-utc, cal : calendar = cal-iso ) : instant
  (cal.dc-to-instant)( d, c, tz, ts )

pub fun instant( d : date, c : clock = clock0, tz : timezone = tz-utc, cal : calendar = cal-iso, ts : timescale = utc() ) : utc instant
  ts.instant(d,c,tz,cal)

// Convert an `:instant` to a `:date`, `:clock`, timezone delta and abbreviation,
// for a given timezone `tz` (=`tz-utc` by default) and calendar (=`cal-iso` by default).
pub fun instant-dc( i : instant, tz :timezone = tz-utc, cal : calendar = cal-iso ) : (date,clock,duration,string)
  //trace("instant-dc: " ++ i.show-raw)
  val (tzdelta,tzabbrv) = (tz.utc-delta)(i)
  val (d,c) = (cal.instant-to-dc)( i, tzdelta )
  (d,c,tzdelta,tzabbrv)

/* Return the instant in time for a given date and clock interpreted by
   calendar `cal` (=`cal-iso`) in a timezone `tz` (=`tz-utc` by default).

  The `month`, `day`, `hour`, `minutes` may be outside their usual ranges
  and will be normalized during the conversion. For example, January 33 converts to February 2.
  This makes it very easy to add- or subtract days or months to an existing time.
  When the `seconds` or fraction of seconds `frac` add up over 60 those extra seconds are
  interpreted as leap seconds.

  Due to timezone transitions, or leap seconds, it is possible to specify dates that never
  happened (as it was skipped by a timezone change), or ambigious times (as a timezone springs back).
  In such cases, the time is always interpreted in the earlier timezone.
*/
pub fun instant( ts : timescale,
                    year : int, month : int = 1, day : int = 1,
                    hours : int = 0, minutes : int = 0, seconds : int = 0,
                    frac : float64 = 0.0, tz : timezone = tz-utc,
                    cal : calendar = cal-iso ) : instant

  val fsecs = seconds.ddouble + frac.ddouble
  ts.instant( Date(year,month,day), Clock(hours,minutes,fsecs), tz, cal )

pub fun instant( year : int, month : int = 1, day : int = 1,
                    hours : int = 0, minutes : int = 0, seconds : int = 0,
                    frac : float64 = 0.0, tz : timezone = tz-utc,
                    cal : calendar = cal-iso, ts : timescale = utc() ) : utc instant

  ts.instant(year,month,day,hours,minutes,seconds,frac,tz,cal)

// Return the day of the week for a calendar `cal` (=`cal-iso`).
fun weekday( d : date, cal : calendar ) : weekday
  // Generic algorithm for any calendar
  /*
  val i    = instant(d.year,d.month,d.day,cal=cal)
  val dow  = (i.days+6)%7      // plus 6 since 2000-01-01 epoch was a Saturday  (note: 0 = sunday, 1 = monday, ..)
  */
  val days = (cal.date-to-days)(d)
  val dow  = (days+6)%7        // plus 6 since 2000-01-01 epoch was a Saturday  (note: 0 = sunday, 1 = monday, ..)
  dow.weekday                  // to ISO week day

// Return the days between two dates interpreted by calendar `cal`.
pub fun days-until( cal : calendar, d1 : date, d2 :date ) : int
  val days1 = (cal.date-to-days)(d1)
  val days2 = (cal.date-to-days)(d2)
  days2 - days1
  /*
  val i1 = instant(d1,cal=cal,ts=ts)
  val i2 = instant(d2,cal=cal,ts=ts)
  i2.days - i1.days
  */


/*----------------------------------------------------------------------------
  Earth calendars
  Abstracts over common patterns to relates dates and time instants to simpler
  relations between dates and days.
----------------------------------------------------------------------------*/

// Create a calendar where we assume that each
// day has 24 hours with 60 minutes, and where minutes are 60 seconds
// (with possibly leap seconds). The function takes care of timezones,
// epoch shifts, and leap seconds, and only needs a calendar `name`,
// a function `days-to-date` to calculate a date given a number of days since the `epoch`,
// a function `date-to-days` to calculate days since the `epoch` for a given date,
// and an optional time scale to be used (=`ts-utc`).\
// `month-prefix` (=`""`) is used when displaying numeric months and is set for
// the ISO week dates to `"W"` for example. The `show-era` function can be given
// to display the era of a date.
pub fun earth-calendar(
              name        : string,
              long-name   : string,
              days-to-date  : (days:int) -> date,
              date-to-days  : date -> int,
              month-prefix  : string = "",
              show-era      : (date)->string = fn(d){ "" }
              // ts   : timescale = ts-ti
            ) : calendar

  fun instant-to-dc(i : instant, tzdelta : duration)
    earth-timestamp-to-dc( i.timestamp, tzdelta.timespan, days-to-date )

  fun dc-to-instant(d : date, c : clock, tz : timezone, ts : timescale )
    earth-timestamp-to-instant( dc-timestamp(d, c, date-to-days, ts.has-leap-seconds), tz, ts)

  Calendar( name,long-name,month-prefix,show-era,
            instant-to-dc,dc-to-instant,
            days-to-date, date-to-days )


fun earth-timestamp-to-dc( t : timestamp, tzdelta: timespan, days-to-date: (int) -> date ) : (date,clock)
  // adjust for timezone (disregarding leap seconds for now) (and possibly going into the next or previous day)
  val (days,clock)     = (t + tzdelta).days-clock
  (days-to-date(days),clock)

fun earth-timestamp-to-instant( t : timestamp, tz : timezone, ts : timescale ) : instant
  // do timezone adjustment ignoring leap seconds (TODO: check correctness)
  val i    = ts.instant(t)
  val iz   = if tz.is-tz-utc then i else
               match((tz.utc-inverse)(i))
                 Just(inv) -> inv
                 Nothing ->
                   // no explicit inverse,
                   // do a double pass to accommodate jumping over a DST boundary.
                   val tzd1 = (tz.utc-delta)(i).fst
                   val tzi1 = ts.instant(t - tzd1.timespan)
                   val tzd2 = (tz.utc-delta)(tzi1).fst
                   if tzd1==tzd2 then tzi1
                    else ts.instant(t - tzd2.timespan)


  iz

pub fun cal-timestamp( cal : calendar, ts : timescale, d : date, c : clock  ) : timestamp
  dc-timestamp(d,c,cal.date-to-days,ts.has-leap-seconds)

fun dc-timestamp( d : date, c : clock, date-to-days: (date) -> int, has-leap-seconds : bool = False) : timestamp
  // check if we are in a leap second and remember
  val (xsecs,leap)  = if !has-leap-seconds || c.seconds < 60.ddouble then (c.seconds,0) else
                        val leap = c.seconds.floor.int - 59
                        (c.seconds - leap.ddouble, leap)

  val secs = (((c.hours*60) + c.minutes)*60).ddouble + xsecs
  val days = date-to-days( d )
  timestamp-days(days,secs,leap)


/*----------------------------------------------------------------------------
  Combine earth calendars/ year-shift earth calendars
----------------------------------------------------------------------------*/

// Create a new calendar from by combining two other calendars. The `switch-date`
// is in terms of the second calendar (`cal2`) and time after (and including) the switch
// date is displayed in `cal2` while times before it in `cal1`. This function is used
// for example for the Julian Gregorian calendar.
pub fun combine-earth-calendars( name : string, long-name : string,
                            switch-date : date, cal1 : calendar, cal2 : calendar,
                            mb-show-era : maybe<(date) -> string> = Nothing
                          ) : calendar

  val switch-days = (cal2.date-to-days)(switch-date) //+ skip
  fun cdays-to-date( days : int ) : date
    ((if days < switch-days then cal1 else cal2).days-to-date)(days)

  fun cdate-to-days( d : date ) : int
    ((if d < switch-date then cal1 else cal2).date-to-days)(d)

  fun cshow-era(d : date ) : string
    match mb-show-era
      Just(showera) -> d.showera
      Nothing -> ((if d < switch-date then cal1 else cal2).show-era)(d)

  earth-calendar( name, long-name, cdays-to-date, cdate-to-days, show-era = cshow-era )

// Create a new calendar from anoter calendar `cal` by adding an offset to the
// years (`year-shift`). This is used for example to create the Ethiopian calendar
// from the Coptic calendar, by using:
// `year-shift-earth-calendar( "EC", "Ethiopian", ~276, cal-coptic )`
pub fun year-shift-earth-calendar( name : string, long-name: string,
                                year-shift : int, cal : calendar,
                                month-prefix  : string = "",
                                show-era      : (date)->string = fn(d){ "" }
                             ) : calendar

  fun shift-days-to-date( days : int ) : date
    val d = (cal.days-to-date)(days)
    d(year = d.year - year-shift)

  fun shift-date-to-days( d : date ) : int
    (cal.date-to-days)( d(year = d.year + year-shift) )

  earth-calendar(
    name, long-name,
    shift-days-to-date, shift-date-to-days,
    month-prefix, show-era
  )


/*----------------------------------------------------------------------------
  Generic solar calendars:
  Abstract over almost all calendars that work in terms of months and solar years.
----------------------------------------------------------------------------*/

// An abstraction over solar calendars. Almost all calendars can be defined
// in terms of this function (and `solar-ecalendar`). Given functions to
// convert days to and from a year, and day-of-the-year to and from month
// and month day, this constructs a full calendar.\
// Also needs an `epoch-shift` that gives the number of days from the
// calendar epoch to the date ``2000-01-01``. By default this is `730119`
// which corresponds to a ``0001-01-01`` ISO calendar epoch. The
// `has-year-zero` (=`True`) parameter is used when the calendar has a
// year zero. The `month-prefix` (=`""`) is used for displaying numeric
// months (and is `"W"` for `cal-iso-week` for example). The `show-era` function
// can be given to display an era for a given date. Finally the `ts` (=`ts-ti`)
// gives the time scale for the calendar.
pub fun solar-calendar( name : string, long-name: string,
                    days-before-year : (year:int) -> int,
                    days-to-yeardoy  : (days:int) -> (int,int),
                    days-before-month: (year:int, month:int) -> int,
                    days-to-month    : (year:int, doy: int ) -> int,
                    epoch-shift   : int = 730119,
                    has-year-zero : bool = True,
                    month-prefix  : string = "",
                    show-era      : (date)->string = fn(d){ "" } ) : calendar

  fun date-to-days( d : date ) : int
    val y = if has-year-zero || d.year.is-pos then d.year else d.year.inc
    days-before-year( y ) + days-before-month( y, d.month )
     + d.day.dec - epoch-shift

  fun days-to-date( days0 : int ) : date
    val days    = days0 + epoch-shift
    val (y,doy) = days-to-yeardoy( days )
    val month   = days-to-month(y, doy)
    val day     = (doy - days-before-month(y, month)) + 1
    val year    = if has-year-zero || y.is-pos then y else y.dec
    Date(year,month,day)

  earth-calendar(
    name, long-name,
    days-to-date, date-to-days,
    month-prefix, show-era
  )

// An abstraction over solar calendars. Almost all calendars can be defined
// in terms of this function (and `solar-calendar`). Given functions to
// convert days to and from a year, and day-of-the-year to and from month
// and month day, this constructs a full calendar. In contrast to `solar-calendar`
// this function just needs an estimation function from days to a year -- for most
// calendars this is much more convenient. The estimation function returns a
// conservative (i.e. lowest) estimate for the year and a `maybe<int>` that
// is the `safe-day`: if the day of the year is lower or equal to that the estimate
// is surely correct. Otherwise the wrapper will calculate the start day for the next
// year until the correct year is found.\
// Also needs an `epoch-shift` that gives the number of days from the
// calendar epoch to the date ``2000-01-01``. By default this is `730119`
// which corresponds to a ``0001-01-01`` ISO calendar epoch. The
// `has-year-zero` (=`True`) parameter is used when the calendar has a
// year zero. The `month-prefix` (=`""`) is used for displaying numeric
// months (and is `"W"` for `cal-iso-week` for example). The `show-era` function
// can be given to display an era for a given date. Finally the `ts` (=`ts-utc`)
// gives the time scale for the calendar.
pub fun solar-ecalendar(  name : string, long-name: string,
                      days-before-year : (year:int) -> int,
                      estimate-year    : (days:int) -> (int,maybe<int>),
                      days-before-month: (year:int, month:int) -> int,
                      days-to-month    : (year:int, doy: int ) -> int,
                      epoch-shift   : int = 730119,
                      has-year-zero : bool = True,
                      month-prefix  : string = "",
                      show-era      : (date)->string = fn(d){ "" } )  : calendar

  fun days-to-yeardoy(days:int) : (int,int)
    val (approx,safe) = estimate-year(days)
    val doy1 = days - days-before-year(approx)
    if (doy1 <= safe.default(0)) then return (approx,doy1)
    val doy2 = days - days-before-year(approx.inc)
    if doy2 < 0 then (approx,doy1) else (approx.inc, doy2)

  solar-calendar(name,long-name,
    days-before-year, days-to-yeardoy,
    days-before-month, days-to-month,
    epoch-shift, has-year-zero, month-prefix, show-era
  )


// ----------------------------------------------------
// ISO calendar:
// We define it here (instead of std/time/calendars) because it is the default calendar.
// ----------------------------------------------------

// Create a standard ISO calendar using a particular time scale
// and calendar `name` (=`ts.name`).
pub fun iso-calendar( _ts : timescale, name : string, long-name : string = name ) : calendar
  solar-ecalendar(
    name, long-name,
    iso-days-before-year, iso-estimate-year,
    iso-days-before-month, iso-doy-to-month
  )

// The standard [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) calendar
// using TI time (`ts-ti`). This is a proleptic Gregorian
// calendar except that it uses the year 0 for 1 BCE, -1 for 2 BCE etc.
//
// This is the default calendar used in the library as it guarantees deterministic
// date calculations while still taking historical leap seconds into account.
pub val cal-iso = iso-calendar( ts-ti, "", "ISO" )

// Create a standard [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) calendar
// using UTC time (`ts-utc-create`) given a provide leap second table (`:leaps-table`).
// This is a proleptic Gregorian
// calendar except that it uses the year 0 for 1 BCE, -1 for 2 BCE etc.
//
// You can create an ISO 8601 UTC calendar using the latest IETF leap
// second data using [`cal-utc-load`](std_time_download.html#cal_utc_load).
pub fun cal-utc-create( leaps : leaps-table ) : calendar
  val ts-utc = ts-utc-create(leaps)
  iso-calendar( ts-utc, "", "ISO" )


/* The standard [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) calendar
but using TI-SLS time (`ts-ti-sls`), i.e. TI with
[smoothed leap seconds](https://www.cl.cam.ac.uk/~mgk25/time/utc-sls/).

This calendar is equivalent to the ISO calendar except for the last 1000 seconds of a day where
a leap second occurs. On such day, the leap second time step (positive or negative)
is distributed over the last 1000 seconds of the day. On the full hour, ISO and ISO-SLS are equal again.

This is a recommended calendar to use for
time stamps or communication with other services since it avoids any potential trouble
with leap seconds while still being quite precise.

Other good properties include:

- All days have 86400 seconds, and the time 23:59:60 never appears
- The time never differs more than one second from UTC
- Time as always equal to UTC on the full and half hour
*/
pub val cal-iso-sls = iso-calendar( ts-ti-sls, "SLS", "ISO-SLS" )

// Create a new ISO 8601 calendar based on UTC-SLS time, i.e. UTC with smoothed leap
// seconds.
pub fun cal-utc-sls-create( leaps : leaps-table ) : calendar
  val ts-utc-sls = ts-utc-sls-create(leaps)
  iso-calendar( ts-utc-sls, "SLS", "ISO-SLS" )


// The (proleptic) [Gregorian calendar](https://en.wikipedia.org/wiki/Gregorian_calendar).
// Just like the ISO calendar except that the year 0 does not exist, i.e. after 1 BCE (=`~1`)
// we have 1 CE (=`1`).
// The calendar short name is ``GC``.
pub val cal-gregorian = solar-ecalendar(
  "GC", "Gregorian",
  iso-days-before-year, iso-estimate-year,
  iso-days-before-month, iso-doy-to-month,
  has-year-zero = False,
  show-era = fn(d:date){ if d.year.is-neg then "BCE" else "CE" }
)

fun iso-days-before-month( year : int, month : int ) : int
  val adj = iso-adjust( month<=2, year )
  (367*month - 362)/12 - adj

fun iso-doy-to-month( year : int, doy : int ) : int
  val adj = iso-adjust( doy<=58,  year)
  (12*(doy + adj) + 373)/367

fun iso-adjust( is-before-march : bool, year : int ) : int
  if is-before-march then 0 elif (iso-is-leap(year)) then 1 else 2

fun iso-is-leap( year : int )
  (year%4 == 0) && (year%100 != 0 || year%400 == 0)

fun iso-days-before-year( year : int )
  val y = year.dec
  val leapdays = (y/4 - y/100) + y/400
  365*y + leapdays

fun iso-estimate-year( days : int ) : (int,maybe<int>)
  val (era,yoe) = divmod(days,146097)
  (1 + 400*era + (100*yoe)/36525, Just(363))

