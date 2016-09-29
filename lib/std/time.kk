/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Basic date and time functions.

   Years, months, days etc. are always 1-based, while durations are always in (fractional) seconds.
*/
module std/time

import std/regex

extern include {
  cs file "time-inline.cs"
  js file "time-inline.js"
}

// A `:time` represents a certain time in a gregorian calendar.
// When a time value is created, it is associated with either the local 
// or UTC timezone which is used for displaying the time.\
// Valid times must be between `min-time` (= ``0001-01-01Z`` UTC) and 
// `max-time` (= ``9999-12-31T23:59:59.999Z`` UTC).
// If a time is created outside this range, the ``0001-01-01Z`` time is returned.
public type time

// Return the current time in the local timezone.
public extern now() : ndet time { 
  cs inline "DateTime.Now"
  js "_time_now"
}

// Return the current time in the UTC timezone.
public extern utc-now() : ndet time { 
  cs inline "DateTime.UtcNow"
  js "_time_utcnow"
}

// Convert a time to the UTC timezone.
// i.e. `utc-now() === now().utc`
public extern utc( t : time ) : time {
  cs inline "t.ToUniversalTime()"
  js "_time_to_utc"
}

// Convert a time to the local timezone.
public extern local( t : time ) : ndet time {
  cs inline "t.ToLocalTime()"
  js "_time_to_local"
}

// Is this time in the UTC timezone?
public extern utc?( t : time ) : bool {
  cs inline "(t.Kind == DateTimeKind.Utc)"
  js inline "(t.isutc)"
}

// Get the timezone offset in seconds. This 
// is the difference between the local time and UTC.
// For example, if the timezone is ``UTC+10:00``, the timezone offset is +10\*60\*60.
public extern timezone-offset(t : time) : int {
  cs "_Time.TimezoneOffset"
  js "_time_timezone_offset"
}

extern new-time(year:int, month : int, day:int,hours:int,minutes:int,seconds:int,milliseconds:int, is-utc:bool ) : time {
  cs "_Time.New"  
  js "_time_new"
}

extern new-time( seconds : double, is-utc:bool ) : time {
  cs "_Time.NewFromSeconds"  
  js "_time_new_from_seconds"
}

// The year in the associated timezone.
public extern year( t : time ) : int {
  cs "_Time.Year"
  js "_time_year"
}

// The month in the associated timezone.
public extern month( t : time ) : int {
  cs "_Time.Month"
  js "_time_month"
}

// The day in the associated timezone.
public extern day( t : time ) : int {
  cs "_Time.Day"
  js "_time_day"
}

// The hours in the associated timezone.
public extern hours( t : time ) : int {
  cs "_Time.Hours"
  js "_time_hours"
}

// The minutes in the associated timezone.
public extern minutes( t : time ) : int {
  cs "_Time.Minutes"
  js "_time_minutes"
}

// The seconds in the associated timezone.
public extern seconds( t : time ) : int {
  cs "_Time.Seconds"
  js "_time_seconds"
}

// The milliseconds in the associated timezone.
public extern milliseconds( t : time ) : int {
  cs "_Time.Milliseconds"
  js "_time_milliseconds"
}

// The day of the week in the associated time zone. 
// This is the ISO day of the week, starting at 1 for Monday and ending at 7 for Sunday.
public extern day-of-week(t:time) : int { 
  cs "_Time.DayOfWeek"
  js "_time_dayofweek" 
}

// Number of fractional seconds since  the _epoch_ (``1970-01-01Z``).
public extern epoch-seconds(t:time) : double { 
  cs "_Time.EpochSecs"
  js "_time_epochsecs" 
}

// Number of fractional seconds since  `min-time` (``0001-01-01Z``).
public extern total-seconds(t:time) : double { 
  cs "_Time.TotalSecs"
  js "_time_totalsecs" 
}

// Return the date of a time in the associated time zone. 
// Returns the year, month (starting at 1), and day of the month (starting at 1).
public fun date( t : time ) : (int,int,int) {
  (t.year,t.month,t.day)
}

// Return the clock time in the associated time zone. Returns the hours, minutes, seconds, and milliseconds.
public fun clock( t : time ) : (int,int,int,int) {
  (t.hours,t.minutes,t.seconds,t.milliseconds)
}

// Are two times equal? (regardless of timezone)
public fun (==)( t1: time, t2:time ) : bool {
  t1.total-seconds == t2.total-seconds
}

// Does time `t1` come before time `t2`? (regardless of timezone)
public fun (<)( t1: time, t2:time ) : bool {
  t1.total-seconds < t2.total-seconds
}

// Does time `t1` come after time `t2`? (regardless of timezone)
public fun (>)( t1 : time, t2: time ) : bool {
  t1.total-seconds > t2.total-seconds
}

// Return the fractional seconds difference between two times. (regardless of timezone)
public fun (-)( t1 : time, t2 : time ) : double {
  t1.total-seconds - t2.total-seconds
}

// The _epoch_ time (``1970-01-01Z``)
public val epoch = utc-time(1970,1,1)

// The minimal time value (``0001-01-01Z``).
public val min-time = utc-time(1,1,1)

// The maximal time value (``9999-12-31T23:59:59.999Z``).
public val max-time = utc-time(9999,12,31,23,59,59,999)


// Compare two times. (regardless of timezone)
public fun compare( t1 : time, t2 :time ) : order {
  if (t1==t2) then Eq
  elif (t1<t2) then Lt
  else Gt
}


// Create a new `:time`. Uses the local time zone by default. Use `utc-time` for a new time in the UTC timezone.
public fun time( year:int, month : int, day:int, hours:int = 0,minutes:int = 0,seconds:int = 0,milliseconds:int = 0) : ndet time {
  new-time(year,month,day,hours,minutes,seconds,milliseconds,False)
}

// Create a new `:time` in the local time zone with a given number of fractional seconds since `min-time` (0001-01-01Z).
public fun time( seconds : double ) : ndet time {
  new-time(seconds,False)
}

// Create a new UTC `:time`
public fun utc-time(year:int, month : int, day:int, hours:int = 0,minutes:int = 0,seconds:int = 0,milliseconds:int = 0) : time {
  new-time(year,month,day,hours,minutes,seconds,milliseconds,True)
}

// Create a new UTC `:time` with a given number of fractional seconds since `min-time` (0001-01-01Z).
public fun utc-time( seconds : double ) : time {
  new-time(seconds,True)
}

// Copy a `:time` value keeping it in the same timezone (local or UTC).
public fun copy(t : time, year:int = t.year, month : int = t.month, day:int = t.day, hours:int = t.hours,minutes:int = t.minutes,seconds:int = t.seconds,milliseconds:int = t.milliseconds) : time {
  new-time(year,month,day,hours,minutes,seconds,milliseconds,t.utc?)
}

// Is this time in the local timezone?
public fun local?( t : time ) : bool {
  !t.utc?
}

// Add a specified number of years, months, and days to a time. 
// Takes leap-years into account.
public fun add-date( t : time, years : int, months : int, days : int ) : time {
  val (y,m,d) = t.date
  val (h,min,s,ms) = t.clock
  new-time(y + years, m + months, d + days, h, min, s, ms, t.utc?)
}

// Add a specified number of hours, minutes, seconds, and optional milliseconds to a time.
public fun add-clock( t : time, hours : int, minutes : int, seconds : int, milliseconds : int = 0 ) : time {
  val (y,m,d) = t.date
  val (h,min,s,ms) = t.clock
  new-time(y, m, d, h + hours, min + minutes, s + seconds, ms + milliseconds, t.utc?)
}

// Add `n` days to a time. Takes leap years into account.
public fun add-days( t : time, days : int ) : time {
  t.add-date( 0, 0, days )
}

// -----------------------------------------------------------
// Advanced calendar functions
// -----------------------------------------------------------

// Is this time in a leap year?
public fun leap-year?( year : int ) : bool {
  ((year%4)==0 && ((year%100)!=0 || (year%400)==0))
}

// The number of days in the given year
public fun days-in-year( year : int ) : int {
  if (year.leap-year?) then 366 else 365
}

// Return the day of the year (starting at 1)
public fun day-of-year( t : time ) : int {
  val (year,month,day) = t.date
  val n = day-counts[month - 1].maybe(0) + day
  if (month > 2 && year.leap-year?) then n+1 else n
}
val day-counts = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]


// Return the ISO week number of the year (between 1 and 53).
public fun week-of-year( t : time ) : int {
  val week = ((t.day-of-year - t.day-of-week) + 10) / 7
  if (week<1) then (t.copy(t.year - 1, 1, 1)).last-week
  elif (week>t.last-week) then 1
  else week
}

// Last ISO week number of the year.
fun last-week( t : time ) : int {
  if (t.long-year?) then 53 else 52
}

// Returns true if the year contains 53 weeks (instead of 52)
fun long-year?( t : time ) : bool {
  val firstday = t.copy(t.year,1,1).day-of-week
  (firstday==4 || (t.year.leap-year? && firstday==3))
}

// Return the day of the year given an ISO week number and an ISO week day.
// The returned day maybe smaller than one (belonging to the previous year),
// or larger than the days of the year (belonging to the next year).
// The returned day number can always be passed directly to the `add-days` or
// the `time` / `utc-time` constructors.
public fun week-to-day-of-year( year : int, week : int, weekday : int ) : int {
  val adjust = utc-time(year,1,4).day-of-week + 3
  (((7*week) + weekday) - adjust)
}

// -----------------------------------------------------------
// Formatting
// -----------------------------------------------------------

// Show the time as an [ISO 8601](http://en.wikipedia.org/wiki/ISO_8601) string. 
// For example `now().show` becomes `"2016-09-08T19:55:11.910-07:00"`.
public fun show( t : time ) : string {
  t.show-iso
}

// Show the time as an [ISO 8601](http://en.wikipedia.org/wiki/ISO_8601) string.
// For example `now().show-iso` becomes `"2016-09-08T19:55:11.910-07:00"`.
public fun show-iso(t : time ) : string {
  t.format("YYYY-MM-DD'T'HH:mm:ssfffZ")
}

// Show the date in ISO format, i.e. ``YYYY-MM-DD`` format.
public fun show-iso-date( t : time ) : string {
  t.format("YYYY-MM-DD")
}

// Show the clock time in ISO format, i.e. ``HH:mm:ss`` format.
public fun show-iso-clock( t : time ) : string {
  t.format("HH:mm:ss")
}

// Show time as a standard [Internet Message Format](https://tools.ietf.org/html/rfc2822#section-3.3) date.\
// For example `now().show-imf` returns `"Fri, 9 Oct 2016 11:57:45 -0700"`
public fun show-imf( t : time ) : string {
  t.format("ddd, D MMM YYYY HH:mm:ss zz")
}

// Show the time as a human readable English string.
// For example `now().en-time` -> `"Thu, 8 Oct 2016, 19:48:11"`.
public fun show-en( t : time ) : string {
  t.format("ddd, D MMM YYYY, HH:mm:ss")
}

// Show the 24-hour clock time in human readable English.
// For example `now().en-time` -> `"09:48:11"`.
public fun show-en-clock( t : time ) : string {
  t.format("HH:mm:ss")
}

// Show the date in human readable English.
// For example `now().en-date` -> `"Thu, 8 Oct 2016"`.
public fun show-en-date( t : time ) : string {
  t.format("ddd, D MMM YYYY")
}



// -----------------------------------------------------------
// Ticks
// -----------------------------------------------------------

// Return a high-resolution timestamp in fractional seconds. 
// The timestamp is guaranteed to be monotonically increasing
// and have at least millisecond resolution. 
public extern ticks() : ndet double {
  cs "_Time.Ticks"
  js "_ticks"
}

// Return the smallest time difference in seconds that `ticks` can measure.
public extern ticks-resolution() : ndet double {
  cs inline "_Time.TicksResolution"
  js inline "_ticks_resolution"
}

// Return the number of fractional seconds that it takes to evaluate `action`.
public fun elapsed( action : () -> <ndet|e> a ) : <ndet|e> (double,a) {
  val t0 = ticks()
  val x = action()
  val t1 = ticks()
  (t1 - t0,x)
}

// Measure the number of fractional seconds that it takes to evaluate `action`, and print `msg` postfixed with the
// measured time in millisecond resolution.
public fun print-elapsed( action : () -> <ndet,console|e> a, msg : string = "elapsed" ) : <ndet,console|e> a {
  val (t,x) = elapsed(action)
  println( msg + " " + t.show-fixed(3) + "s" )
  x
} 



// -----------------------------------------------------------
// Generic Formatting
// -----------------------------------------------------------

fun format-weekday( t : time, n : int, locale : time-locale ) : string {
  if (n==1) then t.day-of-week.show else {
    val days = if (n==2) then locale.day-names-min
               elif (n==3) then locale.day-names-short
               else locale.day-names
    days[t.day-of-week - 1].default("?")               
  }
}

fun format-day( t : time, n : int, locale : time-locale ) : string {
  if (n==3) then t.day-of-year.show
  elif (n==4) then t.day-of-year.showl(3)
  else t.day.showl(n)
}

fun format-week( t : time, n : int, locale : time-locale ) : string {
  t.week-of-year.showl(n)
}

fun format-month( t : time, n : int, locale : time-locale ) : string {
  if (n<=2) then t.month.showl(n) else {
    val months = if (n==3) then locale.month-names-short else locale.month-names
    months[t.month - 1].default("?")
  }
}

fun format-year( t : time, n : int, locale : time-locale ) : string {
  if (n<5) then {
    t.year.abs0.showl(n).last(n).string
  }
  else {
    val y = t.year.abs0.showl(n)
    if (t.year<0) then "-" + y else "+" + y
  }
}

fun format-seconds( t : time, n : int, locale : time-locale ) : string {
  t.seconds.showl(n)
}

fun format-minutes( t : time, n : int, locale : time-locale ) : string {
  t.minutes.showl(n)
}

fun format-hours( t : time, n : int, locale : time-locale ) : string {
  t.hours.showl(n)
}

fun format-hours12( t : time, n : int, locale : time-locale ) : string {
  val h = t.hours
  if (h==0 || h==12) then "12"
  elif (h<=11) then h.showl(n) 
  else (h - 12).showl(n)
}

fun format-ampm( t : time, n : int, locale : time-locale ) : string {
  val h = t.hours
  (if (h<=11) then "am" else "pm").first(n).string
}

fun format-upper-ampm( t : time, n : int, locale : time-locale ) : string {
  format-ampm(t,n,locale).to-upper
}

fun format-frac( t : time, n : int, locale : time-locale ) : string {
  val ms   = t.clock.field4
  if (ms==0) return ""
  val frac = ms.double / 1000.0
  frac.show-fixed(n).tail
}

fun format-frac0( t : time, n : int, locale : time-locale ) : string {
  val ms = t.clock.field4
  "." + ms.showl(3).first(n).string.pad-right(n,"0")
}

fun format-tz-offset( t : time, n : int, hmsep : string, utc : string ) : string {
  show-timezone(t,hmsep=hmsep,utc=utc)
}

fun format-timezone( t : time, n : int, locale : time-locale ) : string {
  format-tz-offset(t,n,if (n==2) then "" else ":",if (n==2) then "+0000" else "+00:00")
}

fun format-utc-timezone( t : time, n : int, locale : time-locale ) : string {
  format-tz-offset(t,n,":",if (n==2) then "" else "Z")
}

fun format-timestamp( t : time, n : int, locale : time-locale ) : string {
  t.total-seconds.show-fixed(3)
}


val formats : list<(char,int,(time,int,time-locale) -> string)> = [
  ('Y', 6, format-year),
  ('M', 4, format-month),
  ('D', 4, format-day),
  ('H', 2, format-hours),
  ('m', 2, format-minutes),
  ('s', 2, format-seconds),
  ('z', 2, format-timezone),
  ('Z', 2, format-utc-timezone),
  ('f', 8, format-frac0),
  ('F', 8, format-frac),
  ('d', 4, format-weekday),
  ('w', 2, format-week),
  ('h', 2, format-hours12),
  ('a', 2, format-ampm),
  ('A', 2, format-upper-ampm),
  ('x', 1, format-timestamp), 
]

/* Format a time according to format string `fmt` and using a optional
   time locale (= `time-locale-en-iso`).

Letters (``a`` to ``z``) are always interpreted as a pattern where unknown letter
patterns are ignored. Any literal text should be quote-escaped i.e. use `"'GMT'ZZ"` 
to display as `"GMT-07:00"` (in the PST time zone). Any characters other then
an ascii letter are displayed as is.

Patterns of 2 letters are zero-padded on the left to always
display as 2 digits. Allowed patterns:

* ``YY``, ```YYYY``: the year in upto 4 digits (``70``,``1970``).
* ``M``, ``MM``: the month. (``1``,``03``)
* ``MMM``, ``MMMM``: name of the month in English. (``Jan``, ``January``)
* ``D``,``DD``: the day of the month. (``1``,``08``)
* ``DDD``,``DDDD``: the day of the year. (``87``,``087``)
* ``d``: the ISO day of the week, 1 for Monday ending in 7 for Sunday.
* ``dd``,``ddd``,``dddd``: the day of the week in English. (``We``,``Wed``,``Wednesday``)
* ``w``,``ww``, the ISO week number of the year between 1 and 53. (``8``,``08``)
* ``h``,``hh``: the hours using a 12-hour clock (with am/pm). (``9``,``09``)
* ``H``,``HH``: the hours using a 24-hour clock. (``21``,``09``)
* ``a``,``aa``: AM/PM designation. (``a``,``am``)
* ``A``,``AA``: AM/PM designation in upper-case. (``A``,``AM``)
* ``f``,...,``ffffffff``: upto 8 digits of a fraction of a second. Starts with a ``.``. (``.320``,``.000``)
* ``F``,...,``FFFFFFFF``: upto 8 digits of a fraction of a second. If not zero, starts with a ``.``.
    In contrast to the ``f`` patterns displays the minimal number of required digits 
    (and is not right-padded with zeros). (``.32``)
* ``z``: timezone offset in hours and minutes separated by a colon. Use ``+00:00`` for UTC time. (``+01:00``)
* ``zz``: timezone offset in hours and minutes without a separator. Use ``+0000`` for UTC time. (``-0700``)
* ``Z``: timezone offset in hours and minutes separated by a colon, use ``Z`` for UTC time.   
* ``ZZ``: timezone offset in hours and minutes separated by colon, use an empty string for UTC time.   
* ``x``: fractional seconds since `min-time`. (``63610768799.429``)
* ``YYYYYY``: the year in [ECMAscript](http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.1.15.1) 6 digits, prepended with the sign. (``+002016``,``-000023``,``+000000``)
* ``'...'``, ``"..."``: anything between quotes is displayed as is. (``'M'M`` becomes ``M11`` for November)

There are also various forms to display dates and times in a locale specific way.
We give examples in English and Dutch. The lower-case ``l`` variants use short
names for month- and day names.

* ``t``: hours and minutes (``3:21pm, 15:21``)
* ``tt``: hours, minutes, and seconds (``3:21:01pm, 15:21:01``)
* ``L``,``l``: a date (``09/29/2016, 29.09.2016``) and (``9/29/2016, 29.9.2016``)
* ``LL``,``ll``: date with month name (``29 September 2016, 29 september 2016``) and (``29 Sep 2016, 29 sep 2016``)
* ``LLL``,``lll``: date with month name and time (``29 September 2016 1:15pm, 29 september 2016 13:15``)
* ``LLLL``,``llll``: date with day name, month name, and time (``Thursday, 29 September 2016 1:15pm``) and (``Thu, 29 Sep 2016 1:15pm``)

For example, to display a time in the standard [Internet Message Format](https://tools.ietf.org/html/rfc2822#section-3.3)
you can use `now().format("ddd, D MMM YYYY HH:mm:ss zz")` displayed as `"Tue, 27 Sep 2016 06:36:55 -0700"` for example.
*/
public fun format( t : time, fmt : string, locale : time-locale = time-locale-en-iso ) : string {
  format-list( t, fmt.expand-locales(locale).expand-locales(locale).list, locale )
}

fun format-list( t : time, fmt : list<char>, locale : time-locale ) : string {  
  match(fmt) {
    Nil -> ""
    Cons(c,cs) -> {
      val (formatted,rest) = format-pat(t,c,cs,locale)
      formatted + format-list(t, unsafe-decreasing(rest), locale)
    }
  }
}

fun format-pat( t : time, h : char, fmt : list<char>, locale : time-locale ) : (string,list<char>) {
  if (h=='"' || h=='\'') {
    // extract escaped part
    val (quoted,end) = fmt.span( fun(c) { c!=h } )
    match(end) {
      Nil          -> (h.string, fmt)
      Cons(_,rest) -> (quoted.string, rest)    
    }
  }
  elif(h.alpha?) {
    // match pattern
    val (hs,rest) = fmt.span(fun(c){c==h})
    val n = hs.length + 1
    formats.foreach-while fun(pattern) {
      val (c,m,f) = pattern
      if (c!=h) then Nothing else Just((f(t,min(n,m),locale), rest))
    }.default( ("",rest) ) // ignore if no match
  }
  else (h.string,fmt) // return as-is
}

fun expand-locales( fmt : string, locale : time-locale ) : string {
  fmt.replace-all(rx-locale) fun(cap) {
    val n = cap.matched.count
    if (cap.groups[1] != "") then {
      cap.matched
    }
    elif (cap.groups[2] != "") then {
      if (n>=2) then locale.format-tt else locale.format-t
    }
    else {
      val nfmt = if (n>=4) then locale.format-llll elif (n==3) then locale.format-lll
                 elif (n==2) then locale.format-ll else locale.format-l
      if (cap.groups[3] != "") then {
        nfmt
      }
      else {
        nfmt.replace-all(rx-long, fun(capl) { capl.matched.tail })
      }
    }
  }
}
val rx-locale = regex(@"('[^']*'|""[^""]*"")|(t+)|(L+)|(l+)")
val rx-long   = regex(@"dddd+|MMMM+|DD(?!D)")

// -----------------------------------------------------------
// Format helpers
// -----------------------------------------------------------

// Show the timezone offset (by default in the ISO 8601 format).
// `now().show-timezone` becomes `"-07:00"`.
// The `utc` (`"Z"`) parameter gives the form used for the UTC timezone,
// while the `hmsep` (`":"`) gives the separator used between the hours and minutes of
// other timezone offsets. The `hrwidth` (=`2`) parameter gives the minimal width of the hours part.
fun show-timezone( t : time, utc : string = "Z", hmsep : string = ":", hrwidth : int = 2 ) : string {
  val ofs    = t.timezone-offset / 60  // convert to minutes
  if (ofs==0) return utc
  val absofs = if (ofs >= 0) then ofs else ~ofs
  (if (ofs >= 0) then "+" else "-") + 
   ((absofs / 60).showl(hrwidth)) + hmsep + ((absofs % 60).showl)
}

fun showl( i : int, width : int = 2 ) : string {
  i.show.pad-left(width,"0")
}



// -----------------------------------------------------------
// Parsing 
// -----------------------------------------------------------

/* Parse an [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) time string.
   Always returns a UTC time. Use `local` to convert to the local time.

````plain
2008-12-31               // just date
2009-W01-3               // week number and day
2008-366                 // day of the year

20081231, 2009W013, 2008366  // without dashes

2008-12-31T09            // with time part, use T or space
2008-12-31 09
2008-12-31T09:20:16
2008-12-31T09:20:16.345  // with milliseconds (can be any fraction)
2008-12-30T24:00:00      // next day at midnight
20081231T092016.345      // without separators

2008-12-31T09Z           // UTC time zone (Z)
2008-12-31 09-07:00      // UTC-07:00 time zone
2008-12-31T09:20:16+0830 // UTC+08:30 time zone (without colon)
````
. 
*/
public fun parse-iso( s : string ) : maybe<time> {
  match(s.find(rx-iso-date)) {
    Nothing -> Nothing
    Just(capd) -> {
      val xyear = capd.num(1)
      val (year,month,day) =
        if (capd.groups[2] != "") 
          then (xyear,capd.num(2),capd.num(3)) 
          else {
            val day = if (capd.groups[4] != "") 
                       then week-to-day-of-year(xyear,capd.num(4),capd.num(5,1))
                       else capd.num(6)
            utc-time(xyear,1,day).date
          }
      match(capd.slice.after.string.find(rx-iso-time)) {
        Nothing    -> Nothing
        Just(capt) -> {
          val hours = capt.num(1)
          val mins  = capt.num(2)
          val secs  = capt.num(3)
          val msecs = parse-int-default((capt.groups[4] + "000").first(3).string)
          val tzsign= if (capt.groups[5]=="+") then 1 else ~1
          val tzhours= capt.num(6) 
          val tzmins = capt.num(7)
          val tzofs = tzsign * ((tzhours * 60) + tzmins)
          Just(utc-time(year,month,day,hours,mins - tzofs,secs,msecs))
        }
      }
    }
  }
}
val rx-iso-date = regex(@"^(\d\d\d\d)\-?(?:(\d\d)\-?(\d\d)|W(\d\d)\-?(\d)?|(\d\d\d))(?=$|[T\+\- ]\d|Z)");
val rx-iso-time = regex(@"^(?:[T ](\d\d)(?:\:?(\d\d)(?:\:?(\d\d))?)?(?:[\.,](\d+))?)?(?:Z|([\+\-])(\d\d)(?:\:?(\d\d))?)?$")

fun num( cap : matched, group : int, default : int = 0 ) : int {
  parse-int-default(cap.groups[group], default = default)
}

// Parse an [Internet Message Format](https://tools.ietf.org/html/rfc2822#section-3.3) time string.
// Always returns a UTC time. Use `local` to convert to the local time.
public fun parse-imf( s : string ) : maybe<time> {
  match(s.find(rx-imf)) {
    Nothing -> Nothing
    Just(cap) -> {
      val day   = cap.num(2)
      val month = month-from-name(cap.groups[3]).default(1)
      val xyear = cap.num(4)
      val hours = cap.num(5)
      val mins  = cap.num(6)
      val secs  = cap.num(7)
      val tzsign= if (cap.groups[8]=="+") then 1 else ~1
      val tzhours= cap.num(9) 
      val tzmins = cap.num(10)
      // adjust 2 digit year
      val year  = if (cap.groups[4].count <= 2) 
                   then (if (xyear < 50) then 2000 + xyear else 1900 + xyear)
                   else xyear
      val tzofs = tzsign * ((tzhours * 60) + tzmins)
      Just(utc-time(year,month,day,hours,mins - tzofs,secs,0))
    }
  }
}
val rx-imf = regex(@"^ *(?:(Mon|Tue|Wed|Thu|Fri|Sat|Sun) *, *)? (\d\d?) *(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) *(\d\d(?:\d\d)?) +(\d\d?):(\d\d?)(?::(\d\d))? *(?:([\+\-])(\d\d)(\d\d) *)?$")

fun month-from-name( mname : string ) : maybe<int> {
  val lname = mname.to-lower
  fun search( names : list<string>, month : int ) : maybe<int> {
    match(names) {
      Nil -> Nothing
      Cons(name,rest) -> {
        if (name.to-lower.starts-with(lname).bool) 
         then Just(month) 
         else search(rest,month+1)
      }
    }
  }
  search(time-locale-en-iso.month-names,1)
}


// -----------------------------------------------------------
// Time locale
// -----------------------------------------------------------

// The `:time-locale` describes time and date formats for a specific locale.
public struct time-locale(
  // Language name, for example `en` or `fr-ca`.                          
  lang-name       : string,                            
  day-names       : list<string>,
  month-names     : list<string>,
  month-names-short: list<string> = month-names.map(fun(m) { m.first(3).string }),
  day-names-short : list<string>  = day-names.map(fun(m) { m.first(3).string }),
  day-names-min   : list<string>  = day-names.map(fun(m) { m.first(2).string }),
  format-t        : string = "HH:mm",
  format-tt       : string = "HH:mm:ss",
  format-l        : string = "YYYY-MM-DD",
  format-ll       : string = "D MMMM YYYY",
  format-lll      : string = "D MMMM YYYY t",
  format-llll     : string = "dddd D MMMM YYYY t"
)

// English time locale (`en`)
public val time-locale-en : time-locale = Time-locale( "en",
  ["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"],
  ["January","February","March","April","May","June","July","August","September","October","November","December"],
  format-t    = "h:mmaa",
  format-tt   = "h:mm:ssaa",
  format-l    = "MM/DD/YYYY",
  format-llll = "dddd, D MMMM YYYY t"
)

// ISO English time locale (`en-iso`). Uses English names for
// months and days but displays numeric dates and times using unambigious ISO format. 
public val time-locale-en-iso : time-locale = Time-locale( "en-iso",
  day-names   = time-locale-en.day-names,
  month-names = time-locale-en.month-names
)

// French time locale (`fr`)
public val time-locale-fr : time-locale = Time-locale( "fr",                                                      
  ["lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"],
  ["janvier","février","mars","avril","mai","juin","juillet","août","septembre","octobre","novembre","décembre"],
  ["janv.","févr.","mars","avr.","mai","juin","juil.","août","sept.","oct.","nov.","déc."],
  ["lun.","mar.","mer.","jeu.","ven.","sam.","dim."],
  ["Lu","Ma","Me","Je","Ve","Sa","Di"],
  format-l    = "DD/MM/YYYY"
)

// German time locale (`de`)
public val time-locale-de : time-locale = Time-locale( "de",
  ["Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag","Sonntag"],
  ["Januar","Februar","März","April","Mai","Juni","Juli","August","September","Oktober","November","Dezember"],
  ["Jan.","Febr.","Mrz.","Apr.","Mai","Jun.","Jul.","Aug.","Sept.","Okt.","Nov.","Dez."],
  ["Mo.","Di.","Mi.","Do.","Fr.","Sa.","So."],
  ["Mo","Di","Mi","Do","Fr","Sa","So"],
  format-l    = "DD.MM.YYYY",
  format-ll   = "D. MMMM YYYY",
  format-lll  = "D. MMMM YYYY t",
  format-llll = "dddd, D. MMMM YYYY t"
)

// Spanish time locale (`es`)
public val time-locale-es : time-locale = Time-locale( "es",
  ["lunes","martes","miércoles","jueves","viernes","sábado","domingo"],
  ["enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre"],
  format-t    = "H:mm",
  format-tt   = "H:mm:ss",
  format-l    = "DD/MM/YYYY",
  format-ll   = "D 'de' MMMM 'de' YYYY",
  format-lll  = "D 'de' MMMM 'de' YYYY H:mm",
  format-llll = "dddd, D 'de' MMMM 'de' YYYY H:mm"
)

// Dutch time locale (`nl`)
public val time-locale-nl : time-locale = Time-locale( "nl",
  ["maandag","dinsdag","woensdag","donderdag","vrijdag","zaterdag","zondag"],
  ["januari","februari","maart","april","mei","juni","juli","augustus","september","oktober","november","december"],
  ["jan","feb","mrt","apr","mei","jun","jul","aug","sep","okt","nov","dec"],
  ["ma.","di.","wo.","do.","vr.","za.","zo."],
  ["Ma","Di","Wo","Do","Vr","Za","Zo"],
  format-l    = "DD-MM-YYYY"
)

val locales = [
  time-locale-de,time-locale-en,
  time-locale-es,time-locale-fr,
  time-locale-nl,time-locale-en-iso
]

// Return a builtin locale given a locale string (e.g. `en-GB`, `es`); 
// Use `time-locale-iso` if no particular match is found.
// Supports `de`,`en`,`es`,`fr`,`nl` and `en-iso`.
public fun get-time-locale( locale : string ) : time-locale {
  locales.find(fun(l) { locale.starts-with(l.lang-name).bool }).default(time-locale-en-iso)
}