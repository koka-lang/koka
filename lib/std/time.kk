/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Basic date and time functions.

   Years, months, days etc. are always 1-based, while durations
   are always in milliseconds.
*/
module std/time

extern include {
  cs file "time-inline.cs"
  js file "time-inline.js"
}

// A `:time` represents a certain time in a gregorian calendar.
// When a time is captured, it is associated with either the local 
// or UTC timezone which is used for displaying the time.
public type time

// Return the current time in the local timezone.
public extern now : () -> ndet time { 
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

// Get the timezone offset in milliseconds. This 
// is the difference between the local time and UTC.
// For example, if the timezone is ``UTC+10``, the timezone offset is ``+10 * 60 * 60 * 1000``.
public extern timezone-offset(t : time) : int {
  cs "_Time.TimezoneOffset"
  js "_time_timezone_offset"
}

extern new-time(year:int, month : int, day:int,hours:int,minutes:int,seconds:int,milliseconds:int, is-utc:bool ) : time {
  cs "_Time.New"  
  js "_time_new"
}

extern new-time( epoch-msecs : double, is-utc:bool ) : time {
  cs "_Time.NewFromEpoch"  
  js "_time_epoch"
}

// Return the date of a time in the associated time zone. 
// Returns the year, month (starting at 1), and day of the month (starting at 1).
public extern date( t : time ) : (int,int,int) {
  cs "_Time.Date"
  js "_time_date"
}

// Return the clock time in the associated time zone. Returns the hours, minutes, seconds, and milliseconds.
public extern clock( t : time ) : (int,int,int,int) {
  cs "_Time.Clock"
  js "_time_clock"
}

// The day of the week in the associated time zone. Starts at 1 for Sunday.
public extern day-of-week(t:time) : int { 
  cs inline "(int)(t.DayOfWeek + 1)"
  js inline "(#1.time.getDay() + 1)" 
}

// Number of milliseconds since  the `epoch` (1 January 1970 00\:00\:00 UTC).
public extern epoch-msecs(t:time) : double { 
  cs "_Time.EpochMsecs"
  js inline "(#1.time.getTime())" 
}

// Do two times occur at the same time? (regardless of timezone)
public fun (==)( t1: time, t2:time ) : bool {
  t1.epoch-msecs == t2.epoch-msecs
}

// Does time `t1` come before time `t2`?
public fun (<)( t1: time, t2:time ) : bool {
  t1.epoch-msecs < t2.epoch-msecs
}


// Does time `t1` come after time `t2`?
public fun (>)( t1 : time, t2: time ) : bool {
  t1.epoch-msecs > t2.epoch-msecs
}

// Return the millisecond difference between two times
public fun (-)( t1 : time, t2 : time ) : double {
  t1.epoch-msecs - t2.epoch-msecs
}

// The _epoch_ date (1 January 1970 00\:00\:00 UTC)
public val epoch : time = utc-time(1970,1,1)


// Compare two times.
public fun compare( t1 : time, t2 :time ) : order {
  if (t1==t2) then Eq
  elif (t1<t2) then Lt
  else Gt
}


// Create a new `:time`. Uses the local time zone by default. Use `utc-time` for a new time in the UTC timezone.
public fun time( year:int, month : int, day:int, hours:int = 0,minutes:int = 0,seconds:int = 0,milliseconds:int = 0) : ndet time {
  new-time(year,month,day,hours,minutes,seconds,milliseconds,False)
}

// Create a new `:time` in the local time zone with a given number milliseconds since the epoch (1 January 1970 00\:00\:00 UTC).
public fun time( epochmsecs : double ) : ndet time {
  new-time(epochmsecs,False)
}

// Create a new UTC `:time`
public fun utc-time(year:int, month : int, day:int, hours:int = 0,minutes:int = 0,seconds:int = 0,milliseconds:int = 0) : time {
  new-time(year,month,day,hours,minutes,seconds,milliseconds,True)
}

// Create a new UTC `:time` with a given number milliseconds since the `epoch` (1 January 1970 00\:00\:00 UTC).
public fun utc-time( epochmsecs : double ) : time {
  new-time(epochmsecs,True)
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


// Is this time in a leap year?
public fun leap-year?( t : time ) : bool {
  val (year,_,_) = t.date();
  return (year%4)==0 && ((year%100)!=0 || (year%400)==0);
}

// Return the day of the year (starting at 1)
public fun day-of-year( t : time ) : int {
  val (_,month,day) = t.date()
  val n = day-counts[month - 1].maybe(0) + day
  if (month > 2 && t.leap-year?) then n+1 else n
}
val day-counts = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]


// -----------------------------------------------------------
// Formatting
// -----------------------------------------------------------


// Show the time as an ISO 8601 string. 
// See <http://en.wikipedia.org/wiki/ISO_8601>.
// For example `now().show` becomes `"2016-09-08T19:55:11.910-07:00"`.
public fun show( t : time ) : string {
  t.show-iso
}

// Show the time as an ISO 8601 string.
// See <http://en.wikipedia.org/wiki/ISO_8601>.
// For example `now().show-iso` becomes `"2016-09-08T19:55:11.910-07:00"`.
public fun show-iso(t : time ) : string {
  t.format("yyyy-MM-dd'T'HH:mm:ssFFFK")
}

// Show the date in ISO format, i.e. ``YYYY-MM-DD`` format.
public fun show-iso-date( t : time ) : string {
  t.format("yyyy-MM-dd")
}

// Show the clock time in ISO format, i.e. ``HH:mm:ss`` format.
public fun show-iso-clock( t : time ) : string {
  t.format("HH:mm:ss")
}

// Show time as a standard Internet Message Format date (See <https://tools.ietf.org/html/rfc2822#section-3.3>).\
// For example `now().show-imf` returns `"Fri, 9 Oct 2016 11:57:45 -0700"`
public fun show-imf( t : time ) : string {
  t.format("ddd, d MMM yyyy HH:mm:ss ZZZ")
}

// Show the time as a human readable English string.
// For example `now().en-time` -> `"Thu, 8 Oct 2016, 19:48:11"`.
public fun show-en( t : time ) : string {
  t.format("ddd, d MMM yyyy, HH:mm:ss")
}

// Show the 24-hour clock time in human readable English.
// For example `now().en-time` -> `"09:48:11"`.
public fun show-en-clock( t : time ) : string {
  t.format("HH:mm:ss")
}

// Show the date in human readable English.
// For example `now().en-date` -> `"Thu, 8 Oct 2016"`.
public fun show-en-date( t : time ) : string {
  t.format("ddd, d MMM yyyy")
}


// -----------------------------------------------------------
// Format helpers
// -----------------------------------------------------------

// Show the timezone offset in ISO format.
// `now().show-timezone` -> `"-07:00"`.
// The `utc` (`"Z"`) parameter gives the form used for the UTC timezone,
// while the `hmsep` (`":"`) gives the separator used between the hours and minutes of
// other timezone offsets. The `hrwidth` (=`2`) parameter gives the minimal width of the hours part.
fun show-timezone( t : time, utc : string = "Z", hmsep : string = ":", hrwidth : int = 2 ) : string {
  val ofs    = t.timezone-offset / 60000  // convert to minutes
  if (ofs==0) return utc
  val absofs = if (ofs >= 0) then ofs else ~ofs
  (if (ofs >= 0) then "+" else "-") + 
   ((absofs / 60).showl(hrwidth)) + hmsep + ((absofs % 60).showl)
}

fun showl( i : int, width : int = 2 ) : string {
  i.show.pad-left(width,"0")
}

// Return the full day of the week in English
fun show-en-day( t : time ) : string {
  days[t.day-of-week - 1].maybe("?")
}

// Return the full month in English
fun show-en-month( t : time ) : string {
  val (_,month,_) = t.date
  months[month - 1].maybe("?")
}

val days = ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]
val months = ["January","February","March","April","May","June",
              "July","August","September","October","November","December"]


// -----------------------------------------------------------
// Generic Formatting
// -----------------------------------------------------------

fun format-day( t : time, n : int ) : string {
  if (n==3) then t.show-en-day.first(3).string
  elif (n==4) then t.show-en-day
  else t.date.thd.showl(n)
}

fun format-month( t : time, n : int ) : string {
  if (n==3) then t.show-en-month.first(3).string
  elif (n==4) then t.show-en-month
  else t.date.snd.showl(n)
}

fun format-year( t : time, n : int ) : string {
  t.date.fst.showl(n).last(n).string
}

fun format-seconds( t : time, n : int ) : string {
  t.clock.thd.showl(n)
}

fun format-minutes( t : time, n : int ) : string {
  t.clock.snd.showl(n)
}

fun format-hours( t : time, n : int ) : string {
  t.clock.fst.showl(n)
}

fun format-hours12( t : time, n : int ) : string {
  val h = t.clock.fst
  if (h <= 12) then h.showl(n) else (h - 12).showl(n)
}

fun format-ampm( t : time, n : int ) : string {
  val h = t.clock.fst
  (if (h <= 12) then "am" else "pm").first(n).string
}

fun format-upper-ampm( t : time, n : int ) : string {
  format-ampm(t,n).to-upper
}

fun format-frac( t : time, n : int ) : string {
  val ms   = t.clock.field4
  if (ms==0) return ""
  val frac = ms.double / 1000.0
  frac.show-fixed(n).tail
}

fun format-frac0( t : time, n : int ) : string {
  val ms = t.clock.field4
  "." + ms.showl(3).first(n).string.pad-right(n,"0")
}

fun format-tz-offset( t : time, n : int, hmsep : string, utc : string ) : string {
  if (n<=2) then {
    val s = show-timezone(t,hmsep=":",hrwidth=n)
    match(s.find(":")) {
      Just(slice) -> slice.before.string
      Nothing     -> s.first(3).string  // should never happen
    }
  }
  else {
    show-timezone(t,hmsep=hmsep,utc=utc)
  } 
}

fun format-timezone( t : time, n : int ) : string {
  format-tz-offset(t,n,":",if (n<4) then "+00:00" else "")
}

fun format-imf-timezone( t : time, n : int ) : string {
  format-tz-offset(t,n,"",if (n<4) then "+0000" else "")
}

fun format-iso-timezone( t : time, n : int ) : string {
  format-tz-offset(t,4,":","Z")
}

val formats : list<(char,int,(time,int) -> string)> = [
  ('y', 5, format-year),
  ('M', 4, format-month),
  ('d', 4, format-day),
  ('H', 2, format-hours),
  ('m', 2, format-minutes),
  ('s', 2, format-seconds),
  ('K', 1, format-iso-timezone),
  ('z', 4, format-timezone),
  ('Z', 4, format-imf-timezone),
  ('f', 8, format-frac0),
  ('F', 8, format-frac),
  ('h', 2, format-hours12),
  ('t', 2, format-ampm),
  ('T', 2, format-upper-ampm),
]

/* Format a time according to format string `fmt`.

Letters (``a`` to ``z``) are always interpreted as a pattern where unknown letter
patterns are ignored. Any literal text should be quote-escaped i.e. use `"'GMT'zzzz"` 
to display as `"GMT-07:00"` (in the PST time zone). Any characters other then
an ascii letter are displayed as is.

Patterns of 2 letters are zero-padded on the left to always
display as 2 digits. Allowed patterns:

* ``y``,``yy``,``yyy``,``yyyy``,``yyyyy``: the year in upto 5 digits (``yyyy`` becomes ``2016``)
* ``M``, ``MM``: the month. (``1``,``03``)
* ``MMM``, ``MMMM``: name of the month in English. (``Jan``, ``January``)
* ``d``,``dd``: the day of the month. (``1``,``08``)
* ``ddd``,``dddd``: the day of the week in English. (``Wed``,``Wednesday``)
* ``h``,``hh``: the hours using a 12-hour clock (with am/pm). (``9``,``09``)
* ``H``,``HH``: the hours using a 24-hour clock. (``21``,``09``)
* ``t``,``tt``: AM/PM designation. (``a``,``am``)
* ``T``,``TT``: AM/PM designation in upper-case. (``A``,``AM``)
* ``f``,...,``ffffffff``: upto 8 digits of afraction of a second. Starts with a ``.``. (``.320``,``.000``)
* ``F``,...,``FFFFFFFF``: upto 8 digits of a fraction of a second. If not zero, starts with a ``.``.
    In contrast to the ``f`` patterns displays the minimal number of required digits 
    (and is not right-padded with zeros). (``.32``)
* ``z``,``zz``: timezone offset in hours. (``-7``,``-07``)
* ``zzz``,``zzzz``: timezone offset in hours and minutes separated by a colon. UTC time for ``zzz`` is displayed as ``+00:00`` while
   with ``zzzz`` UTC is an empty string. (``-07:00``)
* ``Z``,``ZZ``: timezone offset in hours. (``-7``,``-07``)
* ``ZZZ``,``ZZZZ``: timezone offset in hours and minutes. UTC time for ``ZZZ`` is displayed as ``+0000`` while
   with ``ZZZZ`` UTC is an empty string. (``-0700``,``+0800``)
* ``K``: timezone in the ISO8601 standard using ``Z`` for UTC. (``Z``,``+08:00``) 
* ``'...'``, ``"..."``: anything between quotes is displayed as is. (``'M'M`` becomes ``M11`` for November)

For example, to display a time in the standard [Internet Message Format](https://tools.ietf.org/html/rfc2822#section-3.3)
you can use `now().format("ddd, d MMM yyyy HH:mm:ss ZZZ")` displayed as `"Tue, 27 Sep 2016 06:36:55 -0700"` for example.
*/
public fun format( t : time, fmt : string ) : string {
  format-list( t, fmt.list )
}

fun format-list( t : time, fmt : list<char> ) : string {  
  match(fmt) {
    Nil -> ""
    Cons(c,cs) -> {
      val (formatted,rest) = format-pat(t,c,cs)
      formatted + format-list(t, unsafe-decreasing(rest))
    }
  }
}

fun format-pat( t : time, h : char, fmt : list<char> ) : (string,list<char>) {
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
      if (c!=h) then Nothing else Just((f(t,min(n,m)), rest))
    }.default( ("",rest) ) // ignore if no match
  }
  else (h.string,fmt) // return as-is
}


// -----------------------------------------------------------
// Ticks
// -----------------------------------------------------------

// Return a high-resolution timestamp in milliseconds. 
// The timestamp is guaranteed to be monotonically increasing
// and have at least millisecond resolution. 
public extern ticks() : ndet double {
  cs "_Time.Ticks"
  js "_ticks"
}

// Return the smallest time difference in milliseconds that `ticks` can measure.
public extern ticks-resolution() : ndet double {
  cs inline "_Time.TicksResolution"
  js inline "_ticks_resolution"
}

// Return the number of milliseconds that it takes to evaluate `action`.
public fun elapsed( action : () -> <ndet|e> () ) : <ndet|e> double {
  val t0 = ticks()
  action()
  val t1 = ticks()
  (t1 - t0)
}

// Measure the number of milliseconds that it takes to evaluate `action`, and print `msg` postfixed with the
// measured time in millisecond resoltion.
public fun print-elapsed( action : () -> <ndet,console|e> a, msg : string = "elapsed" ) : <ndet,console|e> a {
  val t0 = ticks()
  val x  = action()
  val t1 = ticks()
  val t  = (t1 - t0)
  println( msg + " " + t.show-fixed(0) + "ms" )
  x
} 
