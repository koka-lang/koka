/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Basic date and time functions.
*/
public module std/time

extern include {
  js file "time-inline.js"
}

// A `:time` represents a certain time in a gregorian calendar.
// It is displayed with regard to either the local or UTC timezone.
abstract struct time( tzofs : int, xtime : any )

// Return the current time in the local timezone.
extern now : () -> ndet time { 
  js "_time_now"
}

private extern new-time(year:int, month : int, day:int,hours:int,minutes:int,seconds:int,milliseconds:int, is-utc:bool ) : time {
  js "_time_new"
}

private extern new-time( epochmsecs : double, is-utc:bool ) : time {
  js "_time_epoch"
}

// Return the date of a time in the associated time zone. 
// Returns the year, month (starting at 1), and day of the month (starting at 1).
extern date( t : time ) : (int,int,int) {
  js "_time_date"
}

// Return the clock time in the associated time zone. Returns the hours, minutes, seconds, and milliseconds.
extern clock( t : time ) : (int,int,int,int) {
  js "_time_clock"
}

// The day of the week in the associated time zone. Starts at 0 for Sunday.
extern day-of-week(t:time) : int { 
  js inline "(#1.xtime.getDay())" 
}

// Number of milliseconds since 1 January 1970 00\:00\:00 UTC.
extern epoch-msecs(t:time) : double { 
  js inline "(#1.xtime.getTime())" 
}

// Do two times occur at the same time? (regardless of timezone)
extern (==)( t1: time, t2:time ) : bool {
  js inline "(#1.xtime.getTime() === #2.xtime.getTime())"
}

// Does time `t1` come before time `t2`?
extern (<)( t1: time, t2:time ) : bool {
  js inline "(#1.xtime.getTime() < #2.xtime.getTime())"
}

// Does time `t1` come after time `t2`?
extern (>)( t1 : time, t2: time ) : bool {
  js inline "(#1.xtime.getTime() > #2.xtime.getTime())"
}

// Return the millisecond difference between two times
extern (-)( t1 : time, t2 : time ) : double {
  js inline "(#1.xtime.getTime() - #2.xtime.getTime())"
}

// Compare two times.
fun compare( t1 : time, t2 :time ) : order {
  if (t1==t2) then Eq
  elif (t1<t2) then Lt
  else Gt
}



// Create a new `:time`. Uses the local time zone by default. Use `utc-time` for a new time in the UTC timezone.
fun time( year:int, month : int, day:int, hours:int = 0,minutes:int = 0,seconds:int = 0,milliseconds:int = 0) : ndet time {
  new-time(year,month,day,hours,minutes,seconds,milliseconds,False)
}

// Create a new `:time` in the local time zone with a given number milliseconds since the epoch (1 January 1970 00\:00\:00 UTC).
fun time( epochmsecs : double ) : ndet time {
  new-time(epochmsecs,False)
}

// Create a new UTC `:time`
fun utc-time(year:int, month : int, day:int, hours:int = 0,minutes:int = 0,seconds:int = 0,milliseconds:int = 0) : time {
  new-time(year,month,day,hours,minutes,seconds,milliseconds,True)
}

// Create a new UTC `:time` with a given number milliseconds since the epoch (1 January 1970 00\:00\:00 UTC).
fun utc-time( epochmsecs : double ) : time {
  new-time(epochmsecs,True)
}

// Convert a time to the UTC timezone.
// i.e. `utc-now() === now().utc`
fun utc( d : time ) : time {
  d(tzofs = 0)
}

// Convert a date to the local timezone.
fun local( d : time ) : ndet time {
  d(tzofs = now().tzofs)
}

// Return the current time in the UTC timezone.
fun utc-now() : ndet time { 
  now().utc
}


// Is this time in the UTC timezone?
fun utc?( d : time ) : bool {
  (d.tzofs == 0)
}

// Get the timezone offset in minutes. This 
// is the difference in minutes between UTC and local time.
// For example, if the timezone is UTC+10, the timezone offset is -600.
fun timezone-offset(d : time) : int {
  d.tzofs
}



// Add a specified number of years, months, and days to a time. 
// Takes leap-years into account.
fun add-date( t : time, years : int, months : int, days : int ) : time {
  val (y,m,d) = t.date
  val (h,min,s,ms) = t.clock
  new-time(y + years, m + months, d + days, h, min, s, ms, t.utc?)
}

// Add a specified number of hours, minutes, seconds, and optional milliseconds to a time.
fun add-clock( t : time, hours : int, minutes : int, seconds : int, milliseconds : int = 0 ) : time {
  val (y,m,d) = t.date
  val (h,min,s,ms) = t.clock
  new-time(y, m, d, h + hours, min + minutes, s + seconds, ms + milliseconds, t.utc?)
}

// Add `n` days to a time. Takes leap years into account.
fun add-days( t : time, days : int ) : time {
  t.add-date( 0, 0, days )
}


// Show the time as an ISO 8601 string. 
// See <http://en.wikipedia.org/wiki/ISO_8601>.
// For example `now().show` -> `"2016-09-08T19:55:11.910-07:00"`.
fun show( t : time ) : string {
  t.show-iso
}

// Show the time as an ISO 8601 string.
// See <http://en.wikipedia.org/wiki/ISO_8601>.
// For example `now().show-iso` -> `"2016-09-08T19:55:11.910-07:00"`.
fun show-iso(t : time ) : string {
  t.show-iso-date + "T" + t.show-iso-clock + 
   "." + t.clock.field4.showl(3) +
   t.show-iso-timezone
}

// Show the date in ISO format, i.e. `YYYY-MM-DD` format.
fun show-iso-date( t : time ) : string {
  val (year,month,day) = t.date
  year.showl(4) + "-" + month.showl + "-" + day.showl
}

// Show the clock time in ISO format, i.e. `HH:mm:ss` format.
fun show-iso-clock( t : time ) : string {
  val (hours,minutes,seconds,_) = t.clock
  hours.showl + ":" + minutes.showl + ":" + seconds.showl
}

// Show the timezone offset in ISO format.
// `now().show-iso-timezone` -> `-07:00`.
// The `utc` (`"Z"`) parameter gives the form used for the UTC timezone,
// while the `sep` (`":"`) gives the separator used between the hours and minutes of
// other timezone offsets.
fun show-iso-timezone( t : time, utc : string = "Z", sep : string = ":" ) : string {
  val ofs    = t.tzofs
  if (ofs==0) return utc
  val absofs = if (ofs >= 0) then ofs else ~ofs
  (if (ofs >= 0) then "-" else "+") + // flip sign
   ((absofs / 60).showl) + sep + ((absofs % 60).showl)
}

private fun showl( i : int, width : int = 2 ) : string {
  i.show.pad-left(width,"0")
}

// Show time as a standard Internet Message Format date (See <https://tools.ietf.org/html/rfc2822#section-3.3>).\
// For example `now().show-imf` returns `"Fri, 9 Oct 2016 11:57:45 -0700"`
fun show-imf( t : time ) : string {
  val (year,_,day) = t.date
  [ t.show-en-day.first(3).string + ",",
    day.show,
    t.show-en-month.first(3).string,
    year.showl(4),
    t.show-iso-clock,
    t.show-iso-timezone(utc="+0000",sep="")
  ].join(" ")
}


// Show the time as a human readable English string.
// For example `now().en-time` -> `"Thu Oct 8 2016, 19:48:11 GMT-07:00"`.
fun show-en( t : time ) : string {
  t.show-en-date + ", " + t.show-en-clock
}

// Show the clock time  in human readable English.
// For example `now().en-time` -> `"19:48:11 GMT-07:00"`.
fun show-en-clock( t : time ) : string {
  val (hours,minutes,seconds,_) = t.clock
  [ hours.showl + ":" + minutes.showl + ":" + seconds.showl,
    "GMT" + t.show-iso-timezone(utc="")
  ].join(" ")
}

// Show the date in human readable English.
// For example `now().en-date` -> `"Thu Oct 8 2016"`.
fun show-en-date( t : time ) : string {
  val (year,_,day) = t.date
  [ t.show-en-day.first(3).string,
    t.show-en-month.first(3).string,
    day.show,
    year.showl(4) 
  ].join(" ")
}

// Return the full day of the week in English
fun show-en-day( t : time ) : string {
  days[t.day-of-week].maybe("?")
}

// Return the full month in English
fun show-en-month( t : time ) : string {
  val (_,month,_) = t.date
  months[month - 1].maybe("?")
}

private val days = ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]
private val months = ["January","February","March","April","May","June",
                      "July","August","September","October","November","December"]


// Return a high-resolution timestamp in milliseconds. The timestamp is guaranteed to be monotonically increasing
// and have at least millisecond resolution. 
extern ticks() : ndet double {
  js "_ticks"
}

// Return the smallest time difference the `ticks` can measure.
extern ticks-resolution() : ndet double {
  js inline "_ticks_resolution"
}

// Return the number of milliseconds that it takes to evaluate `action`.
fun elapsed( action : () -> <ndet|e> () ) : <ndet|e> double {
  val t0 = ticks()
  action()
  val t1 = ticks()
  (t1 - t0)
}

// Measure the number of milliseconds that it takes to evaluate `action`, and print `msg` postfixed with the
// measured time.
fun print-elapsed( action : () -> <ndet,console|e> a, msg : string = "elapsed" ) : <ndet,console|e> a {
  val t0 = ticks()
  val x  = action()
  val t1 = ticks()
  val t  = (t1 - t0)
  println( msg + " " + t.show-fixed + "ms" )
  x
} 