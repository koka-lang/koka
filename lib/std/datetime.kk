/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Basic date and time functions.
*/
public module std/datetime

// A `:datetime` represents a certain date and time in a gregorian calendar.
// It is displayed with regard to either the local or UTC timezone.
abstract struct datetime( tzofs : int, date : any )

// Return the current time in the local timezone.
extern now : () -> ndet datetime { 
  js inline "(function(){ var d = new Date(); return Datetime(d.getTimezoneOffset(),d); })()" 
}

// Create a new local `:datetime`
extern datetime(year:int, month : int, day:int = 1,hours:int = 0,minutes:int = 0,seconds:int = 0,milliseconds:int = 0) : datetime {
  js inline "(function(){ var d = new Date(#1,#2 - 1,#3,#4,#5,#6,#7); return Datetime(d.getTimezoneOffset(),d); })()" 
}

// Create a new UTC `:datetime`
extern utc-datetime(year:int, month : int, day:int, hours:int = 0,minutes:int = 0,seconds:int = 0,milliseconds:int = 0) : datetime {
  js inline "(function(){ var d = new Date(Date.UTC(#1,#2 - 1,#3,#4,#5,#6,#7)); return Datetime(0,d); })()" 
}

// Create a new local timezone `:datetime` with a given number milliseconds since the epoch (1 January 1970 00\:00\:00 UTC)
extern datetime( epochmsecs : double ) : datetime {
  js inline "(function(){ var d = new Date(epochmsecs); return Datetime(d.getTimezoneOffset(),d); })()" 
}

// Convert a date to the UTC timezone.
// i.e. `utc-now() === now().utc`
fun utc( d : datetime ) : datetime {
  d(tzofs = 0)
}

// Convert a date to the local timezone.
fun local( d : datetime ) : ndet datetime {
  d(tzofs = now().tzofs)
}

// Return the current time in the UTC timezone.
fun utc-now() : ndet datetime { 
  now().utc
}

// Create a new UTC timezone `:datetime` with a given number milliseconds since the epoch (1 January 1970 00\:00\:00 UTC)
fun utc-datetime( epochmsecs : double ) : datetime {
  datetime(epochmsecs).utc
}

// Is this datetime in the UTC timezone?
fun utc?( d : datetime ) : bool {
  (d.tzofs == 0)
}

// Get the timezone offset in minutes. This 
// is the difference in minutes between UTC and local time.
// For example, if the timezone is UTC+10, the timezone offset is -600.
fun timezone-offset(d : datetime) : int {
  d.tzofs
}

// Get the full year
extern year( d : datetime ) : int {
  js inline "(d.tzofs===0 ? d.date.getUTCFullYear() : d.date.getFullYear())"
}

// Get the month, between 1 and 12
extern month( d : datetime ) : int {
  js inline "(d.tzofs===0 ? d.date.getUTCMonth() : d.date.getMonth())+1"
}

// Get the day of the month, between 1 and 31
extern day( d : datetime ) : int {
  js inline "(d.tzofs===0 ? d.date.getUTCDate() : d.date.getDate())"
}

// Return the day of the week, between 0 (Sunday) and 6 (Saturday)
extern day-of-week( d : datetime ) : int {
  js inline "(d.tzofs===0 ? d.date.getUTCDay() : d.date.getDay())"
}

// Get the hours of the day between 0 and 23
extern hours : (d:datetime) -> int { 
  js inline "(d.tzofs===0 ? d.date.getUTCHours() : d.date.getHours())"
}

// Get the minutes between 0 and 59
extern minutes : (d:datetime) -> int { 
  js inline "(d.tzofs===0 ? d.date.getUTCMinutes() : d.date.getMinutes())" 
}

// Get the seconds between 0 and 59
extern seconds : (d:datetime) -> int { 
  js inline "(d.tzofs===0 ? d.date.getUTCSeconds() : d.date.getSeconds())"
}

// Return the milliseconds between 0 and 999
extern milliseconds(d:datetime) : int { 
  js inline "(d.tzofs===0 ? d.date.getUTCMilliseconds() : d.date.getMilliseconds())" 
}

// Number of milliseconds since 1 January 1970 00\:00\:00 UTC.
extern inline epoch-msecs(d:datetime) : double { 
  js inline "(#1.date.getTime())" 
}

private fun copy( d : datetime, year :int, month : int, day : int, hours : int, minutes : int, seconds : int, milliseconds : int ) : datetime {
  if (d.utc?) 
   then utc-datetime(year,month,day,hours,minutes,seconds,milliseconds)
   else datetime(year,month,day,hours,minutes,seconds,milliseconds)
}

// Add `n` years to a datetime.
fun add-years( d : datetime, n : int ) : datetime {
  d.copy(d.year + n,d.month,d.day, d.hours, d.minutes, d.seconds, d.milliseconds)
}

// Add `n` months to a date.
fun add-months( d : datetime, n : int ) : datetime {
  d.copy(d.year,d.month + n,d.day, d.hours, d.minutes, d.seconds, d.milliseconds)
}

// Add `n` days to a date. Takes leap years etc. into account.
fun add-days( d : datetime, n : int ) : datetime {
  d.copy(d.year,d.month,d.day + n, d.hours, d.minutes, d.seconds, d.milliseconds)
}

// Add `n` hours to a date. 
fun add-hours( d : datetime, n : int ) : datetime {
  d.copy(d.year,d.month,d.day, d.hours + n, d.minutes, d.seconds, d.milliseconds)
}

// Add `n` minutes to a date. 
fun add-minutes( d : datetime, n : int ) : datetime {
  d.copy(d.year,d.month,d.day, d.hours, d.minutes + n, d.seconds, d.milliseconds)
}

// Add `n` seconds to a date. Takes leap seconds into account.
fun add-seconds( d : datetime, n : int ) : datetime {
  d.copy(d.year,d.month,d.day, d.hours, d.minutes, d.seconds + n, d.milliseconds)
}

// Show a `:datetime` as an ISO 8601 string. 
// See <http://en.wikipedia.org/wiki/ISO_8601>.
// For example `now().show` -> `"2016-09-08T19:55:11.910-07:00"`.
fun show( d : datetime ) : string {
  d.show-iso
}

// Show as an ISO 8601 date-time string.
// See <http://en.wikipedia.org/wiki/ISO_8601>.
// For example `now().show-iso` -> `"2016-09-08T19:55:11.910-07:00"`.
fun show-iso(d : datetime ) : string {
  d.show-iso-date + "T" + d.show-iso-time + 
   "." + d.milliseconds.showl(3) +
   d.show-iso-timezone
}

// Show the date in ISO format, i.e. `YYYY-MM-DD` format.
fun show-iso-date( d : datetime ) : string {
  d.year.showl(4) + "-" + d.month.showl + "-" + d.day.showl
}

// Show the time in ISO format, i.e. `hh:mm:ss` format.
fun show-iso-time( d : datetime ) : string {
  d.hours.showl + ":" + d.minutes.showl + ":" + d.seconds.showl
}

// Show the timezone offset in ISO format.
// `now().show-iso-timezone` -> `-07:00`.
// The `utc` (`"Z"`) parameter gives the form used for the UTC timezone,
// while the `sep` (`":"`) gives the separator used between the hours and minutes of
// other timezone offsets.
fun show-iso-timezone( d : datetime, utc : string = "Z", sep : string = ":" ) : string {
  val ofs    = d.tzofs
  if (ofs==0) return utc
  val absofs = if (ofs >= 0) then ofs else ~ofs
  (if (ofs >= 0) then "-" else "+") + // flip sign
   ((absofs / 60).showl) + sep + ((absofs % 60).showl)
}

private fun showl( i : int, width : int = 2 ) : string {
  i.show.pad-left(width,"0")
}

// Show as standard Internet Message Format date (See <https://tools.ietf.org/html/rfc2822#section-3.3>).\
// For example `now().show-imf` returns `"Fri, 9 Oct 2016 11:57:45 -0700"`
fun show-imf( d : datetime ) : string {
  [ d.show-en-day.first(3).string + ",",
    d.day.show,
    d.show-en-month.first(3).string,
    d.year.showl(4),
    d.show-iso-time,
    d.show-iso-timezone(utc="+0000",sep="")
  ].join(" ")
}


// Show a human readable English date/time string.
// For example `now().en-datetime` -> `"Thu Oct 8 2016, 19:48:11 GMT-07:00"`.
fun show-en( d : datetime ) : string {
  d.show-en-date + ", " + d.show-en-time
}

// Show the time portion in human readable English.
// For example `now().en-time` -> `"19:48:11 GMT-07:00"`.
fun show-en-time( d : datetime ) : string {
  [ d.hours.showl + ":" + d.minutes.showl + ":" + d.seconds.showl,
    "GMT" + d.show-iso-timezone(utc="")
  ].join(" ")
}

// Show the date portion in human readable English.
// For example `now().en-date` -> `"Thu Oct 8 2016"`.
fun show-en-date( d : datetime ) : string {
  [ d.show-en-day.first(3).string,
    d.show-en-month.first(3).string,
    d.day.show,
    d.year.showl(4) 
  ].join(" ")
}

// Return the full day of the week in English
fun show-en-day( d : datetime ) : string {
  days[d.day-of-week].maybe("?")
}

// Return the full month in English
fun show-en-month( d : datetime ) : string {
  months[d.month].maybe("?")
}

private val days = ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]
private val months = ["January","February","March","April","May","June",
                      "July","August","September","October","November","December"]
