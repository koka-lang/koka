/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Date and time functions
*/
public module std/datetime

abstract datetime( tzofs : int, date : any )

// Return the current local time.
extern now : () -> ndet datetime { 
  js inline "(function(){ var d = new Date(); return datetime(d.getTimezoneOffset(),d); })()" 
}

// Get the time-zone offset in minutes. This 
// is the difference in minutes between UTC and local time.
// For example, if the time zone is UTC+10, the time zone offset is -600.
fun timezone-offset(d : datetime) : int {
  d.tzofs
}

// Get the full year
extern year( d : datetime ) : int {
  js inline "(d.tzofs===0 ? d.date.getUTCFullYear() : d.date.getFullYear())"
}

// Get the month, between 1 and 12
extern month( d : datetime ) : int {
  js inline "#1.date.getMonth()+1"
}

// Get the day of the month, between 1 and 31
extern day( d : datetime ) : int {
  js inline "#1.date.getDate()"
}

// Get the hours of the day between 0 and 23
extern hours : (datetime) -> int { 
  js inline "#1.date.getHours()" 
}

// Get the minutes between 0 and 59
extern minutes : (datetime) -> int { 
  js inline "#1.date.getMinutes()" 
}

// Get the seconds between 0 and 59
extern seconds : (datetime) -> int { 
  js inline "#1.date.getSeconds()" 
}

// Return the milliseconds between 0 and 999
extern milliseconds : (datetime) -> int { 
  js inline "#1.date.getMilliseconds()" 
}

// Number of milliseconds since 1 January 1970 00\:00\:00 UTC.
extern inline epoch-msecs : (datetime) -> int { 
  js inline "(#1.date.getTime() | 0)" 
}

// Return an ISO 8601 date-time string with a zero UTC offset (`Z`).
// See <http://en.wikipedia.org/wiki/ISO_8601>.
extern iso-datetime : (date) -> string {
  js inline "#1.toISOString()"
}

// Return the date portion of an ISO date/time string (`iso-datetime`).
fun iso-date(d : date) : string {
  val s = d.iso-datetime
  match(s.find("T")) {
    Just(slice) -> slice.before.string
    Nothing     -> s
  }
}

// Return the time portion of an ISO date/time string (`iso-datetime`).
fun iso-time(d : date) : string {
  val s = d.iso-datetime
  match (s.find("T")) {
    Just(slice) -> slice.after.string
    Nothing -> s
  }
}

// Return an ISO 8601 date-time string in the current time zone.
// See <http://en.wikipedia.org/wiki/ISO_8601>.
fun iso-local-datetime(d : date ) : string {
  val ofs = d.timezone-offset
  val xofs = if (ofs >= 0) then ofs else ~ofs
  d.iso-local-date + "T" + d.iso-local-time + 
   "." + d.milliseconds.show.pad-left(3,"0") +
   (if (ofs >= 0) then "-" else "+") + // flip sign
   ((xofs / 60).show2) + ":" + ((xofs % 60).show2)
}


// return the local date in ISO format, i.e. `YYYY-MM-DD` format.
fun iso-local-date( d : date ) : string {
  d.year.show + "-" + d.month.show2 + "-" + d.day.show2
}

// return the local time in ISO format, i.e. `hh:mm:ss` format.
fun iso-local-time( d : date ) : string {
  d.hours.show2 + ":" + d.minutes.show2 + ":" + d.seconds.show2
}

private fun show2( i : int ) : string {
  i.show.pad-left(2,"0")
} 