/*----------------------------------------------------------------------------
   Copyright (C) Microsoft Corporation
  
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Date and time functions
*/
public module std/date

type date

extern now : () -> ndet date { 
  js inline "new Date()" 
}

extern hours : (date) -> int { 
  js inline "#1.getHours()" 
}

extern minutes : (date) -> int { 
  js inline "#1.getMinutes()" 
}

extern seconds : (date) -> int { 
  js inline "#1.getSeconds()" 
}

// Number of milliseconds since 1 January 1970 00:00:00 UTC.
extern time : (date) -> int { 
  js inline "#1.getTime()" 
}


// Return an ISO 8601 date-time string.
// See <http://en.wikipedia.org/wiki/ISO_8601>
extern iso-datetime : (date) -> string {
  js inline "#1.toISOString()"
}

// Return the date portion of an ISO date/time string.
fun iso-date(d : date) : string {
  val s = d.iso-datetime
  val i = s.index-of("T")
  if (i >= 0) then s.substr(0,i) else s
}

// Return the time portion of an ISO date/time string.
fun iso-time(d : date) : string {
  val s = d.iso-datetime
  val i = s.index-of("T")
  if (i >= 0) then s.substr(i) else s
}

// Get the year
extern year( d : date ) : int {
  js inline "#1.getFullYear()"
}

// Get the month, starting at 1
extern month( d : date ) : int {
  js inline "#1.getMonth()+1"
}

// Get the day of the month, starting at 1
extern day( d : date ) : int {
  js inline "#1.getDate()"
}

// return the ISO date in local time
fun iso-local-date( d : date ) : string {
  d.year.show + "-" + d.month.show2 + "-" + d.day.show2
}

// return the ISO time in local time
fun iso-local-time( d : date ) : string {
  d.hours.show2 + ":" + d.minutes.show2 + ":" + d.seconds.show2
}

private fun show2( i : int ) : string {
  i.show.pad-left(2,"0")
} 