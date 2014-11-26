/*----------------------------------------------------------------------------
   Copyright (C) Microsoft Corporation
  
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

public module std/date

type date

external now : () -> ndet date { 
  js inline "new Date()" 
}

external hours : (date) -> int { 
  js inline "#1.getHours()" 
}

external minutes : (date) -> int { 
  js inline "#1.getMinutes()" 
}

external seconds : (date) -> int { 
  js inline "#1.getSeconds()" 
}

// Number of milliseconds since 1 January 1970 00:00:00 UTC.
external time : (date) -> int { 
  js inline "#1.getTime()" 
}


// Return an ISO 8601 date-time string.
// See <http://en.wikipedia.org/wiki/ISO_8601>
external isoDateTime : (date) -> string {
  js inline "#1.toISOString()"
}

// Return the date portion of an ISO date/time string.
function isoDate(d : date) : string {
  val s = d.isoDateTime
  val i = s.indexOf("T")
  if (i >= 0) then s.substr(0,i) else s
}

// Return the time portion of an ISO date/time string.
function isoTime(d : date) : string {
  val s = d.isoDateTime
  val i = s.indexOf("T")
  if (i >= 0) then s.substr(i) else s
}

// Get the year
external year( d : date ) : int {
  js inline "#1.getFullYear()"
}

// Get the month, starting at 1
external month( d : date ) : int {
  js inline "#1.getMonth()+1"
}

// Get the day of the month, starting at 1
external day( d : date ) : int {
  js inline "#1.getDate()"
}

// return the ISO date in local time
function isoLocalDate( d : date ) : string {
  d.year.show + "-" + d.month.show2 + "-" + d.day.show2
}

// return the ISO time in local time
function isoLocalTime( d : date ) : string {
  d.hours.show2 + ":" + d.minutes.show2 + ":" + d.seconds.show2
}

function show2( i : int ) : string {
  i.show.align(2,'0')
} 