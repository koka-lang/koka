/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

// Parsing of time strings.
module std/time/parse

import std/text/regex
import std/num/ddouble
import std/time/duration
import std/time/date
import std/time/calendar
import std/time/calendars  // iso-week, iso-month
import std/time/time
import std/time/locale


// -----------------------------------------------------------
// Parsing 
// -----------------------------------------------------------

/* Parse an [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) time string.
The optional `calendar` (=`cal-iso`) argument can be given to interpret
the date in a different calendar. If a time zone offset is given, the 

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

And also parses the ISO month dates (see `cal-iso-month`):

````plain
2009-M01-03              // just month date
2009-M003                // month date with day of the year
````
. 
*/
public fun parse-iso( s : string, calendar : calendar = cal-iso ) : maybe<time> {
  match(s.find(rx-iso-date)) {
    Nothing -> Nothing
    Just(capd) -> {
      val xyear = capd.num(1)
      val (date,cal) =
        if (capd.groups[4] != "") then // ISO week date
          (Date(xyear,capd.num(4),capd.num(5,1)),cal-iso-week)        
        elif (capd.groups[6] != "") then // ISO day of the year
          (Date(xyear,1,capd.num(6)), calendar)
        elif (capd.groups[7] != "") then // ISO month date
          (Date(xyear,capd.num(7),capd.num(8)), cal-iso-month)
        elif (capd.groups[9] != "") then // ISO month date, day of the year
          (Date(xyear,1,capd.num(9)), cal-iso-month)
        else // (capd.groups[2] != "")  // regular ISO date
          (Date(xyear,capd.num(2),capd.num(3)),calendar)
      match(capd.slice.after.string.find(rx-iso-time)) {
        Nothing    -> Nothing
        Just(capt) -> {
          val hours = capt.num(1)
          val mins  = capt.num(2)
          val secs  = parse-ddouble(capt.groups[3] + "." + capt.groups[4]).default(zero)
          val tzsign= if (capt.groups[5]=="+") then 1 else ~1
          val tzhours= capt.num(6) 
          val tzmins = capt.num(7)
          val tzone = tz-fixed(tzsign*tzhours, tzmins)
          val clock = Clock(hours,mins,secs)
          Just(time(date,clock,tz=tzone,cal=cal))
        }
      }
    }
  }
}
val rx-iso-date = regex(@"^(\d\d\d\d)\-?(?:(\d\d)\-?(\d\d)|W(\d\d)\-?(\d)?|(\d\d\d)|M(\d\d)\-?(\d\d)|M(\d\d\d))(?=$|[T\+\- ]\d|Z)");
val rx-iso-time = regex(@"^(?:[T ](\d\d)(?:\:?(\d\d)(?:\:?(\d\d))?)?(?:[\.,](\d+))?)?(?:Z|([\+\-])(\d\d)(?:\:?(\d\d))?)?$")

fun num( cap : matched, group : int, default : int = 0 ) : int {
  parse-int-default(cap.groups[group], default = default)
}

// Parse an [Internet Message Format](https://tools.ietf.org/html/rfc2822#section-3.3) time string.
// For example: `"Tue, 27 Sep 2016 06:36:55 -0700"`.
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
      val tzone = tz-fixed(tzsign*tzhours, tzmins)
      // adjust 2 digit year
      val year  = if (cap.groups[4].count <= 2) 
                   then (if (xyear < 50) then 2000 + xyear else 1900 + xyear)
                   else xyear
      Just(time(year,month,day,hours,mins,secs,tz=tzone))
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

