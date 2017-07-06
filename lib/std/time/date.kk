/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Date, clock, and week days support. 

   Defines basic types for human readable time. Years, months, days etc. are always 1-based.
   Week dates are absent as they are treated as normal dates, using the `std/time/calendars/cal-iso-week` calendar.
*/
module std/time/date

import std/num/double
import std/num/ddouble
import std/time/timestamp
import std/time/duration
import std/time/instant
import std/time/utc



/*----------------------------------------------------------------------------
  
----------------------------------------------------------------------------*/

// A date consists of a the year, month, and day.
public struct date(
  year : int,
  month: int,
  day  : int 
)

// Create an ISO weekdate where the "month" is the ISO week number.
public fun weekdate( year : int, month: int, weekday : weekday ) : date {
  Date(year,month,weekday.int)
}

// Show a year in ISO format (using 5+ digits and explicit sign for years < 0 or years > 9999)).
public fun show-year( year : int ) : string {
  val (ysign,ywidth) = if (year > 9999) then ("+",5) elif (year.neg?) then ("-",5) else ("",4)
  ysign + year.abs.show0(ywidth)
}

// Show a date in ISO format. `Date(2000,1,1).show == "2000-01-01"`.
// Takes an optional `month-prefix` (=`""`) that is used by the ISO week
// and month calendar to add a `"W"` or `"M"` prefix respectively.
public fun show( d : date, month-prefix : string = "" ) : string {
  val day-width = if (month-prefix=="W") then 1 else 2
  d.year.show-year + "-" + month-prefix + d.month.show0 + "-" + d.day.show0(day-width)
}

// Compare two dates by fields.
public fun compare( d : date, e : date ) : order {
  match(compare(d.year,e.year)) {
    Eq -> match(compare(d.month,e.month)) {
      Eq  -> compare(d.day,e.day)
      ord -> ord
    }
    ord -> ord
  }
}

public fun (<)( i : date, j : date )  : bool { compare(i,j) == Lt }
public fun (<=)( i : date, j : date ) : bool { compare(i,j) != Gt }
public fun (>)( i : date, j : date )  : bool { compare(i,j) == Gt }
public fun (>=)( i : date, j : date ) : bool { compare(i,j) != Lt }
public fun (==)( i : date, j : date ) : bool { compare(i,j) == Eq }
public fun (!=)( i : date, j : date ) : bool { compare(i,j) != Eq }

// Add two dates field-wise together.
public fun (+)( d1 : date, d2 : date ) : date {
  Date(d1.year + d2.year, d1.month + d2.month, d1.day + d2.day )
}

/*----------------------------------------------------------------------------
  
----------------------------------------------------------------------------*/

// A clock consists of the hour, minute, second, and fractional second (between ``0.0` and `1.0`).
public struct clock(
  hours   : int,
  minutes : int,
  seconds : ddouble
)

// Create a clock from a `:duration`; normalizes the clock with seconds and minutes under 60. 
public fun clock( d : duration ) : clock {
  clock(d.seconds)
}

// Create a clock from seconds; normalizes the clock with seconds and minutes under 60. 
public fun clock( seconds : ddouble ) : clock {
  val hm     = seconds.floor.int / 60
  val s      = seconds - hm.ddouble
  val (h,m)  = divmod(hm,60)
  Clock(h, m, s)
}

// Create a clock from a seconds as an `:int` with an optional fraction. 
// Normalizes the clock with seconds and minutes under 60. 
public fun clock( seconds : int, frac : double = 0.0 ) : clock {
  clock(timespan(seconds,frac))
}


// The zero clock
public val clock0 = Clock(0,0,zero)

// Is this a zero clock?
public fun zero?( c : clock ) : bool {
  (c.hours.zero? && c.minutes.zero? && c.seconds.zero?)
}

// Return the total seconds of a `:clock` assuming 60 seconds per 
// minute and 60 minutes per hour.
public fun total-seconds( c : clock ) : ddouble {
  ddouble((c.hours*60 + c.minutes)*60) + c.seconds
}

// Return the whole seconds part of a `:clock`.
public fun whole-seconds( c : clock ) : int {
  c.seconds.trunc.int
}

// Return the fraction of the seconds as milli-seconds (10^-3^).
public fun milli-seconds( c : clock ) : int {
  (c.seconds.fraction.double * 1.0e3).trunc.int
}

// Return the fraction of seconds as nano-seconds ((10^-9^).
public fun nano-seconds( c : clock ) : int {
  (c.seconds.fraction.double * 1.0e9).trunc.int
}

// Round a clock time to a certain number of digits precision (of the fraction of seconds) (default `9`, nano seconds).
public fun round-to-prec( c : clock, prec : int = 9) : clock {
  Clock(c.hours, c.minutes, c.seconds.round-to-prec(prec))
}

// Show a clock in ISO format up to an optional maximum precision (=`9`).\
// `Clock(23,30,fixed(1.123)).show == "23:30:01.123"`\
// `Clock(23,30,fixed(1.123)).show(0) == "23:30:01"`
public fun show( c : clock, prec : int = 9 ) : string {
  c.hours.show0 + ":" + c.minutes.show0 + ":" + show(timestamp(c.seconds),prec,2)
}

// Compare two clocks as by their total seconds.
public fun compare( c : clock, d : clock ) : order {
  compare(c.total-seconds,d.total-seconds)
}

public fun (<)( i : clock, j : clock )  : bool { compare(i,j) == Lt }
public fun (<=)( i : clock, j : clock ) : bool { compare(i,j) != Gt }
public fun (>)( i : clock, j : clock )  : bool { compare(i,j) == Gt }
public fun (>=)( i : clock, j : clock ) : bool { compare(i,j) != Lt }
public fun (==)( i : clock, j : clock ) : bool { compare(i,j) == Eq }
public fun (!=)( i : clock, j : clock ) : bool { compare(i,j) != Eq }

// Add two clock together.
public fun (+)( c : clock, d : clock ) : clock {
  Clock(c.hours + d.hours, c.minutes + d.minutes, c.seconds + d.seconds )
}

/*----------------------------------------------------------------------------
   Days of the week
----------------------------------------------------------------------------*/

// An ISO day of the week, starting at Monday (=1) up to Sunday (=7).
public type weekday {
  Mon 
  Tue
  Wed
  Thu
  Fri
  Sat
  Sun
}

// Convert a `:weekday` to an `:int` using the ISO definition which starts at Monday as 1,
// up to Sunday as 7.
public fun int( wd : weekday ) : int {
  match(wd) {
    Mon -> 1
    Tue -> 2
    Wed -> 3
    Thu -> 4
    Fri -> 5
    Sat -> 6
    Sun -> 7
  }
}

// Convert a weekday number to a `:weekday`(starting at Monday (=1) up to Sunday (=7)). 
// Takes the integer `i - 1` modulo 7, so `0` or `14` also become Sunday etc.
public fun weekday( i : int ) : weekday {
  val d = ((i - 1)%7) 
  if (d==0) then Mon
  elif (d==1) then Tue
  elif (d==2) then Wed
  elif (d==3) then Thu
  elif (d==4) then Fri
  elif (d==5) then Sat
  else Sun 
}


// Show a `:weekday` as an English string (`Sun.show == "Sunday"`).
public fun show( wd : weekday ) : string {
  match(wd) {
    Mon -> "Monday"
    Tue -> "Tuesday"
    Wed -> "Wednesday"
    Thu -> "Thursday"
    Fri -> "Friday"
    Sat -> "Saturday"
    Sun -> "Sunday"
  }
}

// Return the `:weekday` that comes `n` days after week day `wd`.
public fun (+)(wd : weekday, n : int ) : weekday {
  (wd.int + n).weekday
}

// Return the `:weekday` that comes `n` days before week day `wd`.
public fun (-)(wd : weekday, n : int ) : weekday {
  (wd.int - n).weekday
}

// Return the difference between two week days:\
// `wd2 == wd1 + (wd2 - wd1)`
public fun (-)(wd1 : weekday, wd2 : weekday ) : int {
  (wd1.int  - wd2.int)%7
}


// pad with zeros
fun show0( i : int, width : int = 2) : string {
  i.show.pad-left(width,'0')
}

// Show a `:weekday` as a 3 letter English string (`Sun.show-short == "Sun"`)
public fun show-short( wd : weekday ) : string {
  wd.show.first(3).string
}

// Compare weekdays.
public fun compare( wd1 : weekday, wd2 : weekday ) : order {
  compare(wd1.int,wd2.int)
}

public fun (<)( i : weekday, j : weekday )  : bool { compare(i,j) == Lt }
public fun (<=)( i : weekday, j : weekday ) : bool { compare(i,j) != Gt }
public fun (>)( i : weekday, j : weekday )  : bool { compare(i,j) == Gt }
public fun (>=)( i : weekday, j : weekday ) : bool { compare(i,j) != Lt }
public fun (==)( i : weekday, j : weekday ) : bool { compare(i,j) == Eq }
public fun (!=)( i : weekday, j : weekday ) : bool { compare(i,j) != Eq }


/*----------------------------------------------------------------------------

----------------------------------------------------------------------------*/

// Return the ISO calendar date of Easter in a given year (Algorithm by [J.M. Oudin](http://aa.usno.navy.mil/faq/docs/easter.php)).
public fun easter( year : int ) : date {
  val c  = year/100
  val n  = year - 19*(year/19)
  val k  = (c - 17)/25
  val i0 = (((c - c/4) - (c - k)/3) + 19*n) + 15
  val i1 = i0 - 30*(i0/30)
  val i  = i1 - (i1/28)*(1 - (i1/28)*(29/(i1 + 1))*((21 - n)/11))
  val j0 = ((year + year/4 + i + 2) - c) + c/4
  val j  = j0 - 7*(j0/7)
  val l  = i - j
  val m  = 3 + (l + 40)/44
  val d  = (l + 28) - 31*(m/4)
  Date(year,m,d)
}

