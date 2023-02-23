/*----------------------------------------------------------------------------
   Copyright 2012-2021, Microsoft Research, Daan Leijen

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/

/* Date, clock, and week days support.

   Defines basic types for human readable time. Years, months, days etc. are always 1-based.
   Week dates are absent as they are treated as normal dates, using the `std/time/calendars/cal-iso-week` calendar.
*/
module std/time/date

import std/num/float64
import std/num/ddouble

/*----------------------------------------------------------------------------

----------------------------------------------------------------------------*/

// A date consists of a the year, month, and day.
pub value struct date
  year : int
  month: int
  day  : int


// Create an ISO weekdate where the "month" is the ISO week number.
pub fun weekdate( year : int, month: int, weekday : weekday ) : date
  Date(year,month,weekday.int)

// Show a year in ISO format (using 5+ digits and explicit sign for years < 0 or years > 9999)).
pub fun show-year( year : int ) : string
  val (ysign,ywidth) = if year > 9999 then ("+",5) elif year.is-neg then ("-",5) else ("",4)
  ysign ++ year.abs.show0(ywidth)

// Show a date in ISO format. `Date(2000,1,1).show == "2000-01-01"`.
// Takes an optional `month-prefix` (=`""`) that is used by the ISO week
// and month calendar to add a `"W"` or `"M"` prefix respectively.
pub fun show( d : date, month-prefix : string = "" ) : string
  val day-width = if month-prefix=="W" then 1 else 2
  d.year.show-year ++ "-" ++ month-prefix ++ d.month.show0 ++ "-" ++ d.day.show0(day-width)

// Compare two dates by fields.
pub fun compare( d : date, e : date ) : order
  match(compare(d.year,e.year))
    Eq -> match(compare(d.month,e.month))
      Eq  -> compare(d.day,e.day)
      ord -> ord

    ord -> ord


pub fun (<)( i : date, j : date )  : bool { compare(i,j) == Lt }
pub fun (<=)( i : date, j : date ) : bool { compare(i,j) != Gt }
pub fun (>)( i : date, j : date )  : bool { compare(i,j) == Gt }
pub fun (>=)( i : date, j : date ) : bool { compare(i,j) != Lt }
pub fun (==)( i : date, j : date ) : bool { compare(i,j) == Eq }
pub fun (!=)( i : date, j : date ) : bool { compare(i,j) != Eq }

// Add two dates field-wise together.
pub fun (+)( d1 : date, d2 : date ) : date
  Date(d1.year + d2.year, d1.month + d2.month, d1.day + d2.day )

/*----------------------------------------------------------------------------

----------------------------------------------------------------------------*/

// A clock consists of the hour, minute, second, and fractional second (between ``0.0` and `1.0`).
pub struct clock
  hours   : int
  minutes : int
  seconds : ddouble


// Create a clock from a `:duration`; normalizes the clock with seconds and minutes under 60.
//pub fun clock( d : duration ) : clock
//  clock(d.seconds)
//}

// Create a clock from seconds; normalizes the clock with seconds and minutes under 60.
pub fun clock( seconds : ddouble ) : clock
  clock(seconds.floor.int,seconds.ffraction)

// Create a clock from a seconds as an `:int` with an optional fraction.
// Normalizes the clock with seconds and minutes under 60 but
// adds the fraction as is to the final seconds, so that might
// be `>= 60` if the fraction `>= 1.0`;
pub fun clock( seconds : int, frac : float64 ) : clock
  clock(seconds,frac.ddouble)

// Create a clock from a seconds as an `:int` with an optional fraction.
// Normalizes the clock with seconds and minutes under 60 but
// adds the fraction as is to the final seconds, so that might
// be `>= 60` if the fraction `>= 1.0`;
pub fun clock( seconds : int, frac : ddouble = zero) : clock
  val (hm,s) = divmod(seconds,60)
  val (h,m)  = divmod(hm,60)
  Clock(h,m,s.ddouble + frac)

pub fun clock( seconds : ddouble, leap : int) : clock
  clock(seconds.floor.int, seconds.ffraction + leap.ddouble)


// The zero clock
pub val clock0 = Clock(0,0,zero)

// Is this a zero clock?
pub fun is-zero( c : clock ) : bool
  (c.hours.is-zero && c.minutes.is-zero && c.seconds.is-zero)

// Return the total seconds of a `:clock` assuming 60 seconds per
// minute and 60 minutes per hour.
pub fun total-seconds( c : clock ) : ddouble
  ddouble((c.hours*60 + c.minutes)*60) + c.seconds

// Return the whole seconds part of a `:clock`.
pub fun whole-seconds( c : clock ) : int
  c.seconds.truncate.int

// Return the fraction of the seconds as milli-seconds (10^-3^).
pub fun milli-seconds( c : clock ) : int
  (c.seconds.fraction.float64 * 1.0e3).truncate.int

// Return the fraction of seconds as nano-seconds ((10^-9^).
pub fun nano-seconds( c : clock ) : int
  (c.seconds.fraction.float64 * 1.0e9).truncate.int

// Round a clock time to a certain number of digits precision (of the fraction of seconds) (default `9`, nano seconds).
pub fun round-to-prec( c : clock, prec : int = 9) : clock
  Clock(c.hours, c.minutes, c.seconds.round-to-prec(prec))

// Show a clock in ISO format up to an optional maximum precision (=`9`).\
// `Clock(23,30,fixed(1.123)).show == "23:30:01.123"`\
// `Clock(23,30,fixed(1.123)).show(0) == "23:30:01"`
pub fun show( c : clock, prec : int = 9 ) : string
  c.hours.show0 ++ ":" ++ c.minutes.show0 ++ ":" ++ c.seconds.show-seconds(prec,2)

// Show seconds
pub fun show-seconds( secs : ddouble, max-prec : int = 9, secs-width : int = 1, unit : string = "" ) : string
  val s = secs.show-fixed(~max-prec.abs)
  match(s.find("."))
    Nothing -> s.pad-left(secs-width,'0')
    Just(dot) ->
      val f = dot.after.string
      val len3 = ((f.count + 2)/3)*3
      //trace("ts-show: " ++ s ++ ", " ++ len3.show ++ ", prec: " ++ max-prec.show ++ ", " ++ ts.since.show(-9))
      dot.before.string.pad-left(secs-width,'0') ++ "." ++ f.pad-right(len3,'0') ++ unit



// Compare two clocks as by their total seconds.
pub fun compare( c : clock, d : clock ) : order
  compare(c.total-seconds,d.total-seconds)

pub fun (<)( i : clock, j : clock )  : bool { compare(i,j) == Lt }
pub fun (<=)( i : clock, j : clock ) : bool { compare(i,j) != Gt }
pub fun (>)( i : clock, j : clock )  : bool { compare(i,j) == Gt }
pub fun (>=)( i : clock, j : clock ) : bool { compare(i,j) != Lt }
pub fun (==)( i : clock, j : clock ) : bool { compare(i,j) == Eq }
pub fun (!=)( i : clock, j : clock ) : bool { compare(i,j) != Eq }

// Add two clock together.
pub fun (+)( c : clock, d : clock ) : clock
  Clock(c.hours + d.hours, c.minutes + d.minutes, c.seconds + d.seconds )

/*----------------------------------------------------------------------------
   Days of the week
----------------------------------------------------------------------------*/

// An ISO day of the week, starting at Monday (=1) up to Sunday (=7).
pub type weekday
  Mon
  Tue
  Wed
  Thu
  Fri
  Sat
  Sun

// Convert a `:weekday` to an `:int` using the ISO definition which starts at Monday as 1,
// up to Sunday as 7.
pub fun int( wd : weekday ) : int
  match wd
    Mon -> 1
    Tue -> 2
    Wed -> 3
    Thu -> 4
    Fri -> 5
    Sat -> 6
    Sun -> 7


// Convert a weekday number to a `:weekday`(starting at Monday (=1) up to Sunday (=7)).
// Takes the integer `i - 1` modulo 7, so `0` or `14` also become Sunday etc.
pub fun weekday( i : int ) : weekday
  val d = ((i - 1)%7)
  if d==0 then Mon
  elif d==1 then Tue
  elif d==2 then Wed
  elif d==3 then Thu
  elif d==4 then Fri
  elif d==5 then Sat
  else Sun


// Show a `:weekday` as an English string (`Sun.show == "Sunday"`).
pub fun show( wd : weekday ) : string
  match wd
    Mon -> "Monday"
    Tue -> "Tuesday"
    Wed -> "Wednesday"
    Thu -> "Thursday"
    Fri -> "Friday"
    Sat -> "Saturday"
    Sun -> "Sunday"


// Return the `:weekday` that comes `n` days after week day `wd`.
pub fun (+)(wd : weekday, n : int ) : weekday
  (wd.int + n).weekday

// Return the `:weekday` that comes `n` days before week day `wd`.
pub fun (-)(wd : weekday, n : int ) : weekday
  (wd.int - n).weekday

// Return the difference between two week days:\
// `wd2 == wd1 + (wd2 - wd1)`
pub fun (-)(wd1 : weekday, wd2 : weekday ) : int
  (wd1.int  - wd2.int)%7


// pad with zeros
fun show0( i : int, width : int = 2) : string
  i.show.pad-left(width,'0')

// Show a `:weekday` as a 3 letter English string (`Sun.show-short == "Sun"`)
pub fun show-short( wd : weekday ) : string
  wd.show.first(3).string

// Compare weekdays.
pub fun compare( wd1 : weekday, wd2 : weekday ) : order
  compare(wd1.int,wd2.int)

pub fun (<)( i : weekday, j : weekday )  : bool { compare(i,j) == Lt }
pub fun (<=)( i : weekday, j : weekday ) : bool { compare(i,j) != Gt }
pub fun (>)( i : weekday, j : weekday )  : bool { compare(i,j) == Gt }
pub fun (>=)( i : weekday, j : weekday ) : bool { compare(i,j) != Lt }
pub fun (==)( i : weekday, j : weekday ) : bool { compare(i,j) == Eq }
pub fun (!=)( i : weekday, j : weekday ) : bool { compare(i,j) != Eq }

/*----------------------------------------------------------------------------

----------------------------------------------------------------------------*/

// Return the ISO calendar date of Easter in a given year (Algorithm by [J.M. Oudin](http://aa.usno.navy.mil/faq/docs/easter.php)).
pub fun easter( year : int ) : date
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

