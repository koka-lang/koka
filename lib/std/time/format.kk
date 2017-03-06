/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

// Formatting of time into a string.
module std/time/format

import std/text/regex
import std/num/ddouble
import std/time/instant
import std/time/date
import std/time/calendar
import std/time/time
import std/time/locale

val fmt-iso-date       = "YYYY-MM-DD"
val fmt-iso-time       = "HH:mm:ssFFFFFFFFF"
val fmt-iso-timezone   = "Z C"

// Show a time as a standard ISO string. Will use automatic week date
// or month date format for the `cal-iso-week` and `cal-iso-month` calendars.
public fun show-iso( t : time ) : string {
  t.show-iso-date + "T" + t.format(fmt-iso-time + fmt-iso-timezone) 
}

// Show a time as a standard ISO date. Will use automatic week date
// or month date format for the  `cal-iso-week` and `cal-iso-month` calendars.
public fun show-iso-date( t : time ) : string {
  t.format( fmt-iso-date )
}

// Show time as a standard [Internet Message Format](https://tools.ietf.org/html/rfc2822#section-3.3) date.\
// For example `now().show-imf` returns `"Fri, 9 Oct 2016 11:57:45 -0700"`
public fun show-imf( t : time ) : string {
  t.time(cal=cal-iso).format("ddd, D MMM Y HH:mm:ss zz")
}

// Show the time as a human readable string in the given `locale` (=`time-locale-en`)
// For example `now().show-en` -> `"Thu, 8 Oct 2016, 12:20pm"`. Uses the `"llll"` format string.
public fun show-in-locale( t : time, locale : time-locale = time-locale-en ) : string {
  t.format("llll",locale)
}

// Show the date in human readable string in the given `locale` (=`time-locale-en`).
// For example `now().show-en-date` -> `"Thu, 8 Oct 2016"`. Uses the `"ll"` format string.
public fun show-in-locale-date( t : time, locale : time-locale = time-locale-en ) : string {
  t.format("ll",locale)
}

// -----------------------------------------------------------
// Generic Formatting
// -----------------------------------------------------------

fun format-weekday( t : time, n : int, locale : time-locale ) : string {
  val wd = t.weekday
  if (n==1) then wd.show else {
    val days = if (n==2) then locale.day-names-min
               elif (n==3) then locale.day-names-short
               else locale.day-names
    days[wd.int - 1].default("D"+wd.show)
  }
}

fun format-day( t : time, n : int, locale : time-locale ) : string {
  if (n==3) then t.day-of-year.showl(3)
  elif (t.calendar.month-prefix=="W") then t.day.show
  else t.day.showl(n)
}

fun format-month( t : time, n : int, locale : time-locale ) : string {
  if (n<=2) then {
    t.calendar.month-prefix + t.month.showl(n) 
  }
  else {
    val months = if (n==3) then locale.month-names-short else locale.month-names
    months[t.month - 1].default("M" + t.month.showl(2))
  }
}

fun format-year( t : time, n : int, locale : time-locale ) : string {
  if (n==1) then {
    t.year.show
  }
  elif (n<5 || (t.year >= 0 && t.year <= 9999)) then {
    t.year.abs.showl(n).last(n).string
  }
  else {
    val y = t.year.abs.showl(n)
    if (t.year.neg?) then "-" + y else "+" + y
  }
}

fun format-absyear( t : time, n :int, locale : time-locale ) : string {
  t.year.abs.show
}

fun format-era( t : time, n : int, locale : time-locale ) : string {
  (t.calendar.show-era)(t.date)
}

fun format-seconds( t : time, n : int, locale : time-locale ) : string {
  t.seconds.trunc.int.showl(n)
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

fun format-calname( t : time, n : int, locale : time-locale) : string {
  if (t.calendar.month-prefix.notempty?) then ""
   elif (n==2) then t.calendar.long-name 
   else t.calendar.name
}

fun format-ampm( t : time, n : int, locale : time-locale ) : string {
  val h = t.hours
  (if (h<=11) then "am" else "pm").first(n).string
}

fun format-upper-ampm( t : time, n : int, locale : time-locale ) : string {
  format-ampm(t,n,locale).to-upper
}

fun format-frac( t : time, n : int, locale : time-locale ) : string {
  val frac = t.seconds.fraction
  if (frac.zero?) then "" else frac.show-fixed(n).tail
}

fun format-frac0( t : time, n : int, locale : time-locale ) : string {
  val frac = t.seconds.fraction
  if (frac.zero?) then "" else frac.show-fixed(n).tail.pad-right(n+1,'0')
}

fun format-tz-offset( t : time, n : int, hmsep : string, utc : string ) : string {
  show-tzdelta(t.tzdelta,hmsep=hmsep,utc=utc)
}

fun format-timezone( t : time, n : int, locale : time-locale ) : string {
  format-tz-offset(t,n,if (n==2) then "" else ":",if (n==2) then "+0000" else "+00:00")
}

fun format-utc-timezone( t : time, n : int, locale : time-locale ) : string {
  format-tz-offset(t,n,":",if (n==2) then "" else "Z")
}

fun format-timestamp( t : time, n : int, locale : time-locale ) : string {
  t.instant.timestamp.show(3)
}


val formats : list<(char,int,(time,int,time-locale) -> string)> = [
  ('Y', 6, format-year),
  ('M', 4, format-month),
  ('D', 3, format-day),
  ('H', 2, format-hours),
  ('m', 2, format-minutes),
  ('s', 2, format-seconds),
  ('z', 2, format-timezone),
  ('Z', 2, format-utc-timezone),
  ('f', 8, format-frac0),
  ('F', 8, format-frac),
  ('C', 2, format-calname),
  ('E', 1, format-era),
  ('y', 1, format-absyear),
  ('d', 4, format-weekday),
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

* ``Y``: the year as a number (without zero padding) (``1970``, ``203``)
* ``YY``, ```YYYY``: the year in upto 4 digits (``70``, ``1970``, ``0203``).
  If the year is smaller than zero or larger than 9999, the year is displayed
  with 5 or more digits and preprended with an explicit sign (``-00030``, ``+10345``).
* ``M``, ``MM``: the month. (``1``, ``03``). In case of an ISO week (`cal-iso-week`)
  or ISO month (`cal-iso-month`) calendar, the month is prefixed with ``W`` or ``M``.
* ``MMM``, ``MMMM``: name of the month in the specified `locale`. (``Jan``, ``January``)
* ``D``, ``DD``: the day of the month. (``1``, ``08``). If ``DD`` is used and this
  is an ISO week calendar (`cal-iso-week`) just one digit is used for the week day.
* ``DDD``: the day of the year. (``087``)
* ``d``: the ISO day of the week, 1 for Monday ending in 7 for Sunday.
* ``dd``, ``ddd``,``dddd``: the day of the week in the current `locale`. (``We``,``Wed``,``Wednesday``)
* ``h``, ``hh``: the hours using a 12-hour clock (with am/pm). (``9``, ``09``)
* ``H``, ``HH``: the hours using a 24-hour clock. (``21``, ``09``)
* ``m``, ``mm``: the minutes. (``9``, ``09``)
* ``s``, ``ss``: the whole seconds. (``8``, ``08``)
* ``a``, ``aa``: AM/PM designation. (``a``, ``am``)
* ``A``, ``AA``: AM/PM designation in upper-case. (``A``, ``AM``)
* ``f``,...,``fffffffff``: upto 9 digits of a fraction of a second. Starts with a dot. (``.320``,  ``.000``)
* ``F``,...,``FFFFFFFFF``: upto 9 digits of a fraction of a second. If not zero, starts with a dot.
  In contrast to the ``f`` patterns displays the minimal number of required digits 
  (and is not right-padded with zeros). (``.32``)
* ``z``: timezone offset in hours and minutes separated by a colon. Use ``+00:00`` for UTC time. (``+01:00``)
* ``zz``: timezone offset in hours and minutes without a separator. Use ``+0000`` for UTC time. (``-0700``)
* ``Z``: timezone offset in hours and minutes separated by a colon, use ``Z`` for UTC time.   
* ``ZZ``: timezone offset in hours and minutes separated by colon, use an empty string for UTC time.   
* ``x``: fractional seconds since `min-time`. (``63610768799.429``)
* ``YYYYYY``: the year in [ECMAscript](http://www.ecma-international.org/ecma-262/5.1/#sec-15.9.1.15.1) 6 digits, prepended with the sign. (``+002016``,``-000023``,``+000000``)
* ``y``: the absolute value of the year as a number (without zero padding). Useful when
  displaying Julian (`cal-julian`) negative years as ``10 BC`` for example (e.g. ``"y E"``).
* ``C``, ``CC``: the short or long calendar name. (The short name is empty for the standard ISO calendars).
* ``E``: the era name, for example ``CE`` for the Gregorian calendar.
* ``'...'``, ``"..."``: anything between quotes is displayed as is. (``'M'M`` becomes ``M11`` for November)

There are also various forms to display dates and times in a locale specific way.
We give examples in English and Dutch. The lower-case ``l`` variants use short
names for month- and day names.

* ``t``: hours and minutes (``3:21pm, 15:21``)
* ``tt``: hours, minutes, and seconds (``3:21:01pm, 15:21:01``)
* ``L``, ``l``: a date (``09/29/2016, 29.09.2016``) and (``9/29/2016, 29.9.2016``)
* ``LL``, ``ll``: date with month name (``29 September 2016, 29 september 2016``) and (``29 Sep 2016, 29 sep 2016``)
* ``LLL``, ``lll``: date with month name and time (``29 September 2016 1:15pm, 29 september 2016 13:15``)
* ``LLLL``, ``llll``: date with day name, month name, and time (``Thursday, 29 September 2016 1:15pm``) and (``Thu, 29 Sep 2016 1:15pm``)

After formatting, any left- or right white space is trimmed. This allows specifiers
like `"YYYY E C"` that display correctly even if the era or calendar name is empty.

For example, to display a time in the standard [Internet Message Format](https://tools.ietf.org/html/rfc2822#section-3.3)
you can use `now().format("ddd, D MMM Y HH:mm:ss zz")` displayed as `"Tue, 27 Sep 2016 06:36:55 -0700"` for example.\
A standard ISO string can be formatted as, `"YYYY-MM-DD'T'HH:mm:ssFFFFFFFFFZ C"`.

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

fun showl( i : int, width : int = 2 ) : string {
  i.show.pad-left(width,'0')
}