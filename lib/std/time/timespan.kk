/*----------------------------------------------------------------------------
   Copyright (C) 2012-2019 Microsoft Corporation

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Low level time spans

These are in SI seconds but not necessarily TAI SI (as measured on the earths geoid),
but are also used for astronomical SI seconds, like TCG (measured on the earths core)
or TCB (measured on the solar barycenter). Time spans
are used internally to do time scale conversions and calendar calculations.
User code should use `:std/time/duration/duration`s and `:std/time/instant/instant`s instead.
*/
module std/time/timespan

import std/num/ddouble

// Time spans are just a `:ddouble`; this gives it very high precision
// and range to represent time very precisely. (see the [`instant`](std_time_instant.html) module)
public alias timespan = ddouble

// A zero-valued timespan.
public val timespan0 : timespan = zero

public fun timespan( seconds : int, frac : double = 0.0 ) : timespan {
  if (frac.zero?) then ddouble(seconds) else ddouble(seconds) + ddouble(frac)
}

public fun timespan( secs : double ) : timespan {
  ddouble(secs)
}

public fun div( x : ddouble, y : ddouble, prec : int = 0) : ddouble {
  x / y
}


// Show a `:timespan` (without the `s` postfix).
public fun ts-show( ts : timespan, max-prec : int = 9, secs-width : int = 1 ) : string {
  val s = ts.show-fixed(~max-prec.abs)
  match(s.find(".")) {
    Nothing -> s.pad-left(secs-width,'0')
    Just(dot) -> {
      val f = dot.after.string
      val len3 = ((f.count + 2)/3)*3
      dot.before.string.pad-left(secs-width,'0') + "." + f.pad-right(len3,'0')
    }
  }
}



public struct dayspan(
  days: int,                  // days since epoch
  secs: timespan = timespan0, // seconds into this day
  leap: int = 0               // and extra leap seconds (to provide for leap seconds at other times than the end of day)
)

public val isolar-secs-per-day = 86400
public val solar-secs-per-day = isolar-secs-per-day.timespan

public fun solar-dayspan( days : int, secs : timespan ) : dayspan {
  if (!secs.neg? && secs <= solar-secs-per-day) {
    Dayspan(days,secs,0)
  }
  else {
    val dsecs = secs.floor
    val frac  = secs - dsecs
    val (xdays,xsecs) = divmod(dsecs.int, isolar-secs-per-day)
    Dayspan(days + xdays, xsecs.ddouble + frac, 0)
  }
}


public fun ntp2k-to-dayspan( t : timespan ) : dayspan {
  val days = (t / solar-secs-per-day).floor
  val secs = t - (days * solar-secs-per-day)
  Dayspan(days.int, secs, 0)
}

public fun ntp2k-from-dayspan( ds : dayspan ) : timespan {
  (ds.days.ddouble*solar-secs-per-day + ds.secs + ds.leap.timespan)
}
