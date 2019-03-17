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
  days: int,               // days since epoch
  secs: timespan,          // seconds into this day (`0 <= secs < secs-in-day`)
  secs-in-day: timespan    // total seconds in this day (including a possible leap second)
)
