/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

module time5

public import std/num/decimal
public import std/time
import std/time/astro

/*----------------------------------------------------------------------------
  Testing
----------------------------------------------------------------------------*/


fun check(name : string, res : string, tst : () -> string ) : io () {
  val got = tst()
  println(name + ": " + (if (got == res) then "ok: " + res else "failed!:\n expect: " + res + "\n gotten: " + got))
}

fun test-round() {
   // rounding of dates
  check("round0", "2000-01-01T00:00:00Z"){ instant(1999,12,31,23,59,59,0.9999).time.show(3)  }
  check("round1", "1999-12-31T23:59:59.999900Z" ){ instant(1999,12,31,23,59,59,0.9999).time.show(6) }

  // round into a leap second
  check("round2", "2016-12-31T23:59:60Z"){ instant(2016,12,31,23,59,59,0.9998).time.show(3)  }

  // rounding between timescales 
  check("round3", "2000-01-01T00:00:00Z TT"){ instant(2000,1,0,23,59,27,0.816,cal=cal-tai).time(cal=cal-tt).show }

  // round into a different calendar 
  check("round4", "1582-10-04T23:59:59.900Z JG") { time(1582,10,4,23,59,59,0.9,cal=cal-jg).show }
  check("round5", "1582-10-15T00:00:00Z JG") { time(1582,10,4,23,59,59,0.9,cal=cal-jg).show(0) }
}

public fun main() {
  test-round()
}