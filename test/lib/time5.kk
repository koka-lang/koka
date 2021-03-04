/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

module time5

import std/time/timestamp
public import std/num/ddouble
public import std/time
//import std/time/astro

/*----------------------------------------------------------------------------
  Testing
----------------------------------------------------------------------------*/


fun check(name : string, res : string, tst : () -> utc string ) : <io,utc> () {
  val got = tst()
  println(name ++ ": " ++ (if (got == res) then "ok: " ++ res else "failed!:\n expect: " ++ res ++ "\n gotten: " ++ got))
}

fun test-round() {
   // rounding of dates
  check("round0", "2000-01-01T00:00:00Z"){ instant(1999,12,31,23,59,59,0.9999).time.show(3)  }
  check("round1", "1999-12-31T23:59:59.999900Z" ){ instant(1999,12,31,23,59,59,0.9999).time.show(6) }

  // round into a leap second
  check("round2", "2016-12-31T23:59:60Z"){ instant(2016,12,31,23,59,59,0.9998).time.show(3)  }

  // rounding between timescales
  check("round3", "2000-01-01T00:00:00Z TT"){ instant(2000,1,0,23,59,27,0.816,ts=ts-tai).time(ts=ts-tt).show }

  // round into a different calendar
  check("round4", "1582-10-04T23:59:59.900Z JG") { time(1582,10,4,23,59,59,0.9,cal=cal-jg).show }
  check("round5", "1582-10-15T00:00:00Z JG") { time(1582,10,4,23,59,59,0.9,cal=cal-jg).show(0) }
}

fun test-jd() {
  check("jd0", "0"){ time(-4713,11,24,12,ts=ts-tt).jd.show }
  check("jd1", "2441318.5"){ time(1972,1,2,tz=tz-fixed(1)).jd.show }
  check("jd2", "2441318.458333333"){ time(1972,1,2,tz=tz-fixed(1)).instant.jd(ts-ti).show(9) }

  // jd around leap seconds
  check("mjd0", "41318"){ time(1972,1,2,tz=tz-fixed(1)).mjd.show }
  check("mjd1", "41317.958333333"){ time(1972,1,2,tz=tz-fixed(1)).instant.mjd(ts-ti).show(9)   }
  check("mjd2", "57387.5"){ time(2015,12,31,12,0,0).mjd.show }
  check("mjd3", "57753.499994213"){ time(2016,12,31,12,0,0).mjd.show(9) }
  check("mjd4", "57753.5"){ time(2016,12,31,12,0,0,0.5).mjd.show }

  // leap second at 19:00h
  check("mjd5", "36596.791666590"){ instant(1959,1,28,18,59,60,0.01).mjd(ts-ti).show(9) }
  check("mjd6", "1959-01-28T18:59:60.010Z"){ instant-at-mjd(36596.791666590,ts-ti).time.show(3) }
  check("mjd7", "1959-01-28T23:59:59.999999Z"){ instant-at-mjd(36596.99999999999,ts-ti).time.show(6) }
  check("mjd8", "2016-12-31T23:59:60.999999Z"){ instant-at-mjd(57753.99999999999,ts-ti).time.show(6) }  // just before leap seconds of 2017-01-01
}

public fun main() {
  test-round()
  test-jd()
}
