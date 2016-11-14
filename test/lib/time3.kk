/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

module time3

public import std/fixed
public import std/xtime
public import std/time/timestamp
public import std/time/ut1

/*----------------------------------------------------------------------------
  Testing
----------------------------------------------------------------------------*/


fun check(name : string, tst : () -> ndet bool ) : io () {
  println(name + ": " + (if (tst()) then "ok" else "failed!"))
}

fun test-misc() {
  // unix 32/64-bit overflow
  check("misc0"){ instant-at(2038,1,19,3,14,8).timestamp(ts-unix).seconds.int.show-hex == "0x80000000" }
  check("misc1"){ instant-at(292277026596,12,4,15,30,8).timestamp(ts-unix).seconds.int.show-hex == "0x8000000000000000" }

  // rounding of dates
  check("round0"){ instant-at(1999,12,31,23,59,59,0.9999).time.show(3) == "2000-01-01T00:00:00Z" }
  check("round1"){ instant-at(1999,12,31,23,59,59,0.9999).time.show(6) == "2016-12-31T23:59:59.999900Z" }
  // round into a leap second
  check("round2"){ instant-at(2016,12,31,23,59,59,0.9998).time.show(3) == "2016-12-31T23:59:60Z" }
}

fun test-jd() {
  check("jd0"){ instant-at(2000,1,1,12,0,0).jd.show == "2451545" }
  check("jd1"){ instant-at(2013,1,1,0,30,0).jd.show == "2456293.520833333" }
  check("jd2"){ instant-at-jd(0.0).time.show == "-4713-11-24T12:00:00Z" }
  check("jd3"){ instant-at-jd(0.0).time(cal=cal-julian).show == "-4713-01-01T12:00:00Z JC" }
  check("jd4"){ instant-at(2014,1,1).jd(ts-tai).show == "2456658.500405092" }
}

fun test-astro() {
  // check("astro1"){ instant-at(1977,1,1,cal=cal-tai).timestamp(ts-tt).seconds == instant-at(1977,1,1,cal=cal-tai).timestamp(ts-tcg).seconds } 
  check("astro2"){ instant-at-jd(fixed(24431445003725,7),ts-tt).time(cal=cal-tai).show == "1977-01-01T00:00:00Z TAI" }
}

// from: <http://www.iausofa.org/sofa_ts_c.pdf>
// and: <http://docs.astropy.org/en/v0.2.2/time/index.html#time-scale>
fun test-sofa() {
  check("sofa1"){ instant-at(2010,7,24,11,18,7,0.318).time(cal=cal-tt).show == "2010-07-24T11:19:13.502Z TT" }
  check("sofa2"){ instant-at(2008,2,29,23,59,59,0.9).jd == fixed(2454526499998842,9) }
  check("sofa3"){ instant-at(2008,2,29,23,59,59,0.9).jd.round-to-prec(3).instant-at-jd.time.show == "2008-03-01T00:00:00Z" }
  val i = instant-at(2006,1,15,21,24,37,0.5)
  check("sofa4utc"){ i.time(cal=cal-iso).show == "2006-01-15T21:24:37.500Z" }
  check("sofa4ut1"){ i.time(cal=cal-ut1()).show == "2006-01-15T21:24:37.831700800Z UT1" } // sofa: .834100 ; should be between 0.8340874 and 0.8340738
  check("sofa4tai"){ i.time(cal=cal-tai).show == "2006-01-15T21:25:10.500Z TAI" }
  check("sofa4tt"){ i.time(cal=cal-tt).show  == "2006-01-15T21:25:42.684Z TT" }
  check("sofa4tcg"){ i.time(cal=cal-tcg).show == "2006-01-15T21:25:43.322690496Z TCG" }   // sofa: .322690
  check("sofa4tdb"){ i.time(cal=cal-tdb).show == "2006-01-15T21:25:42.684373055Z TDB" }   // sofa: .683799  @ lon:-155.933222, lat: 19.48125
  check("sofa4tcb"){ i.time(cal=cal-tcb).show == "2006-01-15T21:25:56.893821304Z TCB" }   // sofa: .893378  (differs because derived from tdb)
}

public fun main() {
  test-misc()
  test-jd()
  test-astro()
  test-sofa()
}