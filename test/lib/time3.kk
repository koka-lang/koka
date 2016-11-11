/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

module time3

public import std/fixed
public import std/xtime

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
}

fun test-jd() {
  check("jd0"){ instant-at(2000,1,1,12,0,0).jd.show == "2451545" }
  check("jd1"){ instant-at(2013,1,1,0,30,0).jd.show == "2456293.520833333" }
  check("jd2"){ instant-at-jd(0.0).time.show == "-4713-11-24T12:00:00Z" }
  check("jd3"){ instant-at-jd(0.0).time(cal=cal-julian).show == "-4713-01-01T12:00:00Z JC" }
  check("jd4"){ instant-at(2014,1,1).jd(ts-tai).show == "2456658.500405092" }
}

fun test-astro() {
  check("astro1"){ instant-at(1977,1,1,cal=cal-tai).timestamp(ts-tt).seconds == instant-at(1977,1,1,cal=cal-tai).timestamp(ts-tcg).seconds } 
  check("astro2"){ instant-at-jd(fixed(24431445003725,7),ts-tt).time(cal=cal-tai).show == "1977-01-01T00:00:00Z TAI" }
  // http://www.iausofa.org/sofa_ts_c.pdf, page 29
  val i = instant-at(2006,1,15,21,24,37,0.5)
  check("astro3a"){ i.time(cal=cal-tai).show == "2006-01-15T21:25:10.500Z TAI" }
  check("astro3b"){ i.time(cal=cal-tt).show  == "2006-01-15T21:25:42.684Z TT" }
  check("astro3c"){ i.time(cal=cal-tcg).show == "2006-01-15T21:25:43.322690576Z TCG" }
  check("astro3d"){ i.time(cal=cal-tdb).show == "2006-01-15T21:25:42.684546690Z TDB" }  // should be: .684373  at lon:-155.933222, lat: 19.48125
  check("astro3e"){ i.time(cal=cal-tcb).show == "2006-01-15T21:25:56.894125939Z TCB" }  // should be: .893952 (because derived from tdb)
  check("astro3f"){ i.time(cal=cal-ut1()).show == "2006-01-15T21:24:37.831700392Z UT1" } // should be: .834078 or .834100 ?
}

public fun main() {
  test-misc()
  test-jd()
  test-astro()
}