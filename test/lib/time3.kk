/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

module time3

import std/path
import std/file
public import std/fixed
public import std/xtime
public import std/time/timespan
public import std/time/astro
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
}

fun test-jd() {
  check("jd0"){ instant-at(2000,1,1,12,0,0).jd(ts-utc).show == "2451545" }
  check("jd1"){ instant-at(2013,1,1,0,30,0).jd(ts-utc).show == "2456293.520833333" }
  check("jd2"){ instant-at-jd(0.0,ts-utc).time.show == "-04713-11-24T12:00:00Z" }
  check("jd3"){ instant-at-jd(0.0,ts-utc).time(cal=cal-julian).show == "-04713-01-01T12:00:00Z JC" }
  check("jd4"){ instant-at(2014,1,1).jd(ts-tai).show == "2456658.500405092" }
}

fun test-astro() {
  // check("astro1"){ instant-at(1977,1,1,cal=cal-tai).timestamp(ts-tt).seconds == instant-at(1977,1,1,cal=cal-tai).timestamp(ts-tcg).seconds } 
  check("astro2"){ instant-at-jd(fixed(24431445003725,7),ts-tt).time(cal=cal-tai).show == "1977-01-01T00:00:00Z TAI" }
}

public val sofai = instant-at(2006,1,15,21,24,37,0.5)

// from: <http://www.iausofa.org/sofa_ts_c.pdf>
// and: <http://docs.astropy.org/en/v0.2.2/time/index.html#time-scale>
fun test-sofa() {
  check("sofa1"){ instant-at(2010,7,24,11,18,7,0.318).time(cal=cal-tt).show == "2010-07-24T11:19:13.502Z TT" }
  check("sofa2"){ instant-at(2008,2,29,23,59,59,0.9).jd(ts-utc) == fixed(2454526499998842,9) }
  check("sofa3"){ instant-at(2008,2,29,23,59,59,0.9).jd(ts-utc).round-to-prec(3).instant-at-jd(ts-utc).time.show == "2008-03-01T00:00:00Z" }
  val i = sofai
  check("sofa4utc"){ i.time(cal=cal-iso).show == "2006-01-15T21:24:37.500Z" }
  check("sofa4ut1"){ i.time(cal=cal-ut1()).show == "2006-01-15T21:24:37.831700800Z UT1" } // sofa: .834100 ; should be between 0.8340874 and 0.8340738
  check("sofa4tai"){ i.time(cal=cal-tai).show == "2006-01-15T21:25:10.500Z TAI" }
  check("sofa4tt"){ i.time(cal=cal-tt).show  == "2006-01-15T21:25:42.684Z TT" }
  check("sofa4tcg"){ i.time(cal=cal-tcg).show == "2006-01-15T21:25:43.322690496Z TCG" }   // sofa: .322690
  // these differ slightly from SOFA resuls; perhaps due to different earth position (we always assume the geo-center)
  check("sofa4tdb"){ i.time(cal=cal-tdb).show == "2006-01-15T21:25:42.684373055Z TDB" }   // sofa: .683799  @ lon:-155.933222, lat: 19.48125
  check("sofa4tcb"){ i.time(cal=cal-tcb).show == "2006-01-15T21:25:56.893952304Z TCB" }   // sofa: .893378  (differs because derived from tdb)
  // but we agree on the JD number of the skyfield library: http://rhodesmill.org/skyfield/time.html#uniform-time-scales-tai-tt-and-tdb
  check("skyfield-tt") { i.jd(ts-tt,12).show  == "2453751.392855138888" }  // skyfield: 2453751.392855139
  check("skyfield-tdb"){ i.jd(ts-tdb,12).show == "2453751.392855143206" }  // skyfield: 2453751.3928551432
  check("skyfield-tai1"){ instant-at(2014,1,1).jd(ts-tai,12).show == "2456658.500405092592" } // skyfield: 2456658.5004050927 
  check("skyfield-tt1") { instant-at(2014,1,1).jd(ts-tt,12).show  == "2456658.500777592592" } // skyfield: 2456658.5007775929  
  check("skyfield-tdb1"){ instant-at(2014,1,1).jd(ts-tdb,12).show == "2456658.500777591821" } // skyfield: 2456658.500777592  
}

public fun test-ut1() {
  println("load UT1 calendar")
  val calut1= cal-ut1(load-iers-ut1())
  val t = time(2012,6,30,23,59,58)
  check("sofa-ut1"){ sofai.time(cal=calut1).show == "2006-01-15T21:24:37.834087400Z UT1" } // sofa: .834100 ; due to interpolation?
  list(0,4).foreach fun(i) {
    check("leap-ut" + i.show){ 
      val s = (t + duration(i)).time(cal=calut1).show 
      val r = (if (i <= 2) then "2012-06-30T23:59:" + (i+57).show + ".413161800Z UT1" 
                           else "2012-07-01T00:00:" + (i - 3).show.pad-left(2,'0') + ".413236Z UT1")  
      // trace("s=" + s.show + ", r=" + r.show)
      s == r
    }
  }
}

public fun main() {
  test-misc()
  test-jd()
  test-astro()
  test-sofa()
}