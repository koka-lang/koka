/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

module time3

public import std/num/ddouble
public import std/time
public import std/time/timestamp
public import std/time/astro
public import std/time/ut1

/*----------------------------------------------------------------------------
  Testing
----------------------------------------------------------------------------*/

fun check(name : string, res : string, tst : () -> io string ) : io () {
  val got = tst()
  println(name.pad-right(14,' ') + ": " 
    + (if (got == res) then "ok: " + res 
                       else "FAILED!:\n expect: " + res + "\n gotten: " + got + "\n"))
}


public fun test-misc() {
  // unix 32/64-bit overflow
  check("misc0","0x80000000"){  instant(2038,1,19,3,14,8).timestamp(ts-unix).seconds.int.show-hex  }
  check("misc1","0x8000000000000000"){  instant(292277026596,12,4,15,30,8).timestamp(ts-unix).seconds.int.show-hex  } // 9223372036854775808
}

fun test-jd(ts-utc) {
  check("jd0","2451545"){  instant(2000,1,1,12,0,0).jd(ts-utc).show  }
  check("jd1","2456293.520833333"){  instant(2013,1,1,0,30,0).jd(ts-utc).show-fixed(9)  }
  check("jd2","-04713-11-24T12:00:00Z"){  instant-at-jd(0.0,ts-utc).time.show  }
  check("jd3","-04713-01-01T12:00:00Z JC"){  instant-at-jd(0.0,ts-utc).time(cal=cal-julian).show  }
  check("jd4","2456658.500405092593"){  instant(2014,1,1).jd(ts-tai).show-fixed(12)  }
  // fraction of a day with a leap second..
  check("jdleap0","57753.999976852119767") { time(2016,12,31,23,59,59).mjd.show-fixed(15) }
  check("jdleap1","57753.999987268665872") { time(2016,12,31,23,59,59,0.9).mjd.show-fixed(15) }
  check("jdleap2","57753.999988426059883") { time(2016,12,31,23,59,60).mjd.show-fixed(15) }
  check("jdleap-tz1","57754.083331211444312") { time(2017,1,1,1,59,59,0.9,tz=tz-fixed(2)).mjd.show-fixed(15) } // across timezone (should *not* be: 57754.083332175925926)
  check("jdleap-tz2","57754.083332175925926") { time(2017,1,1,1,59,59,0.9,tz=tz-fixed(~2)).mjd.show-fixed(15) } // across timezone without a leap
  // inverses
  check("jdleapx0","2016-12-31T23:59:59Z")  { time(2016,12,31,23,59,59).mjd.instant-at-mjd(ts-utc).time.show }
  check("jdleapx1","2016-12-31T23:59:59.900Z")  { time(2016,12,31,23,59,59,0.9).mjd.instant-at-mjd(ts-utc).time.show }
  check("jdleapx2","2016-12-31T23:59:60Z")       { time(2016,12,31,23,59,60).mjd.instant-at-mjd(ts-utc).time.show }
  check("jdleapx3","2017-01-01T00:00:00Z")       { time(2017).mjd.instant-at-mjd(ts-utc).time.show }
}

fun test-astro() {
  // check("astro1"){ instant(1977,1,1,cal=cal-tai).timestamp(ts-tt).seconds == instant(1977,1,1,cal=cal-tai).timestamp(ts-tcg).seconds } 
  check("astro2","1977-01-01T00:00:00Z TAI"){  instant-at-jd(ddouble-exp(24431445003725,~7),ts-tt).time(cal=cal-tai).show  }
}

public val sofai = instant(2006,1,15,21,24,37,0.5)

// from: <http://www.iausofa.org/sofa_ts_c.pdf>
// and: <http://docs.astropy.org/en/v0.2.2/time/index.html#time-scale>
fun test-sofa(ts-utc) {
  check("sofa1","2010-07-24T11:19:13.502Z TT"){  instant(2010,7,24,11,18,7,0.318).time(cal=cal-tt).show  }
  check("sofa2","2454526.499998842593"){  instant(2008,2,29,23,59,59,0.9).jd(ts-utc).show-fixed(12)  }
  check("sofa3","2008-03-01T00:00:00Z"){  instant(2008,2,29,23,59,59,0.9).jd(ts-utc).round-to-prec(3).instant-at-jd(ts-utc).time.show  }
  val i = sofai
  check("sofa4utc","2006-01-15T21:24:37.500Z"){  i.time(cal=cal-iso).show  }
  check("sofa4ut1","2006-01-15T21:24:37.831700800Z UT1"){  i.time(cal=cal-ut1-approx).show  } // astropy: 37.834077793, sofa: 37.834100 ; should be between 0.8340874 and 0.8340738
  check("sofa4tai","2006-01-15T21:25:10.500Z TAI"){  i.time(cal=cal-tai).show  }
  check("sofa4tt","2006-01-15T21:25:42.684Z TT"){  i.time(cal=cal-tt).show   }
  check("sofa4tcg","2006-01-15T21:25:43.322690496Z TCG"){  i.time(cal=cal-tcg).show  }   // astropy: 43.322690497
  // these differ slightly from astropy but up to 2micro-seconds
  check("sofa4tdb","2006-01-15T21:25:42.684373055Z TDB"){  i.time(cal=cal-tdb).show  }   // astropy: 42.684371185
  check("sofa4tcb","2006-01-15T21:25:56.893952304Z TCB"){  i.time(cal=cal-tcb).show  }   // astropy: 56.893950657  (differs because derived from tdb)
  // but we agree on the JD number of the skyfield library: http://rhodesmill.org/skyfield/time.html#uniform-time-scales-tai-tt-and-tdb
  check("skyfield-tt","2453751.392855138889") { i.jd(ts-tt).show-fixed(12) }  // skyfield: 2453751.392855139
  check("skyfield-tdb","2453751.392855143207"){  i.jd(ts-tdb).show-fixed(12)  }  // skyfield: 2453751.3928551432
  check("skyfield-tai1","2456658.500405092593"){  instant(2014,1,1).jd(ts-tai).show-fixed(12)  } // skyfield: 2456658.5004050927 
  check("skyfield-tt1", "2456658.500777592593") { instant(2014,1,1).jd(ts-tt).show-fixed(12) } // skyfield: 2456658.5007775929  
  check("skyfield-tdb1","2456658.500777591821"){  instant(2014,1,1).jd(ts-tdb).show-fixed(12)  } // skyfield: 2456658.500777592  
}

public fun main() {
  val ts-utc = ts-utc-load()
  test-misc()
  test-jd(ts-utc)
  test-astro()
  test-sofa(ts-utc)
}
