/*----------------------------------------------------------------------------
   Copyright 2012-2021, Microsoft Research, Daan Leijen

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

// Test UT1 time
module time8

import std/os/path
import std/os/file
public import std/num/ddouble
public import std/time
public import std/time/ut1
public import std/time/astro

/*----------------------------------------------------------------------------
  Testing
----------------------------------------------------------------------------*/

fun check(name : string, res : string, tst : () -> <io,utc> string ) : <io,utc> () {
  val got = tst()
  println(name.pad-right(14,' ') ++ ": "
    ++ (if (got == res) then "ok: " ++ res
                       else "FAILED!:\n expect: " ++ res ++ "\n gotten: " ++ got ++ "\n"))
}


public val sofai = ts-ti.instant(2006,1,15,21,24,37,0.5)

// TAI start at 0 on `1958-01-01Z`
// TT is TAI ++ 32.184, so was `1958-01-01T00:00:32.184 TT`
// At that time, delta-T (=TT - UT1) was 32.166s (see: http://maia.usno.navy.mil/ser7/historic_deltat.data)
// so UT1 was `1958-01-01T00:00:00.018 UT1`
// Also, TAI was at that time defined equal to UT2, but when applying
// the defined adjustment from UT1:
//  UT2 = UT1 ++ 0.022*sin(2*pi*t) - 0.012*cos(2*pi*t) - 0.006*sin(4*pi*t) ++ 0.007*cos(4*pi*t)
// where `t` the fraction of the Besselian year, we are off by 0.008s.
// I guess this is due to UT2 being now approximated from UT1?
public fun test-ut2() {
  val i = ts-tai.instant(1958,1,1)
  check("utc at tai epoch", "1958-01-01T00:00:00Z"){ i.time(ts=utc()).show }
  check("tt  at tai epoch", "1958-01-01T00:00:32.184Z TT") { i.time(ts=ts-tt).show }
  check("ut1 at tai epoch", "1958-01-01T00:00:00.018Z UT1"){ i.time(ts=ts-ut1-approx).show }
  check("ut2 at tai epoch", "1958-01-01T00:00:00.013023922Z UT2"){ i.time(ts=ts-ut2-approx).show }
}

public fun test-ut1(verbose:bool = True) {
  println("load UT1 calendar")
  val ts-ut1= try-default(ts-ut1-approx){ ts-ut1-load(verbose=verbose,download-timeout=20.seconds) }
  // on 2017-05-03 the IERS data says:
  //   2006-01-15T00:00:00Z  UT1-UTC = 0.3340869
  //   2006-01-16T00:00:00Z  UT1-UTC = 0.3340733
  // on 2019-03-22 the IERS data says:
  //   2006-01-15T00:00:00Z  UT1-UTC = 0.3340894
  //   2006-01-15T00:00:00Z  UT1-UTC = 0.3340758

  check("sofa-ut1","2006-01-15T21:24:37.834077267Z UT1"){  sofai.time(ts=ts-ut1).show  }
  // astropy: 37.834077793, sofa: 37.834100 ; linear interpolate using double: 37.8340747674305556

  // across a leap second;
  // on 2017-05-03 the IERS data says:
  //   2012-06-30T00:00:00Z  UT1-UTC = -0.5868424
  //   2012-07-01T00:00:00Z  UT1-UTC =  0.4132318
  // on 2019-03-22 the IERS data says:
  //   2012-06-30T00:00:00Z  UT1-UTC = -0.5868367
  //   2012-07-01T00:00:00Z  UT1-UTC =  0.4132375
  check("leap-ut1a","2012-06-30T23:59:57.413237497Z UT1"){ instant(2012,6,30,23,59,58).time(ts=ts-ut1).show }
  check("leap-ut1b","2012-06-30T23:59:58.413237498Z UT1"){ instant(2012,6,30,23,59,59).time(ts=ts-ut1).show }
  check("leap-ut1c","2012-06-30T23:59:59.413237499Z UT1"){ instant(2012,6,30,23,59,60).time(ts=ts-ut1).show }
  check("leap-ut1d","2012-07-01T00:00:00.412237500Z UT1"){ instant(2012,6,30,23,59,60,0.999).time(ts=ts-ut1).show }
  check("leap-ut1e","2012-07-01T00:00:00.413237500Z UT1"){ instant(2012,7,1,0,0,0).time(ts=ts-ut1).show }
  check("leap-ut1f","2012-07-01T00:00:01.413237502Z UT1"){ instant(2012,7,1,0,0,1).time(ts=ts-ut1).show }

}

public fun main() {
  test-ut1(verbose=False)
  test-ut2()
}
