/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

// Test UT1 time
module time8

import std/os/path
import std/os/file
public import std/num/fixed
public import std/time
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


public val sofai = instant(2006,1,15,21,24,37,0.5)

public fun test-ut1() {
  println("load UT1 calendar")
  val calut1= on-exn(cal-ut1-approx){ cal-ut1-load(verbose=True) }
  check("sofa-ut1","2006-01-15T21:24:37.834075267Z UT1"){  sofai.time(cal=calut1).show  } // astropy: 37.834077793, sofa: 37.834100 ; non-interpolated: 37.834087400Z

  // across a leap second
  check("leap-ut1a","2012-06-30T23:59:57.413235998Z UT1"){ instant(2012,6,30,23,59,58).time(cal=calut1).show }
  check("leap-ut1b","2012-06-30T23:59:58.413235999Z UT1"){ instant(2012,6,30,23,59,59).time(cal=calut1).show }
  check("leap-ut1c","2012-06-30T23:59:59.413235999Z UT1"){ instant(2012,6,30,23,59,60).time(cal=calut1).show }
  check("leap-ut1d","2012-07-01T00:00:00.412236Z UT1")   { instant(2012,6,30,23,59,60,0.999).time(cal=calut1).show }
  check("leap-ut1e","2012-07-01T00:00:00.413236Z UT1")   { instant(2012,7,1,0,0,0).time(cal=calut1).show }
  check("leap-ut1f","2012-07-01T00:00:01.413236002Z UT1"){ instant(2012,7,1,0,0,1).time(cal=calut1).show }

}

public fun main() {
  test-ut1()
}