/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

module time3

public import std/fixed
public import std/time/xinstant
public import std/time/xcalendar
public import std/time/timescale

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

public fun main() {
  test-misc()
  test-jd()
}