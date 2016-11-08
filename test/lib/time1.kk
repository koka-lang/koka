/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

module time1

import std/xtime

/*----------------------------------------------------------------------------
  Testing
----------------------------------------------------------------------------*/


fun check(name : string, tst : () -> ndet bool ) : io () {
  println(name + ": " + (if (tst()) then "ok" else "failed!"))
}

fun test-misc() {
  // unix 32/64-bit overflow
  check("misc0"){ instant-at(2038,1,19,3,14,8).timestamp(ts-unix).secs.show-hex == "0x80000000" }
  check("misc1"){ instant-at(292277026596,12,4,15,30,8).timestamp(ts-unix).secs.show-hex == "0x8000000000000000" }
}

fun test-jd() {
  check("jd0"){ time(2000,1,1,12,0,0,cal=cal-tai).instant.jd == 2451545.0 }
  check("jd1"){ time(2013,1,1,0,30,0,cal=cal-tai).instant.jd == (2456293.5208333335) }
  check("jd2"){ instant-at-jd(0.0).time.show == "-4713-11-24T12:00:00Z" }
  check("jd3"){ instant-at-jd(0.0).time(cal=cal-julian).show == "-4713-01-01T12:00:00Z JC" }
  check("jd4"){ instant-at(2014,1,1).jd.show == "2456658.5004050927" }
}

fun test-leap() {
  check("leap1"){ instant-at(2017,1,1,0,0,35,cal=cal-tai).time.show == "2016-12-31T23:59:59Z" }
  check("leap2"){ instant-at(2017,1,1,0,0,36,cal=cal-tai).time.show == "2016-12-31T23:59:60Z" }
  check("leap3"){ instant-at(2017,1,1,0,0,37,cal=cal-tai).time.show == "2017-01-01T00:00:00Z" }
  // 'leap second' around 1972-01-01 of 0.1s
  check("leap4"){ instant-at(1972,1,1,0,0,9,cal=cal-tai).time.show == "1971-12-31T23:59:59.107758Z" }
  check("leap5"){ instant-at(1972,1,1,0,0,9,0.8,cal=cal-tai).time.show == "1971-12-31T23:59:59.907758Z" }
  check("leap6"){ instant-at(1972,1,1,0,0,9,0.9,cal=cal-tai).time.show == "1971-12-31T23:59:60.007758Z" }
  check("leap7"){ instant-at(1972,1,1,0,0,10,cal=cal-tai).time.show == "1972-01-01T00:00:00Z" }
  // discontinuity 1961-08-01 of 0.05s
  check("leap8"){ instant-at(1961,8,1,0,0,1,0.6,cal=cal-tai).time.show == "1961-07-31T23:59:59.902430Z" }
  check("leap9"){ instant-at(1961,8,1,0,0,1,0.7,cal=cal-tai).time.show == "1961-08-01T00:00:00.052430Z" }
  // initial 1.422818 leap second in 1961-01-01
  check("leap10"){ instant-at(1961,1,1,0,0,1,0.4,cal=cal-tai).time.show == "1960-12-31T23:59:61.400Z"}
  check("leap11"){ instant-at(1961,1,1,0,0,1,0.5,cal=cal-tai).time.show == "1961-01-01T00:00:00.077182Z"}  
  // GPS
  check("gps1"){ instant-at(1980,1,6,cal=cal-gps).time.show == "1980-01-06T00:00:00Z" }
  check("gps2"){ instant-at(1980,1,6,cal=cal-gps).time(cal=cal-tai).show == "1980-01-06T00:00:19Z TAI" }
  check("gps3"){ instant-at(2017,1,1,0,0,18,cal=cal-gps).time.show == "2017-01-01T00:00:00Z" }
}


fun test-tz() {
  println( now().time(tz=timezone("US/Pacific")).show )
  println( now().time(tz=tz-local()).show )
  println( "local timezone guess: " + guess-local-timezone().tz-name )

  val z1  = timezone("America/New_York")
  val z2  = timezone("Asia/Tehran")
  val t1  = instant-at(1978,12,30,12,1,0,tz=z1) + duration(14*3600 + 44*60) 
  check("tz1"){ t1.time(tz=z2).show == "1978-12-31T11:45:00+04:00 (IRST)"}
  val t2  = instant-at(1978,12,31,12,1,0,tz=z1) + duration(14*3600 + 44*60) // across DST & leap second
  check("tz2"){ t2.time(tz=z2).show == "1979-01-01T11:14:59+03:30 (IRST)"}

  check("tz3a"){ time(2011,3,13,1,15,tz=z1).show == "2011-03-13T01:15:00-05:00 (EST)" }
  check("tz3b"){ time(2011,3,13,2,15,tz=z1).show == "2011-03-13T01:15:00-05:00 (EST)" }
  check("tz3c"){ time(2011,3,13,3,15,tz=z1).show == "2011-03-13T03:15:00-04:00 (EDT)" }
  
  check("tz4a"){ time(2011,11,6,0,15,tz=z1).show == "2011-11-06T00:15:00-04:00 (EDT)" }
  check("tz4b"){ time(2011,11,6,1,15,tz=z1).show == "2011-11-06T01:15:00-04:00 (EDT)" }
  check("tz4c"){ time(2011,11,6,2,15,tz=z1).show == "2011-11-06T02:15:00-05:00 (EST)" }

  // leap second in other time zone
  check("tz5"){ instant-at(2017,1,1,0,0,36,0.5,cal=cal-tai).time(tz=timezone("US/Pacific")).show == "2016-12-31T15:59:60.500-08:00 (PST)" }
}



public fun test() {
  test-misc()
  test-jd()
  test-leap()
  test-tz()
}