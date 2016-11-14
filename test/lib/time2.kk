/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

module time2

public import std/xtime

/*----------------------------------------------------------------------------
  Testing
----------------------------------------------------------------------------*/

fun check(name : string, tst : () -> ndet bool ) : io () {
  println(name + ": " + (if (tst()) then "ok" else "failed!"))
}

fun test-leap() {
  check("leap1"){ instant-at(2017,1,1,0,0,35,cal=cal-tai).time.show == "2016-12-31T23:59:59Z" }
  check("leap2"){ instant-at(2017,1,1,0,0,36,cal=cal-tai).time.show == "2016-12-31T23:59:60Z" }
  check("leap3"){ instant-at(2017,1,1,0,0,37,cal=cal-tai).time.show == "2017-01-01T00:00:00Z" }
  // 'leap second' around 1972-01-01 of 0.11035s (0.107758s from TAI)
  check("leap4"){ instant-at(1972,1,1,0,0,9,cal=cal-tai).time.show == "1971-12-31T23:59:59.107758Z" }
  check("leap5"){ instant-at(1972,1,1,0,0,9,0.8,cal=cal-tai).time.show == "1971-12-31T23:59:59.907758Z" }
  check("leap6"){ instant-at(1972,1,1,0,0,9,0.9,cal=cal-tai).time.show == "1971-12-31T23:59:60.007758Z" }
  check("leap7"){ instant-at(1972,1,1,0,0,10,cal=cal-tai).time.show == "1972-01-01T00:00:00Z" }
  // discontinuity 1961-08-01 of 0.05s
  check("leap8"){ instant-at(1961,8,1,0,0,1,0.6,cal=cal-tai).time.show == "1961-07-31T23:59:59.902430Z" }
  check("leap9"){ instant-at(1961,8,1,0,0,1,0.7,cal=cal-tai).time.show == "1961-08-01T00:00:00.052430Z" }
  // initial 1.422818 leap second in 1961-01-01
  //  check("leap10x"){ instant-at(1961,1,1,0,0,1,0.4,cal=cal-tai).time.show == "1960-12-31T23:59:61.400Z"}
  //  check("leap11x"){ instant-at(1961,1,1,0,0,1,0.5,cal=cal-tai).time.show == "1961-01-01T00:00:00.077182Z"}  
  // but we make it linear going from 1958-01-01
  check("leap10"){ instant-at(1961,1,1,0,0,1,0.4,cal=cal-tai).time.show == "1960-12-31T23:59:59.977182Z"}
  check("leap11"){ instant-at(1961,1,1,0,0,1,0.5,cal=cal-tai).time.show == "1961-01-01T00:00:00.077182Z"}  
  // TAI-UTC epoch
  check("leap12"){ instant-at(1958,1,1,cal=cal-tai) == instant-at(1958,1,1,cal=cal-iso) }  
  // GPS
  check("gps1"){ instant-at(1980,1,6,cal=cal-gps).time.show == "1980-01-06T00:00:00Z" }
  check("gps2"){ instant-at(1980,1,6,cal=cal-gps).time(cal=cal-tai).show == "1980-01-06T00:00:19Z TAI" }
  check("gps3"){ instant-at(2017,1,1,0,0,18,cal=cal-gps).time.show == "2017-01-01T00:00:00Z" }
}


public fun main() {
  test-leap()
}