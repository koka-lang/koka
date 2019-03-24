/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

// Testing leap seconds
module time2

public import std/num/ddouble
public import std/time

/*----------------------------------------------------------------------------
  Testing
----------------------------------------------------------------------------*/

fun check(name : string, res : string, tst : () -> io string ) : io () {
  val got = tst()
  println(name.pad-right(14,' ') + ": " 
    + (if (got == res) then "ok: " + res 
                       else "FAILED!:\n expect: " + res + "\n gotten: " + got + "\n"))
}


fun test-leap() {
  check("leap1", "2016-12-31T23:59:59Z" ) {  instant(2017,1,1,0,0,35,cal=cal-tai).time.show  }
  check("leap2", "2016-12-31T23:59:60Z" ) {  instant(2017,1,1,0,0,36,cal=cal-tai).time.show  }
  check("leap3", "2017-01-01T00:00:00Z" ) {  instant(2017,1,1,0,0,37,cal=cal-tai).time.show  }
  // 'leap second' around 1972-01-01 of 0.11035s (0.107758s from TAI)
  check("leap4", "1971-12-31T23:59:59.107758Z" ) {  instant(1972,1,1,0,0,9,cal=cal-tai).time.show(6)  }
  check("leap5", "1971-12-31T23:59:59.907758Z" ) {  instant(1972,1,1,0,0,9,0.8,cal=cal-tai).time.show(6)  }
  check("leap6", "1971-12-31T23:59:60.007758Z" ) {  instant(1972,1,1,0,0,9,0.9,cal=cal-tai).time.show(6)  }
  check("leap7", "1972-01-01T00:00:00Z" ) {  instant(1972,1,1,0,0,10,cal=cal-tai).time.show  }
  // discontinuity 1961-08-01 of 0.05s
  check("leap8", "1961-07-31T23:59:59.902430Z" ) {  instant(1961,8,1,0,0,1,0.6,cal=cal-tai).time.show(6)  }
  check("leap9", "1961-08-01T00:00:00.062430Z" ) {  instant(1961,8,1,0,0,1,0.71,cal=cal-tai).time.show(6)  }
  // initial 0.005 leap second on 1961-01-01
  check("leap10", "1960-12-31T23:59:60.005Z") {  instant(1961,1,1,0,0,1,0.4228179,cal=cal-tai).time.show(3)  }
  check("leap11", "1961-01-01T00:00:00Z") {  instant(1961,1,1,0,0,1,0.4228181,cal=cal-tai).time.show(6)  }  
  // TAI-UTC epoch
  check("leap12", instant(1958,1,1,cal=cal-iso).show ) {  instant(1958,1,1,cal=cal-tai).show  }  
  // GPS
  check("gps1", "1980-01-06T00:00:00Z" ) {  instant(1980,1,6,cal=cal-gps).time.show  }
  check("gps2", "1980-01-06T00:00:19Z TAI" ) {  instant(1980,1,6,cal=cal-gps).time(cal=cal-tai).show  }
  check("gps3", "2017-01-01T00:00:00Z" ) {  instant(2017,1,1,0,0,18,cal=cal-gps).time.show  }
  // if (wrongly) overflowing into leap seconds, we keep the UTC offset of just before the next day
  check("leap-ovf1", "2017-01-01T00:00:35.999Z TAI" ) {  instant(2017,1,0,23,59,59,0.999).time(cal=cal-tai).show  }
  check("leap-ovf2", "2017-01-01T00:00:36.900Z TAI" ) {  instant(2017,1,0,23,59,60,0.9).time(cal=cal-tai).show  }
  check("leap-ovf3", "2017-01-01T00:00:37.100Z TAI" ) {  instant(2017,1,0,23,59,61,0.1).time(cal=cal-tai).show  }
  check("leap-ovf4", "2017-01-01T00:00:37Z TAI" ) {  instant(2017,1,1).time(cal=cal-tai).show  }
  // a negative leap second of 1968-02-01
  check("leap-neg1", "1968-02-01T00:00:06.185682Z TAI" ) {  instant(1968,1,31,23,59,59,0.9).time(cal=cal-tai).show(6)  }
  check("leap-neg2", "1968-02-01T00:00:06.235682Z TAI" ) {  instant(1968,1,31,23,59,59,0.95).time(cal=cal-tai).show(6)  }  // overflow of leap seconds
  check("leap-neg3", "1968-02-01T00:00:06.185682Z TAI" ) {  instant(1968,2,1).time(cal=cal-tai).show  }
  // durations over leap seconds
  check("leap-sub1","1.5s") { (time(2016,1,1,0,0,0,0.5) - time(2015,12,31,23,59,59)).show }
  check("leap-sub2","2.5s") { (time(2017,1,1,0,0,0,0.5) - time(2016,12,31,23,59,59)).show }
  check("leap-sub3","2.5s") { (time(2017,1,1,0,0,0,0.5) - time(2017,1,1,2,59,59,tz=tz-fixed(3))).show }
  check("add-days1","2017-01-01T12:00:00Z") { time(2016,12,31,12).add-days(1).show }
  check("add-days2","1582-10-15T00:00:00Z JG") { time(1582,10,4,cal=cal-jg).add-days(1).show}
}

public fun main() {
  //update-leaps-table(verbose=True)
  test-leap()
}