/*----------------------------------------------------------------------------
   Copyright 2012-2021, Microsoft Research, Daan Leijen

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/

// Testing leap seconds
module time2

import std/num/ddouble
import std/time/timestamp
import std/time

/*----------------------------------------------------------------------------
  Testing
----------------------------------------------------------------------------*/

fun check(name : string, res : string, tst : () -> <io,utc> string ) : <io,utc> () {
  val got = tst()
  println(name.pad-right(14,' ') ++ ": "
    ++ (if (got == res) then "ok: " ++ res
                       else "FAILED!:\n expect: " ++ res ++ "\n gotten: " ++ got ++ "\n"))
}


fun test-leap() {
  check("leap1", "2016-12-31T23:59:59Z" ) {  ts-tai.instant(2017,1,1,0,0,35).time(ts=utc()).show  }
  check("leap2", "2016-12-31T23:59:60Z" ) {  ts-tai.instant(2017,1,1,0,0,36).time-utc.show  }
  check("leap3", "2017-01-01T00:00:00Z" ) {  ts-tai.instant(2017,1,1,0,0,37).time-utc.show  }
  // 'leap second' around 1972-01-01 of 0.11035s (0.107758s from TAI)
  check("leap4", "1971-12-31T23:59:59.107758Z" ) {  ts-tai.instant(1972,1,1,0,0,9).time(ts=utc()).show(6)  }
  check("leap5", "1971-12-31T23:59:59.907758Z" ) {  ts-tai.instant(1972,1,1,0,0,9,0.8).time-utc.show(6)  }
  check("leap6", "1971-12-31T23:59:60.007758Z" ) {  ts-tai.instant(1972,1,1,0,0,9,0.9).time-utc.show(6)  }
  check("leap7", "1972-01-01T00:00:00Z" ) {  ts-tai.instant(1972,1,1,0,0,10).time-utc.show  }
  // discontinuity 1961-08-01 of 0.05s
  check("leap8", "1961-07-31T23:59:59.902430Z" ) {  ts-tai.instant(1961,8,1,0,0,1,0.6).time-utc.show(6)  }
  check("leap9", "1961-08-01T00:00:00.062430Z" ) {  ts-tai.instant(1961,8,1,0,0,1,0.71).time-utc.show(6)  }
  // initial 0.005 leap second on 1961-01-01
  check("leap10", "1960-12-31T23:59:60.005Z") {  ts-tai.instant(1961,1,1,0,0,1,0.4228179).time-utc.show(3)  }
  check("leap11", "1961-01-01T00:00:00Z") {  ts-tai.instant(1961,1,1,0,0,1,0.4228181).time-utc.show(6)  }
  // TAI-UTC epoch
  check("leap12", instant(1958,1,1,cal=cal-iso).show ) {  ts-tai.instant(1958,1,1).show  }
  // GPS
  check("gps1", "1980-01-06T00:00:00Z" ) {  ts-gps.instant(1980,1,6).time-utc.show  }
  check("gps2", "1980-01-06T00:00:19Z TAI" ) {  ts-gps.instant(1980,1,6).time(ts=ts-tai).show  }
  check("gps3", "2017-01-01T00:00:00Z" ) {  ts-gps.instant(2017,1,1,0,0,18).time-utc.show  }
  // if (wrongly) overflowing into leap seconds, we keep the UTC offset of just before the next day
  check("leap-ovf1", "2017-01-01T00:00:35.999Z TAI" ) {  instant(2017,1,0,23,59,59,0.999).time-tai.show  }
  check("leap-ovf2", "2017-01-01T00:00:36.900Z TAI" ) {  instant(2017,1,0,23,59,60,0.9).time-tai.show  }
  check("leap-ovf3", "2017-01-01T00:00:37.100Z TAI" ) {  instant(2017,1,0,23,59,61,0.1).time-tai.show  }
  check("leap-ovf4", "2017-01-01T00:00:37Z TAI" ) {  instant(2017,1,1).time-tai.show  }
  // a negative leap second of 1968-02-01 (of -0.01s)
  check("leap-neg1", "1968-02-01T00:00:06.185682Z TAI" ) {  instant(1968,1,31,23,59,59,0.9).time-tai.show(6)  }
  check("leap-neg2", "1968-02-01T00:00:06.235682Z TAI" ) {  instant(1968,1,31,23,59,59,0.95).time-tai.show(6)  }  // (invalid time) overflow of leap seconds
  check("leap-neg3", "1968-02-01T00:00:06.185682Z TAI" ) {  instant(1968,2,1).time-tai.show  }

  // check drift and leap second at 19:00h
  check("leap-drift1", "1959-01-28T19:00:00.512134Z TAI") { instant(1959,1,28,19).time-tai.show(6) }
  check("leap-drift2", "1959-01-28T19:00:00.512134Z TAI") { instant(1959,1,28,18,59,60,0.02).time-tai.show(6) }
  check("leap-drift3", "1959-01-28T19:00:00.492134Z TAI") { instant(1959,1,28,18,59,60).time-tai.show(6) }

  // durations over leap seconds
  check("leap-sub1","1.5s") { (time(2016,1,1,0,0,0,0.5) - time(2015,12,31,23,59,59)).show }
  check("leap-sub2","2.5s") { (time(2017,1,1,0,0,0,0.5) - time(2016,12,31,23,59,59)).show }
  check("leap-sub3","2.5s") { (time(2017,1,1,0,0,0,0.5) - time(2017,1,1,2,59,59,tz=tz-fixed(3))).show }
  check("add-days1","2017-01-01T12:00:00Z") { time(2016,12,31,12).add-days(1).show }
  check("add-days2","1582-10-15T00:00:00Z JG") { time(1582,10,4,cal=cal-jg).add-days(1).show}

  // unix timestamps
  check("unix1", "1972-12-31T23:59:59Z") { unix-instant(94694399.0,ts=utc()).time.show }
  check("unix2", "1972-12-31T23:59:60Z") { unix-instant(94694399.0,1.0,ts=utc()).time.show }
  check("unix3", "1973-01-01T00:00:00Z") { unix-instant(94694400.0,ts=utc()).time.show }
  check("unix4", "94694399") { instant(1972,12,31,23,59,59).unix-timestamp.show }
  check("unix5", "94694400") { instant(1972,12,31,23,59,60).unix-timestamp.show }
  check("unix6", "94694400") { instant(1973,1,1).unix-timestamp.show }
}

pub fun main() {
  //val ts-utc = ts-utc-load()
  // with fun utc() { ts-ti }
  //update-leaps-table(verbose=True)
  // std/core/@default-exn(fn() std/time/utc/@default-utc(fn()
   test-leap()
   //))
}
