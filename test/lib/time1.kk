/*----------------------------------------------------------------------------
   Copyright 2012-2021, Microsoft Research, Daan Leijen

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/

module time1

pub import std/num/ddouble
pub import std/time
pub import std/time/timezone

/*----------------------------------------------------------------------------
  Testing
----------------------------------------------------------------------------*/


fun check(name : string, res : string, tst : () -> <ndet,utc> string ) : <io,utc> () {
  val got = tst()
  println(name.pad-right(14,' ') + ": "
    + (if (got == res) then "ok: " + res
                       else "FAILED!:\n expect: " + res + "\n gotten: " + got + "\n"))
}

pub fun test-timezones() {
  val tzs = load-timezones()
  val tz1 = tzs.timezone("America/New_York")
  val tz2 = tzs.timezone("Pacific/Kosrae")
  val t1  = time(1997,12,31,12,1,0,tz=tz1) + (12.hours + 30.minutes)
  t1.time(tz=tz2).show.println // 1998-01-01T17:31:00+12:00
  val t2  = time(1998,12,31,12,1,0,tz=tz1) + (12.hours + 30.minutes) // across DST & leap second
  t2.time(tz=tz2).show.println // 1999-01-01T16:30:59+11:00

  // from moment-timezones docs: https://github.com/moment/moment-timezone
  val t3 = time(2014,6,1,12)
  t3.time(tz=tz1).show.println
  t3.time(tz=tzs.timezone("America/Los_Angeles")).show.println
  t3.time(tz=tzs.timezone("Asia/Tokyo")).show.println
  t3.time(tz=tzs.timezone("Australia/Sydney")).show.println
  val t4 = time(2014,12,1,12)
  t4.time(tz=tz1).show.println
  t4.time(tz=tzs.timezone("America/Los_Angeles")).show.println
  t4.time(tz=tzs.timezone("Asia/Tokyo")).show.println
  t4.time(tz=tzs.timezone("Australia/Sydney")).show.println

}

fun test-tzs( tzs : timezones ) {
  val z1  = tzs.timezone("America/New_York")
  val z2  = tzs.timezone("Pacific/Kosrae")
  val z3  = tzs.timezone("Asia/Tehran")

  val t1  = instant(1997,12,31,12,1,0,tz=z1) + duration(14*3600 + 44*60)
  check("tz1", "1998-01-01T19:45:00+12:00"){ t1.time(tz=z2).show }
  val t2  = instant(1998,12,31,12,1,0,tz=z1) + duration(14*3600 + 44*60) // across DST & leap second
  check("tz2", "1999-01-01T18:44:59+11:00"){ t2.time(tz=z2).show }
  val t3  = instant(1978,12,31,12,1,0,tz=z1) + duration(14*3600 + 44*60) // across DST & leap second
  check("tz3", "1979-01-01T11:14:59+03:30"){ t3.time(tz=z3).show }

  check("tz3a", "2011-03-13T01:15:00-05:00 (EST)"){ time(2011,3,13,1,15,tz=z1).show }
  check("tz3b", "2011-03-13T01:15:00-05:00 (EST)"){ time(2011,3,13,2,15,tz=z1).show } // non-existent: repeat previous hour for non-existent time!
  check("tz3c", "2011-03-13T03:15:00-04:00 (EDT)"){ time(2011,3,13,3,15,tz=z1).show }

  check("tz4a", "2011-11-06T00:15:00-04:00 (EDT)"){ time(2011,11,6,0,15,tz=z1).show }
  check("tz4b", "2011-11-06T01:15:00-04:00 (EDT)"){ time(2011,11,6,1,15,tz=z1).show } // ambigious: choose earliest
  check("tz4c", "2011-11-06T02:15:00-05:00 (EST)"){ time(2011,11,6,2,15,tz=z1).show }
  check("tz4d", "2011-11-06T01:15:00-04:00 (EDT)"){ instant(2011,11,6,5,15).time(tz=z1).show }
  check("tz4e", "2011-11-06T01:15:00-05:00 (EST)"){ instant(2011,11,6,6,15).time(tz=z1).show }

  check("tz5", "1978-12-31T23:59:59Z") { instant(1979,1,1,3,29,59,tz=z3).time.show }
  check("tz6", "1973-01-01T00:00:11-01:00 TAI") { ts-tai.time(1973,1,1,0,0,11,tz=tz-fixed(~1)).show }

  // leap second in other time zone
  check("tzleap1", "2016-12-31T15:59:60.500-08:00 (PST)"){ ts-tai.instant(2017,1,1,0,0,36,0.5).time-utc(tz=tzs.timezone("US/Pacific")).show }
  check("tzleap2", "1999-01-01T10:59:60.500+11:00"){ ts-tai.instant(1999,1,1,0,0,31,0.5).time-utc(tz=z2).show }
  check("tzleap3", "1979-01-01T03:29:60.500+03:30"){ ts-tai.instant(1979,1,1,0,0,17,0.5).time-utc(tz=z3).show }
  check("tzleap4", "1978-12-31T23:59:59Z"){ instant(1979,1,1,3,29,59,tz=z3).time.show }

  // round into a time zone skip
  //println( time(2011,3,13,2,59,59,0.9999,tz=z1).show )
  check("tzround1", "2011-03-13T01:59:59.999900-05:00 (EST)"){ time(2011,3,13,2,59,59,0.9999,tz=z1).show }
  check("tzround2", "2011-03-13T03:00:00-04:00 (EDT)"){ time(2011,3,13,2,59,59,0.9999,tz=z1).show(3) }

  // round into a timezone leap second (across a DST zone)
  check("tzroundleap1", "1998-12-31T23:59:60Z"){ instant(1999,1,1,10,59,59,0.9999,tz=z2).time.show(3) }
  check("tzroundleap2", "1978-12-31T23:59:60Z"){ instant(1979,1,1,3,29,59,0.9999,tz=z3).time.show(3) }
  check("tzroundleap3", "1999-01-01T10:59:60+11:00"){ time(1999,1,1,10,59,59,0.9999,tz=z2).show(3) }
  check("tzroundleap4", "1979-01-01T03:29:60+03:30"){ time(1979,1,1,3,29,59,0.9999,tz=z3).show(3) }

  // Julian calendar
  check("julian1", "1900-02-29T00:00:00Z JC"){ time(1900,3,0,cal=cal-julian).show }
}


pub fun test() {
  val ts-utc = ts-utc-load()
  with fun utc() { ts-utc }
  println( "tz-local(): " + now().time(tz=tz-local()).show )
  val tzs = //try(timezones-empty){
            load-timezones(verbose=True) //}
  println( "US/Pacific: " + now().time(tz=tzs.timezone("US/Pacific")).show )
  val zlocal = tzs.guess-local-timezone()
  println( "gues local: " + now().time(tz=zlocal).show + " (" + zlocal.name + ")")
  test-tzs(tzs)
}

pub fun main() {
  //val ts-utc = ts-utc-load()
  //with fun utc() { ts-utc }
  test-tzs(try-default(timezones-empty){ load-timezones() })
}
