/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

module time1

public import std/num/fixed
public import std/time
public import std/time/timezone

/*----------------------------------------------------------------------------
  Testing
----------------------------------------------------------------------------*/


fun check(name : string, tst : () -> ndet bool ) : io () {
  println(name + ": " + (if (tst()) then "ok" else "failed!"))
}

fun test-tz() {
  println( "tz-local(): " + now().time(tz=tz-local()).show )
  val tzs = load-timezones()
  println( "US/Pacific: " + now().time(tz=tzs.timezone("US/Pacific")).show )
  val zlocal = tzs.guess-local-timezone()
  println( "gues local: " + now().time(tz=zlocal).show + " (" + zlocal.name + ")")

  val z1  = tzs.timezone("America/New_York")
  val z2  = tzs.timezone("Asia/Tehran")
  val t1  = instant(1978,12,30,12,1,0,tz=z1) + duration(14*3600 + 44*60)
  check("tz1"){ t1.time(tz=z2).show == "1978-12-31T11:45:00+04:00 (IRST)"}
  val t2  = instant(1978,12,31,12,1,0,tz=z1) + duration(14*3600 + 44*60) // across DST & leap second
  check("tz2"){ t2.time(tz=z2).show == "1979-01-01T11:14:59+03:30 (IRST)"}

  check("tz3a"){ time(2011,3,13,1,15,tz=z1).show == "2011-03-13T01:15:00-05:00 (EST)" }
  check("tz3b"){ time(2011,3,13,2,15,tz=z1).show == "2011-03-13T01:15:00-05:00 (EST)" } // non-existent: repeat previous hour for non-existent time!
  check("tz3c"){ time(2011,3,13,3,15,tz=z1).show == "2011-03-13T03:15:00-04:00 (EDT)" }
  
  check("tz4a"){ time(2011,11,6,0,15,tz=z1).show == "2011-11-06T00:15:00-04:00 (EDT)" }
  check("tz4b"){ time(2011,11,6,1,15,tz=z1).show == "2011-11-06T01:15:00-04:00 (EDT)" } // ambigious: choose earliest
  check("tz4c"){ time(2011,11,6,2,15,tz=z1).show == "2011-11-06T02:15:00-05:00 (EST)" }
  check("tz4d") { instant(2011,11,6,5,15).time(tz=z1).show == "2011-11-06T01:15:00-04:00 (EDT)" }
  check("tz4e") { instant(2011,11,6,6,15).time(tz=z1).show == "2011-11-06T01:15:00-05:00 (EST)" }

  // leap second in other time zone
  check("tzleap1"){ instant(2017,1,1,0,0,36,0.5,cal=cal-tai).time(tz=tzs.timezone("US/Pacific")).show == "2016-12-31T15:59:60.500-08:00 (PST)" }
  check("tzleap2"){ instant(1979,1,1,0,0,17,0.5,cal=cal-tai).time(tz=z2).show == "1979-01-01T03:29:60.500+03:30 (IRST)" }
  check("tzleap3"){ instant(1979,1,1,3,29,59,tz=z2).time.show == "1978-12-31T23:59:59Z" }
  
  // round into a time zone skip
  //println( time(2011,3,13,2,59,59,0.9999,tz=z1).show )
  check("tzround1"){ time(2011,3,13,2,59,59,0.9999,tz=z1).show == "2011-03-13T01:59:59.999900-05:00 (EST)" }
  check("tzround2"){ time(2011,3,13,2,59,59,0.9999,tz=z1).show(3) == "2011-03-13T03:00:00-04:00 (EDT)" }

  // round into a timezone leap second (across a DST zone)
  check("tzroundleap1") { instant(1979,1,1,3,29,59,0.9999,tz=z2).time.show(3) == "1978-12-31T23:59:60Z" }
  check("tzroundleap2") { time(1979,1,1,3,29,59,0.9999,tz=z2).show(3) == "1979-01-01T03:29:60+03:30 (IRST)" }

  // Julian calendar
  check("julian1") { time(1900,3,0,cal=cal-julian).show == "1900-02-29T00:00:00Z JC" }
}



public fun main() {
  test-tz()
}