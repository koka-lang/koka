/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

module time1

public import std/fixed
public import std/xtime
public import std/time/timezone

/*----------------------------------------------------------------------------
  Testing
----------------------------------------------------------------------------*/


fun check(name : string, tst : () -> ndet bool ) : io () {
  println(name + ": " + (if (tst()) then "ok" else "failed!"))
}

fun test-tz() {
  println( now().time(tz=timezone("US/Pacific")).show )
  println( now().time(tz=tz-local()).show )
  println( "local timezone guess: " + guess-local-timezone().tz-name )

  val z1  = timezone("America/New_York")
  val z2  = timezone("Asia/Tehran")
  val t1  = instant-at(1978,12,30,12,1,0,tz=z1) + duration(fixed(14*3600 + 44*60))
  check("tz1"){ t1.time(tz=z2).show == "1978-12-31T11:45:00+04:00 (IRST)"}
  val t2  = instant-at(1978,12,31,12,1,0,tz=z1) + duration(fixed(14*3600 + 44*60)) // across DST & leap second
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



public fun main() {
  test-tz()
}