/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

module time6


public import std/time


/*----------------------------------------------------------------------------
  Testing
----------------------------------------------------------------------------*/


fun check(name : string, t1 : time, t2: time, tst : () -> io bool ) : io () {
  if (tst()) then () else {
    println( name + ":failed!\n t1: " + t1.show + "\n t2: " + t2.show )
    error("quit")
  }
}

public fun test-dates(starty : int = 1950, endy : int = 2050, tz : timezone = tz-utc, cal : calendar = cal-iso, print-every : int = 10) {
  var prevt  := time(starty,1,0,tz=tz,cal=cal)
  var prevwd := prevt.weekday
  for(starty,endy) fun(y:int){
    if (y%print-every == 0)  println("tz: " + tz.name + ", cal: " + cal.name + ", year: " + y.show + "...")
    for(1,12) fun(m:int){
      val t1 = time(y,m,tz=tz,cal=cal)
      val dm : int = t1.days-in-month
      for(1,dm) fun(d) {
        val t = time(y,m,d,tz=tz,cal=cal)
        check("larger",t,prevt){ t > prevt }
        check("minday",t,prevt){ t.copy(day=t.day - 1) == prevt }
        check("plusday",t,prevt){ prevt.copy(day=prevt.day + 1) == t }
        val t2 = t.instant.time(tz=tz,cal=cal)
        check("sameday",t,t2){ t.date == t2.date }
        val wd = t.weekday
        check("nextday",t,prevt){ wd == prevwd + 1 }
        check("prevday",t,prevt){ wd - 1 == prevwd }
        prevt := t
        prevwd := wd
      }
    }
  }
}

public fun main() {
  // test-dates(~2000,3000,print-every=100)     // takes too long for regular test
  // test-dates(1450,1650,tz-fixed(1) ,cal-jg)  // fails on the gap start for 'minday' and 'plusday'; and at 1582-10-22 because days-in-month gives 21 for that month!
  test-dates()
  test-dates(tz=tz-fixed(1))
  test-dates(tz=tz-local()) // slowish as javascript getTimeOffset() is slowish
  test-dates(tz=tz-fixed(~1),cal=cal-julian)
  test-dates(tz=tz-fixed(~1),cal=cal-tai)
  test-dates(tz=tz-fixed(1) ,cal=cal-gps)
}

public fun benchmark() {
  print-elapsed(main)
}