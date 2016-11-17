/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

module time4

public import std/xtime

/*----------------------------------------------------------------------------
  Testing
----------------------------------------------------------------------------*/


fun check(name : string, res : string, tst : () -> string ) : io () {
  val got = tst()
  println(name + ": " + (if (got == res) then "ok: " + res else "failed!:\n expect: " + res + "\n gotten: " + got))
}

fun test-week-dates() {
  val dates = [
      (Date(2005,01,01),Weekdate(2004,53,6)),
      (Date(2005,01,02),Weekdate(2004,53,7)),
      (Date(2005,12,31),Weekdate(2005,52,6)),
      (Date(2007,01,01),Weekdate(2007,1,1)),
      (Date(2007,12,30),Weekdate(2007,52,7)),
      (Date(2007,12,31),Weekdate(2008,1,1)),
      (Date(2008,01,01),Weekdate(2008,1,2)),
      (Date(2008,12,28),Weekdate(2008,52,7)),
      (Date(2008,12,29),Weekdate(2009,1,1)),
      (Date(2008,12,30),Weekdate(2009,1,2)),
      (Date(2008,12,31),Weekdate(2009,1,3)),
      (Date(2009,01,01),Weekdate(2009,1,4)),
      (Date(2009,12,31),Weekdate(2009,53,4)),
      (Date(2010,01,01),Weekdate(2009,53,5)),
      (Date(2010,01,02),Weekdate(2009,53,6)),
      (Date(2010,01,03),Weekdate(2009,53,7))]
  dates.foreach( fun(dwd) {
    val (d,wd) = dwd
    val ds = d.show
    val wds = wd.show
    check("date-" + ds, wds) { weekdate(d).show }
    check("date-" + wds, ds)   { date(wd).show }
  })    
}

public fun main() {
  test-week-dates()
}