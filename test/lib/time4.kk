/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

module time4

public import std/fixed
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
      (Date(2005,01,01),"2004-W53-6"),
      (Date(2005,01,02),"2004-W53-7"),
      (Date(2005,12,31),"2005-W52-6"),
      (Date(2007,01,01),"2007-W01-1"),
      (Date(2007,12,30),"2007-W52-7"),
      (Date(2007,12,31),"2008-W01-1"),
      (Date(2008,01,01),"2008-W01-2"),
      (Date(2008,12,28),"2008-W52-7"),
      (Date(2008,12,29),"2009-W01-1"),
      (Date(2008,12,30),"2009-W01-2"),
      (Date(2008,12,31),"2009-W01-3"),
      (Date(2009,01,01),"2009-W01-4"),
      (Date(2009,12,31),"2009-W53-4"),
      (Date(2010,01,01),"2009-W53-5"),
      (Date(2010,01,02),"2009-W53-6"),
      (Date(2010,01,03),"2009-W53-7")]
  dates.foreach( fun(ds) {
    val (d,res) = ds
    check("date-" + res, res) { weekdate(d).show }
  })    
}

public fun main() {
  test-week-dates()
}