/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

module time4

public import std/num/fixed
public import std/xtime

/*----------------------------------------------------------------------------
  Testing
----------------------------------------------------------------------------*/


fun check(name : string, res : string, tst : () -> string ) : io () {
  val got = tst()
  println(name + ": " + (if (got == res) then "ok: " + res 
                         else "failed!:\n expect: " + res + "\n gotten: " + got))
  if (got!=res) error("FAILED")
}

fun test-week-dates() {
  val dates = [ // Gregorian date, ISO week date, "ISO" month date
      (Date(2005,01,01),Weekdate(2004,53,Sat),Date(2004,12,31+6)),
      (Date(2005,01,02),Weekdate(2004,53,Sun),Date(2004,12,31+7)),
      (Date(2005,12,31),Weekdate(2005,52,Sat),Date(2005,12,30)),
      (Date(2007,01,01),Weekdate(2007,1,Mon),Date(2007,1,1)),
      (Date(2007,12,30),Weekdate(2007,52,Sun),Date(2007,12,31)),
      (Date(2007,12,31),Weekdate(2008,1,Mon),Date(2008,1,1)),
      (Date(2008,01,01),Weekdate(2008,1,Tue),Date(2008,1,2)),
      (Date(2008,12,28),Weekdate(2008,52,Sun),Date(2008,12,31)),
      (Date(2008,12,29),Weekdate(2009,1,Mon),Date(2009,1,1)),
      (Date(2008,12,30),Weekdate(2009,1,Tue),Date(2009,1,2)),
      (Date(2008,12,31),Weekdate(2009,1,Wed),Date(2009,1,3)),
      (Date(2009,01,01),Weekdate(2009,1,Thu),Date(2009,1,4)),
      (Date(2009,12,31),Weekdate(2009,53,Thu),Date(2009,12,35)),
      (Date(2010,01,01),Weekdate(2009,53,Fri),Date(2009,12,36)),
      (Date(2010,01,02),Weekdate(2009,53,Sat),Date(2009,12,37)),
      (Date(2010,01,03),Weekdate(2009,53,Sun),Date(2009,12,38))]
  dates.foreach( fun(dwdmd) {
    val (d,wd,md) = dwdmd
    val ds = d.show
    val wds = wd.show
    val mds = md.show
    check("date-" + ds, wds) { time(d).weekdate.show }
    check("date-" + wds, ds) { time(wd).date.show }
    check("date-isow-" + ds, wds) { 
      val i = instant-at(d)
      val id = i.time(cal=cal-iso-week).date
      (Weekdate(id.year,id.month,id.day.weekday)).show
    }
    check("date-isow-" + wds, ds) { 
      val id = Date(wd.year,wd.week,wd.day.int)
      instant-at(id,cal=cal-iso-week).time.date.show
    }
    check("date-isom-" + mds, mds) {  instant-at(d).time(cal=cal-iso-month).date.show  }
    check("date-isom-" + ds, ds) {  instant-at(md,cal=cal-iso-month).time.date.show  }
    println("")
  })    
}

fun test-week-in-years() {
  val long-years = [
004, 009, 015, 020, 026, 032, 037, 043, 048, 054, 060, 065, 071, 076, 082, 088, 093, 099,
105, 111, 116, 122, 128, 133, 139, 144, 150, 156, 161, 167, 172, 178, 184, 189, 195,
201, 207, 212, 218, 224, 229, 235, 240, 246, 252, 257, 263, 268, 274, 280, 285, 291, 296,
303, 308, 314, 320, 325, 331, 336, 342, 348, 353, 359, 364, 370, 376, 381, 387, 392, 398
  ]
  list(2000,2399).foreach( fun(year) {
    val weeks = if (long-years.find(fun(y) { (2000+y)==year }).bool) then 53 else 52
    check("year-" + year.show, weeks.show) { time(year).weeks-in-year.show }

    check("isow-" + year.show, weeks.show) { 
      val d = instant-at(year,12,28).time(cal=cal-iso-week).date 
      if (d.month==53) then "53" else "52"
    }
 
    check("isom-" + year.show, weeks.show) { 
      val d = instant-at(year,12,28).time(cal=cal-iso-month).date 
      if (d.month==12 && d.day > 31) then "53" else "52"
    }
  })
}


fun test-day-in-year() {
  list(1999,2004).foreach(fun(year) {
    list(1,365).foreach( fun(doy) {
      val d = Date(year,1,doy)
      val t = time(d)
      check("doy-" + t.show, doy.show) { t.day-of-year.show }
      //val tw = time(d,cal=cal-iso-week)
      //check("doy-isow-" + tw.show, doy.show) { (tw.month.dec*7 + tw.day).show }
    })
  })
}



fun test-nth() {
  check("nth1", "2016-10-30") { time(2016,10,10).last-weekday-of-month(Sun).date.show }
  check("nth2", "2016-10-02") { time(2016,10,10).nth-weekday-of-month(1,Sun).date.show }
  check("nth3", "2016-10-09") { time(2016,10,10).nth-weekday-of-month(2,Sun).date.show }
  check("nth4", "2017-11-19") { time(2016,10,10).nth-weekday-of-month(60,Sun).date.show }
  check("nth5", "2016-10-30") { time(2016,11,1).nth-weekday(0,Sun).date.show }
  check("nth6", "2016-10-26") { time(2016,11,16).start-of-month.nth-weekday(0,Wed).date.show }
  check("nth7", "2016-11-13") { time(2016,11,10).nth-weekday(1,Sun).date.show }  
}

public fun main() {
  test-day-in-year()
  test-week-in-years()
  test-week-dates()
  test-nth()
}