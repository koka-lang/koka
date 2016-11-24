/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

public module isomonth

public import std/num/fixed
public import std/xtime


//--------------------------------------------------------------
// month- and weekdate
//--------------------------------------------------------------

fun monthdate( wd : weekdate ) {
  val doy   = days-before-week(wd.week) + wd.day.int
  val month = month-of(doy)
  val day   = doy - days-before-month(month)  
  Date(wd.year,month,day)
} 

fun month-of(doy : int ) : int {
  min( floor(doy.double / slope ).int + 1, 12 )
}

fun days-before-month(month : int ) : int {
  floor(slope*(month - 1).double).int
}

val slope-lo= (91.0/3.0)               // = 30.3333
val slope-hi= (91.0/3.0) + (1.0/32.0)  // = 30.3646
val slope   = 30.34



fun weekdate( d : date ) : weekdate {
  val doy  = days-before-month(d.month) + d.day    
  val week = week-of(doy)
  val wday = doy - days-before-week(week) 
  Weekdate(d.year, week, weekday(wday) )
}

fun weekday-of(doy : int) : int {
  doy - days-before-week(week-of(doy)) 
}

fun week-of(doy : int) : int {
  (doy - 1)/7 + 1
}

fun days-before-week(week : int ) : int {
  7*(week - 1)
}

//--------------------------------------------------------------
//  Date from days
//--------------------------------------------------------------

fun date-of(days : int) : date {
  val approx  = gyear-of(days - 3)
  val year    = if(days >= days-before-year(approx+1)) then approx+1 else approx
  val doy     = (days - days-before-year(year)) + 1
  val month   = month-of(doy)
  val day     = doy - days-before-month(month)
  Date(year,month,day)
}

fun gyear-of(days : int ) : int {
  1 + (days - gleapdays-in(days))/365
}

fun gleapdays-in(days : int) : int {
  val ldays = days + 307   // since 0000-03-01
  (ldays/1460 - ldays/36524) + ldays/146096
}


//--------------------------------------------------------------
//  Days from date
//--------------------------------------------------------------

fun days-before-date(d : date) : int {
  days-before-year(d.year) + days-before-month(d.month) + (d.day - 1) 
}

fun days-before-year(year : int) : int {
  val gdays    = gdays-before-gyear(year)
  val weekday  = weekday-of(gdays+1)
  val adjust   = if (weekday <= 4) then (1 - weekday) else (8 - weekday)
  gdays + adjust
}

fun gdays-before-gyear(gyear : int ) : int {
  365*(gyear - 1) + gleapdays-before(gyear) 
}

fun gleapdays-before(gyear : int) : int {
 ((gyear - 1)/4 - (gyear - 1)/100) + (gyear - 1)/400
}


//--------------------------------------------------------------
// 
//--------------------------------------------------------------

fun islong( year : int ) : bool {
  fun p(y:int) { (((y + y/4) - y/100) + y/400) % 7 }
  (p(year)==4 || p(year.dec)==3) 
}

fun lastweek( year : int ) : int {
  if (islong(year)) then 53 else 52
}

fun lastday(month : int ) : int {
  if (month % 3 == 0) then 31 else 30
}


//--------------------------------------------------------------
// Verification
//--------------------------------------------------------------

fun xcheck( msg : string, expect : string, chk :() -> io string ) : io () {
  val res = chk()
  if (res==expect) then println(msg + ": " + expect + ": ok.") 
  else {
    println(msg + ": FAILED!!!\n expect: " + expect + "\n got   : " + res)
    error("failed")
  }
}

fun verify-id1(years=400) {
  for-weekdates(years) fun(wd) {
    xcheck("weekdate", wd.show ) { weekdate(monthdate(wd)).show }
  }
}

fun verify-id2(years=400) {
  for-monthdates(years) fun(date) {
    xcheck("monthdate", date.show ) { monthdate(weekdate(date)).show }
  }
}

fun verify-id3(years=400) {
  for-monthdates(years) fun(date) {
    xcheck("monthdate", date.show) { date-of(days-before-date(date)).show }
  }
}

fun verify-id4(years=400) {
  for-days(years) fun(days) {
    xcheck("days", days.show) { days-before-date(date-of(days)).show }
  }
}


/*----------------------------------------------------------------------------
  Iterate through dates
----------------------------------------------------------------------------*/

fun for-weekdates(years=400, action : (weekdate) -> io ()) : io () {
  for(~years,years) fun(year) {
    for(1,year.lastweek) fun(week) {
      for(1,7) fun(wday) {
        action(Weekdate(year,week,weekday(wday)))
      }
    }
  }
}


fun for-monthdates(years=400, action : (date) -> io ()) : io () {
  for(~years,years) fun(year) {
    for(1,12) fun(month) {
      for(1,month.lastday) fun(day) {
        action(Date(year,month,day))
      }
    }
  }
}

fun for-days(years=400, action : (int) -> io ()) : io () {
  val n = years*366
  for(~1000,n) fun(days) {
    action(days)
  }
}

/*----------------------------------------------------------------------------
  Util
----------------------------------------------------------------------------*/


fun group-by( xs : list<a>, n : int ) : list<list<a>> {
  match(xs) {
    Nil -> Nil
    _   -> Cons(xs.take(n), group-by(unsafe-decreasing(xs.drop(n)),n))
  }
}

fun show( xs : list<(int,int)> ) : string {
  show( xs.map(fun(kv) { (kv.fst.show.pad-left(3,' '),kv.snd.show.pad-left(3,' ')) }), 7)
}

fun show( xs : list<(string,string)>, grp : int = 1 ) : string {
  xs.group-by(grp).map( fun(ys) {
    ys.map(fun(kv) { kv.fst + "->" + kv.snd }).join(", ")
  }).join(",\n")
}



/*----------------------------------------------------------------------------
  Testing
----------------------------------------------------------------------------*/


fun check(name : string, res : string, tst : () -> string ) : io () {
  val got = tst()
  println(name + ": " + (if (got == res) then "ok: " + res else "failed!:\n expect: " + res + "\n gotten: " + got))
}

fun test-week-dates() {
  val dates = [
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
    check("date-iso-" + ds, wds) { 
      val i = instant-at(d)
      val id = i.time(cal=cal-iso-week).date
      (Weekdate(id.year,id.month,id.day.weekday)).show
    }
    check("date-iso-" + wds, ds) { 
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
    check("iso-" + year.show, weeks.show) { 
      val d = instant-at(year,12,28).time(cal=cal-iso-month).date 
      if (d.month==12 && d.day > 31) then "53" else "52"
    }
  })
}


fun test-day-in-year() {
  list(1999,2004).foreach(fun(year) {
    list(1,365).foreach( fun(doy) {
      val d = Date(year,1,doy)
      check("doy-" + time(d).show, doy.show) { time(d).day-of-year.show }
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