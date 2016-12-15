/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

public module isomonth

public import std/num/fixed
public import std/xtime

fun xdate-of( days : int ) : date {
  val (year,doy) = xdoy-of(days)
  
}

// days since 0000-01-01
fun xdoy-of( days : int ) : (int,int) {
  val (era,doe)    = divmod(days,146097)
  val (approx,rem) = divmod(doe*100,36525) // (doe * 400) / 146097
  val start     = if (rem < 36225) then doe+1 else xbefore-year(approx.inc)    
  val (yoe,doy) = if (doe < start) then (approx, doe - xbefore-year(approx))    
                                   else (approx.inc, doe - start)
  // trace("doe: " + doe.show + ", approx: " + approx.show + ", start: " + start.show )
  (400*era + yoe, doy)
}


// Return the days before Gregorian year `gyear`.
fun xbefore-year(gyear : int ) : int {
  365*gyear + xleapdays-upto(gyear) + 1
}

// Number of leap days before a Gregorian year.
fun xleapdays-upto(gyear : int) : int {
  (gyear.dec/4 - gyear.dec/100) + gyear.dec/400
}


fun test-gdays(years : int = 400) {
  for-days(years) fun(day) {
    val d = instant-at-mjd( fixed((day - gmjd-epoch-shift) - 366), ts-utc ).time.date
    //xcheck("gday " + day.show + ", " + d.show, d.year.show) { gyear-of(day).show }
    val (y,_doy) = xdoy-of(day)
    xcheck("xday " + day.show + ", " + d.show, d.year.show ) { y.show }
    ()
  }
}

fun xdate-of-day( day : int ) : date {
  instant-at-mjd( fixed(day - gmjd-epoch-shift), ts-utc).time.date
}

val gmjd-epoch-shift = 678575

fun gyear-of-mjd( mjd : int ) : int {
  gyear-of( mjd + gmjd-epoch-shift )
}




//--------------------------------------------------------------
// ISO week calendar: date to days
//--------------------------------------------------------------

val isow-epoch-shift = 730219 // to Monday, 0001-01-01  

// Convert a `:date` in the ISO week calendar (the weeks are the `month`s of the year) to days since the epoch.
fun isow-date-to-days(d : date ) : int {
  isow-doy-to-days(d.year, 7*d.month.dec + d.day.dec)
}

fun isow-doy-to-days( year : int, doy : int ) : int {
  (isow-before-year(year) + doy) - isow-epoch-shift
}

// Returns the days before `year` from epoch 0001-01-01
fun isow-before-year(year : int) : int {
  val gdays   = gbefore-year(year)
  val weekday = gdays%7  // monday=0, sunday=6
  val adjust  = if (weekday <= 3) then (0 - weekday) else (7 - weekday)
  gdays + adjust
}

// Return the days before Gregorian year `gyear`.
fun gbefore-year(gyear : int ) : int {
  365*(gyear - 1) + gleapdays-before(gyear) 
}

// Number of leap days before a Gregorian year.
fun gleapdays-before(gyear : int) : int {
 ((gyear - 1)/4 - (gyear - 1)/100) + (gyear - 1)/400
}


//--------------------------------------------------------------
// ISO week calendar: days to date
//--------------------------------------------------------------

// Convert days since epoch to an ISO week date.
fun isow-days-to-date(days : int) : date {
  val (year,doy) = isow-days-to-doy(days)
  val (week,day) = divmod(doy,7)
  Date(year,week.inc,day.inc)
}

// Convert days since epoch to an ISO year and day of year.
fun isow-days-to-doy( days:int ) : (int,int) {
  val edays   = days + isow-epoch-shift
  val approx  = gyear-of(edays - 3) // -3 so we are at most one year early
  val doy     = edays - isow-before-year(approx)
  if (doy < 364 || doy < isow-days-in-year(approx))
   then (approx,doy)
   else (approx.inc, edays - isow-before-year(approx.inc))
}


// Return the number of days in an ISO week/month year.
fun isow-days-in-year( year : int ) : int {
  fun weekday(y:int) { 
    (y + gleapdays-before(y)).dec % 7     // The first weekday of the year `y` (0001-01-01 is a Monday)
  }
  if (weekday(year)==3 || weekday(year+1)==4) // `year` starts or ends on a Thursday
   then 371 else 364
}

// Return the Gregorian year that contains `days` after epoch (0001-01-01)
fun gyear-of(days : int ) : int {
  1 + (days - gleapdays-of(days))/365
}

// Return the Gregorian leap days that are contained in `days` after epoch.
fun gleapdays-of(days : int) : int {
  val ldays = days + 306   // since 0000-03-01, so all leap days are the same period away
  (ldays/1460 - ldays/36524) + ldays/146096
}

//--------------------------------------------------------------
//  ISO month calendar
//--------------------------------------------------------------

// Convert an ISO month date to days since epoch. Built upon the ISO week calendar.
fun isom-date-to-days(d : date) : int {
  val (xyear,xmonth) = divmod(d.month.dec,12) // roll over months
  val doy = isom-before-month(xmonth) + d.day.dec
  isow-doy-to-days(d.year + xyear, doy)
}

// Convert days since epoch to an ISO month date. Built upon the ISO week calendar.
fun isom-days-to-date(days:int) : date {
  val (year,doy) = isow-days-to-doy(days)
  val month = isom-month-of( doy )
  val day   = doy - isom-before-month(month)
  Date(year,month.inc,day.inc)
}

// return the days before zero-based month `m`
fun isom-before-month( month : int ) : int {
  30*month + month/3
}

// given a zero based day of the year, returns its zero based month
fun isom-month-of( doy : int ) : int {
  val m = (100*doy.inc)/3034   // (doy - (doy+1)/91)/30
  min(m,11)
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
  if (res==expect) then () // println(msg + ": " + expect + ": ok.") 
  else {
    println(msg + ": FAILED!!!\n expect: " + expect + "\n got   : " + res)
    error("failed")
  }
}


fun verify-id1w(years=400) {
  for-weekdates(years) fun(wd : weekdate) {
    xcheck("isow-date", wd.show ) { wd.date.isow-date-to-days.isow-days-to-date.weekdate.show }
  }
}

fun verify-id1m(years=400) {
  for-monthdates(years) fun(date) {
    xcheck("isom-date", date.show ) { date.isom-date-to-days.isom-days-to-date.show }
  }
}

fun verify-id2w(years=400) {
  for-days(years) fun(days) {
    val d = days.isow-days-to-date
    xcheck("isw-days: " + d.weekdate.show, days.show) { d.isow-date-to-days.show }
  }
}

fun verify-id2m(years=400) {
  for-days(years) fun(days) {
    val d = days.isom-days-to-date
    xcheck("isom-days: " + d.show, days.show) { d.isom-date-to-days.show }
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
  for(~years,years) fun(y) {
    val base = y*366
    for(0,365) fun(day) { action(base + day) }
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