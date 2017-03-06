/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

// Test all calendars across many days
public module time9

public import std/num/ddouble
public import std/time


// ----------------------------------------------------
// Make it easy to work with days since epoch
// ----------------------------------------------------

val mjd-y2000 = 51544

fun date-at-days( days : int, cal : calendar ) : date {
  instant-at-mjd( ddouble(days + mjd-y2000), ts-ti ).time(cal=cal).date
}

fun days-at-date( date : date, cal : calendar ) : int {
  time(date, cal=cal).mjd.int - mjd-y2000
}


/*----------------------------------------------------------------------------
  Testing
----------------------------------------------------------------------------*/
fun check( msg : string, expect : string, result : string ) : io () {
  if (expect==result) then () // println( msg + ": ok.") 
  else {
    println(msg + ": FAILED:\n expect : " + expect + "\n result: " + result )
    error("failure")
  }
}


fun test-day-cal( day : int, cal : calendar ) : io date {
  val date1 = date-at-days( day, cal )
  val days1 = days-at-date( date1, cal )
  val date2 = date-at-days( days1, cal )
  val days2 = days-at-date( date2, cal )
  check( "inverse days1: " + cal.name.show + ", " + date1.show, day.show, days1.show )
  check( "inverse days2: " + day.show, day.show, days2.show )
  check( "inverse date: " + date1.show, date1.show, date2.show )
  date1
}

fun test-day( day : int ) {
  val idate = test-day-cal( day, cal-iso )
  //println(" iso date: " + idate.show + ", " + xidate.show )
  val wdate  = test-day-cal( day, cal-iso-week )
  val mdate  = test-day-cal( day, cal-iso-month )
  check( "equal iso years: " + mdate.year.show, mdate.year.show, wdate.year.show)
  val jdate  = test-day-cal( day, cal-julian )
  val cdate = test-day-cal(day, cal-coptic )
  val edate  = test-day-cal( day, cal-ethiopian )
  ()
}

fun test( years : int = 500 ) {
  for-days( ~years, years ) fun(day){
    test-day(day)    
  }
  println("ok.")
}

fun for-days( ylo : int, yhi : int, action : int -> io () ) : io () {
  println("days in years: " + ylo.show + " up to " + yhi.show)
  for(ylo,yhi) fun(y) {
    val base = y*366 - 730119  // test round the year zero 
    println("year: " + time(2000,1,base).year.show)
    for(0,365) fun(day) { action(base + day)}
  }
}


fun main() {
  test(50)
}