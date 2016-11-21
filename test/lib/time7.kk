/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

// Sample data from the book Calendrical Calculations, appendix C.
public module time7

public import std/num/fixed
public import std/xtime

public struct info(
  jd    : fixed,
  mjd   : fixed,
  wday  : weekday, 
  greg  : string,
  wdate : string,
  julian: string                    
)

val infos = [
  Info( 1507231.5.fixed,  ~892769.fixed,Sun,  "-00586-07-24", "-00586-W29-7", "-00587-07-30"),
  Info( 2430266.5.fixed,   30266.fixed, Mon,  "1941-09-29",   "1941-W40-1",   "1941-09-16"  ),
  Info( 2486076.5.fixed,  86076.fixed,  Sun,  "2094-07-18",   "2094-W28-7",   "2094-07-05"  )
]

fun show( info : info ) : string {
  [info.jd.show,info.mjd.show,info.wday.show,info.greg,info.wdate,info.julian].join(", ")
}


/*----------------------------------------------------------------------------
  Testing
----------------------------------------------------------------------------*/

fun check(name : string, res : string, tst : () -> io string ) : io () {
  val got = tst()
  println(name.pad-right(14,' ') + ": " 
    + (if (got == res) then "ok: " + res 
                       else "FAILED!:\n expect: " + res + "\n gotten: " + got + "\n"))
}

fun check-info( info : info ) : io () {
  println("\n" + info.show)
  val i = instant-at-jd(info.jd,ts-utc)
  val t = i.time
  check("mjd", info.mjd.show){ i.mjd(ts-utc).show }
  check("wday", info.wday.show){ t.weekday.show }
  check("greg", info.greg){ t.date.show }
  check("weekdate", info.wdate ){ t.weekdate.show }
  check("julian", info.julian){ t.time(cal=cal-julian).date.show }
  // todo: test more conversions between the dates
}

fun test-infos() {
  infos.foreach(check-info)
}

fun main() {
  test-infos()
}

