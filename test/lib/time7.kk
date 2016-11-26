/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

// Sample data from the book Calendrical Calculations, appendix C, 30 dates in various calendars.
public module time7

public import std/num/fixed
public import std/xtime

public struct info(
  jd    : double,
  mjd   : int,
  wday  : weekday, 
  greg  : string,
  isoweek : string,
  isomonth: string,
  julian: string,
  coptic: string,
  ethiopian: string
)

val infos = [
  Info( 1507231.5,~892769,Sun,"-00586-07-24","-00586-W29-7","-00586-M07-21","-00587-07-30","-00870-12-06","-00594-12-06"  ),
  Info( 1660037.5,~739963,Wed,"-00168-12-05","-00168-W49-3","-00168-M12-06","-00169-12-08","-00451-04-12","-00175-04-12"),
  Info( 1746893.5,~653107,  Wed,  "0070-09-24", "0070-W39-3", "0070-M09-27",  "0070-09-26","-00213-01-29","0063-01-29" ),
  Info( 1770641.5,~629359,  Sun,  "0135-10-02", "0135-W39-7", "0135-M09-31",  "0135-10-03","-00148-02-05","0128-02-05" ), 
  Info( 1892731.5,~507269,  Wed,  "0470-01-08", "0470-W02-3", "0470-M01-10",  "0470-01-07", "0186-05-12", "0462-05-12" ),
  Info( 1931579.5,~468421,  Mon,  "0576-05-20", "0576-W21-1", "0576-M05-20",  "0576-05-18", "0292-09-23", "0568-09-23" ), 
  Info( 1974851.5,~425149,  Sat,  "0694-11-10", "0694-W45-6", "0694-M11-11",  "0694-11-07", "0411-03-11", "0687-03-11" ),
  Info( 2091164.5,~308836,  Sun,  "1013-04-25", "1013-W16-7", "1013-M04-21",  "1013-04-19", "0729-08-24", "1005-08-24" ),
  Info( 2121509.5,~278491,  Sun,  "1096-05-24", "1096-W21-7", "1096-M05-26",  "1096-05-18", "0812-09-23", "1088-09-23" ),
  Info( 2155779.5,~244221,  Fri,  "1190-03-23", "1190-W12-5", "1190-M03-22",  "1190-03-16", "0906-07-20", "1182-07-20" ),
  Info( 2174029.5,~225971,  Sat,  "1240-03-10", "1240-W10-6", "1240-M03-09",  "1240-03-03", "0956-07-07", "1232-07-07" ),
  Info( 2191584.5,~208416,  Fri,  "1288-04-02", "1288-W14-5", "1288-M04-05",  "1288-03-26", "1004-07-30", "1280-07-30" ),
  Info( 2195261.5,~204739,  Sun,  "1298-04-27", "1298-W17-7", "1298-M04-28",  "1298-04-20", "1014-08-25", "1290-08-25" ),
  Info( 2229274.5,~170726,  Sun,  "1391-06-12", "1391-W23-7", "1391-M06-10",  "1391-06-04", "1107-10-10", "1383-10-10" ),
  Info( 2245580.5,~154420,  Wed,  "1436-02-03", "1436-W05-3", "1436-M02-01",  "1436-01-25", "1152-05-29", "1428-05-29" ),
  Info( 2266100.5,~133900,  Sat,  "1492-04-09", "1492-W14-6", "1492-M04-06",  "1492-03-31", "1208-08-05", "1484-08-05" ),  

  Info( 2430266.5,  30266,  Mon,  "1941-09-29", "1941-W40-1", "1941-M10-01",  "1941-09-16", "1658-01-19", "1934-01-19" ),
  Info( 2430833.5,  30833,  Mon,  "1943-04-19", "1943-W16-1", "1943-M04-15",  "1943-04-06", "1659-08-11", "1935-08-11" ),
  Info( 2431004.5,  31004,  Thu,  "1943-10-07", "1943-W40-4", "1943-M10-04",  "1943-09-24", "1660-01-26", "1936-01-26" ),
  Info( 2448698.5,  48698,  Tue,  "1992-03-17", "1992-W12-2", "1992-M03-19",  "1992-03-04", "1708-07-08", "1984-07-08" ),
  Info( 2450138.5,  50138,  Sun,  "1996-02-25", "1996-W08-7", "1996-M02-26",  "1996-02-12", "1712-06-17", "1988-06-17" ),
  Info( 2465737.5,  65737,  Wed,  "2038-11-10", "2038-W45-3", "2038-M11-08",  "2038-10-28", "1755-03-01", "2031-03-01" ),
  Info( 2486076.5,  86076,  Sun,  "2094-07-18", "2094-W28-7", "2094-M07-14",  "2094-07-05", "1810-11-11", "2086-11-11" )
]

fun show( info : info ) : string {
  [info.jd.show,info.mjd.show,info.wday.show,info.greg,
   info.isoweek,info.isomonth,
   info.julian,info.coptic,info.ethiopian
  ].join(", ")
}


/*----------------------------------------------------------------------------
  Testing
----------------------------------------------------------------------------*/

fun check(name : string, res : string, tst : () -> io string ) : io () {
  val got = tst()
  println(name.pad-right(14,' ') + ": " 
    + (if (got == res) then "ok: " + res 
                       else "FAILED!:\n expect: " + res + "\n gotten: " + got + "\n"))
  if (got != res) error("test failed")
}

fun check-info( info : info ) : io () {
  println("\n" + info.show)
  val i = instant-at-jd(info.jd,ts-utc)
  val t = i.time
  check("mjd", info.mjd.show){ i.mjd(ts-utc).show }
  check("wday", info.wday.show){ t.weekday.show }
  check("greg", info.greg){ t.date.show }
  check("weekdate", info.isoweek ){ t.weekdate.show }
  check("isoweek", info.isoweek ){ t.time(cal=cal-iso-week).date.show("W",1) }
  check("isomonth", info.isomonth ){ t.time(cal=cal-iso-month).date.show("M") }
  check("julian", info.julian){ t.time(cal=cal-julian).date.show }
  check("coptic", info.coptic){ t.time(cal=cal-coptic).date.show }
  check("ethiopian", info.ethiopian){ t.time(cal=cal-ethiopian).date.show }
  // todo: test more conversions between the dates
}

fun test-infos() {
  infos.foreach(check-info)
}

fun main() {
  test-infos()
}

