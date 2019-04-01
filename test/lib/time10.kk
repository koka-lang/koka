/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

// Test UTC-SLS
public module time10

public import std/num/ddouble
public import std/time

/*----------------------------------------------------------------------------
  Testing
----------------------------------------------------------------------------*/

fun check(name : string, res : string, tst : () -> io string ) : io () {
  val got = tst()
  println(name.pad-right(14,' ') + ": "
    + (if (got == res) then "ok: " + res
                       else "FAILED!:\n expect: " + res + "\n gotten: " + got + "\n"))
}

val leap-seconds = "3786825600  36  # 1 Jan 2020" //testing negative 2 seconds
val leaps = parse-leap-seconds(leap-seconds).extend(leaps-table-ti)
val cal-slsx = cal-utc-sls-create(leaps)
val cal-utcx = cal-utc-create(leaps)

fun test-utc-neg() {
  check("leap-neg1", "2020-01-01T00:00:00Z") { instant(2020,1,1,0,0,36,cal=cal-tai).time(cal=cal-utcx).show }
  check("leap-neg2", "2019-12-31T23:59:58Z") { instant(2020,1,1,0,0,35,cal=cal-tai).time(cal=cal-utcx).show }
  check("leap-neg3", "2020-01-01T00:00:34Z TAI") { instant(2019,12,31,23,59,57,cal=cal-utcx).time(cal=cal-tai).show }
  check("leap-neg4", "2020-01-01T00:00:35Z TAI") { instant(2019,12,31,23,59,58,cal=cal-utcx).time(cal=cal-tai).show }
  check("leap-neg5", "2020-01-01T00:00:36Z TAI") { instant(2019,12,31,23,59,59,cal=cal-utcx).time(cal=cal-tai).show}
  check("leap-neg6", "2020-01-01T00:00:36Z TAI") { instant(2020,1,1,cal=cal-utcx).time(cal=cal-tai).show }
}

fun test-sls() {
  check("utc-sls1","2016-12-31T23:43:20Z"){  instant(2017,1,0,23,43,20,cal=cal-iso-sls).time.show  }
  check("utc-sls2","2016-12-31T23:43:22.001001001Z"){  instant(2017,1,0,23,43,22,cal=cal-iso-sls).time.show  }
  check("utc-sls3","2016-12-31T23:59:60.989989990Z"){  instant(2017,1,0,23,59,59,0.99,cal=cal-iso-sls).time.show  }
  check("utc-sls4","2016-12-31T23:43:20Z SLS"){  instant(2017,1,0,23,43,20).time(cal=cal-iso-sls).show  }
  check("utc-sls5","2016-12-31T23:43:21Z SLS"){  instant(2017,1,0,23,43,21).time(cal=cal-iso-sls).show  }
  check("utc-sls6","2016-12-31T23:43:21.999Z SLS"){  instant(2017,1,0,23,43,22).time(cal=cal-iso-sls).show  }
  check("utc-sls7","2016-12-31T23:59:59.900100Z SLS"){  instant(2017,1,0,23,59,60,0.9).time(cal=cal-iso-sls).show  }

  check("sls-neg2", "2019-12-31T23:43:19Z SLS") { instant(2020,1,0,23,43,19,cal=cal-utcx).time(cal=cal-slsx).show }
  check("sls-neg1", "2019-12-31T23:43:20.001Z SLS") { instant(2020,1,0,23,43,20,cal=cal-utcx).time(cal=cal-slsx).show }
  check("sls-neg4", "2019-12-31T23:43:21.002Z SLS") { instant(2020,1,0,23,43,21,cal=cal-utcx).time(cal=cal-slsx).show }
  check("sls-neg3", "2019-12-31T23:59:59.899900Z SLS") { instant(2020,1,0,23,59,58,0.9,cal=cal-utcx).time(cal=cal-slsx).show }
  check("sls-neg5", "2019-12-31T23:59:58.900099900Z") { instant(2020,1,0,23,59,59,0.9,cal=cal-slsx).time(cal=cal-utcx).show}
  check("sls-neg6", "2019-12-31T23:43:19.999000999Z"){ instant(2020,1,0,23,43,20,cal=cal-slsx).time(cal=cal-utcx).show}
  check("sls-neg7", "2019-12-31T23:43:19Z") { instant(2020,1,0,23,43,19,cal=cal-slsx).time(cal=cal-utcx).show }
}

public fun main() {
  test-sls()
  test-utc-neg()
}
