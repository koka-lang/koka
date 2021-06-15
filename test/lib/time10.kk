/*----------------------------------------------------------------------------
   Copyright 2012-2021, Microsoft Research, Daan Leijen

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/

// Test UTC-SLS
public module time10

import std/time/timestamp
public import std/num/ddouble
public import std/time

/*----------------------------------------------------------------------------
  Testing
----------------------------------------------------------------------------*/

fun check(name : string, res : string, tst : () -> <io,utc> string ) : <io,utc> () {
  val got = tst()
  println(name.pad-right(14,' ') ++ ": "
    ++ (if (got == res) then "ok: " ++ res
                       else "FAILED!:\n expect: " ++ res ++ "\n gotten: " ++ got ++ "\n"))
}

val leap-seconds = "3786825600  36  # 1 Jan 2020" //testing negative leap second
val leaps = parse-leap-seconds(leap-seconds).extend(leaps-table-ti)
val ts-slsx = ts-utc-sls-create(leaps)
val ts-utcx = ts-utc-create(leaps)

fun test-utc-neg() {
  check("leap-neg1", "2020-01-01T00:00:00Z") { instant(2020,1,1,0,0,36,ts=ts-tai).time(ts=ts-utcx).show }
  check("leap-neg2", "2019-12-31T23:59:58Z") { instant(2020,1,1,0,0,35,ts=ts-tai).time(ts=ts-utcx).show }
  check("leap-neg3", "2020-01-01T00:00:34Z TAI") { instant(2019,12,31,23,59,57,ts=ts-utcx).time(ts=ts-tai).show }
  check("leap-neg4", "2020-01-01T00:00:35Z TAI") { instant(2019,12,31,23,59,58,ts=ts-utcx).time(ts=ts-tai).show }
  check("leap-neg5", "2020-01-01T00:00:36Z TAI") { instant(2019,12,31,23,59,59,ts=ts-utcx).time(ts=ts-tai).show}
  check("leap-neg6", "2020-01-01T00:00:36Z TAI") { instant(2020,1,1,ts=ts-utcx).time(ts=ts-tai).show }
}

fun test-sls() {
  check("utc-sls1","2016-12-31T23:43:20Z"){  ts-ti-sls.instant(2017,1,0,23,43,20).time-utc().show  }
  check("utc-sls2","2016-12-31T23:43:22.001001001Z"){  instant(2017,1,0,23,43,22,ts=ts-ti-sls).time(ts=utc()).show  }
  check("utc-sls3","2016-12-31T23:59:60.989989990Z"){  instant(2017,1,0,23,59,59,0.99,ts=ts-ti-sls).time(ts=utc()).show  }
  check("utc-sls4","2016-12-31T23:43:20Z TI-SLS"){  instant(2017,1,0,23,43,20).time(ts=ts-ti-sls).show  }
  check("utc-sls5","2016-12-31T23:43:21Z TI-SLS"){  instant(2017,1,0,23,43,21).time(ts=ts-ti-sls).show  }
  check("utc-sls6","2016-12-31T23:43:21.999Z TI-SLS"){  instant(2017,1,0,23,43,22).time(ts=ts-ti-sls).show  }
  check("utc-sls7","2016-12-31T23:59:59.900100Z TI-SLS"){  instant(2017,1,0,23,59,60,0.9).time(ts=ts-ti-sls).show  }

  check("sls-neg2", "2019-12-31T23:43:19Z UTC-SLS") { instant(2020,1,0,23,43,19,ts=ts-utcx).time(ts=ts-slsx).show }
  check("sls-neg1", "2019-12-31T23:43:20.001Z UTC-SLS") { instant(2020,1,0,23,43,20,ts=ts-utcx).time(ts=ts-slsx).show }
  check("sls-neg4", "2019-12-31T23:43:21.002Z UTC-SLS") { instant(2020,1,0,23,43,21,ts=ts-utcx).time(ts=ts-slsx).show }
  check("sls-neg3", "2019-12-31T23:59:59.899900Z UTC-SLS") { instant(2020,1,0,23,59,58,0.9,ts=ts-utcx).time(ts=ts-slsx).show }
  check("sls-neg5", "2019-12-31T23:59:58.900099900Z") { instant(2020,1,0,23,59,59,0.9,ts=ts-slsx).time(ts=ts-utcx).show}
  check("sls-neg6", "2019-12-31T23:43:19.999000999Z"){ instant(2020,1,0,23,43,20,ts=ts-slsx).time(ts=ts-utcx).show}
  check("sls-neg7", "2019-12-31T23:43:19Z") { instant(2020,1,0,23,43,19,ts=ts-slsx).time(ts=ts-utcx).show }
}

public fun main() {
  //with fun utc() { ts-utc-load() }
  test-sls()
  test-utc-neg()
}
