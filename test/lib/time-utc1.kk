/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

module time-utc1

import std/os/file
public import std/num/decimal
public import std/time

/*----------------------------------------------------------------------------
  Testing
----------------------------------------------------------------------------*/


fun check(name : string, tst : () -> ndet bool ) : io () {
  println(name + ": " + (if (tst()) then "ok" else "failed!"))
}


public fun show-leap-steps() {
  val steps = get-leap-steps()
  var dlast := Date(0,0,0)  
  val rows = steps.map( fun(step) {
    val (ts,ofs1,ofs2,(base,dstart,delta)) = step
    val prec = 7
    val diff = (ofs2 - ofs1).round-to-prec(prec)
    val t = unsafe-instant(ts,ts-utc).time.round-to-prec(0)
    val c = if (t.clock.hours != 0) then ", " + t.clock.hours.show + ":" + t.clock.minutes.show.pad-left(2,'0') else ""
    val dds = unsafe-instant(dstart,ts-utc).time.date
    val ds  = if (dds==dlast) then "&quad;&quad;\"&quad;&quad;" else dds.show
    dlast := dds
    ( "| " + [
      (t.date.show + c).pad-right(17,' '),
      ((if (diff.neg?) then "" else "+") + diff.show(6)).pad-right(24,' '),
      ofs1.show(prec).pad-right(14,' '),
      ofs2.show(prec).pad-right(14,' '),
      (if (delta.zero?) then "" else 
          base.show(6) + "s + " + (if (dds.year >= 1959) then delta.show(7) else delta.show(8)) + "&times;&Delta;(" 
        + ds + ")").pad-right(47,' '),
      (if (delta.zero?) then "" else 
           "-" + ((delta.double / 86400.0)*1.0e10).int.show + "&times;10^-10^" ).pad-right(10,' ') 
    ].join(" | ") + " |" 
     + (if (t.date==Date(1972,1,1) || t.date==Date(1960,1,1)) then leap-steps-sep else "")
     )
  })
  println(rows.take(30).join("\n"))
  val table = leap-steps-header + rows.join("\n") + leap-steps-footer + "\n"
  write-text-file-sync("leap-steps.txt", table)
}

val leap-steps-header = @"
|-------------------|--------------------------|----------------|----------------|-------------------------------------------------|-----------|
| Date (UTC) &quad; | Time steps &quad; &quad; | (TAI--UTC)     | (TAI--UTC)     | Drift                                           | &quad; Frequency |
|                   |                          | Just before\ \ | On the date&quad; |                                                 | Offset    |
+:------------------|--------------------------|:---------------|:---------------|-------------------------------------------------|----------:+
"
val leap-steps-footer = @"
|-------------------|--------------------------|----------------|----------------|-------------------------------------------------|-----------|
{white-space:nowrap; col-2-padding-left:1em; .sans-serif; }
"

val leap-steps-sep = @"
| \                 |                          |                |                   |                                                                 |                    |"


public fun test() {
  show-leap-steps()
}

public fun main() {
  ()
}