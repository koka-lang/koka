/*----------------------------------------------------------------------------
   Copyright 2012-2021, Microsoft Research, Daan Leijen

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/

module time-utc1

import std/os/file
import std/num/ddouble
pub import std/num/decimal
pub import std/time

/*----------------------------------------------------------------------------
  Testing
----------------------------------------------------------------------------*/


fun check(name : string, tst : () -> ndet bool ) : io () {
  println(name + ": " + (if (tst()) then "ok" else "failed!"))
}


pub fun show-leap-steps(ts-utc : timescale) {
  val steps = get-leap-steps()
  var dlast := Date(0,0,0)
  val rows = steps.map( fn(step) {
    val (ts,ofs1,ofs2,(base,dstart,delta)) = step
    val prec = 6
    val diff = (ofs2 - ofs1).round-to-prec(prec)
    val t = ts-utc.instant(ts).time.round-to-prec(0)
    val c = if (t.clock.hours != 0) then ", " + t.clock.hours.show + ":" + t.clock.minutes.show.pad-left(2,'0') else ""
    val dds = ts-utc.instant(dstart).time.date
    val ds  = if (dds==dlast) then "&quad;&quad;\"&quad;&quad;" else dds.show
    dlast := dds
    ( "| " + [
      (t.date.show + c).pad-right(17,' '),
      ((if (diff.neg?) then "" else "+") + (if (diff.round-to-prec(3).zero?) then "0" else diff.show(3))).pad-right(24,' '),
      ofs1.show(prec).pad-right(14,' '),
      ofs2.show(prec).pad-right(14,' '),
      (if (delta.zero?) then "" else
          base.show(6) + "s + " + (if (dds.year >= 1959) then delta.show(7) else delta.show(8)) + "&times;&Delta;("
        + ds + ")").pad-right(47,' '),
      (if (delta.zero?) then "" else
           "-" + ((delta.float64 / 86400.0)*1.0e10).int.show + "&times;10^-10^" ).pad-right(10,' ')
    ].join(" | ") + " |"
     + (if (t.date==Date(1972,1,1) || t.date==Date(1960,1,1)) then leap-steps-sep else "")
     )
  })
  println(rows.take(100).join("\n"))
  val table = leap-steps-header + rows.join("\n") + leap-steps-footer + "\n"
  write-text-file-sync("leap-steps.txt", table)
}

val leap-steps-header = r"
|-------------------|--------------------------|----------------|----------------|-------------------------------------------------|-----------|
| Date (UTC) &quad; | Time steps &quad; &quad; | (TAI--UTC)     | (TAI--UTC)     | Drift                                           | &quad; Frequency |
|                   |                          | Just before\ \ | On the date&quad; |                                                 | Offset    |
+:------------------|--------------------------|:---------------|:---------------|-------------------------------------------------|----------:+
"
val leap-steps-footer = r"
|-------------------|--------------------------|----------------|----------------|-------------------------------------------------|-----------|
{white-space:nowrap; col-2-padding-left:1em; .sans-serif; }
"

val leap-steps-sep = r"
| \                 |                          |                |                   |                                                                 |                    |"


pub fun test() {
  val ts-utc = try-default(ts-ti){ts-utc-load(verbose=True)}
  show-leap-steps(ts-utc)
}

pub fun main() {
  ()
}
