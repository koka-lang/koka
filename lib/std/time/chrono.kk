/*----------------------------------------------------------------------------
   Copyright 2012-2021, Microsoft Research, Daan Leijen.

   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/

/* Get the system time.

*/
module std/time/chrono

import std/time/timestamp
import std/time/duration
import std/time/instant
import std/time/utc

extern import
  c  file "chrono-inline.c"
  cs file "chrono-inline.cs"
  js file "chrono-inline.js"

// The current `:instant` in time as returned by the system clock
// in the UTC timescale. Equivalent to `now-in(utc())`, see
// `now-in` for further information about the system clock.
pub fun now() : <ndet,utc> instant
  now-in(utc())


// The current `:instant` in time as returned by the system clock
// in an optional time scale `ts` (`= ts-ti`).
//
// This uses the best available system clock for the requested
// timescale. For example
// it uses [``CLOCK_UTC``](http://www.madore.org/~david/computers/unix-leap-seconds.html)
// when available to get proper UTC time, or ``CLOCK_TAI`` for TAI time.
//
// Otherwise, it usually uses Unix (POSIX) time (``CLOCK_REALTIME``).
// Unfortunately, most operating systems cannot not report time in leap
// seconds accurately. The `now` function is limited by the OS in this case.
//
// To guard against inaccurate clocks and increase monotonicity,
// the `now` function guarantees that if the current measurement is
// upto 1 second in the past with regard to the previous call to `now`,
// that the returned instant is monotonic by adding nano seconds to the
// previous measurement until the system clock catches up again.
//
// This is effective in particular on older OS's where the time sometimes jumps
// back one second after a leap second. By limiting the adjustment to at most
// one second it ensures the clock catches up soon and does not affect the user
// setting a new time in the past.
pub fun now-in( ts : timescale = ts-ti) : ndet instant
  // on current backends (C#, JavaScript) we can only use `unix-now` :-(
  val (secs,frac) = unix-now()
  val leap = 0
  unix-instant(timespan(secs,frac),leap,ts)

// Returns a unix time stamp as seconds and fraction of seconds;
// The fraction of seconds is for added precision if necessary,
// and can be larger than one to indicate leap seconds.
// This still needs to be adjusted to our epoch and taking account of leap seconds.
extern unix-now() : ndet (float64,float64)
  c  "kk_time_unix_now_tuple"
  cs "_Chrono.UnixNow"
  js "_unix_now"

// Return the smallest time difference that the system clock can measure.
pub fun now-resolution() : ndet duration
  duration(xnow-resolution())

extern xnow-resolution() : ndet float64
  c "kk_time_dresolution"
  cs "_Chrono.NowResolution"
  js "_now_resolution"

