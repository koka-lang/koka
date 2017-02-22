/*----------------------------------------------------------------------------
   Copyright (C) 2012-2016 Microsoft Corporation
    
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

/* Get the system time.

*/
module std/time/chrono

import std/time/timestamp
import std/time/duration
import std/time/instant
import std/time/utc

extern include {
  // cs file "time-inline.cs"
  js file "chrono-inline.js"
}


// The current `:instant` in time as returned by the system clock.
public fun now() : ndet instant {
  val (secs,frac) = unix-now()
  unix-instant(secs,frac)
} 

// Returns a unix time stamp; this still needs
// to be adjusted to our epoch and taking account of leap seconds.
extern unix-now() : ndet (double,double) {
  js "_unix_now"
}

// The resolution in seconds of the system clock.
public fun now-precision() : ndet duration {
  duration(xnow-precision())
}

extern xnow-precision() : ndet double {
  js "_now_precision"
}

