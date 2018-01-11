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
  cs file "chrono-inline.cs"
  js file "chrono-inline.js"
}


// The current `:instant` in time as returned by the system clock.
public fun now() : ndet instant {
  val (secs,frac) = unix-now()
  unix-instant(secs,frac)
} 

// Returns a unix time stamp as seconds and fraction of seconds; 
// The fraction of seconds is for added precision if necessary,
// and can be larger than one to indicate leap seconds.
// This still needs to be adjusted to our epoch and taking account of leap seconds.
extern unix-now() : ndet (double,double) {
  cs "_Chrono.UnixNow"
  js "_unix_now"
}

// Return the smallest time difference that the system clock can measure.
public fun now-resolution() : ndet duration {
  duration(xnow-resolution())
}

extern xnow-resolution() : ndet double {
  cs "_Chrono.NowResolution"
  js "_now_resolution"
}

