/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
var prev = 0;

function _unix_now() {
  var now  = Date.now() * 1e-3; // from milliseconds to seconds
  var diff = now - prev;
  if (prev != 0 && diff <= 0 && diff >= -1) {
    // negative timestep of less than 1 second.
    // in this case we assume this is a leap second on an OS that
    // jumps back (instead of smearing).
    // keep increasing by 1 nano second until the clock catches up to
    // maintain monotonicity.
    now = prev + 1e-9;
  }
  prev = now;
  var secs = Math.floor(now);
  var frac = now - secs;
  return $std_core._tuple2_(secs, frac);
}

function _now_precision() {
  return 0.001; // millisecond precision
}
