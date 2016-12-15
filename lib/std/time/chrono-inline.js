/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

function _unix_now() {
  var now  = Date.now() * 1e-3; // in seconds
  var secs = Math.floor(now);
  var frac = now - secs;
  return $std_core._tuple2_(secs, frac);
}

function _now_precision() {
  return 0.001; // millisecond precision
}
