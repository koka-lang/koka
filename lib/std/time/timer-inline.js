/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

var _ticks;
var _ticks_loadtime = 0.0;
var _ticks_resolution = 0.001; // milli seconds

if (typeof process !== 'undefined' && typeof process.hrtime === 'function') {
  _ticks_resolution = 1.0e-9; // nano seconds
  _ticks = function() {
    var t = process.hrtime();
    return $std_core._tuple2_(t[0], t[1] * _ticks_resolution); // to seconds
  }
}
else if (typeof performance !== 'undefined' && typeof performance.now === 'function') {
  _ticks_resolution = 1.0e-6; // micro seconds
  _ticks = function() { 
    var secs = performance.now() * 0.001;
    return $std_core._tuple2_(secs, 0.0); 
  };
}
else if (typeof Date.now === 'function') {
  _ticks_resolution = 1.0e-3; // milli seconds
  _ticks = function() { return $std_core._tuple2_( (Date.now() - _ticks_loadtime) * _ticks_resolution, 0.0); };
}
else {
  _ticks_resolution = 1.0e-3; // milli seconds
  _ticks = function() { return $std_core._tuple2_( (new Date().getTime() - _ticks_loadtime) * _ticks_resolution, 0.0 ); };
}

var _ticks_loadtime = _ticks();



