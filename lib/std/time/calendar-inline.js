/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/


function _local_get_timezone() {
  return null; // not possible in javascript :-(
}

function _local_utc_offset(tz,i) {
  return (new Date(i * 1000).getTimezoneOffset())*-60;
}


var _ticks;
var _ticks_loadtime = 0.0;
var _ticks_resolution = 0.001; // milli seconds

if (typeof process !== 'undefined' && typeof process.hrtime === 'function') {
  _ticks_resolution = 1.0e-9; // nano seconds
  _ticks = function() {
    var t = process.hrtime();
    return (t[0] + (t[1] * _ticks_resolution)); // to seconds
  }
}
else if (typeof performance !== 'undefined' && typeof performance.now === 'function') {
  _ticks_resolution = 1.0e-6; // micro seconds
  _ticks = function() { return (performance.now() * 0.001); }
}
else if (typeof Date.now === 'function') {
  _ticks_resolution = 1.0e-3; // milli seconds
  _ticks = function() { return (Date.now() - _ticks_loadtime) * _ticks_resolution; }
}
else {
  _ticks_resolution = 1.0e-3; // milli seconds
  _ticks = function() { return (new Date().getTime() - _ticks_loadtime) * _ticks_resolution; }
}

var _ticks_loadtime = _ticks();



