/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

function _time_now() {
  var d = new Date(); 
  return Time(d.getTimezoneOffset(),d); 
}

function _time_new(year, month, day, hours, minutes, seconds, milliseconds, isutc ) {
  var d = (isutc ? new Date(Date.UTC(year,month-1,day,hours,minutes,seconds,milliseconds)) : new Date(year,month-1,day,hours,minutes,seconds,milliseconds)); 
  return Time( (isutc ? 0 : d.getTimezoneOffset()), d);
}

function _time_epoch( epochmsecs, isutc ) {
  var d = new Date(epochmsecs); 
  return Time( (isutc ? 0 : d.getTimezoneOffset()), d);
}

function _time_date(t) {
  if (t.tzofs===0) {
    return $std_core._tuple3_( t.xtime.getUTCFullYear(), t.xtime.getUTCMonth() + 1, t.xtime.getUTCDate() );
  }
  else {
    return $std_core._tuple3_( t.xtime.getFullYear(), t.xtime.getMonth() + 1, t.xtime.getDate());
  }
}

function _time_clock(t) {
  if (t.tzofs===0) {
    return $std_core._tuple4_( t.xtime.getUTCHours(), t.xtime.getUTCMinutes(), t.xtime.getSeconds(), t.xtime.getUTCMilliseconds() );
  }
  else {
    return $std_core._tuple4_( t.xtime.getHours(), t.xtime.getMinutes(), t.xtime.getSeconds(), t.xtime.getMilliseconds());
  }
}


var _ticks;
var _ticks_loadtime = 0.0;
var _ticks_resolution = 1.0;

if (typeof process !== 'undefined' && typeof process.hrtime === 'function') {
  _ticks = function() {
    var t = process.hrtime();
    return ((t[0]*1000.0) + (t[1]/1.0e6)); // to micro-seconds
  }
  _ticks_resolution = 0.000001;
}
else if (typeof performance !== 'undefined' && typeof performance.now === 'function') {
  _ticks = function() { return performance.now(); }
  _ticks_resolution = 0.001;
}
else if (typeof Date.now === 'function') {
  _ticks = function() { return Date.now() - _ticks_loadtime; }
}
else {
  _ticks = function() { return new Date().getTime() - _ticks_loadtime; }
}

var _ticks_loadtime = _ticks();



