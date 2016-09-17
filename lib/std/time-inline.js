/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

function _time_now() {
  return { time: new Date(), isutc: false }; 
}

function _time_utcnow() {
  return { time : new Date(), isutc: true }; 
}

function _time_to_utc(t) {
  if (t.isutc) return t;
  return { time: t.time, isutc: true };
}

function _time_to_local(t) {
  if (!t.isutc) return t;
  return { time: t.time, isutc: false };
}

function _time_timezone_offset(t) {
  return (t.isutc ? 0 : -t.time.getTimezoneOffset() * 60000);
}

function _time_new(year, month, day, hours, minutes, seconds, milliseconds, isutc ) {
  var t = (isutc ? new Date(Date.UTC(year,month-1,day,hours,minutes,seconds,milliseconds))  
                 : new Date(year,month-1,day,hours,minutes,seconds,milliseconds)); 
  return { time: t, isutc: isutc };
}

function _time_epoch( epochmsecs, isutc ) {
  return { time: new Date(epochmsecs), isutc: isutc };
}

function _time_date(t) {
  if (t.isutc) {
    return $std_core._tuple3_( t.time.getUTCFullYear(), t.time.getUTCMonth() + 1, t.time.getUTCDate() );
  }
  else {
    return $std_core._tuple3_( t.time.getFullYear(), t.time.getMonth() + 1, t.time.getDate());
  }
}

function _time_clock(t) {
  if (t.isutc) {
    return $std_core._tuple4_( t.time.getUTCHours(), t.time.getUTCMinutes(), t.time.getSeconds(), t.time.getUTCMilliseconds() );
  }
  else {
    return $std_core._tuple4_( t.time.getHours(), t.time.getMinutes(), t.time.getSeconds(), t.time.getMilliseconds());
  }
}


var _ticks;
var _ticks_loadtime = 0.0;
var _ticks_resolution = 1.0;

if (typeof process !== 'undefined' && typeof process.hrtime === 'function') {
  _ticks = function() {
    var t = process.hrtime();
    return ((t[0]*1000.0) + (t[1]/1.0e6)); // to milli-seconds
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



