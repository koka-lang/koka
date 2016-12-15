/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

// Create a fresh date allowing for years between 0 and 99
function _date_new(year, month, day, hours, minutes, seconds, milliseconds, isutc ) {
  var xyear = (year>=0&&year<=99 ? 2000 + year : year);
  var t = (isutc ? new Date(Date.UTC(xyear,month,day,hours,minutes,seconds,milliseconds))  
                 : new Date(xyear,month,day,hours,minutes,seconds,milliseconds)); 
  if (year>=0&&year<=99) {
    // adjust year again (note: t.getFullYear may be different from year due to large day value for example)
    if (isutc) t.setUTCFullYear( t.getUTCFullYear() - 2000 );
          else t.setFullYear( t.getFullYear() - 2000 );
  } 
  return t;
}

// Ensure dates are between 0001-01-01Z and 9999-12-31T23:59:59.999Z
var _date_min = _date_new(1,0,1,0,0,0,0,true);
var _time_min = _date_min.getTime();
var _time_max = Date.UTC(9999,11,31,23,59,59,999);

function _time_create(t,isutc) {
  var ms = t.getTime();
  return { 
    isutc: isutc, 
    time: (ms>=_time_min && ms<=_time_max ? t : _date_min) 
  };
}


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
  return (t.isutc ? 0 : -t.time.getTimezoneOffset() * 60);
}

function _time_new(year, month, day, hours, minutes, seconds, milliseconds, isutc ) {
  var t = _date_new(year,month-1,day,hours,minutes,seconds,milliseconds,isutc);
  return _time_create(t,isutc);
}

function _time_new_from_seconds( seconds, isutc ) {
  return _time_create( new Date(seconds*1000.0 + _time_min), isutc );
}

function _time_epochsecs( t ) {
  return t.time.getTime() / 1000.0;
}

function _time_totalsecs( t ) {
  return (t.time.getTime() - _time_min) / 1000.0;
}

function _time_dayofweek( t ) {
  var d = (t.isutc ? t.time.getUTCDay() : t.time.getDay());
  return (d===0 ? 7 : d);
}

function _time_year(t)    { return (t.isutc ? t.time.getUTCFullYear() : t.time.getFullYear()); }
function _time_month(t)   { return (1 + (t.isutc ? t.time.getUTCMonth() : t.time.getMonth())); }
function _time_day(t)     { return (t.isutc ? t.time.getUTCDate() : t.time.getDate()); }
function _time_hours(t)   { return (t.isutc ? t.time.getUTCHours() : t.time.getHours()); }
function _time_minutes(t) { return (t.isutc ? t.time.getUTCMinutes() : t.time.getMinutes()); }
function _time_seconds(t) { return (t.isutc ? t.time.getUTCSeconds() : t.time.getSeconds()); }
function _time_milliseconds(t) { return (t.isutc ? t.time.getUTCMilliseconds() : t.time.getMilliseconds()); }


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



