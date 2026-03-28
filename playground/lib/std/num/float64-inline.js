/*---------------------------------------------------------------------------
  Copyright 2017-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

var _big_endian = undefined;

function _check_big_endian() {
  var arrayBuffer = new ArrayBuffer(2);
  var uint8Array = new Uint8Array(arrayBuffer);
  var uint16array = new Uint16Array(arrayBuffer);
  uint8Array[0] = 0x11;
  uint8Array[1] = 0x22;
  return (uint16array[0] === 0x1122);
}

function _is_big_endian() {
  if (_big_endian===undefined) { _big_endian = _check_big_endian();  }
  return _big_endian;
}


var _buf         = new ArrayBuffer(8);
var _buf_float64 = new Float64Array(_buf);
var _buf_int32   = new Int32Array(_buf);


function _double_to_bits(d) {
  _buf_float64[0] = d;
  var lo;
  var hi;
  if (_is_big_endian()) {
    hi = _buf_int32[0]; lo = _buf_int32[1];
  }
  else {
    lo = _buf_int32[0]; hi = _buf_int32[1];
  }
  return (BigInt(hi) << 32n) + (lo >= 0 ? BigInt(lo) : 0x100000000n + BigInt(lo));
}

function _double_from_bits(i) {
  var hi = Number(i>>32n)|0;
  var lo = Number(i&0xFFFFFFFFn)|0;
  if (_is_big_endian()) {
    _buf_int32[0] = hi; _buf_int32[1] = lo;
  }
  else {
    _buf_int32[0] = lo; _buf_int32[1] = hi;
  }
  return _buf_float64[0];
}

var _splitter    = Math.pow(2,27) + 1;      // 0x1.0000002p+27 // 134217729.0 = 2^27 + 1
var _splitbound  = Math.pow(2,296);         // 0x1.0p996 // 6.696928794914171e+299  = 2^996
var _two28       = Math.pow(2,28);          // 0x1.0p28 // 268435456.0 = 2^28
var _twomin28    = Math.pow(2,-28);         // 0x1.0p-28  // 3.7252902984619140625e-09 = 2^-28

function _split( x ) {
  if (x > _splitbound || x < -_splitbound) {
    var y = x * _twomin28;
    var t = y * _splitter;
    var hi = t - (t - y);
    var lo = y - hi;
    return { hi: hi*_two28, lo: lo*_two28 };
  }
  else {
    var t = x * _splitter;
    var hi = t - (t - x);
    var lo = x - hi;
    return { hi: hi, lo: lo };
  }
}

function _fmadd(x,y,z) {
  var xx = _split(x);
  var yy = (x===y ? xx : _split(y));
  return ((xx.hi*yy.hi + z) + (xx.hi*yy.lo + xx.lo*yy.hi)) + (xx.lo*yy.lo);
}



/*------------------------------------------------
  Number formatting
------------------------------------------------*/

function _double_show_special(d) {
  if (isNaN(d)) {
    return "nan"
  }
  else if (d === -Infinity) {
    return "-inf"
  }
  else if (d === Infinity) {
    return "inf"
  }
  else {
    return "nan"
  }
}

function _double_fix_exp(s) {
  // an exponent has at least 2 digits (following the C standard)
  return s.replace(/([eE][\+\-])(\d)$/,function(m,p1,p2){ return (p2==="0" ? "" : p1 + "0" + p2); });
}

function _double_show_exp(d,fractionDigits) {
  var s;
  if (!isFinite(d)) {
    s = _double_show_special(d);
  }
  else if (d===0.0 && Object.is(d,-0.0)) {
    s = "-0";
  }
  else if (fractionDigits < 0) {
    // use at most |fractionDigits|
    s = d.toExponential();
  }
  else {
    // use exactly |fractionDigits|.
    if (fractionDigits > 20) fractionDigits = 20;
    s = d.toExponential(fractionDigits);
  }
  return _double_fix_exp(s);
}

function _double_show_fixed(d, fractionDigits) {
  var dabs = (d < 0.0 ? -d : d);
  if (!isFinite(d)) {
    return _double_show_special(d);
  }
  else if (dabs < 1.0e-15 || dabs > 1.0e+21) {
    return _double_show_exp(d,fractionDigits);
  }
  else if (fractionDigits < 0) {
    // use at most |fractionDigits|
    var s = d.toFixed(-fractionDigits);              // show at full precision
    var cap = /^([\-\+]?\d+)(\.\d+)$/.exec(s);
    if (!cap) return _double_fix_exp(s);
    var frac = cap[2].substr(0,1 - fractionDigits);  // then cut off
    return cap[1] + frac.replace(/(?:\.|([1-9]))0+$/,"$1"); // remove trailing zeros
  }
  else {
    // use exactly fractionDigits
    if (fractionDigits > 20) fractionDigits = 20;
    return _double_fix_exp(d.toFixed(fractionDigits));
  }
}

function _trimzeros(s) {
  return s.replace(/\.?0+$/,"");
}

function _gformat(x,format) {
  if (typeof x === "number" && !isFinite(x)) return _double_show_special(x);
  var hex = /^[xX]([0-9]*)/.exec(format)
  if (hex) {
    var w = parseInt(hex[1]);
    var s = x.toString(16)
    if (format[0] == 'X') s = s.toUpperCase();
    return (s.length<w ? _string_repeat("0",w - s.length) + s : s );
  }
  var exp = /^[eE]([0-9]*)/.exec(format)
  if (exp) {
    var w = parseInt(exp[1]);
    return (w>0 && w<=20 ? x.toExponential(w) : x.toExponential());
  }
  var fix = /^[fF]([0-9]*)/.exec(format)
  if (fix) {
    var w = parseInt(fix[1]);
    return _trimzeros((w > 0 && w <= 20) ? x.toFixed(w) : x.toFixed(20));
  }
  var expfix = /^[gG]([0-9]*)/.exec(format)
  if (expfix) {
    var w = parseInt(expfix[1]);
    return (w>0&&w<=20 ? x.toPrecision(w) : x.toPrecision());
  }
  /* default */
  return x.toString();
}

