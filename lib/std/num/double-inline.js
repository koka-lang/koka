/*---------------------------------------------------------------------------
  Copyright 2017 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
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


var _buf       = new ArrayBuffer(8);
var _buf_float = new Float64Array(_buf);
var _buf_int   = new Int32Array(_buf);

function _double_to_bits(d) {
  _buf_float[0] = d;
  return (_is_big_endian() ? { fst: _buf_int[1], snd: _buf_int[0] } : { fst: _buf_int[0], snd: _buf_int[1] });
}

function _double_from_bits(lo,hi) {
  if (_is_big_endian()) {
    _buf_int[0] = hi|0; _buf_int[1] = lo|0;
  }
  else {
    _buf_int[0] = lo|0; _buf_int[1] = hi|0;
  }
  return _buf_float[0];
}

var _splitter    = Math.Pow(2,27) + 1;      // 0x1.0000002p+27 // 134217729.0 = 2^27 + 1
var _splitbound  = Math.Pow(2,296);         // 0x1.0p996 // 6.696928794914171e+299  = 2^996
var _two28       = Math.Pow(2,28);          // 0x1.0p28 // 268435456.0 = 2^28
var _twomin28    = Math.Pow(2,-28);         // 0x1.0p-28  // 3.7252902984619140625e-09 = 2^-28

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
