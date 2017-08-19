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

function _double_to_bits(d) {
  var buf   = new ArrayBuffer(8);
  var float = new Float64Array(buf);
  var int  = new Int32Array(buf);
  float[0] = d;
  return (_is_big_endian() ? { fst: int[1], snd: int[0] } : { fst: int[0], snd: int[1] });
}

function _double_from_bits(lo,hi) {
  var buf   = new ArrayBuffer(8);
  var float = new Float64Array(buf);
  var int  = new Int32Array(buf);
  if (_is_big_endian()) {
    int[0] = hi|0; int[1] = lo|0;
  }
  else {
    int[0] = lo|0; int[1] = hi|0;
  }
  return float[0];
}
