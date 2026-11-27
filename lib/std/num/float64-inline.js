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

// exponent with at least 2 digits (following the C standard)
function _double_fix_exp(s) {
  return s.replace(/e([\+\-])(\d)$/, function(m,sign,digit){ return "e" + sign + "0" + digit; });
}

// remove trailing zeros of the fraction (and a trailing dot), keeping any exponent
function _double_trim_zeros(s) {
  return s.replace(/\.(\d*?)0+(?=e|$)/, function(m,digits){ return (digits ? "." + digits : ""); });
}

function _double_clamp_prec(prec) {
  return (prec > 100 ? 100 : (prec < -100 ? -100 : prec));
}

// returns `{m,e}` such that `x == m * 2^e` with `m` odd (for a finite `x > 0`)
function _double_odd_mant(x) {
  const v = new DataView(new ArrayBuffer(8));
  v.setFloat64(0, x);
  const hi = v.getUint32(0);
  const bexp = (hi >>> 20) & 0x7FF;
  let m = (hi & 0xFFFFF) * 4294967296 + v.getUint32(4);
  let e = -1074;
  if (bexp !== 0) { m += 4503599627370496; e = bexp - 1075; }
  while (m % 2 === 0) { m /= 2; e++; }
  return { m: m, e: e };
}

// if `x*10^g == n + 0.5` exactly (where `x == r.m * 2^r.e`), returns `n` as a BigInt
function _double_tie(r, g) {
  if (r.e !== -(g+1)) return undefined;
  let t = BigInt(r.m);  // t == 2*x*10^g
  if (g >= 0) {
    t *= 5n ** BigInt(g);
  }
  else {
    const p = 5n ** BigInt(-g);
    if (t % p !== 0n) return undefined;
    t /= p;
  }
  return (t - 1n) / 2n;
}

function _double_even(n) {
  return (n % 2n === 0n ? n : n + 1n);
}

// `x.toFixed(f)` for `x >= 0` but with exact ties rounded to even (as in C)
function _double_to_fixed(x, f) {
  if (x >= 1.0e21) return BigInt(x).toString() + (f > 0 ? "." + "0".repeat(f) : "");
  const n = (x === 0 ? undefined : _double_tie(_double_odd_mant(x), f));
  if (n === undefined) return x.toFixed(f);
  const s = _double_even(n).toString().padStart(f+1, "0");
  return (f > 0 ? s.slice(0,-f) + "." + s.slice(-f) : s);
}

// `x.toExponential(f)` for `x >= 0` but with exact ties rounded to even (as in C)
function _double_to_exp(x, f) {
  if (x !== 0) {
    const r = _double_odd_mant(x);
    const g = -r.e - 1;  // the only scale at which `x` can be a tie
    const n = _double_tie(r, g);
    if (n !== undefined && n > 0n && n.toString().length === f+1) {
      let digits = _double_even(n).toString();
      let exp = f - g;
      if (digits.length > f+1) { digits = digits.slice(0,-1); exp++; }  // rounded up to a power of 10
      return digits[0] + (f > 0 ? "." + digits.slice(1) : "") + "e" + (exp < 0 ? "-" : "+") + Math.abs(exp);
    }
  }
  return x.toExponential(f);
}

function _double_sign(d) {
  return (d < 0 || Object.is(d,-0) ? "-" : "");
}

// exponential notation with exactly `prec` fraction digits if `prec >= 0`, or otherwise
// the shortest notation that reads back as `d` with at most `-prec` fraction digits.
function _double_show_exp(d, prec) {
  if (!isFinite(d)) return _double_show_special(d);
  const x = Math.abs(d);
  const p = _double_clamp_prec(prec);
  let s;
  if (p >= 0) {
    s = _double_to_exp(x, p);
  }
  else {
    s = x.toExponential();  // shortest
    const dot = s.indexOf(".");
    const frac = (dot < 0 ? 0 : s.indexOf("e") - dot - 1);
    if (frac > -p) s = _double_trim_zeros(_double_to_exp(x, -p));
  }
  return _double_sign(d) + _double_fix_exp(s);
}

// fixed notation with exactly `prec` fraction digits if `prec >= 0`, or otherwise
// the shortest notation that reads back as `d` with at most `-prec` fraction digits.
function _double_show_fixed(d, prec) {
  if (!isFinite(d)) return _double_show_special(d);
  const x = Math.abs(d);
  const p = _double_clamp_prec(prec);
  const sign = _double_sign(d);
  if (p >= 0) return sign + _double_to_fixed(x, p);
  const s = x.toExponential();  // shortest
  const i = s.indexOf("e");
  const exp = parseInt(s.slice(i+1));
  const digits = s.slice(0,i).replace(".","");
  if (digits.length - 1 - exp > -p) return sign + _double_trim_zeros(_double_to_fixed(x, -p));
  if (exp < 0) return sign + "0." + "0".repeat(-exp-1) + digits;
  if (digits.length <= exp+1) return sign + digits + "0".repeat(exp+1-digits.length);
  return sign + digits.slice(0,exp+1) + "." + digits.slice(exp+1);
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

