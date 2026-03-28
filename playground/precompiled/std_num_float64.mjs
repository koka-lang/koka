// Koka generated module: std/num/float64, koka version: 3.2.4
"use strict";
 
// imports
import * as $std_core_types from './std_core_types.mjs';
import * as $std_core_hnd from './std_core_hnd.mjs';
import * as $std_core_exn from './std_core_exn.mjs';
import * as $std_core_bool from './std_core_bool.mjs';
import * as $std_core_order from './std_core_order.mjs';
import * as $std_core_char from './std_core_char.mjs';
import * as $std_core_int from './std_core_int.mjs';
import * as $std_core_vector from './std_core_vector.mjs';
import * as $std_core_string from './std_core_string.mjs';
import * as $std_core_sslice from './std_core_sslice.mjs';
import * as $std_core_list from './std_core_list.mjs';
import * as $std_core_maybe from './std_core_maybe.mjs';
import * as $std_core_maybe2 from './std_core_maybe2.mjs';
import * as $std_core_either from './std_core_either.mjs';
import * as $std_core_result from './std_core_result.mjs';
import * as $std_core_tuple from './std_core_tuple.mjs';
import * as $std_core_lazy from './std_core_lazy.mjs';
import * as $std_core_show from './std_core_show.mjs';
import * as $std_core_debug from './std_core_debug.mjs';
import * as $std_core_delayed from './std_core_delayed.mjs';
import * as $std_core_console from './std_core_console.mjs';
import * as $std_core from './std_core.mjs';
import * as $std_text_parse from './std_text_parse.mjs';
import * as $std_num_int32 from './std_num_int32.mjs';
import * as $std_num_int64 from './std_num_int64.mjs';
 
// externals
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
 
// type declarations
 
// declarations
 
 
// &pi;
export var pi;
var pi = 3.141592653589793;
 
 
// &pi;
export var flt_pi;
var flt_pi = 3.141592653589793;
 
 
// 2&pi;
export var flt_twopi;
var flt_twopi = 6.283185307179586;
 
 
// &pi;/2
export var flt_pi2;
var flt_pi2 = 1.5707963267948966;
 
 
// &pi;/4
export var flt_pi4;
var flt_pi4 = 0.7853981633974483;
 
 
// 3&pi;/4
export var flt_pi34;
var flt_pi34 = 2.356194490192345;
 
 
// The [_e_](https://en.wikipedia.org/wiki/E_(mathematical_constant)) constant.
export var flt_e;
var flt_e = 2.718281828459045;
 
 
// The natural logarithm of 2
export var flt_ln2;
var flt_ln2 = 0.6931471805599453;
 
 
// The natural logarithm of 10
export var flt_ln10;
var flt_ln10 = 2.302585092994046;
 
 
// The base-2 logarithm of _e_.
export var flt_log2e;
var flt_log2e = 1.4426950408889634;
 
 
// The base-10 logarithm of _e_.
export var flt_log10e;
var flt_log10e = 0.4342944819032518;
 
 
// The square-root of 2
export var flt_sqrt2;
var flt_sqrt2 = 1.4142135623730951;
 
 
// `1.0 / sqrt(2.0)`  (= `sqrt(0.5)`)
export var flt_sqrt12;
var flt_sqrt12 = 0.7071067811865476;
 
 
// [Euler's constant](https://en.wikipedia.org/wiki/Euler%E2%80%93Mascheroni_constant)
export var flt_euler;
var flt_euler = 0.5772156649015329;
 
 
// Maximum float64 value
export var flt_max;
var flt_max = 1.7976931348623157e308;
 
 
// Smallest positive normalized float64 value
export var flt_min;
var flt_min = 2.2250738585072014e-308;
 
 
// Smallest positive subnormal value (i.e. [``DBL_TRUE_MIN``](https://en.cppreference.com/w/cpp/types/climits))
export var flt_true_min;
var flt_true_min = 5.0e-324;
 
 
// Machine epsilon: the difference between 1.0 and the next representable `:float64` value.
export var flt_epsilon;
var flt_epsilon = 2.220446049250313e-16;
 
 
// maximal precision in decimal digits of a `:float64`.
export var flt_max_prec;
var flt_max_prec = 15;
 
 
// Is the value negative?
export function is_neg(d) /* (d : float64) -> bool */  {
  return (d < (0.0));
}
 
 
// Returns the largest of two floats
export function max(x, y) /* (x : float64, y : float64) -> float64 */  {
  return ((x >= y)) ? x : y;
}
 
 
// Is this a [subnormal](https://en.wikipedia.org/wiki/Denormal_number) value?
// (i.e. `0 < d.abs < flt-min`)
export function is_subnormal(d) /* (d : float64) -> bool */  {
  if ((d !== (0.0))) {
    return ((Math.abs(d)) < (2.2250738585072014e-308));
  }
  else {
    return false;
  }
}
 
 
// Extend a `:float32` to a `:float64`
export function float32_fs_float64(f) /* (f : float32) -> float64 */  {
  return (f);
}
 
 
// Convert an 64-bit integer to a `:float64`.
export function int64_fs_float64(i) /* (i : int64) -> float64 */  {
  return $std_core_types._int_to_double(($std_core_types._int_from_int64(i)));
}
 
export function make_neginf() /* () -> float64 */  {
  return -Infinity;
}
 
 
// Negative infinity
export var neginf;
var neginf = make_neginf();
 
export function make_posinf() /* () -> float64 */  {
  return Infinity;
}
 
 
// Positive infinity
export var posinf;
var posinf = make_posinf();
 
 
// Is the value positive?
export function is_pos(d) /* (d : float64) -> bool */  {
  return (d > (0.0));
}
 
 
// Is the value zero?
export function is_zero(d) /* (d : float64) -> bool */  {
  return (d === (0.0));
}
 
 
// Returns the smallest of two floats
export function min(x, y) /* (x : float64, y : float64) -> float64 */  {
  return ((x <= y)) ? x : y;
}
 
 
// Returns the smallest element of a list of floats (or `0` for the empty list)
export function minimum(xs) /* (xs : list<float64>) -> float64 */  {
  if (xs === null) {
    return 0.0;
  }
  else {
    return $std_core_list.foldl(xs.tail, xs.head, min);
  }
}
 
 
// Returns the largest element of a list of floats (or `0` for the empty list)
export function maximum(xs) /* (xs : list<float64>) -> float64 */  {
  if (xs === null) {
    return 0.0;
  }
  else {
    return $std_core_list.foldl(xs.tail, xs.head, max);
  }
}
 
export function make_nan() /* () -> float64 */  {
  return NaN;
}
 
 
// Represents a value that is _not a number_ (NaN)
export var nan;
var nan = make_nan();
 
 
// Compare the argument to zero.
export function sign(d) /* (d : float64) -> order */  {
  if ((d < (0.0))) {
    return $std_core_types.Lt;
  }
  else {
    return ((d > (0.0))) ? $std_core_types.Gt : $std_core_types.Eq;
  }
}
 
export function show_fixedx(d, prec) /* (d : float64, prec : int32) -> string */  {
  return _double_show_fixed(d,prec);
}
 
export function show_expx(d, prec) /* (d : float64, prec : int32) -> string */  {
  return _double_show_exp(d,prec);
}
 
 
// Show a `:float64` in exponential (scientific) notation.
// The optional `precision` (= `-17`) specifies the precision.
// If `>=0` it specifies the number of digits behind the dot (up to `17` max).
// If negative, then at most the absolute value of `precision` digits behind the dot are used.
export function show_exp(d, precision) /* (d : float64, precision : ? int) -> string */  {
  var _x0 = (precision !== undefined) ? precision : -17;
  return show_expx(d, $std_core_types._int_clamp32(_x0));
}
 
 
// Show a `:float64` fixed-point notation.
// The optional `precision` (= `-2`) specifies the maximum precision.
// If `>=0` it specifies the number of digits behind the dot (up to `20` max).
// If negative, then at most the absolute value of `precision` digits behind the dot are used.
// This may still show a number in exponential notation if the it is too small or large,
// in particular, for  a `d` where `d > 1.0e21` or `d < 1.0e-15`, or if
// `precision.abs > 17`, the `show-exp` routine is used.
export function show_fixed(d, precision) /* (d : float64, precision : ? int) -> string */  {
   
  var dabs = Math.abs(d);
  if ((dabs < (1.0e-15))) {
    var _x1 = (precision !== undefined) ? precision : -2;
    return show_expx(d, $std_core_types._int_clamp32(_x1));
  }
  else {
    if ((dabs > (1.0e21))) {
      var _x2 = (precision !== undefined) ? precision : -2;
      return show_expx(d, $std_core_types._int_clamp32(_x2));
    }
    else {
      var _x3 = (precision !== undefined) ? precision : -2;
      return show_fixedx(d, $std_core_types._int_clamp32(_x3));
    }
  }
}
 
 
// Show a `:float64` as a string.
// If `d >= 1.0e-5` and `d < 1.0e+21`, `show-fixed` is used and otherwise `show-exp`.
// Default `precision` is `-17`.
export function show(d, precision) /* (d : float64, precision : ? int) -> string */  {
   
  var dabs = Math.abs(d);
  if ((dabs >= (1.0e-5))) {
    if ((dabs < (1.0e21))) {
      var _x4 = (precision !== undefined) ? precision : -17;
      return show_fixed(d, _x4);
    }
    else {
      var _x5 = (precision !== undefined) ? precision : -17;
      return show_expx(d, $std_core_types._int_clamp32(_x5));
    }
  }
  else {
    var _x6 = (precision !== undefined) ? precision : -17;
    return show_expx(d, $std_core_types._int_clamp32(_x6));
  }
}
 
 
// Is this value equal to negative or positive infinity ?
export function is_inf(d) /* (d : float64) -> bool */  {
  return ((d) === Infinity || (d) === -Infinity);
}
 
 
// Is this a negative zero value?
export function is_negzero(d) /* (d : float64) -> bool */  {
  if ((d === (0.0))) {
    return (((((1.0) / d))) === -Infinity);
  }
  else {
    return false;
  }
}
 
 
// fused multiply-add. Computes `(x*y)+z` as if to infinite precision
// with only the final result rounded back to a `:float64`.
export function fmadd(x, y, z) /* (x : float64, y : float64, z : float64) -> float64 */  {
  return _fmadd(x,y,z);
}
 
 
// Return the integral part of a `:float64` `d` .
// If `d >= 0.0` , return the largest integer equal or less to `d` ,
// If `d < 0.0` , return the smallest integer equal or larger to `d` .
export function truncate(d) /* (d : float64) -> float64 */  {
  return ((d >= (0.0))) ? Math.floor(d) : Math.ceil(d);
}
 
 
// Return the fractional part of a `:float64` `d`.\
// `d.truncate + d.fraction === d`\
// `(-2.4).fraction === -0.4`
export function fraction(d) /* (d : float64) -> float64 */  {
  var _x7 = ((d >= (0.0))) ? Math.floor(d) : Math.ceil(d);
  return (d - _x7);
}
 
 
// Return the 'floored' fraction of `d`, always greater or equal to zero.\
// `d.floor + d.ffraction === d`\
// `(-2.4).ffraction === 0.6`
export function ffraction(d) /* (d : float64) -> float64 */  {
  return (d - (Math.floor(d)));
}
 
 
// Round a float64 to a specified precision. Rounds to the  even number in case of a tie.\
// `123.456.round-to-prec(2) == 123.46`\
// `123.456.round-to-prec(-1) == 120.0`\
export function round_to_prec(d, prec) /* (d : float64, prec : int) -> float64 */  {
  if ($std_core_types._int_le(prec,0)) {
    return $std_core_types._double_round_even(d);
  }
  else {
     
    var p = Math.pow(10.0,($std_core_types._int_to_double(prec)));
    return (($std_core_types._double_round_even(((d * p)))) / p);
  }
}
 
 
// Round a `:float64` to a `:float32`
export function float64_fs_float32(f) /* (f : float64) -> float32 */  {
  return $std_core_types._double_to_float(f);
}
 
 
// Short version of `float32` for convenience, e.g. `1.337.f32`. For example:
// ```
// > 1.337.show-hex ++ " != " ++ 1.337.f32.float64.show-hex
// "0x1.5645A1CAC0831p+0 != 0x1.5645A2p+0"
// ```
// .
export function f32(f) /* (f : float64) -> float32 */  {
  return float64_fs_float32(f);
}
 
 
// Return the logarithm in base `base` of a `:float64` `f`
export function log(f, base) /* (f : float64, base : float64) -> float64 */  {
  return ((Math.log(f)) / (Math.log(base)));
}
 
export function log2p1(x) /* (x : float64) -> float64 */  {
  return ((1.4426950408889634) * (Math.log1p(x)));
}
 
export function exp2m1(x) /* (x : float64) -> float64 */  {
  return Math.expm1((((0.6931471805599453) * x)));
}
 
 
// Returns `ln(exp(x) + exp(y))`.
// Avoids overflow/underflow errors.
export function lnaddexp(x, y) /* (x : float64, y : float64) -> float64 */  {
  if ((x === y)) {
    return (x + (0.6931471805599453));
  }
  else {
     
    var z = (x - y);
    if ((z > (0.0))) {
      return (x + (Math.log1p((Math.exp(((-z)))))));
    }
    else {
      return (y + (Math.log1p((Math.exp(z)))));
    }
  }
}
 
 
// Returns `log2( exp2(x) + exp2(y) )`.
// Avoids overflow/underflow errors.
export function logaddexp2(x, y) /* (x : float64, y : float64) -> float64 */  {
  if ((x === y)) {
    return (x + (1.0));
  }
  else {
     
    var z = (x - y);
    if ((z > (0.0))) {
       
      var x_0_10009 = Math.pow(2.0,((-z)));
      return (x + (((1.4426950408889634) * (Math.log1p(x_0_10009)))));
    }
    else {
       
      var x_1_10010 = Math.pow(2.0,z);
      return (y + (((1.4426950408889634) * (Math.log1p(x_1_10010)))));
    }
  }
}
 
 
// Return if two floats are nearly equal with respect to some `epsilon` (=`8*flt-epsilon`).
// The epsilon is the nearest difference for numbers around 1.0. The routine automatically
// scales the epsilon for larger and smaller numbers, and for subnormal numbers.
export function nearly_eq(x, y, epsilon) /* (x : float64, y : float64, epsilon : ? float64) -> bool */  {
   
  var _uniq_epsilon_1783 = (epsilon !== undefined) ? epsilon : ((8.0) * (2.220446049250313e-16));
  if ((x === y)) {
    return true;
  }
  else {
     
    var diff = Math.abs(((x - y)));
    if ((x === (0.0))) {
      return ((((2.0) * diff)) < ((_uniq_epsilon_1783 * (2.2250738585072014e-308))));
    }
    else {
      if ((y === (0.0))) {
        return ((((2.0) * diff)) < ((_uniq_epsilon_1783 * (2.2250738585072014e-308))));
      }
      else {
        if ((diff < (2.2250738585072014e-308))) {
          return ((((2.0) * diff)) < ((_uniq_epsilon_1783 * (2.2250738585072014e-308))));
        }
        else {
           
          var sum_0 = ((Math.abs(x)) + (Math.abs(y)));
          var _x8 = ((sum_0 > (1.7976931348623157e308))) ? 1.7976931348623157e308 : sum_0;
          return ((((((2.0) * diff)) / _x8)) < _uniq_epsilon_1783);
        }
      }
    }
  }
}
 
 
// Return if two floats are nearly equal with respect to an `epsilon` of `8*flt-epsilon`.
// See also `nearly-eq` which takes an explicit `epsilon`.
export function _lp__tilde__eq__rp_(x, y) /* (x : float64, y : float64) -> bool */  {
  return nearly_eq(x, y);
}
 
 
// Low-level: return the bits of a 64-bit `:float64` as in `:int64`
export function float64_to_bits(d) /* (d : float64) -> int64 */  {
  return _double_to_bits(d);
}
 
 
// Low-level: create a `:float64` from the given 64-bit integer.
export function float64_from_bits(i) /* (i : int64) -> float64 */  {
  return _double_from_bits(i);
}
 
 
// Calculate 2&middot;^`e`^ for an integer `e`.
// Uses efficient bit conversion for exponents between  -1022 and 1023 and
// otherwise falls back to the regular `exp2` function converting `e` to a float64.
export function exp2i(e) /* (e : int) -> float64 */  {
  if ($std_core_types._int_ge(e,(-1022))) {
    if ($std_core_types._int_le(e,1023)) {
       
      var i_10011 = $std_num_int64.int64($std_core_types._int_add(1023,e));
      return float64_from_bits($std_core_types._int64_shl(i_10011,52n));
    }
    else {
      return Math.pow(2.0,($std_core_types._int_to_double(e)));
    }
  }
  else {
    return Math.pow(2.0,($std_core_types._int_to_double(e)));
  }
}
 
export var one_p1023;
var one_p1023 = 8.98846567431158e307;
 
export var one_m1022;
var one_m1022 = 2.2250738585072014e-308;
 
export function mul_exp2(x, e) /* (x : float64, e : int) -> float64 */  {
  return (x * (exp2i(e)));
}
 
 
// 'Load exponent': returns `x`&middot;2^`e`^ for a `is-finite` `x` and
// otherwise `x` itself. See also `encode` which loads an integer mantissa.
export function ldexp(x, e) /* (x : float64, e : int) -> float64 */  {
   
  var b_10015 = isFinite(x);
  if (b_10015) {
    if ($std_core_types._int_ge(e,(-1022))) {
      if ($std_core_types._int_le(e,1023)) {
        return (x * (exp2i(e)));
      }
      else {
        if ($std_core_types._int_le(e,2046)) {
           
          var x_1_10018 = (x * (8.98846567431158e307));
           
          var e_1_10019 = $std_core_types._int_sub(e,1023);
          return (x_1_10018 * (exp2i(e_1_10019)));
        }
        else {
          if ($std_core_types._int_le(e,3069)) {
             
            var x_3_10022 = (((x * (8.98846567431158e307))) * (8.98846567431158e307));
             
            var e_2_10023 = $std_core_types._int_sub(e,2046);
            return (x_3_10022 * (exp2i(e_2_10023)));
          }
          else {
            return ((x < (0.0))) ? neginf : posinf;
          }
        }
      }
    }
    else {
      if ($std_core_types._int_ge(e,(-2044))) {
         
        var x_5_10026 = (x * (2.2250738585072014e-308));
         
        var e_3_10027 = $std_core_types._int_add(e,1022);
        return (x_5_10026 * (exp2i(e_3_10027)));
      }
      else {
        if ($std_core_types._int_ge(e,(-3066))) {
           
          var x_7_10030 = (((x * (2.2250738585072014e-308))) * (2.2250738585072014e-308));
           
          var e_4_10031 = $std_core_types._int_add(e,2044);
          return (x_7_10030 * (exp2i(e_4_10031)));
        }
        else {
          return ((x < (0.0))) ? (-0.0) : 0.0;
        }
      }
    }
  }
  else {
    return x;
  }
}
 
 
// Create a float64 `d` given a mantissa `man` and exponent `exp`
// such that `man`&middot;2^`exp`^ =  `d` exactly (if it is representable
// by a `:float64`). See also `ldexp`.
export function encode(man, exp) /* (man : int, exp : int) -> float64 */  {
  return ldexp($std_core_types._int_to_double(man), exp);
}
 
 
// decode a normalized float64 (i.e. not subnormal)
export function decode_normalized(d, e_adjust) /* (d : float64, e-adjust : ? int) -> (int, int) */  {
   
  var i = float64_to_bits(d);
   
  var exp = BigInt.asIntN(64,(($std_core_types._int64_shr(i,52n)) & 2047n) - 1043n);
   
  var man = BigInt.asIntN(64,(i & 4503599627370495n) + 4503599627370496n);
   
  var x_0_10039 = $std_core_types._int_from_int64(exp);
   
  var x_10037 = $std_core_types._int_sub(x_0_10039,32);
  if (0n > i) {
    var _x9 = BigInt.asIntN(64,0n - man);
  }
  else {
    var _x9 = man;
  }
  var _x10 = (e_adjust !== undefined) ? e_adjust : 0;
  return $std_core_types.Tuple2($std_core_types._int_from_int64(_x9), $std_core_types._int_add(x_10037,_x10));
}
 
 
// Decode a float64 `d` into a tuple `(m,e)` of a mantissa `m` and exponent `e`
// such that `m`&middot;2^`e`^ =  `d` exactly. The mantissa `m` is
// always either 0 or in the range [2^52^, 2^53^). See also `frexp`.
export function decode(d) /* (d : float64) -> (int, int) */  {
  if ((d === (0.0))) {
    return $std_core_types.Tuple2(0, 0);
  }
  else {
    if ((d !== (0.0))) {
      var _x11 = ((Math.abs(d)) < (2.2250738585072014e-308));
      if (_x11) {
        return decode_normalized((d * (1.8014398509481984e16)), -54);
      }
      else {
        return decode_normalized(d, 0);
      }
    }
    else {
      return decode_normalized(d, 0);
    }
  }
}
 
 
// 'Fraction/exponent': return the normalized fraction `f` and exponent `exp`
// for a number `x` such that `x == f`&middot;2^`exp`^.
// The absolute value of the fraction `f` is always in the range [0.5, 1.0), or
// one of `0.0`, `-0.0`, `neginf`, `posinf`, or `nan`.
// See also `decode` which  decodes to an integer mantissa.
export function frexp(x) /* (x : float64) -> (float64, int) */  {
   
  var b_10042 = isFinite(x);
  if (b_10042) {
    if ((x === (0.0))) {
      var _x12 = (((((1.0) / x))) === -Infinity);
      if (_x12) {
        return $std_core_types.Tuple2(x, 0);
      }
      else {
        var _x13 = decode(x);
        return $std_core_types.Tuple2((($std_core_types._int_to_double((_x13.fst))) * (1.1102230246251565e-16)), $std_core_types._int_add((_x13.snd),53));
      }
    }
    else {
      var _x14 = decode(x);
      return $std_core_types.Tuple2((($std_core_types._int_to_double((_x14.fst))) * (1.1102230246251565e-16)), $std_core_types._int_add((_x14.snd),53));
    }
  }
  else {
    return $std_core_types.Tuple2(x, 0);
  }
}
 
 
// Returns the greatest `:float64` less than `x`.
// This behaves exactly as `nextDown` in the IEEE 754-2008 standard.
// The identity `x.next-down == ~next-down(~x)` holds for all `x`.
// When `x` is finite `x == x.next-down.next-up` also holds.
export function next_down(x) /* (x : float64) -> float64 */  {
  if (isNaN(x)) {
    return x;
  }
  else {
    if (((x) === -Infinity)) {
      return x;
    }
    else {
      if ((x === (0.0))) {
        return (-(5.0e-324));
      }
      else {
         
        var i = float64_to_bits(x);
         
        if (0n > i) {
          var next = BigInt.asIntN(64,i + 1n);
        }
        else {
          var next = BigInt.asIntN(64,i - 1n);
        }
        return float64_from_bits(next);
      }
    }
  }
}
 
 
// Returns the least `:float64` greater than `x`.
// This behaves exactly as `nextUp` in the IEEE 754-2008 standard.
// The identity `x.next-up == ~next-down(~x)` holds for all `x`.
// When `x` is finite `x == x.next-up.next-down` also holds.
export function next_up(x) /* (x : float64) -> float64 */  {
  if (isNaN(x)) {
    return x;
  }
  else {
    if (((x) === Infinity)) {
      return x;
    }
    else {
      if ((x === (0.0))) {
        return 5.0e-324;
      }
      else {
         
        var i = float64_to_bits(x);
         
        if (0n > i) {
          var next = BigInt.asIntN(64,i - 1n);
        }
        else {
          var next = BigInt.asIntN(64,i + 1n);
        }
        return float64_from_bits(next);
      }
    }
  }
}
 
 
// Compare floats using a total ordering on the `:float64`.
// The ordering follows the `totalOrder` predicate as defined in IEEE 754-2008 exactly.
// The values are ordered in following order:
// negative quiet nan,
// negative signaling nan,
// `neginf`,
// -finite,
// -0.0,
// +0.0,
// finite,
// `posinf`,
// signaling nan,
// and quiet nan.
export function cmp(x, y) /* (x : float64, y : float64) -> order */  {
   
  var bx = float64_to_bits(x);
   
  var by = float64_to_bits(y);
   
  var i_10052 = $std_core_types._int64_sar(bx,63n);
   
  var ix = ($std_core_types._int64_shr(i_10052,1n)) ^ bx;
   
  var i_1_10056 = $std_core_types._int64_sar(by,63n);
   
  var iy = ($std_core_types._int64_shr(i_1_10056,1n)) ^ by;
  if ((ix < iy)) {
    return $std_core_types.Lt;
  }
  else {
    return ((ix > iy)) ? $std_core_types.Gt : $std_core_types.Eq;
  }
}
 
 
// The midpoint is the average of `x` and `y`.
// Avoids overflow on large numbers.
export function midpoint(x, y) /* (x : float64, y : float64) -> float64 */  {
  if ((x !== (0.0))) {
    var _x15 = ((Math.abs(x)) < (2.2250738585072014e-308));
    if (_x15) {
      return (((x + y)) / (2.0));
    }
    else {
      if ((y !== (0.0))) {
        var _x16 = ((Math.abs(y)) < (2.2250738585072014e-308));
        if (_x16) {
          return (((x + y)) / (2.0));
        }
        else {
          return (((x / (2.0))) + ((y / (2.0))));
        }
      }
      else {
        return (((x / (2.0))) + ((y / (2.0))));
      }
    }
  }
  else {
    if ((y !== (0.0))) {
      var _x17 = ((Math.abs(y)) < (2.2250738585072014e-308));
      if (_x17) {
        return (((x + y)) / (2.0));
      }
      else {
        return (((x / (2.0))) + ((y / (2.0))));
      }
    }
    else {
      return (((x / (2.0))) + ((y / (2.0))));
    }
  }
}
 
 
// Linear interpolation, calculating `x + t*(y - x)` but avoids troublesome edge cases.
// Follows the C++20 [specification](https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2019/p0811r3.html).
// In particular, if `x.is-finite && y.is-finite`, then:
// - exact: `lerp(x,y,0.0) == x` and  `lerp(x,y,1.0) == y`
// - monotonic: if `x <= y` and `t1 <= t2`, then `cmp( lerp(x,y,t1), lerp(x,y,t2) ) <= Eq` (and other cases)
// - deterministic: only `lerp(x,x,flt-inf)` results in `nan`
// - bounded: `t<0.0 || t>1.0 || is-finite(lerp(x,y,t))`
// - consistent: `lerp(x,x,t) == x`
export function lerp(x, y, t) /* (x : float64, y : float64, t : float64) -> float64 */  {
  if ((x <= (0.0))) {
    if ((y >= (0.0))) {
      return (((t * y)) + (((((1.0) - t)) * x)));
    }
    else {
      if ((x >= (0.0))) {
        if ((y <= (0.0))) {
          return (((t * y)) + (((((1.0) - t)) * x)));
        }
        else {
          if ((t === (1.0))) {
            return y;
          }
          else {
             
            var z = (x + ((t * ((y - x)))));
             
            var x_0_10064 = (t > (1.0));
             
            var y_0_10065 = (y > x);
            if (x_0_10064) {
              var _x18 = y_0_10065;
            }
            else {
              var _x18 = (y_0_10065) ? false : true;
            }
            if (_x18) {
              return ((y >= z)) ? y : z;
            }
            else {
              return ((y <= z)) ? y : z;
            }
          }
        }
      }
      else {
        if ((t === (1.0))) {
          return y;
        }
        else {
           
          var z_0 = (x + ((t * ((y - x)))));
           
          var x_3_10070 = (t > (1.0));
           
          var y_3_10071 = (y > x);
          if (x_3_10070) {
            var _x19 = y_3_10071;
          }
          else {
            var _x19 = (y_3_10071) ? false : true;
          }
          if (_x19) {
            return ((y >= z_0)) ? y : z_0;
          }
          else {
            return ((y <= z_0)) ? y : z_0;
          }
        }
      }
    }
  }
  else {
    if ((x >= (0.0))) {
      if ((y <= (0.0))) {
        return (((t * y)) + (((((1.0) - t)) * x)));
      }
      else {
        if ((t === (1.0))) {
          return y;
        }
        else {
           
          var z_1 = (x + ((t * ((y - x)))));
           
          var x_6_10076 = (t > (1.0));
           
          var y_6_10077 = (y > x);
          if (x_6_10076) {
            var _x20 = y_6_10077;
          }
          else {
            var _x20 = (y_6_10077) ? false : true;
          }
          if (_x20) {
            return ((y >= z_1)) ? y : z_1;
          }
          else {
            return ((y <= z_1)) ? y : z_1;
          }
        }
      }
    }
    else {
      if ((t === (1.0))) {
        return y;
      }
      else {
         
        var z_0_0 = (x + ((t * ((y - x)))));
         
        var x_9_10082 = (t > (1.0));
         
        var y_9_10083 = (y > x);
        if (x_9_10082) {
          var _x21 = y_9_10083;
        }
        else {
          var _x21 = (y_9_10083) ? false : true;
        }
        if (_x21) {
          return ((y >= z_0_0)) ? y : z_0_0;
        }
        else {
          return ((y <= z_0_0)) ? y : z_0_0;
        }
      }
    }
  }
}
 
 
/* Show a float64 in [hexadecimal notation](https://books.google.com/books?id=FgMsCwAAQBAJ&pg=PA41).
An advantage of this format is that it precisely represents the `:float64` and can
reliably (and efficiently) be parsed back, i.e. `d.show-hex.parse-float64 == Just(d)`.
The significant is the _hexadecimal_ fraction while the
exponent after the `p` is the _decimal_ power of 2.
 For example, ``0xA.Fp-10`` = (10 + 15/16)&middot;2^-10^  (not 2^-16^!) = 0.01068115234375.
 Equivalently, ``0xA.Fp-10 == 0x5.78p-9 == 0x2.BCp-8 == 0x1.5Ep-7``.
```
> flt-min.show-hex
"0x1.0p-1022"
> 0.1.show-hex
"0x1.999999999999Ap-4"
> flt-max.show-hex
"0x1.FFFFFFFFFFFFFp+1023"
> -0.0.show-hex
"-0x0.0p+0"
> nan.show-hex
"NaN"
> 0.01068115234375.show-hex
"0x1.5Ep-7"
```
.
*/
export function show_hex(d, width, use_capitals, pre) /* (d : float64, width : ? int, use-capitals : ? bool, pre : ? string) -> string */  {
   
  var b_10088 = isFinite(d);
  if (b_10088) {
    var _x22 = decode(d);
     
    var _x23 = (use_capitals !== undefined) ? use_capitals : true;
    var man = $std_core_show.show_hex($std_core_types._int_abs((_x22.fst)), 1, _x23, "");
     
    var x_0_10091 = $std_core_string.chars_fs_count(man);
     
    var y_10090 = $std_core_types._int_mul(4,($std_core_types._int_sub(x_0_10091,1)));
     
    var exp0 = $std_core_types._int_add((_x22.snd),y_10090);
     
    var _x24 = ($std_core_types._int_ge(exp0,0)) ? "+" : "";
    var exp = $std_core_types._lp__plus__plus__rp_(_x24, $std_core_int.show(exp0));
     
    var _x27 = (width !== undefined) ? width : 1;
    var _x26 = $std_core_types._int_ge(1,_x27);
    if (_x26) {
      var _x25 = 1;
    }
    else {
      var _x25 = (width !== undefined) ? width : 1;
    }
    var frac = $std_core_string.pad_right($std_core_sslice.trim_right($std_core_sslice.tail(man), "0"), _x25, 0x0030);
     
    if ((d < (0.0))) {
      var sign_0 = "-";
    }
    else {
      if ((d === (0.0))) {
        var _x28 = (((((1.0) / d))) === -Infinity);
        var sign_0 = (_x28) ? "-" : "";
      }
      else {
        var sign_0 = "";
      }
    }
    var _x23 = (pre !== undefined) ? pre : "0x";
    return $std_core_types._lp__plus__plus__rp_(sign_0, $std_core_types._lp__plus__plus__rp_(_x23, $std_core_types._lp__plus__plus__rp_($std_core_sslice.head(man), $std_core_types._lp__plus__plus__rp_(".", $std_core_types._lp__plus__plus__rp_(frac, $std_core_types._lp__plus__plus__rp_("p", exp))))));
  }
  else {
    return show(d);
  }
}
 
 
// monadic lift
export function _mlift_phexdouble_10171(wild___2) /* (wild_@2 : char) -> std/text/parse/parse int */  {
  return $std_text_parse.pint();
}
 
 
// monadic lift
export function _mlift_phexdouble_10172(frac, man, exp) /* (frac : string, man : string, exp : int) -> std/text/parse/parse float64 */  {
   
  var _x_x1_0_10166 = $std_core_types._lp__plus__plus__rp_(man, frac);
   
  var _x_x1_10164 = $std_core_hnd._open_none2(function(s /* string */ , hex /* ? bool */ ) {
      var _x24 = (hex !== undefined) ? hex : false;
      return $std_core_int.xparse(s, _x24);
    }, _x_x1_0_10166, true);
   
  var m_0 = $std_core_hnd._open_none2(function(m /* maybe<int> */ , nothing /* int */ ) {
      return (m === null) ? nothing : m.value;
    }, _x_x1_10164, 0);
   
  var y_10161 = $std_core_types._int_mul(4,($std_core_string.chars_fs_count(frac)));
   
  var e = $std_core_types._int_sub(exp,y_10161);
  return $std_core_hnd._open_none2(function(man_0 /* int */ , exp_0 /* int */ ) {
      return ldexp($std_core_types._int_to_double(man_0), exp_0);
    }, m_0, e);
}
 
 
// monadic lift
export function _mlift_phexdouble_10173(wild___1) /* (wild_@1 : char) -> std/text/parse/parse string */  {
  return $std_text_parse.hex_digits();
}
 
 
// monadic lift
export function _mlift_phexdouble_10174(man, _y_x10133) /* (man : string, string) -> std/text/parse/parse float64 */  {
   
  var frac = $std_core_sslice.trim_right(_y_x10133, "0");
   
  var x_10194 = $std_text_parse._lp__bar__bar__rp_(function() {
       
      var x_0_10196 = $std_text_parse.one_of("pP");
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(_mlift_phexdouble_10171);
      }
      else {
        return $std_text_parse.pint();
      }
    }, function() {
      return 0;
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(exp /* int */ ) {
      return _mlift_phexdouble_10172(frac, man, exp);
    });
  }
  else {
    return _mlift_phexdouble_10172(frac, man, x_10194);
  }
}
 
 
// monadic lift
export function _mlift_phexdouble_10175(man) /* (man : string) -> std/text/parse/parse float64 */  {
   
  var x_10198 = $std_text_parse._lp__bar__bar__rp_(function() {
       
      var x_0_10200 = $std_text_parse.char(0x002E);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(_mlift_phexdouble_10173);
      }
      else {
        return $std_text_parse.hex_digits();
      }
    }, function() {
      return "";
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10133 /* string */ ) {
      return _mlift_phexdouble_10174(man, _y_x10133);
    });
  }
  else {
    return _mlift_phexdouble_10174(man, x_10198);
  }
}
 
 
// monadic lift
export function _mlift_phexdouble_10176(wild___0) /* (wild_@0 : char) -> std/text/parse/parse float64 */  {
   
  var x_10202 = $std_text_parse.hex_digits();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_phexdouble_10175);
  }
  else {
    return _mlift_phexdouble_10175(x_10202);
  }
}
 
 
// monadic lift
export function _mlift_phexdouble_10177(wild__) /* (wild_ : char) -> std/text/parse/parse float64 */  {
   
  var x_10204 = $std_text_parse.one_of("xX");
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_phexdouble_10176);
  }
  else {
    return _mlift_phexdouble_10176(x_10204);
  }
}
 
export function phexdouble() /* () -> std/text/parse/parse float64 */  {
   
  var x_10206 = $std_text_parse.char(0x0030);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_phexdouble_10177);
  }
  else {
     
    var x_0_10209 = $std_text_parse.one_of("xX");
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(_mlift_phexdouble_10176);
    }
    else {
       
      var x_1_10212 = $std_text_parse.hex_digits();
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(_mlift_phexdouble_10175);
      }
      else {
         
        var x_2_10215 = $std_text_parse._lp__bar__bar__rp_(function() {
             
            var x_3_10218 = $std_text_parse.char(0x002E);
            if ($std_core_hnd._yielding()) {
              return $std_core_hnd.yield_extend(_mlift_phexdouble_10173);
            }
            else {
              return $std_text_parse.hex_digits();
            }
          }, function() {
            return "";
          });
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(_y_x10133 /* string */ ) {
            return _mlift_phexdouble_10174(x_1_10212, _y_x10133);
          });
        }
        else {
           
          var frac = $std_core_sslice.trim_right(x_2_10215, "0");
           
          var x_4_10220 = $std_text_parse._lp__bar__bar__rp_(function() {
               
              var x_5_10223 = $std_text_parse.one_of("pP");
              if ($std_core_hnd._yielding()) {
                return $std_core_hnd.yield_extend(_mlift_phexdouble_10171);
              }
              else {
                return $std_text_parse.pint();
              }
            }, function() {
              return 0;
            });
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(function(exp /* int */ ) {
              return _mlift_phexdouble_10172(frac, x_1_10212, exp);
            });
          }
          else {
             
            var _x_x1_0_10166 = $std_core_types._lp__plus__plus__rp_(x_1_10212, frac);
             
            var _x_x1_10164 = $std_core_hnd._open_none2(function(s /* string */ , hex /* ? bool */ ) {
                var _x24 = (hex !== undefined) ? hex : false;
                return $std_core_int.xparse(s, _x24);
              }, _x_x1_0_10166, true);
             
            var m_0 = $std_core_hnd._open_none2(function(m /* maybe<int> */ , nothing /* int */ ) {
                return (m === null) ? nothing : m.value;
              }, _x_x1_10164, 0);
             
            var y_10161 = $std_core_types._int_mul(4,($std_core_string.chars_fs_count(frac)));
             
            var e = $std_core_types._int_sub(x_4_10220,y_10161);
            return $std_core_hnd._open_none2(function(man_0 /* int */ , exp_0_0 /* int */ ) {
                return ldexp($std_core_types._int_to_double(man_0), exp_0_0);
              }, m_0, e);
          }
        }
      }
    }
  }
}
 
 
// Return `nan` on failure
export function prim_parse_float64(s) /* (s : string) -> float64 */  {
  return parseFloat(s);
}
 
 
// monadic lift
export function _mlift_pdecdouble_10178(wild___0) /* (wild_@0 : char) -> std/text/parse/parse int */  {
  return $std_text_parse.pint();
}
 
 
// monadic lift
export function _mlift_pdecdouble_10179(cur, exp) /* (cur : sslice/sslice, exp : int) -> std/text/parse/parse float64 */  {
   
  var _x_x1_10170 = $std_core_sslice.string(cur);
  return $std_core_hnd._open_none1(prim_parse_float64, _x_x1_10170);
}
 
 
// monadic lift
export function _mlift_pdecdouble_10180(wild__) /* (wild_ : char) -> std/text/parse/parse string */  {
  return $std_text_parse.digits0();
}
 
 
// monadic lift
export function _mlift_pdecdouble_10181(cur, _y_x10141) /* (cur : sslice/sslice, string) -> std/text/parse/parse float64 */  {
   
  var frac = $std_core_sslice.trim_right(_y_x10141, "0");
   
  var x_10225 = $std_text_parse._lp__bar__bar__rp_(function() {
       
      var x_0_10227 = $std_text_parse.one_of("eE");
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(_mlift_pdecdouble_10178);
      }
      else {
        return $std_text_parse.pint();
      }
    }, function() {
      return 0;
    });
   
  function next_10226(exp) /* (int) -> std/text/parse/parse float64 */  {
     
    var _x_x1_10170 = $std_core_sslice.string(cur);
    return $std_core_hnd._open_none1(prim_parse_float64, _x_x1_10170);
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(next_10226);
  }
  else {
    return next_10226(x_10225);
  }
}
 
 
// monadic lift
export function _mlift_pdecdouble_10182(cur, man) /* (cur : sslice/sslice, man : string) -> std/text/parse/parse float64 */  {
   
  var x_10231 = $std_text_parse._lp__bar__bar__rp_(function() {
       
      var x_0_10233 = $std_text_parse.char(0x002E);
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(_mlift_pdecdouble_10180);
      }
      else {
        return $std_text_parse.digits0();
      }
    }, function() {
      return "";
    });
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(_y_x10141 /* string */ ) {
      return _mlift_pdecdouble_10181(cur, _y_x10141);
    });
  }
  else {
    return _mlift_pdecdouble_10181(cur, x_10231);
  }
}
 
 
// monadic lift
export function _mlift_pdecdouble_10183(cur) /* (cur : sslice/sslice) -> std/text/parse/parse float64 */  {
   
  var x_10235 = $std_text_parse.digits();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(man /* string */ ) {
      return _mlift_pdecdouble_10182(cur, man);
    });
  }
  else {
    return _mlift_pdecdouble_10182(cur, x_10235);
  }
}
 
export function pdecdouble() /* () -> std/text/parse/parse float64 */  {
   
  var ev_10240 = $std_core_hnd._evv_at(0);
   
  var _x24 = $std_text_parse.current_input_fs__select(ev_10240.hnd);
  var x_10237 = _x24(ev_10240.marker, ev_10240);
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_pdecdouble_10183);
  }
  else {
     
    var x_0_10242 = $std_text_parse.digits();
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(man /* string */ ) {
        return _mlift_pdecdouble_10182(x_10237, man);
      });
    }
    else {
       
      var x_1_10245 = $std_text_parse._lp__bar__bar__rp_(function() {
           
          var x_2_10248 = $std_text_parse.char(0x002E);
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(_mlift_pdecdouble_10180);
          }
          else {
            return $std_text_parse.digits0();
          }
        }, function() {
          return "";
        });
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(function(_y_x10141 /* string */ ) {
          return _mlift_pdecdouble_10181(x_10237, _y_x10141);
        });
      }
      else {
         
        var frac = $std_core_sslice.trim_right(x_1_10245, "0");
         
        var x_3_10250 = $std_text_parse._lp__bar__bar__rp_(function() {
             
            var x_4_10253 = $std_text_parse.one_of("eE");
            if ($std_core_hnd._yielding()) {
              return $std_core_hnd.yield_extend(_mlift_pdecdouble_10178);
            }
            else {
              return $std_text_parse.pint();
            }
          }, function() {
            return 0;
          });
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(function(exp /* int */ ) {
             
            var _x_x1_10170 = $std_core_sslice.string(x_10237);
            return $std_core_hnd._open_none1(prim_parse_float64, _x_x1_10170);
          });
        }
        else {
           
          var _x_x1_10170_0 = $std_core_sslice.string(x_10237);
          return $std_core_hnd._open_none1(prim_parse_float64, _x_x1_10170_0);
        }
      }
    }
  }
}
 
 
// monadic lift
export function _mlift_pspecial_10184(wild__) /* (wild_ : string) -> std/text/parse/parse float64 */  {
  return nan;
}
 
 
// monadic lift
export function _mlift_pspecial_10185(wild___0) /* (wild_@0 : string) -> std/text/parse/parse float64 */  {
  return posinf;
}
 
 
// monadic lift
export function _mlift_pspecial_10186(wild___1) /* (wild_@1 : string) -> std/text/parse/parse float64 */  {
  return posinf;
}
 
export function pspecial() /* () -> std/text/parse/parse float64 */  {
  return $std_text_parse.choose($std_core_types.Cons(function() {
       
      var x_10257 = $std_text_parse.pstring("nan");
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(_mlift_pspecial_10184);
      }
      else {
        return nan;
      }
    }, $std_core_types.Cons(function() {
         
        var x_0_10259 = $std_text_parse.pstring("infinity");
        if ($std_core_hnd._yielding()) {
          return $std_core_hnd.yield_extend(_mlift_pspecial_10185);
        }
        else {
          return posinf;
        }
      }, $std_core_types.Cons(function() {
           
          var x_1_10261 = $std_text_parse.pstring("inf");
          if ($std_core_hnd._yielding()) {
            return $std_core_hnd.yield_extend(_mlift_pspecial_10186);
          }
          else {
            return posinf;
          }
        }, $std_core_types.Nil))));
}
 
 
// monadic lift
export function _mlift_pdouble_10187(wild__) /* (wild_ : string) -> std/text/parse/parse float64 */  {
  return nan;
}
 
 
// monadic lift
export function _mlift_pdouble_10188(wild___0) /* (wild_@0 : string) -> std/text/parse/parse float64 */  {
  return posinf;
}
 
 
// monadic lift
export function _mlift_pdouble_10189(wild___1) /* (wild_@1 : string) -> std/text/parse/parse float64 */  {
  return posinf;
}
 
 
// monadic lift
export function _mlift_pdouble_10190(neg, d) /* (neg : bool, d : float64) -> std/text/parse/parse float64 */  {
  return (neg) ? (-d) : d;
}
 
 
// monadic lift
export function _mlift_pdouble_10191(neg) /* (neg : bool) -> std/text/parse/parse float64 */  {
   
  var x_10263 = $std_text_parse.choose($std_core_types.Cons(phexdouble, $std_core_types.Cons(pdecdouble, $std_core_types.Cons(function() {
          return $std_text_parse.choose($std_core_types.Cons(function() {
               
              var x_0_10265 = $std_text_parse.pstring("nan");
              if ($std_core_hnd._yielding()) {
                return $std_core_hnd.yield_extend(_mlift_pdouble_10187);
              }
              else {
                return nan;
              }
            }, $std_core_types.Cons(function() {
                 
                var x_1_10267 = $std_text_parse.pstring("infinity");
                if ($std_core_hnd._yielding()) {
                  return $std_core_hnd.yield_extend(_mlift_pdouble_10188);
                }
                else {
                  return posinf;
                }
              }, $std_core_types.Cons(function() {
                   
                  var x_2_10269 = $std_text_parse.pstring("inf");
                  if ($std_core_hnd._yielding()) {
                    return $std_core_hnd.yield_extend(_mlift_pdouble_10189);
                  }
                  else {
                    return posinf;
                  }
                }, $std_core_types.Nil))));
        }, $std_core_types.Cons(function() {
            return 0.0;
          }, $std_core_types.Nil)))));
   
  function next_10264(d) /* (float64) -> std/text/parse/parse float64 */  {
    return (neg) ? (-d) : d;
  }
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(next_10264);
  }
  else {
    return next_10264(x_10263);
  }
}
 
export function pdouble() /* () -> std/text/parse/parse float64 */  {
   
  var x_10273 = $std_text_parse.sign();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(_mlift_pdouble_10191);
  }
  else {
     
    var x_0_10276 = $std_text_parse.choose($std_core_types.Cons(phexdouble, $std_core_types.Cons(pdecdouble, $std_core_types.Cons(function() {
            return $std_text_parse.choose($std_core_types.Cons(function() {
                 
                var x_1_10279 = $std_text_parse.pstring("nan");
                if ($std_core_hnd._yielding()) {
                  return $std_core_hnd.yield_extend(_mlift_pdouble_10187);
                }
                else {
                  return nan;
                }
              }, $std_core_types.Cons(function() {
                   
                  var x_2_10281 = $std_text_parse.pstring("infinity");
                  if ($std_core_hnd._yielding()) {
                    return $std_core_hnd.yield_extend(_mlift_pdouble_10188);
                  }
                  else {
                    return posinf;
                  }
                }, $std_core_types.Cons(function() {
                     
                    var x_3_10283 = $std_text_parse.pstring("inf");
                    if ($std_core_hnd._yielding()) {
                      return $std_core_hnd.yield_extend(_mlift_pdouble_10189);
                    }
                    else {
                      return posinf;
                    }
                  }, $std_core_types.Nil))));
          }, $std_core_types.Cons(function() {
              return 0.0;
            }, $std_core_types.Nil)))));
    if ($std_core_hnd._yielding()) {
      return $std_core_hnd.yield_extend(function(d /* float64 */ ) {
        return (x_10273) ? (-d) : d;
      });
    }
    else {
      return (x_10273) ? (-x_0_10276) : x_0_10276;
    }
  }
}
 
 
// monadic lift
export function _mlift_parse_float64_10192(x, wild__) /* (x : float64, wild_ : ()) -> std/text/parse/parse float64 */  {
  return x;
}
 
 
// monadic lift
export function _mlift_parse_float64_10193(x) /* (x : float64) -> std/text/parse/parse float64 */  {
   
  var x_0_10287 = $std_text_parse.eof();
  if ($std_core_hnd._yielding()) {
    return $std_core_hnd.yield_extend(function(wild__ /* () */ ) {
      return x;
    });
  }
  else {
    return x;
  }
}
 
 
// Parse a float64 number. Can be "NaN", "Inf(inity)" (case-insensitive),
// a fix-point number (`1.2`) or in scientific notation (`-2.3e-5`).
// Also allows floats in [hexadecimal notation](https://books.google.com/books?id=FgMsCwAAQBAJ&pg=PA41) (`0xA.Fp-10`) that can
// be represented precisely (and are the preferred _round trip_ format).
export function parse_float64(s) /* (s : string) -> maybe<float64> */  {
   
  var s_0_10108 = $std_core_string.to_lower((((((s).replace(/^\s\s*/,'')))).replace(/\s+$/,'')));
   
  var input_10106 = $std_core_sslice.Sslice(s_0_10108, 0, s_0_10108.length);
   
  var perr_10105 = $std_text_parse.parse(input_10106, function() {
       
      var x_10291 = pdouble();
      if ($std_core_hnd._yielding()) {
        return $std_core_hnd.yield_extend(_mlift_parse_float64_10193);
      }
      else {
        return _mlift_parse_float64_10193(x_10291);
      }
    });
  if (perr_10105._tag === 1) {
    return $std_core_types.Just(perr_10105.result);
  }
  else {
    return $std_core_types.Nothing;
  }
}
 
 
// Return the sum of a list of floats.
// Uses [Kahan-Babu&scaron;kan-Neumaier summation](https://en.wikipedia.org/wiki/Kahan_summation_algorithm#Further_enhancements)
// to minimize rounding errors. This
// is more precise as Kahan summation and about as fast.\
// `[1.0e3,1.0e97,1.0e3,-1.0e97].sum == 2000.0`\
// while\
// `[1.0e3,1.0e97,1.0e3,-1.0e97].foldl(0.0,(+)) == 0.0` (!)\
// A. Neumaier, _Rundungsfehleranalyse einiger Verfahren zur Summation endlicher Summen_.
// Math. Mechanik, 54:39--51, 1974.
export function sum(xs) /* (xs : list<float64>) -> float64 */  {
  return function() {
     
    var loc = { value: (0.0) };
     
    var loc_0 = { value: (0.0) };
     
    $std_core_list.foreach(xs, function(x /* float64 */ ) {
         
        var t = ((((loc).value)) + x);
         
        var _x24 = ((Math.abs((((loc).value)))) >= (Math.abs(x)));
        if (_x24) {
          var c = ((((((loc).value)) - t)) + x);
        }
        else {
          var c = (((x - t)) + (((loc).value)));
        }
         
        ((loc_0).value = (((((loc_0).value)) + c)));
        return ((loc).value = t);
      });
     
    var res_0 = ((((loc).value)) + (((loc_0).value)));
     
    var res = $std_core_hnd.prompt_local_var(loc_0, res_0);
    return $std_core_hnd.prompt_local_var(loc, res);
  }();
}
 
 
// The hypotenuse of `x` and `y`: `sqrt(x*x + y*y)`.
// Prevents overflow for large numbers.
export function hypot(x, y) /* (x : float64, y : float64) -> float64 */  {
   
  var xx = Math.abs(x);
   
  var yy = Math.abs(y);
   
  var lo = ((xx <= yy)) ? xx : yy;
   
  var hi = ((xx >= yy)) ? xx : yy;
  if ((hi === (0.0))) {
    return 0.0;
  }
  else {
     
    var z = (lo / hi);
    return (hi * (Math.sqrt((((1.0) + ((z * z)))))));
  }
}
 
 
// The square of a float64
export function sqr(x) /* (x : float64) -> float64 */  {
  return (x * x);
}
 
 
// The square root of the sum of the squares of three floats.
// Prevents overflow for large numbers.
export function xyz_fs_hypot(x, y, z) /* (x : float64, y : float64, z : float64) -> float64 */  {
   
  var xx = Math.abs(x);
   
  var yy = Math.abs(y);
   
  var zz = Math.abs(z);
   
  var x_0_10115 = ((xx >= yy)) ? xx : yy;
   
  var hi = ((x_0_10115 >= zz)) ? x_0_10115 : zz;
  if ((hi === (0.0))) {
    return 0.0;
  }
  else {
     
    var x_4_10121 = (zz / hi);
     
    var x_2_10119 = (xx / hi);
     
    var x_3_10120 = (yy / hi);
    return (hi * (Math.sqrt(((((((x_2_10119 * x_2_10119)) + ((x_3_10120 * x_3_10120)))) + ((x_4_10121 * x_4_10121)))))));
  }
}
 
 
// The maximum of the absolute values.
export function abs_max(x, y) /* (x : float64, y : float64) -> float64 */  {
   
  var x_0_10122 = Math.abs(x);
   
  var y_0_10123 = Math.abs(y);
  return ((x_0_10122 >= y_0_10123)) ? x_0_10122 : y_0_10123;
}
 
 
// The maximum of a list of absolute values.
export function list_fs_abs_max(xs) /* (xs : list<float64>) -> float64 */  {
  return $std_core_list.foldl(xs, 0.0, function(m /* float64 */ , x /* float64 */ ) {
       
      var x_0_10124 = Math.abs(x);
      return ((x_0_10124 >= m)) ? x_0_10124 : m;
    });
}
 
 
// The square root of the sum of squares of a list of floats.
// Prevents overflow for large numbers and uses Kahan-Babu&scaron;kan-Neumaier summation
// for precision.
export function list_fs_hypot(xs) /* (xs : list<float64>) -> float64 */  {
   
  var hi = $std_core_list.foldl(xs, 0.0, function(m /* float64 */ , x /* float64 */ ) {
       
      var x_0_10124 = Math.abs(x);
      return ((x_0_10124 >= m)) ? x_0_10124 : m;
    });
  if ((hi === (0.0))) {
    return 0.0;
  }
  else {
    return (hi * (Math.sqrt((sum($std_core_list.map(xs, function(x_0 /* float64 */ ) {
         
        var x_0_10126 = (x_0 / hi);
        return (x_0_10126 * x_0_10126);
      }))))));
  }
}
 
export var rad2deg;
var rad2deg = ((180.0) / (3.141592653589793));
 
export var deg2rad;
var deg2rad = ((3.141592653589793) / (180.0));
 
 
// Convert radians to degrees.
export function deg(rad_0) /* (rad : float64) -> float64 */  {
  return (rad_0 * rad2deg);
}
 
 
// Convert degrees to radians.
export function rad(deg_0) /* (deg : float64) -> float64 */  {
  return (deg_0 * deg2rad);
}
 
 
// Return `x` with the sign of `y`.
export function with_sign_of(x, y) /* (x : float64, y : float64) -> float64 */  {
  if ((y < (0.0))) {
    return (-(Math.abs(x)));
  }
  else {
    return Math.abs(x);
  }
}
 
 
// The area hyperbolic tangent of `d`
export function atanh(d) /* (d : float64) -> float64 */  {
  return Math.atanh(d);
}
 
 
// The area hyperbolic cosine of `d`
export function acosh(d) /* (d : float64) -> float64 */  {
  return Math.acosh(d);
}
 
 
// The area hyperbolic sine of `d`
export function asinh(d) /* (d : float64) -> float64 */  {
  return Math.asinh(d);
}