// Koka generated module: std/core/types, koka version: 3.2.4
"use strict";
 
// imports
 
// externals
/*---------------------------------------------------------------------------
  Copyright 2020-2024, Microsoft Research, Daan Leijen.
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
/* assign here so inlined primitives are available in std/core/int itself */
const $std_core_types = {
  "_int_clamp32": _int_clamp32,
  "_int_from_int32": _int_from_int32,
  "Tuple2": Tuple2
}
/*
const $std_core_types = {
            // integer operations that will be inlined
              "_int_string": _int_string
            , "_int_const": _int_const
            , "_int_double": _int_double
            , "_int_clamp8": _int_clamp8
            , "_int_clamp16": _int_clamp16
            , "_int_clamp32": _int_clamp32
            , "_int_clamp64": _int_clamp64
            , "_int_clamp_byte": _int_clamp_byte
            , "_int_from_int32": _int_from_int32
            , "_int_from_int64": _int_from_int64
            , "_int_to_double": _int_to_double
            , "_int_to_float": _int_to_float
            , "_int_iszero": _int_iszero
            , "_int_isodd": _int_isodd
            , "_int_negate": _int_negate
            , "_int_abs": _int_abs
            , "_int_sign": _int_sign
            , "_int_add": _int_add
            , "_int_sub": _int_sub
            , "_int_mul": _int_mul
            , "_int_div": _int_div
            , "_int_mod": _int_mod
            , "_int_divmod": _int_divmod
            , "_int_compare": _int_compare
            , "_int_eq": _int_eq
            , "_int_ne": _int_ne
            , "_int_gt": _int_gt
            , "_int_ge": _int_ge
            , "_int_lt": _int_lt
            , "_int_le": _int_le
            , "_int_parse": _int_parse
            // todo: move to std/num/float64
            , "_double_to_float": _double_to_float
            , "_double_to_int32": _double_to_int32
            , "_double_round_even": _double_round_even
            };
*/
/*------------------------------------------------
  32-bit integer operations
--------------------------------------------------*/
export function _int32_multiply(x,y) {
  // todo: should this be: return ((x*y) & 0xFFFFFFFF);
  var xhi = (x >> 16) & 0xFFFF;
  var xlo = x & 0xFFFF;
  var yhi = (y >> 16) & 0xFFFF;
  var ylo = y & 0xFFFF;
  var hi  = ((xhi * ylo) + (xlo * yhi));
  return (((hi << 16) + (xlo * ylo))|0);
}
export function _int32_rotl(x,y) {
  const shift = y & 31;
  return ((x << shift) | (x >>> (32 - shift)));
}
export function _int32_rotr(x,y) {
  const shift = y & 31;
  return ((x >>> shift) | (x << (32 - shift)));
}
export function _int32_clz(x) {
  return Math.clz32(x);
}
export function _int32_ctz(x) {
  var i = (x|0);
  if (i === 0) return 32;
  i = (i & ((~i) + 1));  // keep only least significant bit
  return ((31 - Math.clz32(i))|0);
}
export function _int32_clrsb(x) {
  var i = (x|0);
  i = i ^ (i >> 31);
  return (i===0 ? 31 : _int32_clz(i) - 1);
}
export function _int32_parity(x) {
  var i = x|0;
  i ^= (i >>> 16);
  i ^= (i >>> 8);
  i ^= (i >>> 4);
  i &= 0x0F;
  return (((0x6996 >> x) & 1) === 0);  // 0x6996 = 0b0110100110010110  == "mini" 16 bit lookup table with a bit set if the value has non-even parity
}
export function _int32_popcount(x) {  // : int
  var i = (x|0);
  i = i - ((i >> 1) & 0x55555555);
  i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
  i = (i + (i >> 4)) & 0x0F0F0F0F;
  i = i + (i >> 8);
  i = i + (i >> 16);
  return (i & 0x3F);
}
export function _int32_bswap(x) {
  var i = (x|0);
  return (((i&0xFF)<<24)|((i&0xFF00)<<8)|((i&0xFF0000)>>>8)|((i&0xFF000000)>>>24));
}
export function _int32_breverse(x) {
  // from: http://graphics.stanford.edu/~seander/bithacks.html#ReverseParallel
  var i = (x|0);
  i = ((i >>> 1) & (0x55555555)) | ((i & (0x55555555)) << 1); // swap odd and even bits
  i = ((i >>> 2) & (0x33333333)) | ((i & (0x33333333)) << 2); // swap 2-bit pairs
  i = ((i >>> 4) & (0x0F0F0F0F)) | ((i & (0x0F0F0F0F)) << 4); // swap 4-bit nibbles
  return _int32_bswap(i);
}
const _int65 = 0x10000000000000000n;
export function _int64_shr(x,y) {
  const shift = y & 63n;
  if (shift === 0n) {
    return x;
  }
  else if (x >= 0n) {
    return (x >> shift);
  }
  else {
    const i = (x + _int65) >> shift;  // make positive (in 65 bits) and shift
    return BigInt.asIntN(64, i);
  }
}
export function _int64_sar(x,y) {
  const shift = y & 63n;
  return (x >> shift);
}
export function _int64_shl(x,y) {
  const shift = y & 63n;
  return BigInt.asIntN(64, x << shift);
}
export function _int64_rotl(x,y) {
  const shift = y & 63n;
  const lo = _int64_shr(x, 64n - shift);
  return BigInt.asIntN(64, (x << shift) | lo);
}
export function _int64_rotr(x,y) {
  return _int64_rotl(x, 64n - y);
}
export function _int64_from_uint32(x) {
  return (x >= 0 ? BigInt(x) : 0x100000000n + BigInt(x))
}
export function _int64_from_int32(x) {
  return BigInt(x);
}
function _int64_hi(x) {
  return (Number( (x>>32n) & 0xFFFFFFFFn ) | 0);
}
function _int64_lo(x) {
  return (Number( x & 0xFFFFFFFFn ) | 0);
}
export function _int64_hi_lo(hi,lo) {
  return (_int64_from_int32(hi) << 32n) + _int64_from_uint32(lo);
}
export function _int64_ctz(x) {
  const lo = _int64_lo(x);
  if (lo === 0) {
    const hi = _int64_hi(x);
    return (32 + _int32_ctz(hi));
  }
  else {
    return (_int32_ctz(lo));
  }
}
export function _int64_clz(x) {
  const hi = _int64_hi(x);
  if (hi === 0) {
    const lo = _int64_lo(x);
    return (32 + _int32_clz(lo));
  }
  else {
    return (_int32_clz(hi));
  }
}
export function _int64_clrsb(x) {
  const hi = _int64_hi(x);
  const lo = _int64_lo(x);
  if (hi === 0) {
    return (lo < 0 ? 31 : 32 + _int32_clrsb(lo));
  }
  else if (hi === -1) {
    return (lo >= 0 ? 31 : 32 + _int32_clrsb(lo));
  }
  else {
    return (_int32_clrsb(hi));
  }
}
export function _int64_parity(x) {
  const hi = _int64_hi(x);
  const lo = _int64_lo(x);
  const i = (lo ^ hi);
  return _int32_parity(i);
}
export function _int64_popcount(x) {
  const hi = _int64_hi(x);
  const lo = _int64_lo(x);
  return (_int32_popcount(hi) + _int32_popcount(lo));
}
export function _int64_bswap(x) {
  const hi = _int64_hi(x);
  const lo = _int64_lo(x);
  return _int64_hi_lo(_int32_bswap(lo),_int32_bswap(hi));
}
export function _int64_breverse(x) {
  const hi = _int64_hi(x);
  const lo = _int64_lo(x);
  return _int64_hi_lo(_int32_breverse(lo),_int32_breverse(hi));
}
const _max_uint32n =  0xFFFFFFFFn;
const _max_int32n  =  0x7FFFFFFFn;
const _min_int32n  = -0x80000000n;
export function _int64_clamp_int32(x) {
  return Number( x > _max_int32n ? _max_int32n : (x < _min_int32n ? _min_int32n : x) );
}
export function _int64_clamp_uint32(x) {
  return Number(x > _max_uint32n ? -1 : (x < 0 ? 0 : (x <= _max_int32n ? x : x - 0x100000000n)));
}
export function _int64_imul( x, y ) {
  const z = x*y; BigInt.asUintN(64,z);
  const lo = BigInt.asUintN(64,z);
  const hi = z >> 64n;
  return $std_core_types.Tuple2(hi,lo);
}
function _int64_as_uint64(x) {
  return (x < 0 ? x + 0x10000000000000000n : x);
}
export function _int64_umul( x, y ) {
  return _int64_imul(_int64_as_uint64(x), _int64_as_uint64(y));
}
export function _int32_imul(x,y) {
  const z = BigInt(x) * BigInt(y);
  const lo = Number(BigInt.asUintN(32,z));
  const hi = Number(BigInt.asIntN(32,z >> 32n));
  return $std_core_types.Tuple2(hi,lo);
}
function _int32_as_uint32(x) {
  return (x < 0 ? x + 0x100000000 : x);
}
export function _int32_umul( x, y ) {
  return _int32_imul(_int32_as_uint32(x), _int32_as_uint32(y));
}
/*------------------------------------------------
  double arithmetic
------------------------------------------------*/
export var _double_trunc = Math.trunc || function(x){ return x - x%1; };
// Round a double with rounding to even on a tie.
export function _double_round_even(d) {
  const r = Math.round(d); // rounds to +Infinity on a tie
  if (Math.abs(d - r) == 0.5) {
    // exactly in-between, round to the nearest even number
    return 2.0*Math.round(d/2.0);
  }
  else {
    return r;
  }
}
/*------------------------------------------------
  integer arithmetic
------------------------------------------------*/
// We represent integers as a regular number as long as it is within _min_precise and _max_precise.
// Outside that we use Integer objects.
// An Integer is just a wrapper around a BigInt; we use that so we can do faster basic
// operations. For example, for multiplication, we just multiply directly as `x * y`, and then check
// afterwards if it succeeded; if one of `x` or `y` was an `Integer` it will have been
// cast to `NaN` (or something like `[object Object]1`) and we can fall back to big integer arithmetic.
// This is expensive if big integers dominate but we expect the vast majority of operations to work
// on "small" integers. (Note that we cannot use BigInt directly as `x * y` in such case could lead
// to type error exceptions.)
const _max_precise = 9007199254740991; // 2^53 -1
const _min_precise = -_max_precise;
const _max_int32 =  0x7FFFFFFF;
const _min_int32 = -0x80000000;
const _max_int64 =  0x7FFFFFFFFFFFFFFFn;
const _min_int64 = -0x8000000000000000n;
// is a number small?
function _is_small(x) {
  return (x >= _min_precise && x <= _max_precise);
}
const Integer = (function(){
  function Integer(x) {
    if (x instanceof Integer) {
      this.value = x.value;
    }
    else {
      this.value = BigInt(x);
    }
  }
  Integer.prototype.to_number = function() {
    const x = Number(this.value);
    return (isNaN(x) ? 0 : x);
  }
  Integer.prototype.toString = function(radix) {
    return this.value.toString(radix);
  }
  Integer.prototype.valueOf = function() {
    return NaN; // important for optimized functions
  }
  return Integer;
})();
export function _int(x) {
  return (_is_small(x) ? Number(x) : new Integer(x));
}
export function _big(x) {
  return (x instanceof Integer ? x.value : BigInt(x));
}
function _integer_add(x,y) {
  return _int( _big(x) + _big(y) );
}
function _integer_sub(x,y) {
  return _int( _big(x) - _big(y) );
}
function _integer_mul(x,y) {
  return _int( _big(x) * _big(y) );
}
function _integer_pow(x,y) {
  return _int( _big(x) ** _big(y) );
}
function _integer_cdiv(x,y) {
  return _int( _big(x) / _big(y) );
}
function _integer_cmod(x,y) {
  return _int( _big(x) % _big(y) );
}
function _integer_cdivmod(x,y) {
  const i = _big(x);
  const j = _big(y);
  return $std_core_types.Tuple2( _int(i/j), _int(i%j) );
}
function _integer_div(x,y) {
  const i = _big(x);
  const j = _big(y);
  const q = i / j;
  const r = i % j;
  return _int( r < 0n ? (j > 0n ? q - 1n : q + 1n) : q );
}
function _integer_mod(x,y) {
  const i = _big(x);
  const j = _big(y);
  const r = i % j;
  return _int( r < 0n ? (j > 0n ? r + j : r - j) : r );
}
function _integer_divmod(x,y) {
  const i = _big(x);
  const j = _big(y);
  var q = i / j;
  var r = i % j;
  if (r < 0n) {
    if (j > 0n) { q--; r += j; }
           else { q++; r -= j; }
  }
  return $std_core_types.Tuple2( _int(q), _int(r) );
}
function _integer_neg(x) {
  return _int( 0n - _big(x));
}
function _integer_inc(x) {
  return _int( _big(x) + 1n );
}
function _integer_dec(x) {
  return _int( _big(x) - 1n );
}
function _integer_abs(x) {
  const i = _big(x);
  return _int( i >= 0n ? i : 0n - i );
}
function _integer_compare(x,y) {
  const i = _big(x);
  const j = _big(y);
  return (i === j ? $std_core_types.Eq : (i > j ? $std_core_types.Gt : $std_core_types.Lt));
}
function _integer_lt(x,y) {
  return (_big(x) < _big(y));
}
function _integer_lte(x,y) {
  return (_big(x) <= _big(y));
}
function _integer_gt(x,y) {
  return (_big(x) > _big(y));
}
function _integer_gte(x,y) {
  return (_big(x) >= _big(y));
}
function _integer_eq(x,y) {
  return (_big(x) === _big(y));
}
function _integer_neq(x,y) {
  return (_big(x) !== _big(y));
}
function _integer_mul_pow10(x,n) {
  return _int( _big(x) * (10n ** _big(n)) );
}
function _integer_div_pow10(x,n) {
  return _integer_div( x, _int( 10n ** _big(n) ) );
}
function _integer_cdiv_pow10(x,n) {
  return _integer_cdiv( x, _int( 10n ** _big(n) ) );
}
function _integer_count_pow10(x) {
  const zeros = _big(x).toString().match(/(0+)n$/);
  return (zeros == null || zeros.length <= 1 ? 0 : zeros[1].length);
}
function _integer_count_digits(x) {
  const i = _big(x);
  const s = (i >= 0n ? i : 0n - i).toString();
  return (s.length - 1);
}
function _integer_is_odd(x) {
  const i = _big(x);
  if (_is_small(i)) {
    return ((Number(i) & 1) === 1 );
  }
  else {
    return ((i % 2n) === 1n);
  }
}
/*------------------------------------------------
  int arithmetic
------------------------------------------------*/
export function _int_add(x,y) {
  const z = x + y;
  return (_is_small(z) ? z : _integer_add(x,y));
}
export function _int_sub(x,y) {
  const z = x - y;
  return (_is_small(z) ? z : _integer_sub(x,y));
}
export function _int_mul(x,y) {
  const z = x * y;
  return (_is_small(z) ? z : _integer_mul(x,y));
}
export function _int_iszero(x) {
  return (x instanceof Integer ? x.value === 0n : x===0);
}
export function _int_isodd(x) {
  return (x instanceof Integer ? _integer_is_odd(x) : (x&1)===1);
}
export function _int_negate(x) {
  const z = 0 - x;
  return (_is_small(z) ? z : _integer_neg(x));
}
export function _int_abs(x) {
  return (x instanceof Integer ? _integer_abs(x) : Math.abs(x) );
}
export function _int_cdivmod(x,y) {
  const q = _double_trunc(x / y);
  if (!isNaN(q)) {
    return $std_core_types.Tuple2(q,(x%y));
  }
  else {
    return _integer_cdivmod(x,y);
  }
}
export function _int_cdiv(x,y) {
  const q = _double_trunc(x / y);
  return (!isNaN(q) ? q : _integer_cdiv(x,y));
}
export function _int_cmod(x,y) {
  const r = (x % y);
  return (!isNaN(r) ? r : _integer_cmod(x,y));
}
export function _int_divmod(x,y) {
  if (_int_iszero(y)) return 0;
  var q = _double_trunc(x / y);
  if (!isNaN(q)) {
    var r = x%y;
    if (r<0) {
      if (y>0) { q--; r += y; }
          else { q++; r -= y; }
    }
    return $std_core_types.Tuple2(q,r);
  }
  else {
    return _integer_divmod(x,y)
  }
}
export function _int_div(x,y) {
  if (_int_iszero(y)) return 0;
  const q = _double_trunc(x/y);
  if (!isNaN(q)) {
    const r = (x%y);
    return (r<0 ? (y>0 ? q-1 : q+1) : q);
  }
  else return _integer_div(x,y);
}
export function _int_mod(x,y) {
  if (_int_iszero(y)) return 0;
  const r = (x%y);
  if (!isNaN(r)) {
    return (r<0 ? (y>0 ? r+y : r-y) : r);
  }
  else return _integer_mod(x,y);
}
export function _int_compare(x,y) {
  const d = x - y;
  if (!isNaN(d)) {
    return (d>0 ? $std_core_types.Gt : (d<0 ? $std_core_types.Lt : $std_core_types.Eq));
  }
  else {
    return _integer_compare(x,y);
  }
}
export function _int_sign(x) {
  return _int_compare(x,0);
}
export function _int_eq(x,y)   {
  const d = x - y;
  if (!isNaN(d)) {
    return (d === 0);
  }
  else {
    return _integer_eq(x,y);
  }
}
export function _int_ne(x,y)   {
  const d = x - y;
  if (!isNaN(d)) {
    return (d !== 0);
  }
  else {
    return _integer_neq(x,y);
  }
}
export function _int_lt(x,y) {
  const d = x - y;
  if (!isNaN(d)) {
    return (d < 0);
  }
  else {
    return _integer_lt(x,y);
  }
}
export function _int_le(x,y) {
  const d = x - y;
  if (!isNaN(d)) {
    return (d <= 0);
  }
  else {
    return _integer_lte(x,y);
  }
}
export function _int_gt(x,y) {
  const d = x - y;
  if (!isNaN(d)) {
    return (d > 0);
  }
  else {
    return _integer_gt(x,y);
  }
}
export function _int_ge(x,y) {
  const d = x - y;
  if (!isNaN(d)) {
    return (d >= 0);
  }
  else {
    return _integer_gte(x,y);
  }
}
export function _int_pow(i,exp) {
	if (_is_small(i)) {
		var j = Math.pow(i);
		if (_is_small(j)) return j;
	}
	return _integer_pow(i,exp);
}
export function _int_mul_pow10(i,n) {
  const s = _int_sign(n);
  if (s === 0) return i;
  if (s < 0) return _int_cdiv_pow10(i, _int_negate(n) );
  return (_is_small(i) && n <= 14 ? _int_mul(i,Math.pow(10,n)) : _integer_mul_pow10(i,n) );
}
export function _int_cdiv_pow10(i,n) {
  const s = _int_sign(n);
  if (s === 0) return i;
  if (s < 0) return _int_mul_pow10(i, _int_negate(n) );
  return (_is_small(i) && n <= 14 ? _int_cdiv(i,Math.pow(10,n)) : _integer_cdiv_pow10(i,n) );
}
function _count_pow10( x ) {
  var j = 0;
  while(x!==0) {
    var m = x%10;
    if (m===0) { j++; }
          else break;
    x = x/10;
  }
  return j;
}
export function _int_count_pow10(i) {
  return (_is_small(i) ? _count_pow10(i) : _integer_count_pow10(i) );
}
function _count_digits8( x ) {  // only for -1e8 < x < 1e8
  if (x===0) return 0;
  x = Math.abs(x)
  if (x < 1e4) { // 1 - 4
    if (x < 1e2) return (x < 10 ? 1 : 2);
            else return (x < 1000 ? 3 : 4);
  }
  else { // 5 - 8
    if (x < 1e6) return (x < 1e5 ? 5 : 6);
            else return (x < 1e7 ? 7 : 8);
  }
}
export function _int_count_digits(i) {
  return (i > -1e8 && i < 1e8 ? _count_digits8(i) : _integer_count_digits(i) );
}
// create an int from a string.
export function _int_string(s) {
  if (s.length < 15) return parseInt(s);
                else return _int( BigInt(s) );
}
// create an int from a big int
export function _int_const(i) {
  return _int(i);
}
// create an int from a double.
export function _int_double(x) {
  if (_is_small(x)) return _double_round_even(x);
  if (isFinite(x)) return new Integer(x);
  if (x===Infinity) return _max_int32;
  if (x===-Infinity) return _min_int32;
  return 0;
}
function _int_to_number(x) {
  return (x instanceof Integer ? x.to_number() : x);
}
export function _int_clamp8(x) {
  const v = _int_to_number(x);
  if (v > 127) return 127;
  if (v < -128) return -128;
  return (v|0);
}
export function _int_clamp16(x) {
  const v = _int_to_number(x);
  if (v > 32767) return 32767;
  if (v < -32768) return -32768;
  return (v|0);
}
// Clamp a big integer into a 32 bit integer range.
export function _int_clamp32(x) {
  const v = _int_to_number(x);
  if (v > _max_int32) return _max_int32;
  if (v < _min_int32) return _min_int32;
  return (v|0);
}
export function _int_from_int32(x) {
  // console.log("int_from_int32: " + x + ": " + typeof x)
  return x;
}
export function _int_clamp64(x) {
  if (_is_small(x)) return BigInt(x);
  const v = _big(x);
  if (v > _max_int64) return _max_int64;
  if (v < _min_int64) return _min_int64;
  return v;
}
export function _int_from_int64(x) {
  return _int(x);
}
export function _int_clamp_byte(x) {
  const v = _int_to_number(x);
  if (v > 255) return 255;
  if (v < 0) return 0;
  return (v|0);
}
// Clamp a double into a 32 bit integer range.
export function _double_to_int32(x) {
  if (x > _max_int32) return _max_int32;
  if (x < _min_int32) return _min_int32;
  if (isNaN(x)) return 0;
  return (x|0);
}
export function _int_to_double(x) {
  return (x instanceof Integer ? x.to_number() : x);
}
var _buf_float32 = null;
export function _double_to_float(d) {
  if (Math.fround) {
    return Math.fround(d);
  }
  else {
    if (_buf_float32 === null) {
      _buf_float32 = new Float32Array(1);
    }
    _buf_float32[0] = d;
    return _buf_float32[0];
  }
}
export function _int_to_float(x) {
  return _double_to_float( _int_to_double(x) );
}
export function _int_showhex(x,upper) {
  const s = x.toString(16);
  return (upper ? s.toUpperCase() : s);
}
/*---------------------------------------------------------------------------
  Copyright 2012-2023, Microsoft Research, Daan Leijen.
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
export function _cctx_empty() {
  return _Cctx(undefined,{obj:undefined,field_name:""})
}
export function _cctx_create(res,field_addr) {
  return _Cctx(res,field_addr);
}
export function _cctx_extend(acc,res,field_addr) {
  if (acc.res===undefined) {
    return _Cctx(res,field_addr);
  }
  else {
    acc.holeptr.obj[acc.holeptr.field_name] = res;
    return _Cctx(acc.res,field_addr);
  }
}
export function _cctx_compose(ctx1,ctx2) {
  if (ctx2.res==undefined) {
    return ctx1;
  }
  else {
    return _cctx_extend(ctx1,ctx2.res,ctx2.holeptr);
  }
}
export function _cctx_apply(acc,res) {
  if (acc.res===undefined) {
    return res;
  }
  else {
    acc.holeptr.obj[acc.holeptr.field_name] = res;
    return acc.res;
  }
}
 
// type declarations
// type @field-addr
// type @optional
export function _OptArg(value) /* forall<a> (value : a) -> ? a */  {
  return value;
}
export const _NoOptArg = undefined; // forall<a> ? a
// type @reuse
// type @valueop
export const _Valueop = { _tag: 1 }; // forall<e,a> -> e a
// type alloc
// type any
// type bool
export const False = false;
export const True = true;
// type box
export function Box(unbox) /* forall<a> (unbox : a) -> box<a> */  {
  return unbox;
}
// type cctx
export function _Cctx(res, holeptr) /* forall<a,b> (res : a, holeptr : @field-addr<b>) -> cctx<a,b> */  {
  return { res: res, holeptr: holeptr };
}
// type char
// type div
// type effect-extend
// type either
export function Left(left) /* forall<a,b> (left : a) -> either<a,b> */  {
  return { _tag: 1, left: left };
}
export function Right(right) /* forall<a,b> (right : b) -> either<a,b> */  {
  return { _tag: 2, right: right };
}
// type float32
// type float64
// type global
// type handled
// type handled1
// type hdiv
export const _Hdiv = { _tag: 1 }; // forall<h,a,e> hdiv<h,a,e>
export const _Hnodiv = { _tag: 2 }; // forall<h,a,e> hdiv<h,a,e>
// type int
// type int16
// type int32
// type int64
// type int8
// type intptr_t
// type list
export const Nil = null; // forall<a> list<a>
export function Cons(head, tail) /* forall<a> (head : a, tail : list<a>) -> list<a> */  {
  return { head: head, tail: tail };
}
// type local
// type local-var
// type maybe
export const Nothing = null; // forall<a> maybe<a>
export function Just(value) /* forall<a> (value : a) -> maybe<a> */  {
  return { value: value };
}
// type maybe2
export const Nothing2 = { _tag: 1 }; // forall<a,b> maybe2<a,b>
export function Just2(fst, snd) /* forall<a,b> (fst : a, snd : b) -> maybe2<a,b> */  {
  return { _tag: 2, fst: fst, snd: snd };
}
// type ndet
// type nhandled
// type nhandled1
// type order
export const Lt = 1; // order
export const Eq = 2; // order
export const Gt = 3; // order
// type order2
export function Lt2(lt, gt) /* forall<a> (lt : a, gt : a) -> order2<a> */  {
  return { _tag: 1, lt: lt, gt: gt };
}
export function Eq2(eq) /* forall<a> (eq : a) -> order2<a> */  {
  return { _tag: 2, eq: eq };
}
export function Gt2(lt, gt) /* forall<a> (lt : a, gt : a) -> order2<a> */  {
  return { _tag: 3, lt: lt, gt: gt };
}
// type pad
export const Pad = { _tag: 1 }; // pad
// type read
// type ref
// type result
export function $Error(error) /* forall<a,b> (error : b) -> result<a,b> */  {
  return { _tag: 1, error: error };
}
export function Ok(value) /* forall<a,b> (value : a) -> result<a,b> */  {
  return { _tag: 2, value: value };
}
// type ssize_t
// type total
// type write
// type string
// type tuple2
export function Tuple2(fst, snd) /* forall<a,b> (fst : a, snd : b) -> (a, b) */  {
  return { fst: fst, snd: snd };
}
// type tuple3
export function Tuple3(fst, snd, thd) /* forall<a,b,c> (fst : a, snd : b, thd : c) -> (a, b, c) */  {
  return { fst: fst, snd: snd, thd: thd };
}
// type tuple4
export function Tuple4(fst, snd, thd, field4) /* forall<a,b,c,d> (fst : a, snd : b, thd : c, field4 : d) -> (a, b, c, d) */  {
  return { fst: fst, snd: snd, thd: thd, field4: field4 };
}
// type tuple5
export function Tuple5(fst, snd, thd, field4, field5) /* forall<a,b,c,d,a1> (fst : a, snd : b, thd : c, field4 : d, field5 : a1) -> (a, b, c, d, a1) */  {
  return { fst: fst, snd: snd, thd: thd, field4: field4, field5: field5 };
}
// type unit
export const Unit = 1; // ()
// type vector
// type void
 
// declarations
 
export function _no_reuse() /* () -> @reuse */  {
  return null;
}
 
 
// Automatically generated. Tests for the `False` constructor of the `:bool` type.
export function is_false(bool) /* (bool : bool) -> bool */  {
  return (!bool);
}
 
 
// Automatically generated. Tests for the `True` constructor of the `:bool` type.
export function is_true(bool) /* (bool : bool) -> bool */  {
  return (bool);
}
 
export function unit_fs__copy(_this) /* (()) -> () */  {
  return Unit;
}
 
 
// Automatically generated. Retrieves the `fst` constructor field of the `:tuple2` type.
export function tuple2_fs_fst(tuple2) /* forall<a,b> (tuple2 : (a, b)) -> a */  {
  return tuple2.fst;
}
 
 
// Automatically generated. Retrieves the `snd` constructor field of the `:tuple2` type.
export function tuple2_fs_snd(tuple2) /* forall<a,b> (tuple2 : (a, b)) -> b */  {
  return tuple2.snd;
}
 
export function tuple2_fs__copy(_this, fst, snd) /* forall<a,b> ((a, b), fst : ? a, snd : ? b) -> (a, b) */  {
  if (fst !== undefined) {
    var _x0 = fst;
  }
  else {
    var _x0 = _this.fst;
  }
  if (snd !== undefined) {
    var _x1 = snd;
  }
  else {
    var _x1 = _this.snd;
  }
  return Tuple2(_x0, _x1);
}
 
 
// Automatically generated. Retrieves the `fst` constructor field of the `:tuple3` type.
export function tuple3_fs_fst(tuple3) /* forall<a,b,c> (tuple3 : (a, b, c)) -> a */  {
  return tuple3.fst;
}
 
 
// Automatically generated. Retrieves the `snd` constructor field of the `:tuple3` type.
export function tuple3_fs_snd(tuple3) /* forall<a,b,c> (tuple3 : (a, b, c)) -> b */  {
  return tuple3.snd;
}
 
 
// Automatically generated. Retrieves the `thd` constructor field of the `:tuple3` type.
export function tuple3_fs_thd(tuple3) /* forall<a,b,c> (tuple3 : (a, b, c)) -> c */  {
  return tuple3.thd;
}
 
export function tuple3_fs__copy(_this, fst, snd, thd) /* forall<a,b,c> ((a, b, c), fst : ? a, snd : ? b, thd : ? c) -> (a, b, c) */  {
  if (fst !== undefined) {
    var _x2 = fst;
  }
  else {
    var _x2 = _this.fst;
  }
  if (snd !== undefined) {
    var _x3 = snd;
  }
  else {
    var _x3 = _this.snd;
  }
  if (thd !== undefined) {
    var _x4 = thd;
  }
  else {
    var _x4 = _this.thd;
  }
  return Tuple3(_x2, _x3, _x4);
}
 
 
// Automatically generated. Retrieves the `field4` constructor field of the `:tuple4` type.
export function tuple4_fs_field4(tuple4) /* forall<a,b,c,d> (tuple4 : (a, b, c, d)) -> d */  {
  return tuple4.field4;
}
 
 
// Automatically generated. Retrieves the `fst` constructor field of the `:tuple4` type.
export function tuple4_fs_fst(tuple4) /* forall<a,b,c,d> (tuple4 : (a, b, c, d)) -> a */  {
  return tuple4.fst;
}
 
 
// Automatically generated. Retrieves the `snd` constructor field of the `:tuple4` type.
export function tuple4_fs_snd(tuple4) /* forall<a,b,c,d> (tuple4 : (a, b, c, d)) -> b */  {
  return tuple4.snd;
}
 
 
// Automatically generated. Retrieves the `thd` constructor field of the `:tuple4` type.
export function tuple4_fs_thd(tuple4) /* forall<a,b,c,d> (tuple4 : (a, b, c, d)) -> c */  {
  return tuple4.thd;
}
 
export function tuple4_fs__copy(_this, fst, snd, thd, field4) /* forall<a,b,c,d> ((a, b, c, d), fst : ? a, snd : ? b, thd : ? c, field4 : ? d) -> (a, b, c, d) */  {
  if (fst !== undefined) {
    var _x5 = fst;
  }
  else {
    var _x5 = _this.fst;
  }
  if (snd !== undefined) {
    var _x6 = snd;
  }
  else {
    var _x6 = _this.snd;
  }
  if (thd !== undefined) {
    var _x7 = thd;
  }
  else {
    var _x7 = _this.thd;
  }
  if (field4 !== undefined) {
    var _x8 = field4;
  }
  else {
    var _x8 = _this.field4;
  }
  return Tuple4(_x5, _x6, _x7, _x8);
}
 
 
// Automatically generated. Retrieves the `field4` constructor field of the `:tuple5` type.
export function tuple5_fs_field4(tuple5) /* forall<a,b,c,d,a1> (tuple5 : (a, b, c, d, a1)) -> d */  {
  return tuple5.field4;
}
 
 
// Automatically generated. Retrieves the `field5` constructor field of the `:tuple5` type.
export function tuple5_fs_field5(tuple5) /* forall<a,b,c,d,a1> (tuple5 : (a, b, c, d, a1)) -> a1 */  {
  return tuple5.field5;
}
 
 
// Automatically generated. Retrieves the `fst` constructor field of the `:tuple5` type.
export function tuple5_fs_fst(tuple5) /* forall<a,b,c,d,a1> (tuple5 : (a, b, c, d, a1)) -> a */  {
  return tuple5.fst;
}
 
 
// Automatically generated. Retrieves the `snd` constructor field of the `:tuple5` type.
export function tuple5_fs_snd(tuple5) /* forall<a,b,c,d,a1> (tuple5 : (a, b, c, d, a1)) -> b */  {
  return tuple5.snd;
}
 
 
// Automatically generated. Retrieves the `thd` constructor field of the `:tuple5` type.
export function tuple5_fs_thd(tuple5) /* forall<a,b,c,d,a1> (tuple5 : (a, b, c, d, a1)) -> c */  {
  return tuple5.thd;
}
 
export function tuple5_fs__copy(_this, fst, snd, thd, field4, field5) /* forall<a,b,c,d,a1> ((a, b, c, d, a1), fst : ? a, snd : ? b, thd : ? c, field4 : ? d, field5 : ? a1) -> (a, b, c, d, a1) */  {
  if (fst !== undefined) {
    var _x9 = fst;
  }
  else {
    var _x9 = _this.fst;
  }
  if (snd !== undefined) {
    var _x10 = snd;
  }
  else {
    var _x10 = _this.snd;
  }
  if (thd !== undefined) {
    var _x11 = thd;
  }
  else {
    var _x11 = _this.thd;
  }
  if (field4 !== undefined) {
    var _x12 = field4;
  }
  else {
    var _x12 = _this.field4;
  }
  if (field5 !== undefined) {
    var _x13 = field5;
  }
  else {
    var _x13 = _this.field5;
  }
  return Tuple5(_x9, _x10, _x11, _x12, _x13);
}
 
 
// Automatically generated. Tests for the `Nothing` constructor of the `:maybe` type.
export function is_nothing(maybe) /* forall<a> (maybe : maybe<a>) -> bool */  {
  return (maybe === null);
}
 
 
// Automatically generated. Tests for the `Just` constructor of the `:maybe` type.
export function is_just(maybe) /* forall<a> (maybe : maybe<a>) -> bool */  {
  return (maybe !== null);
}
 
 
// Automatically generated. Tests for the `Nothing2` constructor of the `:maybe2` type.
export function is_nothing2(maybe2) /* forall<a,b> (maybe2 : maybe2<a,b>) -> bool */  {
  return (maybe2._tag === 1);
}
 
 
// Automatically generated. Tests for the `Just2` constructor of the `:maybe2` type.
export function is_just2(maybe2) /* forall<a,b> (maybe2 : maybe2<a,b>) -> bool */  {
  return (maybe2._tag === 2);
}
 
 
// Automatically generated. Tests for the `Error` constructor of the `:result` type.
export function is_error(result) /* forall<a,b> (result : result<a,b>) -> bool */  {
  return (result._tag === 1);
}
 
 
// Automatically generated. Tests for the `Ok` constructor of the `:result` type.
export function is_ok(result) /* forall<a,b> (result : result<a,b>) -> bool */  {
  return (result._tag === 2);
}
 
 
// Automatically generated. Tests for the `Left` constructor of the `:either` type.
export function is_left(either) /* forall<a,b> (either : either<a,b>) -> bool */  {
  return (either._tag === 1);
}
 
 
// Automatically generated. Tests for the `Right` constructor of the `:either` type.
export function is_right(either) /* forall<a,b> (either : either<a,b>) -> bool */  {
  return (either._tag === 2);
}
 
 
// Automatically generated. Tests for the `Lt` constructor of the `:order` type.
export function is_lt(order) /* (order : order) -> bool */  {
  return (order === 1);
}
 
 
// Automatically generated. Tests for the `Eq` constructor of the `:order` type.
export function is_eq(order) /* (order : order) -> bool */  {
  return (order === 2);
}
 
 
// Automatically generated. Tests for the `Gt` constructor of the `:order` type.
export function is_gt(order) /* (order : order) -> bool */  {
  return (order === 3);
}
 
 
// Automatically generated. Tests for the `Lt2` constructor of the `:order2` type.
export function is_lt2(order2) /* forall<a> (order2 : order2<a>) -> bool */  {
  return (order2._tag === 1);
}
 
 
// Automatically generated. Tests for the `Eq2` constructor of the `:order2` type.
export function is_eq2(order2) /* forall<a> (order2 : order2<a>) -> bool */  {
  return (order2._tag === 2);
}
 
 
// Automatically generated. Tests for the `Gt2` constructor of the `:order2` type.
export function is_gt2(order2) /* forall<a> (order2 : order2<a>) -> bool */  {
  return (order2._tag === 3);
}
 
 
// Automatically generated. Tests for the `Nil` constructor of the `:list` type.
export function is_nil(list) /* forall<a> (list : list<a>) -> bool */  {
  return (list === null);
}
 
 
// Automatically generated. Tests for the `Cons` constructor of the `:list` type.
export function is_cons(list) /* forall<a> (list : list<a>) -> bool */  {
  return (list !== null);
}
 
 
// Automatically generated. Retrieves the `unbox` constructor field of the `:box` type.
export function box_fs_unbox(box) /* forall<a> (box : box<a>) -> a */  {
  return box;
}
 
export function box_fs__copy(_this, unbox) /* forall<a> (box<a>, unbox : ? a) -> box<a> */  {
  if (unbox !== undefined) {
    var _x14 = unbox;
  }
  else {
    var _x14 = _this;
  }
  return _x14;
}
 
export function pad_fs__copy(_this) /* (pad) -> pad */  {
  return Pad;
}
 
 
// Prevent inlining an expression by passing it to `keep` (which is a non-inlineable identity function)
export function keep(x) /* forall<a> (x : a) -> a */  {
  return x;
}
 
 
// The identity function returns its argument unchanged
export function id(x) /* forall<a> (x : a) -> a */  {
  return x;
}
 
 
// Logical conjunction. This is compiled specially avoid evaluating the second argument if `x==False`.
export function _lp__amp__amp__rp_(x, y) /* (x : bool, y : bool) -> bool */  {
  return (x) ? y : false;
}
 
 
// Logical disjunction. This is compiled specially avoid evaluating the second argument if `x==True`.
export function _lp__bar__bar__rp_(x, y) /* (x : bool, y : bool) -> bool */  {
  return (x) ? true : y;
}
 
 
// Logical negation
export function bool_fs__lp__excl__rp_(b) /* (b : bool) -> bool */  {
  return (b) ? false : true;
}
 
 
// Logical negation
export function not(b) /* (b : bool) -> bool */  {
  return (b) ? false : true;
}
 
 
// Convert an integer to an `:ssize_t`. The number is _clamped_ to the maximal or minimum `:ssize_t`
// value if it is outside the range of an `:ssize_t`.
// Needed for evidence indices in `module std/core/hnd`
export function _make_ssize__t(i) /* (i : int) -> ssize_t */  {
  return $std_core_types._int_clamp32(i);
}
 
 
// Append two strings
export function _lp__plus__plus__rp_(x, y) /* (x : string, y : string) -> string */  {
  return (x + y);
}
 
 
// _Internal_: generated by type inference and later refined into one of the `open` variants in `std/core/hnd`.
export function _open(x) /* forall<e,e1,a,b> (x : a) -> e1 b */  {
  return x;
}
 
 
// If a heap effect is unobservable, the heap effect can be erased by using the `run` fun.
// See also: _State in Haskell, by Simon Peyton Jones and John Launchbury_.
export function run(action) /* forall<e,a> (action : forall<h> () -> <alloc<h>,read<h>,write<h>|e> a) -> e a */  {
  return ((action)());
}
 
 
// _Internal_: if local mutation is unobservable, the `:local` effect can be erased by using the `local-scope` function.
// See also: _State in Haskell, by Simon Peyton Jones and John Launchbury_.
export function local_scope(action) /* forall<a,e> (action : forall<h> () -> <local<h>|e> a) -> e a */  {
  return action();
}
 
 
// _Internal_. Internal function name for field addresses.
export function _field_addr_of(x, conname, fieldname) /* forall<a> (x : a, conname : string, fieldname : string) -> @field-addr<a> */  {
  
}
 
 
// Automatically generated. Retrieves the `holeptr` constructor field of the `:cctx` type.
export function cctx_fs_holeptr(cctx) /* forall<a,b> (cctx : cctx<a,b>) -> @field-addr<b> */  {
  return cctx.holeptr;
}
 
 
// Automatically generated. Retrieves the `res` constructor field of the `:cctx` type.
export function cctx_fs_res(cctx) /* forall<a,b> (cctx : cctx<a,b>) -> a */  {
  return cctx.res;
}