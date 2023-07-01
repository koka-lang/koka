/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/


/* assign here so inlined primitives are available in system.core itself */
const $std_core = {"_throw_exception": _throw_exception
            , "_error_from_exception": _error_from_exception
            , "_unsupported_external": _unsupported_external
            // primitive operations emitted by the compiler
            , "_int32_multiply": _int32_multiply                      
            , "vlist": _vlist
            , "_vector_at": _vector_at
            // integer operations that will be inlined
            , "_int_string": _int_string
            , "_int_const": _int_const
            , "_int_double": _int_double
            , "_int_clamp8": _int_clamp8
            , "_int_clamp16": _int_clamp16      
            , "_int_clamp32": _int_clamp32
            , "_int_clamp64": _int_clamp64
            , "_int_clamp_byte": _int_clamp_byte
            , "_int_from_int32": _int_from_int32
            , "_int_from_int64": _int_from_int64
            , "_double_to_int32": _double_to_int32
            , "_double_round_even": _double_round_even
            , "_int_to_double": _int_to_double
            , "_int_to_float": _int_to_float
            , "_double_to_float": _double_to_float
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
            };

/*------------------------------------------------
  32-bit integer operations
--------------------------------------------------*/

export function _int32_multiply(x,y) {
  var xhi = (x >> 16) & 0xFFFF;
  var xlo = x & 0xFFFF;
  var yhi = (y >> 16) & 0xFFFF;
  var ylo = y & 0xFFFF;
  var hi  = ((xhi * ylo) + (xlo * yhi));
  return (((hi << 16) + (xlo * ylo))|0)
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

export function _int32_bits_popcount(x) {
  var i = (x|0);
  i = i - ((i >> 1) & 0x55555555);
  i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
  i = (i + (i >> 4)) & 0x0F0F0F0F;
  i = i + (i >> 8);
  i = i + (i >> 16);
  return (i & 0x3F);
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

export function _int64_ctz(x) {
  const lo = Number( x & 0xFFFFFFFFn );
  if (lo === 0) {
    const hi = Number( _int64_shr(x,32n) );  
    return BigInt(32 + _int32_ctz(hi));
  }
  else {
    return BigInt(_int32_ctz(lo));
  }
}

export function _int64_clz(x) {
  const hi = Number( _int64_shr(x,32n) );  
  if (hi === 0) {
    const lo = Number( x & 0xFFFFFFFFn );
    return BigInt(32 + _int32_clz(lo));
  }
  else {
    return BigInt(_int32_clz(hi));
  }
}

export function _int64_bits_popcount(x) {
  const hi = Number( _int64_shr(x,32n) );  
  const lo = Number( x & 0xFFFFFFFFn );
  return BigInt(_int32_bits_popcount(hi) + _int32_bits_popcount(lo));
}


export function _int64_from_uint32(x) {
  return (x >= 0 ? BigInt(x) : 0x100000000n + BigInt(x))
}

export function _int64_from_int32(x) {
  return BigInt(x);
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
  return $std_core_types._Tuple2_( _int(i/j), _int(i%j) );
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
  return $std_core_types._Tuple2_( _int(q), _int(r) );
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
    return $std_core_types._Tuple2_(q,(x%y));
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
    return $std_core_types._Tuple2_(q,r);
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

function _int_showhex(x,upper) {
  const s = x.toString(16);
  return (upper ? s.toUpperCase() : s);
}

function _int_parse(s,hex) {
  if (s==="") return $std_core_types.Nothing;
  const cappre  = /^([\-\+])?(0[xX])?(.*)$/.exec(s);
  const sdigits = cappre[3].toLowerCase();
  const sign    = cappre[1] || "";
  if (cappre[2]) hex = true;
  if (hex) {
    const cap = /^[0-9a-f]+$/.exec(sdigits);
    if (!cap) return  $std_core_types.Nothing;
    return $std_core_types.Just( _int_string(sign + "0x" + sdigits) );
  }
  else {
    const rx = /^([0-9]+)(?:\.([0-9]+))?(?:[eE]\+?([0-9]+))?$/;
    const cap = rx.exec(sdigits);
    if (!cap) return $std_core_types.Nothing;
    var sig  = cap[1];
    const frac = cap[2] || "";
    var exp  = (cap[3] ? parseInt(cap[3]) : 0);
    if (frac.length > exp) return $std_core_types.Nothing;
    exp = exp - frac.length;
    sig = sig + frac;
    var x = _int_string(sign + sig);
    if (exp > 0) {
      x = _int_mul_pow10( x, exp );
    }
    return $std_core_types.Just(x);
  }
}
