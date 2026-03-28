// Koka generated module: std/core/int, koka version: 3.2.4
"use strict";
 
// imports
import * as $std_core_types from './std_core_types.mjs';
 
// externals
/*---------------------------------------------------------------------------
  Copyright 2012-2024, Microsoft Research, Daan Leijen.
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
export function _int_parse(s,hex) {
  if (s==="") return $std_core_types.Nothing;
  const cappre  = /^([\-\+])?(0[xX])?(.*)$/.exec(s);
  if (!cappre) return $std_core_types.Nothing;
  const sdigits = cappre[3]?.toLowerCase() || "";
  const sign    = cappre[1] || "";
  if (cappre[2]) hex = true;
  if (hex) {
    const cap = /^[0-9a-f]+$/.exec(sdigits);
    if (!cap) return  $std_core_types.Nothing;
    return $std_core_types.Just( $std_core_types._int_string(sign + "0x" + sdigits) );
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
    var x = $std_core_types._int_string(sign + sig);
    if (exp > 0) {
      x = $std_core_types._int_mul_pow10( x, exp );
    }
    return $std_core_types.Just(x);
  }
}
 
// type declarations
 
// declarations
 
export function order(i) /* (i : int) -> order */  {
  if ($std_core_types._int_lt(i,0)) {
    return $std_core_types.Lt;
  }
  else {
    return ($std_core_types._int_gt(i,0)) ? $std_core_types.Gt : $std_core_types.Eq;
  }
}
 
 
// Compare two integers
export function cmp(x, y) /* (x : int, y : int) -> order */  {
  if ($std_core_types._int_eq(x,y)) {
    return $std_core_types.Eq;
  }
  else {
    return ($std_core_types._int_gt(x,y)) ? $std_core_types.Gt : $std_core_types.Lt;
  }
}
 
 
// Order two integers in ascending order.
export function order2(x, y) /* (x : int, y : int) -> order2<int> */  {
  if ($std_core_types._int_eq(x,y)) {
    return $std_core_types.Eq2(x);
  }
  else {
    if ($std_core_types._int_lt(x,y)) {
      return $std_core_types.Lt2(x, y);
    }
    else {
      return $std_core_types.Gt2(y, x);
    }
  }
}
 
 
// Add two integers.
export function _lp__plus__rp_(x, y) /* (x : int, y : int) -> int */  {
  return $std_core_types._int_add(x,y);
}
 
 
// Subtract two integers.
export function _lp__dash__rp_(x, y) /* (x : int, y : int) -> int */  {
  return $std_core_types._int_sub(x,y);
}
 
 
/* Euclidean-0 division & modulus.
Euclidean division is defined as: For any `D`  and `d`  where ``d!=0`` , we have:
1. ``D == d*(D/d) + (D%d)``
2. ``D%d``  is always positive where ``0 <= D%d < abs(d)``
Moreover, Euclidean-0 is a total function, for the case where `d==0`  we have
that ``D%0 == D``  and ``D/0 == 0`` . So property (1) still holds, but not property (2).
Useful laws that hold for Euclidean-0 division:
* ``D/(-d) == -(D/d)``
* ``D%(-d) == D%d``
* ``D/(2^n) == sar(D,n)         ``  (where ``2^n`` means ``2`` to the power of ``n``)
* ``D%(2^n) == D & ((2^n) - 1)  ``
See also _Division and modulus for computer scientists, Daan Leijen, 2001_ for further information
(available at: <https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf>).
*/
export function divmod(x, y) /* (x : int, y : int) -> (int, int) */  {
  return $std_core_types._int_divmod(x,y);
}
 
export function negate(i) /* (i : int) -> int */  {
  return $std_core_types._int_negate(i);
}
 
 
// Increment
export function inc(i) /* (i : int) -> int */  {
  return $std_core_types._int_add(i,1);
}
 
 
// Decrement
export function dec(i) /* (i : int) -> int */  {
  return $std_core_types._int_sub(i,1);
}
 
 
// Raise an integer `i` to the power of `exp`.
export function pow(i, exp) /* (i : int, exp : int) -> int */  {
  return $std_core_types._int_pow(i,exp);
}
 
 
// Raise an integer `i` to the power of `exp`.
export function _lp__hat__rp_(i, exp) /* (i : int, exp : int) -> int */  {
  return pow(i, exp);
}
 
 
// Return the number of decimal digits of `i`. Return `0` when `i==0`.
export function count_digits(i) /* (i : int) -> int */  {
  return $std_core_types._int_count_digits(i);
}
 
export function mul_exp10(i, n) /* (i : int, n : int) -> int */  {
  return $std_core_types._int_mul_pow10(i,n);
}
 
 
// Calculate ``10^exp``
export function exp10(exp) /* (exp : int) -> int */  {
  return mul_exp10(1, exp);
}
 
 
// Calculate ``2^exp``.
export function exp2(exp) /* (exp : int) -> int */  {
  return pow(2, exp);
}
 
 
// Return the number of ending `0` digits of `i`. Return `0` when `i==0`.
export function is_exp10(i) /* (i : int) -> int */  {
  return $std_core_types._int_count_pow10(i);
}
 
export function cdiv_exp10(i, n) /* (i : int, n : int) -> int */  {
  return $std_core_types._int_cdiv_pow10(i,n);
}
 
export function cdivmod_exp10(i, n) /* (i : int, n : int) -> (int, int) */  {
  if ($std_core_types._int_le(n,0)) {
    return $std_core_types.Tuple2(i, 0);
  }
  else {
     
    var cq = cdiv_exp10(i, n);
     
    var y_10005 = mul_exp10(cq, n);
     
    var cr = $std_core_types._int_sub(i,y_10005);
    return $std_core_types.Tuple2(cq, cr);
  }
}
 
 
// Is the integer negative (strictly smaller than zero)
export function is_neg(i) /* (i : int) -> bool */  {
  return $std_core_types._int_lt(i,0);
}
 
export function divmod_exp10(i, n) /* (i : int, n : int) -> (int, int) */  {
  var _x0 = cdivmod_exp10(i, n);
   
  var b_10006 = $std_core_types._int_lt((_x0.snd),0);
  if (b_10006) {
     
    var y_0_10012 = mul_exp10(1, n);
    return $std_core_types.Tuple2($std_core_types._int_sub((_x0.fst),1), $std_core_types._int_add((_x0.snd),y_0_10012));
  }
  else {
    return $std_core_types.Tuple2(_x0.fst, _x0.snd);
  }
}
 
 
// Is this an even integer?
export function is_even(i) /* (i : int) -> bool */  {
   
  var b_10014 = $std_core_types._int_isodd(i);
  return (b_10014) ? false : true;
}
 
 
// Is the integer positive (strictly greater than zero)
export function is_pos(i) /* (i : int) -> bool */  {
  return $std_core_types._int_gt(i,0);
}
 
 
// Compare an integer `i` with zero
export function sign(i) /* (i : int) -> order */  {
  if ($std_core_types._int_eq(i,0)) {
    return $std_core_types.Eq;
  }
  else {
    return ($std_core_types._int_gt(i,0)) ? $std_core_types.Gt : $std_core_types.Lt;
  }
}
 
 
// Return the minimum of two integers
export function min(i, j) /* (i : int, j : int) -> int */  {
  return ($std_core_types._int_le(i,j)) ? i : j;
}
 
 
// Return the maximum of two integers
export function max(i, j) /* (i : int, j : int) -> int */  {
  return ($std_core_types._int_ge(i,j)) ? i : j;
}
 
 
// Transform an integer to a maybe type, using `Nothing` for `0`
export function maybe(i) /* (i : int) -> maybe<int> */  {
  if ($std_core_types._int_eq(i,0)) {
    return $std_core_types.Nothing;
  }
  else {
    return $std_core_types.Just(i);
  }
}
 
 
// Convert an `:int` to a string
export function show(i) /* (i : int) -> string */  {
  return i.toString();
}
 
 
// Convert an int to a boolean, using `False` for 0 and `True` otherwise.
export function bool(i) /* (i : int) -> bool */  {
  return $std_core_types._int_ne(i,0);
}
 
 
// Convert a `:maybe<int>` to an `:int` using zero for `Nothing`
export function mbint(m) /* (m : maybe<int>) -> int */  {
  return (m === null) ? 0 : m.value;
}
 
export function xparse(s, hex) /* (s : string, hex : bool) -> maybe<int> */  {
  return _int_parse(s,hex);
}
 
 
// Parse an integer.
// If an illegal digit character is encountered `Nothing` is returned.
// An empty string, or a string starting with white space will result in `Nothing`
// A string can start with a `-` sign for negative numbers,
// and with `0x` or `0X` for hexadecimal numbers (in which case the `hex` parameter is ignored).
export function parse_int(s, hex) /* (s : string, hex : ? bool) -> maybe<int> */  {
  var _x1 = (hex !== undefined) ? hex : false;
  return xparse(s, _x1);
}
 
 
// clamp an `:int` to fit in an `:int8`.
export function int8(i) /* (i : int) -> int8 */  {
  return $std_core_types._int_clamp8(i);
}
 
 
// clamp an `:int` to fit in an `:int8` but interpret the `:int` as an unsigned 8-bit value,
// and clamp between 0 and 255.
export function uint8(i) /* (i : int) -> int8 */  {
  return $std_core_types._int_clamp_byte(i);
}
 
 
// clamp an `:int` to fit in an `:int16`.
export function int16(i) /* (i : int) -> int16 */  {
  return $std_core_types._int_clamp16(i);
}
 
 
// clamp an `:int` to fit in an `:intptr_t`.
export function intptr__t(i) /* (i : int) -> intptr_t */  {
  return $std_core_types._int_clamp64(i);
}
 
 
// Convert an integer to an `:ssize_t`. The number is _clamped_ to the maximal or minimum `:ssize_t`
// value if it is outside the range of an `:ssize_t`.
// Needed for evidence indices in `module std/core/hnd`
export function ssize__t(i) /* (i : int) -> ssize_t */  {
  return $std_core_types._int_clamp32(i);
}