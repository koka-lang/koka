/*---------------------------------------------------------------------------
  Copyright 2012-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Standard integer `:int` functions.
//
// These are arbitrary precision signed integer with an efficient runtime representation.
// For more details, see:
//
// >   "What About the Integer Numbers?
// >   "Fast Arithmetic with Tagged Integers -- A Plea for Hardware Support"
// >   Daan Leijen, Technical report MSR-TR-2022-17, 2022-07-11, v1.0. Presented at the ML workshop 2022."
// >   <https://www.microsoft.com/en-us/research/uploads/prod/2022/07/int.pdf>
//
module std/core/int

import std/core/types

extern import
  c  file "inline/int.h"
  js file "inline/int.js"

pub fip fun order( i : int ) : order
  if i < 0 then Lt
  elif i > 0 then Gt
  else Eq

// Compare two integers
pub inline fip fun cmp(^x : int, ^y : int) : order
  if (x==y) then Eq elif (x>y) then Gt else Lt
  // c inline "kk_int_as_order(kk_integer_cmp_borrow(#1,#2,kk_context()),kk_context())"
  // cs "Primitive.IntCompare"
  // js "$std_core_types._int_compare"

// Order two integers in ascending order.
pub inline fip fun order2(x : int, y : int) : order2<int>
  if (x==y) then Eq2(x) elif (x < y) then Lt2(x,y) else Gt2(y,x)

// Are two integers equal?
pub inline fip extern (==)(^x : int, ^y : int) : bool
  c  "kk_integer_eq_borrow"
  cs inline "(#1 == #2)"
  js "$std_core_types._int_eq"

// Are two integers not equal?
pub inline fip extern (!=)(^x : int, ^y : int) : bool
  c  "kk_integer_neq_borrow"
  cs inline "(#1 != #2)"
  js "$std_core_types._int_ne"

// Is the first integer smaller or equal to the second?
pub inline fip extern (<=)(^x : int, ^y : int) : bool
  c  "kk_integer_lte_borrow"
  cs inline "(#1 <= #2)"
  js "$std_core_types._int_le"

// Is the first integer greater or equal to the second?
pub inline fip extern (>=)(^x : int, ^y : int) : bool
  c  "kk_integer_gte_borrow"
  cs inline "(#1 >= #2)"
  js "$std_core_types._int_ge"

// Is the first integer smaller than the second?
pub inline fip extern (<)(^x : int, ^y : int) : bool
  c  "kk_integer_lt_borrow"
  cs inline "(#1 < #2)"
  js "$std_core_types._int_lt"

// Is the first integer greater than the second?
pub inline fip extern (>)(^x : int, ^y : int) : bool
  c  "kk_integer_gt_borrow"
  cs inline "(#1 > #2)"
  js "$std_core_types._int_gt"

inline fip extern int-add : (int,int) -> int
  c  "kk_integer_add"
  cs inline "(#1 + #2)"
  js "$std_core_types._int_add"

// Add two integers.
pub fip fun (+)(x : int, y : int ) : int
  int-add(x,y)

inline fip extern int-sub : (int,int) -> int
  c  "kk_integer_sub"
  cs inline "(#1 - #2)"
  js "$std_core_types._int_sub"

// Substract two integers.
pub fip fun (-)(x : int, y : int ) : int
  int-sub(x,y)

// Multiply two integers.
pub inline fip extern (*) : (int,int) -> int
  c  "kk_integer_mul"
  cs inline "(#1 * #2)"
  js "$std_core_types._int_mul"

// Euclidean-0 division of two integers. See also `divmod:(x : int, y : int) -> (int,int)`.
pub inline fip extern (/)(x:int,y:int) : int
  c  "kk_integer_div"
  cs "Primitive.IntDiv"
  js "$std_core_types._int_div"

// Euclidean modulus of two integers; always a non-negative number. See also `divmod:(x : int, y : int) -> (int,int)`.
pub inline fip extern (%)  : (int,int) -> int
  c  "kk_integer_mod"
  cs "Primitive.IntMod"
  js "$std_core_types._int_mod"

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
pub fip extern divmod(x:int,y:int) : (int,int)
  c  "kk_integer_div_mod_tuple"
  cs "Primitive.IntDivMod"
  js "$std_core_types._int_divmod"

pub fip fun negate(i : int) : int
  ~i

// Negate an integer.
pub inline fip extern (~)(i:int) : int
  c  "kk_integer_neg"
  cs inline "(-#1)"
  js "$std_core_types._int_negate"

// Is this an odd integer?
pub inline fip extern is-odd( i : int ) : bool
  c  "kk_integer_is_odd"
  cs inline "!(#1.IsEven)"
  js "$std_core_types._int_isodd"

// Is this equal to zero?
pub inline fip extern is-zero( ^x : int) : bool
  c  inline "kk_integer_is_zero_borrow(#1)"
  cs inline "(#1.IsZero)"
  js "$std_core_types._int_iszero"

// Return the absolute value of an integer.
pub inline fip extern abs(i : int) : int
  c  "kk_integer_abs"
  cs "BigInteger.Abs"
  js "$std_core_types._int_abs"



// Increment
pub fip fun inc( i : int ) : int
  i + 1

// Decrement
pub fip fun dec( i : int ) : int
  i - 1

// Calculate ``10^exp``
pub fip fun exp10( exp : int ) : int
  1.mul-exp10(exp)

// Raise an integer `i` to the power of `exp`.
pub fip extern pow( i : int, exp : int ) : int
  c  "kk_integer_pow"
  cs "Primitive.IntPow"
  js "$std_core_types._int_pow"

// Raise an integer `i` to the power of `exp`.
pub fip fun (^)(i : int, exp : int ) : int
  pow(i,exp)

// Calculate ``2^exp``.
pub fip fun exp2( exp : int ) : int
  pow(2,exp)

// Return the number of ending `0` digits of `i`. Return `0` when `i==0`.
pub fip extern is-exp10( i : int ) : int
  c  "kk_integer_ctz"
  cs "Primitive.IntCountPow10"
  js "$std_core_types._int_count_pow10"

// Return the number of decimal digits of `i`. Return `0` when `i==0`.
pub fip extern count-digits( i : int ) : int
  c  "kk_integer_count_digits"
  cs "Primitive.IntCountDigits"
  js "$std_core_types._int_count_digits"

pub fip extern mul-exp10( i : int, n : int ) : int
  c  "kk_integer_mul_pow10"
  cs "Primitive.IntMulPow10"
  js "$std_core_types._int_mul_pow10"

pub fip extern cdiv-exp10( i : int, n : int ) : int
  c  "kk_integer_cdiv_pow10"
  cs "Primitive.IntCDivPow10"
  js "$std_core_types._int_cdiv_pow10"

pub fun cdivmod-exp10( i : int, n : int ) : (int,int)
  if n <= 0 return (i,0)
  val cq = i.cdiv-exp10(n)
  val cr = i - cq.mul-exp10(n)
  (cq,cr)

pub fun divmod-exp10( i : int, n : int ) : (int,int)
  val (cq,cr) = cdivmod-exp10(i,n)
  if !cr.is-neg then (cq,cr) else (cq.dec, cr + exp10(n))

// Is this an even integer?
pub fip fun is-even(i:int) : bool
  !is-odd(i)

// Is the integer positive (strictly greater than zero)
pub fip fun is-pos(i : int ) : bool
  i > 0

// Is the integer negative (strictly smaller than zero)
pub fip fun is-neg(i : int ) : bool
  i < 0

// Compare an integer `i` with zero
pub inline fip fun sign( ^i : int ) : order
  cmp(i,0)
  // c  inline "kk_int_as_order(kk_integer_signum_borrow(#1,kk_context()),kk_context())"
  // cs "Primitive.IntSign"
  // js "$std_core_types._int_sign"

// Return the minimum of two integers
pub fip fun min( i : int, j : int ) : int
  if i <= j then i else j

// Return the maximum of two integers
pub fip fun max( i : int, j : int ) : int
  if i >= j then i else j

// Transform an integer to a maybe type, using `Nothing` for `0`
pub fun maybe( i : int ) : maybe<int>
  if i==0 then Nothing else Just(i)

// Convert an `:int` to a string
pub extern show( i : int ) : string
  c "kk_integer_to_string"
  cs inline "#1.ToString()"
  js inline "#1.toString()"

// Convert an int to a boolean, using `False` for 0 and `True` otherwise.
pub fun bool( i : int ) : bool
  i != 0

// Convert a `:maybe<int>` to an `:int` using zero for `Nothing`
pub fip fun mbint( m : maybe<int> ) : int
  match m
    Nothing -> 0
    Just(i) -> i


// ----------------------------------------------------------------------------
// Parse numbers
// ----------------------------------------------------------------------------

// Parse an integer using `parseInt`. If an illegal digit character is encountered the
// `default` value is returned. An empty string will also result in `default`.
// pub fun parse-int-default( s : string, def : int = 0, hex : bool = False ) : int
//   if s.is-empty then def else s.parse-int(hex).default(def)

// Parse an integer.
// If an illegal digit character is encountered `Nothing` is returned.
// An empty string, or a string starting with white space will result in `Nothing`
// A string can start with a `-` sign for negative numbers,
// and with `0x` or `0X` for hexadecimal numbers (in which case the `hex` parameter is ignored).
pub fun parse-int( s : string, hex : bool = False) : maybe<int>
  s.xparse(hex)

noinline extern xparse( s : string, hex : bool ) : maybe<int>
  c  "kk_integer_xparse"
  cs "Primitive.IntParse"
  js "_int_parse"



// ----------------------------------------------------------------------------
// int8, int16, intptr_t
// ----------------------------------------------------------------------------

// clamp an `:int` to fit in an `:int8`.
pub fip extern int8( i : int) : int8
  c  "kk_integer_clamp_int8"
  cs "Primitive.IntToInt8"
  js "$std_core_types._int_clamp8"

// Convert an `:int8` to an `:int`.
pub inline fip extern int8/int( i : int8 ) : int
  c  "kk_integer_from_int8"
  cs inline "(new BigInteger(#1))"
  js "$std_core_types._int_from_int32"


// clamp an `:int` to fit in an `:int8` but interpret the `:int` as an unsigned 8-bit value,
// and clamp between 0 and 255.
pub fip extern uint8( i : int) : int8
  c  "kk_integer_clamp_byte"
  cs "Primitive.IntToUInt8"
  js "$std_core_types._int_clamp_byte"

// Convert an `:int8` to an `:int` but interpret the `:int8` as an unsigned 8-bit value between 0 and 255.
pub inline fip extern int8/uint( i : int8 ) : int
  c  "kk_integer_from_uint8"
  cs inline "(new BigInteger(#1 >= 0 ? #1 : 256 + #1))"
  js "$std_core_types._int_from_int32"


// clamp an `:int` to fit in an `:int16`.
pub fip extern int16( i : int) : int16
  c  "kk_integer_clamp_int16"
  cs "Primitive.IntToInt16"
  js "$std_core_types._int_clamp16"

// Convert an `:int16` to an `:int`.
pub inline fip extern int16/int( i : int16 ) : int
  c  "kk_integer_from_int16"
  cs inline "(new BigInteger(#1))"
  js "$std_core_types._int_from_int32"



// clamp an `:int` to fit in an `:intptr_t`.
pub fip extern intptr_t( i : int) : intptr_t
  c  "kk_integer_clamp_intptr_t"
  cs "Primitive.IntToInt64"
  js "$std_core_types._int_clamp64"

// Convert an `:intptr_t` to an `:int`.
pub inline fip extern intptr_t/int( i : intptr_t ) : int
  c  "kk_integer_from_intptr_t"
  cs inline "(new BigInteger(#1))"
  js "$std_core_types._int_from_int64"

// Convert an integer to an `:ssize_t`. The number is _clamped_ to the maximal or minimum `:ssize_t`
// value if it is outside the range of an `:ssize_t`.
// Needed for evidence indices in `module std/core/hnd`
pub fip extern ssize_t( i : int) : ssize_t
  c  "kk_integer_clamp_ssize_t"
  cs "Primitive.IntToInt32"
  js "$std_core_types._int_clamp32"

// Convert an `:ssize_t` to an `:int`.
pub inline fip extern ssize_t/int( i : ssize_t ) : int
  c  "kk_integer_from_ssize_t"
  cs inline "(new BigInteger(#1))"
  js "$std_core_types._int_from_int32"