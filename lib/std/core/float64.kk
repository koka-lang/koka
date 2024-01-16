/*---------------------------------------------------------------------------
  Copyright 2012-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Basic `:float64` functions.
//
// A 64-bit IEEE 754 double precision floating point value.
// See also `module std/num/float64` for more advanced operations.
module std/core/float64

import std/core/types

extern import
  c file "inline/float64.h"

pub inline fip extern (==) : (float64,float64) -> bool  { inline "(#1 == #2)"; js inline "(#1 === #2)" }
pub inline fip extern (!=) : (float64,float64) -> bool  { inline "(#1 != #2)"; js inline "(#1 !== #2)" }
pub inline fip extern (<=) : (float64,float64) -> bool  { inline "(#1 <= #2)" }
pub inline fip extern (>=) : (float64,float64) -> bool  { inline "(#1 >= #2)" }
pub inline fip extern (<)  : (float64,float64) -> bool  { inline "(#1 < #2)" }
pub inline fip extern (>)  : (float64,float64) -> bool  { inline "(#1 > #2)" }
pub inline fip extern (+) : (float64,float64) -> float64 { inline "(#1 + #2)" }
pub inline fip extern (-) : (float64,float64) -> float64 { inline "(#1 - #2)" }
pub inline fip extern (*) : (float64,float64) -> float64 { inline "(#1 * #2)" }
pub inline fip extern (/) : (float64,float64) -> float64 { inline "(#1 / #2)" }
pub inline fip extern (%) : (float64,float64) -> float64 { c inline "fmod(#1,#2)"; inline "(#1 % #2)" }


// Is the value negative?
pub fip fun is-neg( d : float64 ) : bool
  d < 0.0

// Is the value positive?
pub fip fun is-pos( d : float64 ) : bool
  d > 0.0

// Is the value zero?
pub fip fun is-zero( d : float64 ) : bool
  d == 0.0

pub fip fun sign( d : float64 ) : order
  if d < 0.0 then Lt elif d > 0.0 then Gt else Eq

// Negate a `:float64`.
pub inline fip extern (~)( f : float64 ) : float64
  inline "(-#1)"  // inline so `~0.0` becomes negative zero

// convert a `:float64` to an `:int` using `round` to round to its nearest integer.
// (rounding to an even number on a tie)
// Returns `0` if the argument is not `finite?`.
pub inline extern int( f : float64 ) : int
  c  "kk_integer_from_double"
  cs "Primitive.IntDouble"
  js "$std_core_types._int_double"

// Returns the value `f`  raised to the power `p` .
pub inline fip extern (^)( f : float64, p : float64) : float64
  c inline "pow(#1,#2)"
  cs "Math.Pow"
  js "Math.pow"

// Return the absolute value of a `:float64` `f`
pub inline fip extern abs( f : float64 ) : float64
  c inline "kk_double_abs(#1)"
  cs "Math.Abs"
  js "Math.abs"

// Returns the smallest of two floats
pub fip fun min( x : float64, y : float64 ) : float64
  if x <= y then x else y

// Returns the largest of two floats
pub fip fun max( x : float64, y : float64 ) : float64
  if x >= y then x else y

// Show a `:float64` as a string.
// If `d >= 1.0e-5` and `d < 1.0e+21`, `show-fixed` is used and otherwise `show-exp`.
// Default `precision` is `-17`.
pub fun show( d : float64, precision : int = -17 ) : string
  val dabs = d.abs
  if dabs >= 1.0e-5 && dabs < 1.0e+21
    then show-fixed(d,precision)
    else show-exp(d,precision)

// Show a `:float64` fixed-point notation.
// The optional `precision` (= `-2`) specifies the maximum precision.
// If `>=0` it specifies the number of digits behind the dot (up to `20` max).
// If negative, then at most the absolute value of `precision` digits behind the dot are used.
// This may still show a number in exponential notation if the it is too small or large,
// in particular, for  a `d` where `d > 1.0e21` or `d < 1.0e-15`, or if
// `precision.abs > 17`, the `show-exp` routine is used.
pub fun show-fixed( d : float64, precision : int = -2 ) : string
  val dabs = d.abs
  if dabs < 1.0e-15 || dabs > 1.0e+21
    then show-exp(d,precision)
    else show-fixedx(d, precision.int32)

extern show-fixedx( d : float64, prec : int32 ) : string
  c  "kk_double_show_fixed"
  cs "Primitive.DoubleShowFixed"
  js "_double_show_fixed"

// Show a `:float64` in exponential (scientific) notation.
// The optional `precision` (= `-17`) specifies the precision.
// If `>=0` it specifies the number of digits behind the dot (up to `17` max).
// If negative, then at most the absolute value of `precision` digits behind the dot are used.
pub fun show-exp( d : float64, precision : int = -17 )
  show-expx(d,precision.int32)

extern show-expx( d : float64, prec : int32 ) : string
  c  "kk_double_show_exp"
  cs "Primitive.DoubleShowExp"
  js "_double_show_exp"
