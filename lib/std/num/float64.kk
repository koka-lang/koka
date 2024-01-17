/*---------------------------------------------------------------------------
  Copyright 2017-2020 Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* 64-bit IEEE floating point numbers.

Also described as _double_ precision or _binary64_ numbers in the standards.
See also <https://en.wikipedia.org/wiki/Double-precision_floating-point_format>.

*/
module std/num/float64

import std/text/parse
import std/num/int32
import std/num/int64

extern import
  c  file "float64-inline"
  js file "float64-inline.js"


//-----------------------------------------
// Constants
//-----------------------------------------

// &pi;
pub val pi          = 0x1.9_21FB_5444_2D18p+1 //3.141592653589793238462643383279502884

// &pi;
pub val flt-pi      = pi

// 2&pi;
pub val flt-twopi   = 0x1.9_21FB_5444_2D18p+2

// &pi;/2
pub val flt-pi2     = 0x1.9_21FB_5444_2D18p+0

// &pi;/4
pub val flt-pi4     = 0x1.9_21FB_5444_2D18p-1

// 3&pi;/4
pub val flt-pi34    =0x1.2_D97C_7F33_21D2p+1

// The [_e_](https://en.wikipedia.org/wiki/E_(mathematical_constant)) constant.
pub val flt-e       = 0x1.5_BF0A_8B14_5769p+1 // 2.718281828459045235360287471352662498

// The natural logarithm of 2
pub val flt-ln2     = 0x1.6_2E42_FEFA_39EFp-1 // 0.693147180559945309417232121458176568

// The natural logarithm of 10
pub val flt-ln10    = 0x1.2_6BB1_BBB5_5516p+1 // 2.302585092994045684017991454684364208

// The base-2 logarithm of _e_.
pub val flt-log2e   =  0x1.7_1547_652B_82FEp+0 // 1.442695040888963407359924681001892137

// The base-10 logarithm of _e_.
pub val flt-log10e  = 0x1.B_CB7B_1526_E50Ep-2 // 0.434294481903251827651128918916605082

// The square-root of 2
pub val flt-sqrt2   = 0x1.6_A09E_667F_3BCDp+0 // 1.414213562373095048801688724209698079

// `1.0 / sqrt(2.0)`  (= `sqrt(0.5)`)
pub val flt-sqrt12  = 0x1.6_A09E_667F_3BCDp-1 // 0.707106781186547524400844362104849039

// [Euler's constant](https://en.wikipedia.org/wiki/Euler%E2%80%93Mascheroni_constant)
pub val flt-euler   = 0x1.2_788C_FC6F_B619p-1 // 0.577215664901532860606512090082402431

// Maximum float64 value
pub val flt-max     = 0x1.F_FFFF_FFFF_FFFFp+1023 // 1.79769313486231570815e+308

// Smallest positive normalized float64 value
pub val flt-min     = 0x1.0p-1022 // 2.22507385850720138309e-308

// Smallest positive subnormal value (i.e. [``DBL_TRUE_MIN``](https://en.cppreference.com/w/cpp/types/climits))
pub val flt-true-min= 0x1.0p-1074 // 4.9406564584124654418e-324

// Machine epsilon: the difference between 1.0 and the next representable `:float64` value.
pub val flt-epsilon = 0x1.0p-52 // 2.2204460492503130808e-16

// maximal precision in decimal digits of a `:float64`.
pub val flt-max-prec= 15

// Represents a value that is _not a number_ (NaN)
pub val nan : float64    = make-nan()

// Positive infinity
pub val posinf : float64 = make-posinf()

// Negative infinity
pub val neginf : float64 = make-neginf()


// ---------------------------------------
// Basic operations
// ----------------------------------------

// Are two floating point number equal?
pub inline fip extern (==)(x : float64, y : float64) : bool
  inline "(#1 == #2)"
  js inline "(#1 === #2)"

// Are two floating point number not equal?
pub inline fip extern (!=)(x : float64, y : float64) : bool
  inline "(#1 != #2)"
  js inline "(#1 !== #2)"

// Is the first floating point number smaller or equal to the second?
pub inline fip extern (<=)( x : float64, y : float64) : bool
  inline "(#1 <= #2)"

// Is the first floating point number greater or equal to the second?
pub inline fip extern (>=)( x : float64, y : float64) : bool
  inline "(#1 >= #2)"

// Is the first floating point number smaller than the second?
pub inline fip extern (<)( x : float64, y : float64) : bool
  inline "(#1 < #2)"

// Is the first floating point number greater than the second?
pub inline fip extern (>)( x : float64, y : float64) : bool
  inline "(#1 > #2)"

// Add two floating point numbers.
pub inline fip extern (+)( x : float64, y : float64) : float64
  inline "(#1 + #2)"

// Subtract two floating point numbers.
pub inline fip extern (-)( x : float64, y : float64) : float64
  inline "(#1 - #2)"

// Multiply two floating point numbers.
pub inline fip extern (*)( x : float64, y : float64) : float64
  inline "(#1 * #2)"

// Divide two floating point numbers.
pub inline fip extern (/)( x : float64, y : float64) : float64
  inline "(#1 / #2)"

// Modulus of two floating point numbers.
pub inline fip extern (%)( x : float64, y : float64) : float64
  c inline "fmod(#1,#2)"
  inline "(#1 % #2)"

// Is the value negative?
pub fip fun is-neg( d : float64 ) : bool
  d < 0.0

// Is the value positive?
pub fip fun is-pos( d : float64 ) : bool
  d > 0.0

// Is the value zero?
pub fip fun is-zero( d : float64 ) : bool
  d == 0.0

// Compare the argument to zero.
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

// Convert an integer to a `:float64`. May return `nan` if the integer is too large to represent as a `:float64`.
pub inline fip extern float64( i : int) : float64
  c  "kk_integer_as_double"
  cs "Primitive.IntToDouble"
  js "$std_core_types._int_to_double"

// Convert an integer to a `:float32`. May return `nan` if the integer is too large to represent as a `:float32`.
pub inline fip extern float32( i : int) : float32
  c  "kk_integer_as_float"
  cs "Primitive.IntToFloat"
  js "$std_core_types._int_to_float"

// Convert an 64-bit integer to a `:float64`.
pub fip fun int64/float64( i : int64 ) : float64
  i.int.float64



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


// Returns the smallest element of a list of floats (or `0` for the empty list)
pub fun minimum( xs : list<float64> ) : float64
  match xs
    Nil -> 0.0
    Cons(x,xx) -> xx.foldl( x, min )

// Returns the largest element of a list of floats (or `0` for the empty list)
pub fun maximum( xs : list<float64> ) : float64
  match xs
    Nil -> 0.0
    Cons(x,xx) -> xx.foldl( x, max )


//-----------------------------------------
// NaN, Infinity
//-----------------------------------------

extern make-nan() : float64
  c  inline "(double)NAN"
  cs inline "double.NaN"
  js inline "NaN"

extern make-posinf() : float64
  c  inline "HUGE_VAL"
  cs inline "double.PositiveInfinity"
  js inline "Infinity"

extern make-neginf() : float64
  c  inline "-HUGE_VAL"
  cs inline "double.NegativeInfinity"
  js inline "-Infinity"


// Is this value equal to NaN ?
pub inline extern is-nan( d : float64 ) : bool
  c  inline "isnan(#1)"
  cs "double.IsNaN"
  js "isNaN"

// Is this value equal to negative or positive infinity ?
pub extern is-inf( d : float64 ) : bool
  c  inline "isinf(#1)"
  cs "double.IsInfinity"
  js inline "((#1) === Infinity || (#1) === -Infinity)"

// Is this value equal to positive infinity ?
pub inline extern is-posinf( d : float64 ) : bool
  c  inline "(isinf(#1) && !signbit(#1))"
  cs "double.IsPositiveInfinity"
  js inline "((#1) === Infinity)"

// Is this value equal to negative infinity ?
pub inline extern is-neginf( d : float64 ) : bool
  c inline "(isinf(#1) && signbit(#1))"
  cs "double.IsNegativeInfinity"
  js inline "((#1) === -Infinity)"

// Is this a finite number (i.e. not `nan` or infinity)
pub inline extern is-finite( d : float64 ) : bool
  c  inline "isfinite(#1)"
  cs inline "(!double.IsNaN(#1) && !double.IsInfinity(#1))"
  js "isFinite"

// Is this a negative zero value?
pub fun is-negzero( d : float64 ) : bool
  (d==0.0 && is-neginf(1.0 / d))

// Is this a [subnormal](https://en.wikipedia.org/wiki/Denormal_number) value?
// (i.e. `0 < d.abs < flt-min`)
pub fun is-subnormal( d :float64 ) : bool
  (d != 0.0 && d.abs < flt-min)


//-----------------------------------------
// Rounding
//-----------------------------------------

// Round a float64 to its nearest integral value.
// If the value is halfway between two integers, the value is rounded towards the even one.
pub inline extern round( d : float64 ) : float64
  c  "kk_double_round_even"
  cs inline "Math.Round(#1,MidpointRounding.ToEven)"
  js "$std_core_types._double_round_even"

// Return the largest integer equal or less than `d`
pub inline extern floor( d : float64 ) : float64
  c  "floor"
  cs "Math.Floor"
  js "Math.floor"

// Return the smallest integer equal or larger than `d`
pub inline extern ceiling( d : float64 ) : float64
  c  "ceil"
  cs "Math.Ceiling"
  js "Math.ceil"

// fused multiply-add. Computes `(x*y)+z` as if to infinite precision
// with only the final result rounded back to a `:float64`.
pub extern fmadd( x : float64, y : float64, z : float64 ) : float64
  c  "fma"
  js "_fmadd"
  cs "Math.FusedMultiplyAdd"

// Return the integral part of a `:float64` `d` .
// If `d >= 0.0` , return the largest integer equal or less to `d` ,
// If `d < 0.0` , return the smallest integer equal or larger to `d` .
pub fun truncate(d : float64) : float64
  if d >= 0.0 then floor(d) else ceiling(d)

// Return the fractional part of a `:float64` `d`.\
// `d.truncate + d.fraction === d`\
// `(-2.4).fraction === -0.4`
pub fun fraction(d : float64) : float64
  d - d.truncate

// Return the 'floored' fraction of `d`, always greater or equal to zero.\
// `d.floor + d.ffraction === d`\
// `(-2.4).ffraction === 0.6`
pub fun ffraction(d : float64) : float64
  d - d.floor


// Round a float64 to a specified precision. Rounds to the  even number in case of a tie.\
// `123.456.round-to-prec(2) == 123.46`\
// `123.456.round-to-prec(-1) == 120.0`\
pub fun round-to-prec( d : float64, prec : int  ) : float64
  if prec <= 0 then d.round else
    val p  = exp10(prec.float64)
    (d * p).round / p


// Round a `:float64` to a `:float32`
pub extern float64/float32( f : float64 ) : float32
  c inline "(float)(#1)"
  js "$std_core_types._double_to_float"

// Extend a `:float32` to a `:float64`
pub extern float32/float64( f : float32 ) : float64
  c inline "(double)(#1)"
  js inline "(#1)"

// Short version of `float32` for convenience, e.g. `1.337.f32`. For example:
// ```
// > 1.337.show-hex ++ " != " ++ 1.337.f32.float64.show-hex
// "0x1.5645A1CAC0831p+0 != 0x1.5645A2p+0"
// ```
// .
pub fun f32( f : float64 ) : float32
  f.float32


//-----------------------------------------
// Powers
//-----------------------------------------

// Return the square root of a value `d`
// Returns `nan` if `d == nan`  or if `d`  is negative.
// Returns `inf` if `d == inf` .
pub inline extern sqrt( d : float64 ) : float64
  c  "sqrt"
  cs "Math.Sqrt"
  js "Math.sqrt"


// Return the cubic root of a value `d`
// Returns `nan` if `d == nan`  or if `d`  is negative.
// Returns `inf` if `d == inf` .
pub inline extern cbrt( d : float64 ) : float64
  c  "cbrt"
  cs "Math.Cbrt"
  js "Math.cbrt"

// Return the `d` raised to the power of `p`.
pub inline extern pow( d : float64, p : float64) : float64
  c  "pow"
  cs "Math.Pow"
  js "Math.pow"

// Return the natural logarithm (in base _e_)  of a `:float64` `f`
pub inline extern ln( f : float64 ) : float64
  c  "log"
  cs "Math.Log"
  js "Math.log"

// Return the logarithm in base 10 of a `:float64` `f`.
pub inline extern log10( f : float64 ) : float64
  c  "log10"
  cs "Math.Log10"
  js "Math.log10"

// Return the logarithm in base 2 of a `:float64` `f`.
pub inline extern log2( f : float64 ) : float64
  c  "log2"
  cs "Math.Log2"
  js "Math.log2"

// Return the logarithm in base `base` of a `:float64` `f`
pub fun log( f : float64, base : float64 ) : float64
  ln(f) / ln(base)

// Return _e_ to the power of `p`.
pub inline extern exp( p : float64 ) : float64
  c  "exp"
  cs "Math.Exp"
  js "Math.exp"

// Return the 10 to the power of `p`.
pub inline extern exp10( p : float64 ) : float64
  c  inline "pow(10.0,#1)"
  cs inline "Math.Pow(10.0,#1)"
  js inline "Math.pow(10.0,#1)"

// Return the 2 to the power of `p`.
pub inline extern exp2( p : float64 ) : float64
  c  "exp2"
  cs inline "Math.Pow(2.0,#1)"
  js inline "Math.pow(2.0,#1)"


fun log2p1( x : float64 ) : float64
  flt-log2e * ln1p(x)

fun exp2m1( x : float64 ) : float64
  expm1(flt-ln2 * x)

// Returns `ln(exp(x) + exp(y))`.
// Avoids overlow/underflow errors.
pub fun lnaddexp( x : float64, y : float64 ) : float64
  if x==y then x + flt-ln2 else
    val z = x - y
    if z > 0.0 then x + ln1p(exp(~z))
               else y + ln1p(exp(z))


// Returns `log2( exp2(x) + exp2(y) )`.
// Avoids overlow/underflow errors.
pub fun logaddexp2( x : float64, y : float64 ) : float64
  if x==y then x + 1.0 else
    val z = x - y
    if z > 0.0 then x + log2p1(exp2(~z))
               else y + log2p1(exp2(z))


// Return if two floats are nearly equal with respect to some `epsilon` (=`8*flt-epsilon`).
// The epsilon is the nearest difference for numbers around 1.0. The routine automatically
// scales the epsilon for larger and smaller numbers, and for subnormal numbers.
pub fun nearly-eq( x : float64, y : float64, epsilon : float64 = 8.0*flt-epsilon ) : bool
  if x == y return True
  val diff = (x - y).abs
  if x==0.0 || y==0.0 || diff < flt-min then
    // very close to zero, scale the epsilon for denormalized numbers
    (2.0*diff < (epsilon * flt-min))
  else
    val sum = x.abs + y.abs
    ((2.0*diff / (if sum > flt-max then flt-max else sum)) < epsilon)


// Return if two floats are nearly equal with respect to an `epsilon` of `8*flt-epsilon`.
// See also `nearly-eq` which takes an explicit `epsilon`.
pub fun (~=)(x : float64, y : float64 ) : bool
  nearly-eq(x,y)

//-----------------------------------------
// Ldexp/Frexp
//-----------------------------------------

// Low-level: return the bits of a 64-bit `:float64` as in `:int64`
pub extern float64-to-bits( d : float64 ) : int64
  c  "kk_double_to_bits"
  cs "Primitive.DoubleToBits"
  js "_double_to_bits"


// Low-level: create a `:float64` from the given 64-bit integer.
pub extern float64-from-bits( i : int64 ) : float64
  c  "kk_double_from_bits"
  cs "Primitive.DoubleFromBits"
  js "_double_from_bits"


// Calculate 2&middot;^`e`^ for an integer `e`.
// Uses efficient bit conversion for exponents between  -1022 and 1023 and
// otherwise falls back to the regular `exp2` function converting `e` to a float64.
pub fun exp2i( e : int ) : float64
  if e >= -1022 && e <= 1023
    then float64-from-bits( (1023 + e).int64.shl(52) )
    else exp2( e.float64 )


// Create a float64 `d` given a mantissa `man` and exponent `exp`
// such that `man`&middot;2^`exp`^ =  `d` exactly (if it is representable
// by a `:float64`). See also `ldexp`.
pub fun encode( man : int, exp : int ) : float64
  ldexp(man.float64,exp)

val one-p1023 = 0x1.0p1023
val one-m1022 = 0x1.0p-1022   // = flt-min

fun mul-exp2( x : float64, e : int ) : float64
  x * exp2i(e)

// 'Load exponent': returns `x`&middot;2^`e`^ for a `is-finite` `x` and
// otherwise `x` itself. See also `encode` which loads an integer mantissa.
pub fun ldexp( x : float64, e : int ) : float64
  if !is-finite(x) then x
  elif e >= -1022 then
    if e <= 1023 then mul-exp2(x,e)  // usually this branch
    elif e <= 2046 then mul-exp2( x*one-p1023, e - 1023 )
    elif e <= 3069 then mul-exp2( x*one-p1023*one-p1023, e - 2046 )
    elif x < 0.0 then neginf else posinf
  else
    if e >= -2044 then mul-exp2(x*one-m1022, e + 1022)
    elif e >= -3066 then mul-exp2(x*one-m1022*one-m1022, e + 2044 )
    elif x < 0.0 then -0.0 else 0.0


// Decode a float64 `d` into a tuple `(m,e)` of a mantissa `m` and exponent `e`
// such that `m`&middot;2^`e`^ =  `d` exactly. The mantissa `m` is
// always either 0 or in the range [2^52^, 2^53^). See also `frexp`.
pub fun decode( d : float64 ) : (int,int)
  if d==0.0 then (0,0)
  elif d.is-subnormal then decode-normalized(d * 0x1.0p54, -54)
  else decode-normalized(d,0)

// decode a normalized float64 (i.e. not subnormal)
fun decode-normalized( d : float64, e-adjust : int = 0 ) : (int,int)
  val i    = float64-to-bits(d)
  val exp  = and(i.shr(52),0x7FF.int64) - 1043.int64
  val man  = and(i,0x000F_FFFF_FFFF_FFFF.int64) + 0x0010_0000_0000_0000.int64
  ( (if i.is-neg then ~man else man).int, exp.int - 32 + e-adjust )


// 'Fraction/exponent': return the normalized fraction `f` and exponent `exp`
// for a number `x` such that `x == f`&middot;2^`exp`^.
// The absolute value of the fraction `f` is always in the range [0.5, 1.0), or
// one of `0.0`, `-0.0`, `neginf`, `posinf`, or `nan`.
// See also `decode` which  decodes to an integer mantissa.
pub fun frexp( x : float64 ) : (float64, int)
  if !x.is-finite || x.is-negzero return (x,0)
  val (m,e) = x.decode
  (m.float64 * 0x1.0p-53, e + 53  )


// Returns the greatest `:float64` less than `x`.
// This behaves exactly as `nextDown` in the IEEE 754-2008 standard.
// The identity `x.next-down == ~next-down(~x)` holds for all `x`.
// When `x` is finite `x == x.next-down.next-up` also holds.
pub fun next-down( x : float64 ) : float64
  if x.is-nan || x.is-neginf then x
  elif x.is-zero then ~flt-true-min
  else
    val i = float64-to-bits(x)
    val next = if i.is-neg then i.inc else i.dec
    float64-from-bits(next)


// Returns the least `:float64` greater than `x`.
// This behaves exactly as `nextUp` in the IEEE 754-2008 standard.
// The identity `x.next-up == ~next-down(~x)` holds for all `x`.
// When `x` is finite `x == x.next-up.next-down` also holds.
pub fun next-up( x : float64 ) : float64
  if x.is-nan || x.is-posinf then x
  elif x.is-zero then flt-true-min
  else
    val i = float64-to-bits(x)
    val next = if i.is-neg then i.dec else i.inc
    float64-from-bits(next)


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
pub fun cmp( x : float64, y : float64 ) : order
  val bx = float64-to-bits(x)
  val by = float64-to-bits(y)
  // flip exp+mantissa bits if negative to get a two's complement number
  val ix = bx.sar(63).shr(1).xor(bx)
  val iy = by.sar(63).shr(1).xor(by)
  cmp(ix,iy)


// The midpoint is the average of `x` and `y`.
// Avoids overflow on large numbers.
pub fun midpoint( x : float64, y : float64 ) : float64
  if is-subnormal(x) || is-subnormal(y)
    then (x + y) / 2.0
    else (x / 2.0) + (y / 2.0)


// Linear interpolation, calculating `x + t*(y - x)` but avoids troublesome edge cases.
// Follows the C++20 [specification](https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2019/p0811r3.html).
// In particular, if `x.is-finite && y.is-finite`, then:
// - exact: `lerp(x,y,0.0) == x` and  `lerp(x,y,1.0) == y`
// - monotonic: if `x <= y` and `t1 <= t2`, then `cmp( lerp(x,y,t1), lerp(x,y,t2) ) <= Eq` (and other cases)
// - deterministic: only `lerp(x,x,flt-inf)` results in `nan`
// - bounded: `t<0.0 || t>1.0 || is-finite(lerp(x,y,t))`
// - consistent: `lerp(x,x,t) == x`
pub fun lerp( x : float64, y : float64, t : float64 ) : float64
  if (x <= 0.0 && y >= 0.0) || (x >= 0.0 && y <= 0.0) then
    t*y + (1.0 - t)*x
  elif t == 1.0 then
    y
  else
    val z = x + t*(y - x)
    if ((t > 1.0) == (y > x)) then max(y,z) else min(y,z)


//-----------------------------------------
// Show in hexadecimal
//-----------------------------------------

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
pub fun show-hex( d : float64, width : int = 1, use-capitals : bool = True, pre : string = "0x" ) : string
  if !d.is-finite then d.show else
    val (m,e) = d.decode
    val man   = m.abs.show-hex(1,use-capitals,"")
    val exp0  = e + 4*(man.count - 1)
    val exp   = (if exp0 >= 0 then "+" else "") ++ exp0.show
    val frac  = man.tail.trim-right("0").pad-right(max(1,width),'0')
    val sign  = if d.is-neg || d.is-negzero then "-" else ""
    sign ++ pre ++ man.head ++ "." ++ frac ++ "p" ++ exp


//-----------------------------------------
// Parse a float64
//-----------------------------------------

// Parse a float64 number. Can be "NaN", "Inf(inity)" (case-insensitive),
// a fix-point number (`1.2`) or in scientific notation (`-2.3e-5`).
// Also allows floats in [hexadecimal notation](https://books.google.com/books?id=FgMsCwAAQBAJ&pg=PA41) (`0xA.Fp-10`) that can
// be represented precisely (and are the preferred _round trip_ format).
pub fun parse-float64( s : string ) : maybe<float64>
  s.trim.to-lower.slice.parse-eof(pdouble).maybe

pub fun pdouble() : parse float64
  val neg = sign()
  val d = [{ phexdouble() }, { pdecdouble() }, { pspecial() }, { 0.0 }].choose
  if neg then ~d else d

fun phexdouble() : parse float64
  char('0')
  one-of("xX")
  val man  = hex-digits()
  val frac = optional( "", { char('.'); hex-digits() }).trim-right("0")
  val exp : int = optional( 0, { one-of("pP"); pint() })
  val m : int = (man ++ frac).parse-int(hex=True).default(0)
  val e : int = exp - 4*frac.count
  encode(m,e)

fun pdecdouble() : parse float64
  val cur  = current-input()
  val man  = digits()
  val frac = optional("", { char('.'); digits0() }).trim-right("0")
  val exp : int = optional( 0, { one-of("eE"); pint() })
  //val m : int = (man ++ frac).parse-int.default(0)
  //val e : int = exp - frac.count
  //m.float64 * exp10(e.float64)
  cur.string.prim-parse-float64  // more precision than simple multiply

fun pspecial() : parse float64
  [{ pstring("nan"); nan },
   { pstring("infinity"); posinf },
   { pstring("inf"); posinf }
  ].choose

// Return `nan` on failure
extern prim-parse-float64( s : string ) : float64
  c  "kk_prim_parse_double"
  cs "Primitive.DoubleParse"
  js "parseFloat"


//-----------------------------------------
// Various
//-----------------------------------------

// Return the sum of a list of floats.
// Uses [Kahan-Babu&scaron;kan-Neumaier summation](https://en.wikipedia.org/wiki/Kahan_summation_algorithm#Further_enhancements)
// to minimize rounding errors. This
// is more precise as Kahan summation and about as fast.\
// `[1.0e3,1.0e97,1.0e3,-1.0e97].sum == 2000.0`\
// while\
// `[1.0e3,1.0e97,1.0e3,-1.0e97].foldl(0.0,(+)) == 0.0` (!)\
// A. Neumaier, _Rundungsfehleranalyse einiger Verfahren zur Summation endlicher Summen_.
// Math. Mechanik, 54:39--51, 1974.
pub fun sum( xs : list<float64> ) : float64
  var total := 0.0
  var comp  := 0.0
  xs.foreach fn(x)
    val t = total + x
    val c = if total.abs >= x.abs then (total - t) + x else (x - t) + total
    comp  := comp + c
    total := t
  total + comp;

// The hypotenuse of `x` and `y`: `sqrt(x*x + y*y)`.
// Prevents overflow for large numbers.
pub fun hypot( x : float64, y : float64 ) : float64
  val xx = abs(x)
  val yy = abs(y)
  val lo = min(xx,yy)
  val hi = max(xx,yy)
  if hi==0.0 then 0.0 else
    val z  = lo / hi
    hi * sqrt( 1.0 + z*z )


// The square root of the sum of the squares of three floats.
// Prevents overflow for large numbers.
pub fun xyz/hypot( x : float64, y : float64, z : float64 ) : float64
  val xx = abs(x)
  val yy = abs(y)
  val zz = abs(z)
  val hi = max(max(xx,yy),zz)
  if hi==0.0 then 0.0 else
    hi * sqrt( sqr(xx / hi) + sqr(yy / hi) + sqr(zz / hi) )


// The square root of the sum of squares of a list of floats.
// Prevents overflow for large numbers and uses Kahan-Babu&scaron;kan-Neumaier summation
// for precision.
pub fun list/hypot( xs : list<float64> ) : float64
  val hi = xs.abs-max
  if hi==0.0 then 0.0 else
    hi * xs.map( fn(x) sqr(x / hi) ).sum.sqrt


// The square of a float64
pub fun sqr(x : float64 ): float64
  x*x

// The maximum of the absolute values.
pub fun abs-max( x : float64, y : float64 ) : float64
  max(abs(x),abs(y))

// The maximum of a list of absolute values.
pub fun list/abs-max( xs : list<float64> ) : float64
  xs.foldl(0.0, fn(m,x) max(abs(x),m) )

//-----------------------------------------
// Trigonometry
//-----------------------------------------

val rad2deg : float64 = 180.0/pi
val deg2rad : float64 = pi/180.0

// Convert radians to degrees.
pub fun deg( rad : float64 ) : float64
  rad * rad2deg

// Convert degrees to radians.
pub fun rad( deg : float64 ) : float64
  deg * deg2rad


// Return `x` with the sign of `y`.
pub fun with-sign-of( x : float64, y : float64 ) : float64
  if y < 0.0 then ~(x.abs) else x.abs


// Return the sine of an angle in radians `d`.
pub inline extern sin( d : float64 ) : float64
  c  "sin"
  cs "Math.Sin"
  js "Math.sin"

// Return the cosine of an angle in radians `d`.
pub inline extern cos( d : float64 ) : float64
  c  "cos"
  cs "Math.Cos"
  js "Math.cos"

// Return the tangent of an angle in radians `d`.
pub inline extern tan( d : float64 ) : float64
  c  "tan"
  cs "Math.Tan"
  js "Math.tan"

// Return the arc-tangent of `d`
pub inline extern atan( d : float64 ) : float64
  c  "atan"
  cs "Math.Atan"
  js "Math.atan"

// Return the arc-tangent of a point (`x`,`y`).
pub inline extern atan2( x : float64, y : float64 ) : float64
  c  "atan2"
  cs "Math.Atan2"
  js "Math.atan2"

// Return the arc-cosine of `d`
pub inline extern acos( d : float64 ) : float64
  c  "acos"
  cs "Math.Acos"
  js "Math.acos"

// Return the arc-sine of `d`
pub inline extern asin( d : float64 ) : float64
  c  "asin"
  cs "Math.Asin"
  js "Math.asin"


// The hyperbolic tangent of `d`
pub inline extern tanh( d : float64 ) : float64
  c  "tanh"
  cs "Math.Tanh"
  js "Math.tanh"

// The hyperbolic cosine of `d`
pub inline extern cosh( d : float64 ) : float64
  c  "cosh"
  cs "Math.Cosh"
  js "Math.cosh"

// The hyperbolic sine of `d`
pub inline extern sinh( d : float64 ) : float64
  c  "sinh"
  cs "Math.Sinh"
  js "Math.sinh"

// Return `ln(1.0 + x)`.
// Avoids potential imprecision for small `x` where adding `1.0` explicitly
// may lead to rounding errors.
pub inline extern ln1p( d : float64 ) : float64
  c  "log1p"
  js "Math.log1p"

// Return `exp(x - 1.0)`.
// Avoids rounding errors for values of `x` very close to `1.0`.
pub inline extern expm1( d : float64 ) : float64
  c  "expm1"
  js "Math.expm1"

// The area hyperbolic tangent of `d`
pub extern atanh( d : float64 ) : float64
  c  "atanh"
  js "Math.atanh"

// The area hyperbolic cosine of `d`
pub extern acosh( d : float64 ) : float64
  c  "acosh"
  js "Math.acosh"

// The area hyperbolic sine of `d`
pub extern asinh( d : float64 ) : float64
  c "asinh"
  js "Math.asinh"


// ------------------------
// explicit implementations
// ------------------------

/*
// Return `log(1.0 + x)`.
// Avoids potential imprecision for small `x` where adding `1.0` explicitly
// may lead to rounding errors.
pub fun log1p( x : float64 ) : float64
  if x.is-posinf then x else
    val y = 1.0 + x
    val z = y - 1.0
    if z==0.0 then x else
      log(y) * (x / z)


// Return `exp(x - 1.0)`.
// Avoids rounding errors for values of `x` very close to `1.0`.
pub fun expm1( x : float64 ) : float64
  if x.is-posinf then x else
    val y = exp(x)
    if y==1.0 then x
    elif y - 1.0 == -1.0 then -1.0
    else (y - 1.0) * (x / log(y))


// The area hyperbolic tangent of `x`
pub fun atanh( x : float64 ) : float64
  0.5*log( (1.0 + x) / (1.0 - x))
  /*
  // 0.5*log( (1.0 + x) / (1.0 - x)) = 0.5*log1p( (2.0*x) / (1-x) )
  if x.abs >= 0.5 then 0.5*log1p(2.0*(x/(1.0 - x))) else
    val x2 = x + x
    0.5*log1p(x2 + (x2*x)/(1.0 - x))

  */

// The area hyperbolic cosine of `x`
pub fun acosh( x : float64 ) : float64
  // log(x + sqrt((x - 1.0)*(x + 1.0)));
  if x > 0x1.0p28 then flt-ln2 + log(x)
  elif x > 2.0 then
    log(2.0*x - 1.0/(x + sqrt(x.sqr - 1.0)))

  elif x <= 1.0 then (if x < 1.0 then nan else 0.0)
  else
    val xm1 = x - 1.0
    log1p( xm1 + sqrt(2.0*xm1 + xm1*xm1) )


// The area hyperbolic sine of `x`
pub fun asinh( x : float64 ) : float64
  //log( x + sqrt(x.sqr + 1.0))
  val xa = x.abs
  if xa > 0x1.0p28 then (log(xa) + flt-ln2).with-sign-of(x)
  elif xa >= 2.0 then (log(2.0*xa + 1.0/(xa + sqrt(xa.sqr + 1.0)))).with-sign-of(x)
  elif xa == 0.0 then 0.0
  else
    val xa2 = xa.sqr
    log1p( xa + xa2/(1.0 + sqrt( xa2 + 1.0 ))).with-sign-of(x)


*/
