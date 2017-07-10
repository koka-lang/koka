/*---------------------------------------------------------------------------
  Copyright 2017 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* Various operations on a 64-bit IEEE `:double`.

*/
public module std/num/double

import std/num/int32

extern include {
  js file "double-inline.js"
}


//-----------------------------------------
// Constants
//-----------------------------------------


// &pi;
val pi          = 3.141592653589793238462643383279502884
val dbl-pi      = pi

// 2&pi;
val dbl-twopi   = 6.283185307179586476925286766559005768

// &pi;/2
val dbl-pi2     = 1.570796326794896619231321691639751442

// The [_e_](https://en.wikipedia.org/wiki/E_(mathematical_constant)) constant.
val dbl-e       = 2.718281828459045235360287471352662498

// The natural logarithm of 2
val dbl-log2    = 0.693147180559945309417232121458176568

// The natural logarithm of 10
val dbl-log10   = 2.302585092994045684017991454684364208

// The base-2 logarithm of _e_.
val dbl-log2e   =  1.442695040888963407359924681001892137

// The base-10 logarithm of _e_.
val dbl-log10e  = 0.434294481903251827651128918916605082

// The square-root of 2
val dbl-sqrt2   = 1.414213562373095048801688724209698079

// `1.0 / sqrt(2.0)`
val dbl-sqrt12 = 0.707106781186547524400844362104849039

// [Euler's constant](https://en.wikipedia.org/wiki/Euler%E2%80%93Mascheroni_constant)
val dbl-euler   = 0.577215664901532860606512090082402431

// Maximum double value
val dbl-max     = 1.79769313486231570815e+308
                  
// Smallest positive normalized double value
val dbl-min     = 2.22507385850720138309e-308

// Smallest positive denormalized value (i.e. ``DBL_TRUE_MIN``)
val dbl-true-min= 4.9406564584124654418e-324

// Machine epsilon: the difference between 1.0 and the next representable `:double` value.
val dbl-epsilon = 2.2204460492503130808e-16

// maximal precision in decimal digits of a `:double`.
val dbl-max-prec= 15

// Represents a value that is _not a number_ (NaN)
val nan : double    = 0.0/0.0

// Positive infinity
val posinf : double = 1.0/0.0

// Negative infinity
val neginf : double = -1.0/0.0


//-----------------------------------------
// NaN, Infinity
//-----------------------------------------

// Is this value equal to NaN ?
extern inline nan? : (double) -> bool {
  cs "double.IsNaN"
  js "isNaN"
} 

// Is this value equal to negative or positive infinity ?
extern inf? : (double) -> bool {
  cs "double.IsInfinity"
  js inline "((#1) === Infinity || (#1) === -Infinity)"
}

// Is this value equal to positive infinity ?
extern inline posinf? : (double) -> bool {
  cs "double.IsPositiveInfinity"
  js inline "((#1) === Infinity)"
}

// Is this value equal to negative infinity ?
extern inline neginf? : (double) -> bool {
  cs "double.IsNegativeInfinity"
  js inline "((#1) === -Infinity)"
}

// Is this a finite number (i.e. not `nan` or infinity)
extern inline finite?( d : double ) : bool {
  cs inline "(!double.IsNaN(#1) && !double.IsInfinity(#1))"
  js "isFinite"
}

//-----------------------------------------
// Rounding
//-----------------------------------------


// Round a double to its nearest integral value.
// If the value is halfway between two integers, the value is rounded to the even one.
extern round : (d:double) -> double {
  cs "Math.Round"
  js "Math.round"
}

// Return the largest integer equal or less than `d` 
extern inline floor : (d:double) -> double {
  cs "Math.Floor"
  js "Math.floor"
}

// Return the smallest integer equal or larger than `d` 
extern inline ceiling : (d:double) -> double {
  cs "Math.Ceiling"
  js "Math.ceil"
}

// Return the integral part of a `:double` `d` .
// If `d >= 0.0` , return the largest integer equal or less to `d` ,
// If `d < 0.0` , return the smallest integer equal or larger to `d` .
fun trunc(d : double) : double {
  if (d >= 0.0) then floor(d) else ceiling(d)
}

// Return the fractional part of a `:double` `d`.\
// `d.trunc + d.fraction === d`\
// `(-2.4).fraction === -0.4`
fun fraction(d : double) : double {
  d - d.trunc
}

// Return the 'floored' fraction of `d`, always greater or equal to zero.\
// `d.floor + d.ffraction === d`\
// `(-2.4).ffraction === 0.6`
fun ffraction(d : double) : double {
  d - d.floor
}


// Round a double to a specified precision.\
// `123.456.round-to-prec(2) == 123.46`\
// `123.456.round-to-prec(-1) == 120.0`\
fun round-to-prec( d : double, prec : int  ) : double {
  if (prec <= 0) then d.round else {
    val p  = exp10(prec.double)
    (d * p).round / p
  }
}


//-----------------------------------------
// Powers
//-----------------------------------------

// Return the square root of a value `d` 
// Returns `nan` if `d == nan`  or if `d`  is negative.
// Returns `inf` if `d == inf` .
extern inline sqrt : (d:double) -> double {
  cs "Math.Sqrt"
  js "Math.sqrt"
}


// Return the `d` raised to the power of `p`. 
extern inline pow : (d:double, p:double) -> double  { 
  cs "Math.Pow"
  js "Math.pow" 
}

// Return the natural logarithm (in base _e_)  of a `:double` `d` 
extern inline log : (d:double) -> double  { 
  cs "Math.Log"
  js "Math.log" 
}

// Return the logarithm in base 10 of a `:double` `d`.
fun log10( d : double ) : double {
  log(d) / dbl-log10
}

// Return the logarithm in base 2 of a `:double` `d`.
fun log2( d : double ) : double {
  log(d) / dbl-log2
}

// Return _e_ to the power of `p`.
extern inline exp( p : double ) : double {
  cs "Math.Exp"
  js "Math.exp"
}

// Return the 10 to the power of `p`.
fun exp10( p : double ) : double {
  pow(10.0,p)
}

// Return the 2 to the power of `p`.
fun exp2( p : double ) : double {
  pow(2.0,p)
}

// Return `log(1.0 + x)`.
// Avoids potential imprecision for small `x` where adding `1.0` explicitly
// may lead to rounding errors.
fun log1p( x : double ) : double {
  if (x.posinf?) then x else {
    val y = 1.0 + x
    val z = y - 1.0
    if (z==0.0) then x else {
      log(y) * (x / z)
    }
  }
}

// Return `exp(x - 1.0)`.
// Avoids rounding errors for values of `x` very close to `1.0`.
fun expm1( x : double ) : double {
  if (x.posinf?) then x else {
    val y = exp(x)
    if (y==1.0) then x 
    elif (y - 1.0 == -1.0) then -1.0
    else (y - 1.0) * (x / log(y))
  }
}

private fun log2p1( x : double ) : double {
  dbl-log2e * log1p(x)
}

private fun exp2m1( x : double ) : double {
  expm1(dbl-log2 * x)
}

// Returns `log(exp(x) + exp(y))`.
// Avoids overlow/underflow errors.
fun logaddexp( x : double, y : double ) : double {
  if (x==y) then x + dbl-log2 else {
    val z = x - y
    if (z > 0.0) then x + log1p(exp(~z))
                 else y + log1p(exp(z))
  }
}

// Returns `log2( exp2(x) + exp2(y) )`.
// Avoids overlow/underflow errors.
fun logaddexp2( x : double, y : double ) : double {
  if (x==y) then x + 1.0 else {
    val z = x - y
    if (z > 0.0) then x + log2p1(exp2(~z))
                 else y + log2p1(exp2(z))
  }
}

// Return if two doubles are nearly equal with respect to some `epsilon` (=`10*dbl-epsilon`).
// The epsilon is the nearest difference for numbers around 1.0. The routine automatically
// scales the epsilon for larger and smaller numbers, and for numbers close to zero.
fun nearly-eq( x : double, y : double, epsilon : double = 10.0*dbl-epsilon ) : bool {
  if (x == y) return True
  val diff = (x - y).abs
  if (x==0.0 || y==0.0 || diff < dbl-min) {
    // very close to zero, scale the epsilon for denormalized numbers
    (2.0*diff < (epsilon * dbl-min))
  }
  else {
    val sum = x.abs + y.abs
    ((2.0*diff / (if (sum > dbl-max) then dbl-max else sum)) < epsilon)
  }
}

// Return if two doubles are nearly equal with respect to an `epsilon` of `10*dbl-epsilon`.
// See also `nearly-eq` which takes an explicit `epsilon`.
fun (~=)(x : double, y : double ) : bool {
  nearly-eq(x,y)
}

//-----------------------------------------
// Ldexp/Frexp
//-----------------------------------------

// Low-level: return the `(lo,hi)` bits of a 64-bit double.
extern double-to-bits( d : double ) : (int32,int32) {
  cs "Primitive.DoubleToBits"
  js "_double_to_bits"
}

// Low-level: create a `:double` from the given `lo` and `hi` 32-bits. 
extern double-from-bits( lo : int32, hi : int32 ) : double {
  cs "Primitive.DoubleFromBits"
  js "_double_from_bits"
}

private fun ldexpx( x : double, e : int ) : double {
  x * double-from-bits( zero, (0x3FF + e).int32.shl(20) )
}

private val one-p1023 = 8.98846567431157953865e+307
private val one-m1022 = 2.22507385850720138309e-308   // = dbl-min

// 'Load exponent': returns `x`&middot;2^`exp`^.
fun ldexp( x : double, e : int ) : double {
  if (!finite?(x)) then x
  elif (e >= -1022) then {
    if (e <= 1023) then ldexpx(x,e)  // usually this branch
    elif (e <= 2046) then ldexpx( x*one-p1023, e - 1023 )
    elif (e <= 3069) then ldexpx( x*one-p1023*one-p1023, e - 2046 )
    elif (x < 0.0) then neginf else posinf
  }
  else {
    if (e >= -2044) then ldexpx(x*one-m1022, e + 1022)
    elif (e >= -3066) then ldexpx(x*one-m1022*one-m1022, e + 2044 )
    elif (x < 0.0) then -0.0 else 0.0
  }
}

private val two54 = 18014398509481984.0;

// 'Fraction/exponent': return the normalized fraction `f` and exponent `exp`
// for a number `x` such that `x == f`&middot;2^`exp`^.
fun frexp( x : double ) : (double,int) {
  val (lo,hi) = x.double-to-bits
  val e = hi.exponent
  if (e == 0x7FF || x == 0.0) {  // NaN or zero
    (x,0)
  }
  elif (e == 0) { // denormalized, multiply with 2^54
    val (ylo,yhi) = (x * two54).double-to-bits
    ( normalize(ylo,yhi), yhi.exponent - 1022 - 54 )
  }
  else {
    ( normalize(lo,hi), e - 1022 )
  }
}

private fun exponent( hi : int32 ) : int {
  (hi.shr(20) & 0x07FF.int32).int
}

// set the exponent to -1 so the number falls between [0.5 and 1) 
private fun normalize( lo : int32, hi : int32 ) : double {
  val hix = (hi & 0x800FFFFF.int32) | 0x3FE00000.int32
  double-from-bits(lo,hix)
}


//-----------------------------------------
// Various
//-----------------------------------------

// Return the sum of a list of doubles. 
// Uses [Kahan-Babu&scaron;kan-Neumaier summation](https://en.wikipedia.org/wiki/Kahan_summation_algorithm#Further_enhancements) 
// to minimize rounding errors. This
// is more precise as Kahan summation and about as fast.\
// `[1.0e3,1.0e97,1.0e3,-1.0e97].sum == 2000.0`\
// A. Neumaier, _Rundungsfehleranalyse einiger Verfahren zur Summation endlicher Summen_.
// Math. Mechanik, 54:39--51, 1974.
fun sum( xs : list<double> ) : double
{
  var total := 0.0
  var comp  := 0.0
  xs.foreach fun(x) {
    val t = total + x
    val c = if (total.abs >= x.abs) then (total - t) + x else (x - t) + total
    comp  := comp + c
    total := t
  }
  total + comp;
}

// The hypotenuse of `x` and `y`: `sqrt(x*x + y*y)`.
// Prevents overflow for large numbers.
fun hypot( x : double, y : double ) : double {
  val xx = abs(x)
  val yy = abs(y)
  val lo = min(xx,yy)
  val hi = max(xx,yy)
  if (hi==0.0) then 0.0 else {
    val z  = lo / hi
    hi * sqrt( 1.0 + z*z )
  }
}

// The square root of the sum of the squares of three doubles.
// Prevents overflow for large numbers.
fun hypot( x : double, y : double, z : double ) : double {
  val xx = abs(x)
  val yy = abs(y)
  val zz = abs(z)
  val hi = max(max(xx,yy),zz)
  if (hi==0.0) then 0.0 else {
    hi * sqrt( sqr(xx / hi) + sqr(yy / hi) + sqr(zz / hi) )
  }
}

// The square root of the sum of squares of a list of doubles.
// Prevents overflow for large numbers and uses Kahan-Babu&scaron;kan-Neumaier summation
// for precision.
fun hypot( xs : list<double> ) : double {
  val hi = xs.abs-max
  if (hi==0.0) then 0.0 else {
    hi * xs.map(fun(x){ sqr(x / hi) }).sum.sqrt 
  }
}

// The square of a double
fun sqr(x : double ): double {
  x*x
}

// The maximum of the absolute values.
fun abs-max( x : double, y : double ) : double {
  max(abs(x),abs(y))
} 

// The maximum of a list of absolute values.
fun abs-max( xs : list<double> ) : double {
  xs.foldr(0.0,fun(x,m){ max(abs(x),m) })
}

//-----------------------------------------
// Trigonometry
//-----------------------------------------

private val rad2deg : double = 180.0/pi
private val deg2rad : double = pi/180.0

// Convert radians to degrees.
fun deg( rad : double ) : double {
  rad * rad2deg
}

// Convert degrees to radians.
fun rad( deg : double ) : double {
  deg * deg2rad
}

// Return the sine of an angle in radians `d`.
extern inline sin( d : double ) : double {
  cs "Math.Sin"
  js "Math.sin"
}

// Return the cosine of an angle in radians `d`.
extern inline cos( d : double ) : double {
  cs "Math.Cos"
  js "Math.cos"
}

// Return the tangent of an angle in radians `d`.
extern inline tan( d : double ) : double {
  cs "Math.Tan"
  js "Math.tan"
}
// Return the arc-tangent of `d`
extern inline atan( d : double ) : double {
  cs "Math.Atan"
  js "Math.atan"
}

// Return the arc-tangent of a point (`x`,`y`).
extern inline atan2( x : double, y : double ) : double {
  cs "Math.Atan2"
  js "Math.atan2"
}

// Return the arc-cosine of `d`
extern inline acos( d : double ) : double {
  cs "Math.Acos"
  js "Math.acos"
}

// Return the arc-sine of `d`
extern inline asin( d : double ) : double {
  cs "Math.Asin"
  js "Math.asin"
}


// The hyperbolic tangent of `d`
extern inline tanh( d : double ) : double {
  cs "Math.Tanh"
  js "Math.tanh"
}

// The hyperbolic cosine of `d`
extern inline cosh( d : double ) : double {
  cs "Math.Cosh"
  js "Math.cosh"
}

// The hyperbolic sine of `d`
extern inline sinh( d : double ) : double {
  cs "Math.Sinh"
  js "Math.sinh"
}


// Return `x` with the sign of `y`.
public fun with-sign-of( x : double, y : double ) : double {
  if (y < 0.0) then ~(x.abs) else x.abs
}


// The area hyperbolic tangent of `x`
fun atanh( x : double ) : double {
  0.5*log( (1.0 + x) / (1.0 - x))
  /*
  // 0.5*log( (1.0 + x) / (1.0 - x)) = 0.5*log1p( (2.0*x) / (1-x) )
  if (x.abs >= 0.5) then 0.5*log1p(2.0*(x/(1.0 - x))) else {
    val x2 = x + x
    0.5*log1p(x2 + (x2*x)/(1.0 - x))    
  }
  */
}

private val exp2_28 : double = 268435456.0

// The area hyperbolic cosine of `x`
fun acosh( x : double ) : double {
  // log(x + sqrt((x - 1.0)*(x + 1.0)));
  if (x > exp2_28) then dbl-log2 + log(x)
  elif (x > 2.0) then {
    log(2.0*x - 1.0/(x + sqrt(x.sqr - 1.0)))
  }
  elif (x <= 1.0) then (if (x < 1.0) then nan else 0.0)
  else {
    val xm1 = x - 1.0
    log1p( xm1 + sqrt(2.0*xm1 + xm1*xm1) )
  }
}

// The area hyperbolic sine of `x`
fun asinh( x : double ) : double {
  //log( x + sqrt(x.sqr + 1.0))
  val xa = x.abs
  if (xa > exp2_28) then (log(xa) + dbl-log2).with-sign-of(x) 
  elif (xa >= 2.0) then (log(2.0*xa + 1.0/(xa + sqrt(xa.sqr + 1.0)))).with-sign-of(x)
  elif (xa == 0.0) then 0.0
  else {
    val xa2 = xa.sqr
    log1p( xa + xa2/(1.0 + sqrt( xa2 + 1.0 ))).with-sign-of(x)
  }
}

