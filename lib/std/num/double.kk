/*---------------------------------------------------------------------------
  Copyright 2017 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* Various operations on a 64-bit IEEE `:double`.

*/
public module std/num/double


//-----------------------------------------
// Constants
//-----------------------------------------


// &pi;
val pi          = 3.141592653589793116

// 2&pi;
val dbl-twopi   = 6.283185307179586232

// &pi;/2
val dbl-pi2     = 1.570796326794896558

// The _e_ constant.
val dbl-e       = 2.718281828459045091

// The natural logarithm of 2
val dbl-log2    = 0.6931471805599452862

// The natural logarithm of 10
val dbl-log10   = 2.302585092994045901

// Maximum double value
val dbl-max     = 1.79769313486231570815e+308
                  
// Smallest positive normalized double value
val dbl-min     = 2.22507385850720138309e-308

// Smallest positive denormalized value (i.e. ``DBL_TRUE_MIN``)
val dbl-denorm-min = 4.9406564584124654418e-324

// Machine epsilon: the difference between 1.0 and the next representable `:double` value.
val dbl-epsilon = 2.2204460492503130808e-16

// maximal precision in decimal digits of a `:double`.
val dbl-max-prec = 15

//-----------------------------------------
// Powers
//-----------------------------------------


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
fun pow10( p : double ) : double {
  pow(10.0,p)
}

// Return the 2 to the power of `p`.
fun pow2( p : double ) : double {
  pow(2.0,p)
}


// Round a double to a specified precision
fun round-to-prec( d : double, prec : int  ) : double {
  if (prec <= 0) then d.round else {
    val p  = pow10(prec.double)
    (d * p).round / p
  }
}

//-----------------------------------------
// Other
//-----------------------------------------

// 'Load exponent': returns `x`&middot;2^`exp`^.
fun ldexp( x : double, exp : int ) : double {
  if (!finite?(x)) then x else x*pow2(exp.double)
}

// 'Fraction/exponent': return the normalized fraction `f` and exponent `exp`
// for a number `x` such that `x == f`&middot;2^`exp`^.
fun frexp( x : double ) : (double,int) {
  if (!finite?(x)) then (x,0) else {
    val exp  = log2(x.abs).trunc.int
    val frac = if (exp > 1023) 
                then (x / pow2(1023.0))/pow2(exp.double - 1023.0) // prevent overflow
                else (x / pow2(exp.double))
    (frac,exp)
  }
}


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
  if (hi==0) then 0.0 else {
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


// Return the sine of an angle in radians `d`.
extern inline sin( d : double ) : double {
  js "Math.sin"
}

// Return the cosine of an angle in radians `d`.
extern inline cos( d : double ) : double {
  js "Math.cos"
}

// Return the tangent of an angle in radians `d`.
extern inline tan( d : double ) : double {
  js "Math.tan"
}
// Return the arc-tangent of `d`
extern inline atan( d : double ) : double {
  js "Math.atan"
}

// Return the arc-tangent of a point (`x`,`y`).
extern inline atan2( x : double, y : double ) : double {
  js "Math.atan2"
}

// Return the arc-cosine of `d`
extern inline acos( d : double ) : double {
  js "Math.acos"
}

// Return the arc-sine of `d`
extern inline asin( d : double ) : double {
  js "Math.asin"
}


// The hyperbolic tangent of `d`
extern inline tanh( d : double ) : double {
  js "Math.tanh"
}

// The hyperbolic cosine of `d`
extern inline cosh( d : double ) : double {
  js "Math.cosh"
}

// The hyperbolic sine of `d`
extern inline sinh( d : double ) : double {
  js "Math.sinh"
}


// The area hyperbolic tangent of `d`
extern inline atanh( d : double ) : double {
  js "Math.atanh"
}

// The area hyperbolic cosine of `d`
extern inline acosh( d : double ) : double {
  js "Math.acosh"
}

// The area hyperbolic sine of `d`
extern inline asinh( d : double ) : double {
  js "Math.asinh"
}

