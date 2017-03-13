/*---------------------------------------------------------------------------
  Copyright 2017 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* Advanced operations on `:double`

*/
public module std/num/double

// &pi;
val pi = 3.141592653589793116

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

