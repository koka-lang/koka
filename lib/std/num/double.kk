/*---------------------------------------------------------------------------
  Copyright 2017 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* Advanced operations on `:double`

*/
public module std/num/double

// Return the arc-tangent in radians of a number
extern inline atan( d : double ) : double {
  js "Math.atan"
}

// Return the arc-tangent of the quotient of its arguments
extern inline atan2( x : double, y : double ) : double {
  js "Math.atan2"
}

// Return the arc-cosine in radians of a number
extern inline acos( d : double ) : double {
  js "Math.acos"
}

// Return the arc-sine in radians of a number
extern inline asin( d : double ) : double {
  js "Math.asin"
}


// Return the hyperbolic tangent of an angle in radians
extern inline atanh( d : double ) : double {
  js "Math.tanh"
}

// Return the hyperbolic cosine of an angle in radians
extern inline acosh( d : double ) : double {
  js "Math.cosh"
}

// Return the hyperbolic sine of an angle in radians
extern inline asinh( d : double ) : double {
  js "Math.sinh"
}


// Return the hyperbolic arc-tangent in radians of a number
extern inline atanh( d : double ) : double {
  js "Math.atanh"
}

// Return the hyperbolic arc-cosine in radians of a number
extern inline acosh( d : double ) : double {
  js "Math.acosh"
}

// Return the hyperbolic arc-sine in radians of a number
extern inline asinh( d : double ) : double {
  js "Math.asinh"
}

