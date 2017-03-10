/*---------------------------------------------------------------------------
  Copyright 2017 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* Operations on double double 128-bit floating point numbers.

The `:ddouble` type implements [double double][ddwiki] 128-bit floating point numbers
as a pair of IEEE `:double` values. This extends the precision to 31 decimal digits
(versus 15 for `:double`), but keeps the same range as
a `:double` with a maximum  value of about 1.8&middot;10^308^ (`dd-max`). Because
`:double`s usually have hardware support, a `:ddouble` is usually much faster
than arbitrary precision floating point numbers.

## Precision {-}

If you add two regular 64-bit `:double` values you can quickly notice the imprecision
especially because decimal fractions cannot be represented precisely. For example:

```
> 0.1 + 0.2
0.30000000000000004
```

When we show the `0.1` to 20 digits of precision, we see that it is not precisely 
represented by a `:double`:

```
> 0.1.show(20)
"0.10000000000000000555"
```

If we convert this to a `:ddouble` we can see with even more precision how 0.1 is
represented by a `:double`:

```
> 0.1.ddouble
0.1000000000000000055511151231258
```

However, if we convert the number `0.1` directly to a `:ddouble` instead of going through
a `:double` constant, we get `0.1` exactly:

```
> ddouble("0.1")   // for convenience; using `ddouble-exp(1,-1)` is more efficient
0.1
```

This is possible because a `:ddouble` uses two `:double`s to represent numbers; in this
case the first (_hi_) double is (the inexact) `0.1` while the second one (_lo_) is the
correction to that number. The `show-sum` function shows this internal pair of numbers:

```
> ddouble("0.1").show-sum(20)
"0.10000000000000000555 + -5.5511151231257830103e-18"

> ddouble("0.1") + ddouble("0.2")
0.3
```

More precisely, a `:ddouble` _d_ is represented as a pair of `:double`s, _hi_ and _lo_
such that the number represented by _d_ is _hi_+_lo_, where \|_lo_\| &le; 0.5&middot;ulp(_hi_).

## Notes {-}

Note that despite the extra precision, underflow/overflow and machine precision are 
not as well-defined as with a regular `:double` [@Hida:qd;@Hida:qdlib;@lin;@Dekker].
The implementation is based closely on the [QD] C++ library,
and assumes proper 64-bit IEEE `:double`s with correct rounding.


## References {-}

~ Bibliography { caption:"0" }

~~ BibItem { #Hida:qdlib; searchterm:"Hida+Library+for+double-double+and+quad-double+arithmetic" }
Yozo Hida,\ Xiaoye\ S.\ Li, and David\ H.\ Bailey.
_Library for double-double and quad-double arithmetic._ 
(2007). [pdf](http://www.jaist.ac.jp/~s1410018/papers/qd.pdf).
~~

~~ BibItem { #Hida:qd; searchterm:"Hida+Quad-Double+Arithmetic:+Algorithms,+Implementation,+and+Application" }
Yozo Hida,\ Xiaoye S.\ Li, and David\ H.\ Bailey.
_Quad-Double Arithmetic: Algorithms, Implementation, and Application_.
Lawrence Berkeley National Laboratory Technical Report LBNL-46996 (2000). [pdf](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.4.5769).
~~

~~ Bibitem { #lin; searchterm:'Linnainmaa+"Software+for+Doubled-Precision+Floating-Point+Computations"' }
Seppo\ Linnainmaa.
_Software for Doubled-Precision Floating-Point Computations_.
ACM Transactions on Mathematical Software (TOMS), Volume **7** Issue 3, Sept. 1981, 272--283.
~~

~~ BibItem { #dekker; searchterm:"Dekker+A+Floating-Point+Technique+for+Extending+the+Available+Precision" }
T.\ Dekker.
_A Floating-Point Technique for Extending the Available Precision_.
Numerische\ Mathematik **18**(3), June 1971, 224--242.
~~
~

[ddwiki]: https://en.wikipedia.org/wiki/Quadruple-precision_floating-point_format#Double-double_arithmetic
[qd]:     http://crd-legacy.lbl.gov/~dhbailey/mpdist

\/
*/
module std/num/ddouble

import std/num/double   
import std/text/regex

/* The `:double` type implements [double double][ddwiki] 128-bit floating point numbers
as a pair of IEEE `:double` values. This extends the precision to 31 decimal digits
(versus 15 for `:double`), but keeps the same range as
a `:double` with a maximum  value of about 1.8&middot;10^308^. Because
`:double`s usually have hardware support, a `:ddouble` is usually much faster
than arbitrary precision floating point numbers.

Internally a `:ddouble` _d_ is represented as a pair of `:double`s, _hi_ and _lo_
such that the number represented by _d_ is _hi_+_lo_, where \|_lo_\| &le; 0.5&middot;ulp(_hi_).
*/
abstract struct ddouble(
  hi : double,
  lo : double
)

// Create a `:ddouble` from a `:double`.
public fun ddouble( d : double ) : ddouble {
  Ddouble(d,0.0)
}


val maxprecise : int = 9007199254740991
val minprecise : int = ~maxprecise

fun precise?(i : int ) : bool {
  (i >= minprecise && i <= maxprecise)
}



// Create a `:ddouble` from an `:int`. 
// A `:ddouble` can represent integers precisely up to 30 digits.
// If an integer is passed that is out of range an infinity is returned.
public fun ddouble( i : int ) : ddouble {
  ddouble-int-exp(i,0)
}

fun ddouble-int-exp( i : int, e : int ) : ddouble {
  if (i.precise?) then small-exp( i, e ) else {
    // ensure we convert at least 31 (and at most 45) digits precisely
    val p    = i.count-digits
    val x    = i
    val px   = p - 15
    val (hi,y) = x.divmod-pow10(px)
    val py   = px - 15
    if (py <= 0) then { 
      // if 30 or less digits we can convert with two doubles
      small-exp(hi, px + e) + small-exp( y, e)
    }
    else {
      // otherwise use 2 initial 15 digits and 1 trailing value but
      // optimize the case of trailing zeros.
      val (mid,z) = y.divmod-pow10(py)
      val pz = z.pow10?  // number of trailing zeros
      val lo = z.cdiv-pow10(pz) 
      //trace("ddouble-i: " + i.show + ", hi:" + hi.show + ", mid: " + mid.show + ", lo: " + lo.show)
      (small-exp( hi, px + e) + small-exp( mid, py + e)) + small-exp( lo, pz + e)
    }
  }
}
fun small-exp( i : int, e : int ) {
  i.double.ddouble.mul-pow10(e)
}


// &pi; 
public val dd-pi      = Ddouble(3.141592653589793116e+00,1.224646799147353207e-16)  

// 2&pi;
public val dd-twopi   = Ddouble(6.283185307179586232e+00,2.449293598294706414e-16)

// &pi;/2
public val dd-pi2     = Ddouble(1.570796326794896558e+00,6.123233995736766036e-17)

// &pi;/4
public val dd-pi4     = Ddouble(7.853981633974482790e-01,3.061616997868383018e-17)

// 3&midpoint;&pi/4
public val dd-pi34    = Ddouble(2.356194490192344837e+00,9.1848509936051484375e-17)

// The _e_ constant.
public val dd-e       = Ddouble(2.718281828459045091e+00,1.445646891729250158e-16)

// The natural logarithm of 2
public val dd-log2    = Ddouble(6.931471805599452862e-01,2.319046813846299558e-17)

// The natural logarithm of 10
public val dd-log10   = Ddouble(2.302585092994045901e+00,-2.170756223382249351e-16)

// The 'machine epsilon'
public val dd-epsilon = ddouble(4.93038065763132e-32)

// The maximum representable `:ddouble`
public val dd-max     = Ddouble(1.79769313486231570815e+308, 9.97920154767359795037e+291)

// The smallest positive `:ddouble` that is still normalized
public val dd-min     = ddouble(2.0041683600089728e-292)  // normalized = 2^(-1022 + 53)

// Not-A-Number
public val dd-nan     = Ddouble(nan,0.0)

// Positive infinity
public val dd-posinf = Ddouble(posinf,0.0)

// Negative infinity
public val dd-neginf = Ddouble(neginf,0.0)

// maximal precision in decimal digits of a `:ddouble`.
public val dd-max-prec = 31


/*------------------------------------------------------
   Compare etc.
------------------------------------------------------*/

// Zero constant
public val zero = ddouble(0.0)

// One
public val one  = ddouble(1.0)

// Ten (`10.ddouble`)
public val ten  = ddouble(10.0)

val two         = ddouble(2.0)

// Is this `:ddouble` equal to zero?
public fun zero?( x : ddouble ) : bool {
  x.hi.zero?
}

// Is this `:ddouble` negative?
public fun neg?( x : ddouble ) : bool {
  x.hi.neg?
}

// Is this `:ddouble` positive?
public fun pos?( x : ddouble ) : bool {
  x.hi.pos?
}

// Return the sign of the `:ddouble`.
public fun sign?( x : ddouble ) : order {
  if (x.hi==0.0) then Eq
  elif (x.hi < 0.0) then Lt
  else Gt
}

// Is this `:ddouble` not-a-number?
public fun nan?( x : ddouble ) : bool {
  x.hi.nan? || x.lo.nan?
}

// Is this a finite `:ddouble`? (i.e. not `nan?` or `inf?`)
public fun finite?( x : ddouble ) : bool {
  x.hi.finite? && x.lo.finite?
}

// Is this an infinite value.
public fun inf?( x : ddouble ) : bool {
  x.hi.inf?
}

// Does `x` equal positive infinity?
public fun posinf?( x : ddouble ) : bool {
  x.hi.posinf?
}

// Does `x` equal negative infinity?
public fun neginf?( x : ddouble ) : bool {
  x.hi.neginf?
}


// Compare two `:ddouble` values.
public fun compare( x : ddouble, y : ddouble ) : order {
  match(compare(x.hi,y.hi)) {
    Eq  -> compare(x.lo,y.lo)
    ord -> ord
  }
}

public fun (>) (x : ddouble, y : ddouble) : bool { compare(x,y) == Gt }
public fun (>=)(x : ddouble, y : ddouble) : bool { compare(x,y) != Lt }
public fun (==)(x : ddouble, y : ddouble) : bool { compare(x,y) == Eq }
public fun (!=)(x : ddouble, y : ddouble) : bool { compare(x,y) != Eq }
public fun (<) (x : ddouble, y : ddouble) : bool { compare(x,y) == Lt }
public fun (<=)(x : ddouble, y : ddouble) : bool { compare(x,y) != Gt }

// The minimum of `x` and `y`.
public fun min( x : ddouble, y : ddouble ) : ddouble {
  if (x <= y) then x else y
}

// The maximum of `x` and `y`
public fun max( x : ddouble, y : ddouble ) : ddouble {
  if (x >= y) then x else y
}

/*------------------------------------------------------
   Addition
------------------------------------------------------*/

struct edouble(
  num : double,
  err : double              
)

// often called `twosum` in literature
fun sum( x : double, y : double ) : edouble {
  val z    = x + y
  val diff = z - x
  val err  = (x - (z - diff)) + (y - diff)
  Edouble(z, if (z.finite?) then err else z)
}

fun dsum( x : double, y : double ) : ddouble {
  val z    = x + y
  val diff = z - x
  val err  = (x - (z - diff)) + (y - diff)
  Ddouble(z, if (z.finite?) then err else z)
}

fun quicksum( x : double, y : double ) : edouble {
  val z   = x + y
  val err = y - (z - x)
  Edouble(z, if (z.finite?) then err else z)
}

fun dquicksum( x : double, y : double ) : ddouble {
  if (!x.finite?) return ddouble(x)
  val z   = x + y
  val err = y - (z - x)
  Ddouble(z, if (z.finite?) then err else z)
}

// Add two `:ddouble`s
public fun (+)( x : ddouble, y : ddouble ) : ddouble {
  val z1 = sum(x.hi,y.hi)
  val lo = sum(x.lo,y.lo)
  val e1 = z1.err + lo.num
  val z2 = quicksum(z1.num,e1)
  val e2 = z2.err + lo.err
  dquicksum(z2.num,e2)
}

// Negate a `:ddouble`.
public fun (~)( x : ddouble ) : ddouble {
  Ddouble(~x.hi,~x.lo)
}

// Subtract two values.
public fun (-)( x : ddouble, y : ddouble ) : ddouble {
  x + (~y)
}

// Return the absolute value.
public fun abs( x : ddouble ) : ddouble {
  if (x.neg?) then ~x else x
}

// Increment by one.
public fun inc( x : ddouble ) :ddouble {
  x + one
}


// Decrement by one.
public fun dec( x : ddouble ) :ddouble {
  x - one
}


// The sum of a list of values.
public fun sum( xs : list<ddouble> ) : ddouble {
  xs.foldl(zero,(+))
}

/*------------------------------------------------------
   Multiply and divide
------------------------------------------------------*/
val splitter = 134217729.0              // = 2^27 + 1
val splitbound  = 6.69692879491417e+299 // = 2^996
val two28 = 268435456.0                 // = 2^28

fun split( x : double ) : (double,double) {
  if (x > splitbound || x < ~splitbound) {
    val y = x * 3.7252902984619140625e-09  // 2^-28
    val t = y * splitter
    val hi = t - (t - y)
    val lo = y - hi
    (hi*two28, lo*two28)
  }
  else {
    val t = x * splitter
    val hi = t - (t - x)
    val lo = x - hi
    (hi,lo)
  }
}

// often called `twoproduct` in literature
fun prod( x : double, y : double ) : edouble {
  val z = x*y
  val (xhi,xlo) = split(x)
  val (yhi,ylo) = split(y)
  val err = ((xhi*yhi - z) + (xhi*ylo + xlo*yhi)) + (xlo*ylo)
  Edouble(z,if (z.finite?) then err else z)
}

fun prodsqr( x : double ) : edouble {
  val z = x*x
  val (h,l) = split(x)
  val err = ((h*h - z) + (2.0*h*l)) + (l*l)
  Edouble(z,if (z.finite?) then err else z)
}

// `prod` & `prodsqr` can also be implemented using 'fused multiply add'
/* 
fun prod( x : double, y : double ) : edouble {
  val z = x*y
  err = fmadd(x,y,~z)
  Edouble(z,err)
}

fun prodsqr( x : double ) : edouble {
  val z = x*x
  val err = fmadd(x,x,~z)
  Edouble(z,err)
}
*/

// Multiply two `:ddouble`s
public fun (*)( x : ddouble, y : ddouble ) : ddouble {
  val z  = prod(x.hi, y.hi)
  val e  = z.err + (x.hi*y.lo + x.lo*y.hi)
  dquicksum(z.num, e)
}

// Multiply `x` with itself.
public fun sqr( x : ddouble ) : ddouble {
  val z = prodsqr(x.hi)
  val e = (z.err + (2.0*x.hi*x.lo)) + (x.lo*x.lo)
  dquicksum(z.num,e)
}


// Divide two `:ddouble`s
public fun (/)( x : ddouble, y : ddouble ) : ddouble {
  val q1 = ddouble(x.hi / y.hi)
  if (!q1.finite?) return q1
  val r1 = x - (y * q1)
  val q2 = ddouble(r1.hi / y.hi)
  val r2 = r1 - (y * q2)
  val q3 = ddouble(r2.hi / y.hi)
  val q  = dquicksum(q1.hi,q2.hi)
  q + q3
}

// Remainder of two `:ddouble`s
public fun (%)( x : ddouble, y : ddouble ) : ddouble {
  val n = (x / y).round
  x - (n*y)
}


// Division and remainder of two `:ddouble`s
public fun divrem( x : ddouble, y : ddouble ) : (ddouble,ddouble) {
  val n = (x / y).round
  (n, x - (n*y))
}

/*------------------------------------------------------
   Rounding
------------------------------------------------------*/

// Convert a `:ddouble` to a `:double` (losing precision)
public fun double( x : ddouble ) : double {
  x.hi
}

// Convert a `:ddouble` to an integer (using `round`)
public fun int( x : ddouble ) : int {
  val r = x.round
  if (!r.finite?) then 0 else {
    val i = r.hi.int
    if (i.precise?) then i else {
      // todo: optimize?
      r.show-fixed().parse-int-default(0)
    }
  }
}

// Round a `:ddouble` to the nearest integer (rounding to the nearest even number in case of a tie).
public fun round( x : ddouble ) : ddouble {
  val r = x.hi.round
  if (r == x.hi) {
    dquicksum(r,x.lo.round)
  }
  elif(abs(r - x.hi) == 0.5 && x.lo.neg?) {
    Ddouble(r - 1.0,0.0)
  }
  else {
    Ddouble(r,0.0)
  }
}

// Round to negative infinity.
public fun floor( x : ddouble ) : ddouble {
  val r = x.hi.floor
  if (r == x.hi) 
   then dquicksum(r,x.lo.floor)
   else Ddouble(r,0.0)
}


// Round to positive infinity.
public fun ceiling( x : ddouble ) : ddouble {
  val r = x.hi.ceiling
  if (r == x.hi) 
   then dquicksum(r,x.lo.ceiling)
   else Ddouble(r,0.0)
}


// Round to zero.
public fun trunc( x : ddouble ) : ddouble {
  if (x.neg?) then x.ceiling else x.floor
}


// The fraction of `x` such that `x.trunc + x.fraction == x`.
public fun fraction( x : ddouble ) : ddouble {
  x - x.trunc
}

// The _floored_ fraction of `x`. This is always positive, such that `x.floor + x.ffraction == x`.
public fun ffraction( x : ddouble ) : ddouble {
  x - x.floor
}


// Round a `:ddouble` to a specified precision. Uses `round` if the precision is smaller or equal to zero.
public fun round-to-prec( x : ddouble, prec : int  ) : ddouble {
  if (prec <= 0) then x.round 
  elif (prec > dd-max-prec) then x 
  else {
    val p : ddouble = pow10(prec)
    (x * p).round / p
  }
}

/*------------------------------------------------------
   Exponents (needed for showing ddouble's)
------------------------------------------------------*/
fun ldexp( x : double, exp : double ) : double {
  x*pow2(exp)
}

// 'Load exponent': returns `x`&middot;2^`exp`^.
public fun ldexp( x : ddouble, exp : double ) : ddouble {
  Ddouble(ldexp(x.hi,exp),ldexp(x.lo,exp))
}

fun npwr-acc( x : ddouble, acc : ddouble, n : int ) : ddouble {
  if (n <= 0) then acc 
  elif (n.even?) then npwr-acc( sqr(x), acc, unsafe-decreasing(n/2) )
  else npwr-acc( x, x*acc, unsafe-decreasing(n.dec))
}

fun npwr( x : ddouble, n : int ) : ddouble {
  if (n==0) then (if (x.zero?) then dd-nan else one)
  elif (n==1) then x
  else x.npwr-acc(one,n)
}

// Return `x` to the power of `n`.
public fun pow( x : ddouble, n : int ) : ddouble {
  val p = npwr(x,n.abs)
  if (n.neg?) then (one / p) else p
}

// Return 10 to the power of `exp`.
public fun pow10( exp : int ) : ddouble {
  pow(ten,exp)
}


// Return 2 to the power of `exp`.
public fun pow2( exp : int ) : ddouble {
  pow(two,exp)
}


// Create a `:ddouble` `x` such that `x` equals `d`&middot;10^`e`^.
public fun ddouble-exp( d : double, e : int ) {
  d.ddouble.mul-pow10(e)
}

// Create a `:ddouble` `x` such that `x` equals `i`&middot;10^`e`^.
public fun ddouble-exp( i : int, exp : int ) : ddouble {
  i.ddouble.mul-pow10(exp)
}

fun mul-pow10( x : ddouble, exp : int ) : ddouble {
  if (exp.zero?) then x else x * pow10(exp)
}

/*------------------------------------------------------
   Show
------------------------------------------------------*/

// Show a `:ddouble` as a pair of precise `:double`s with an optional precision.
// Use this if you need to guarantee that you can parse back `:ddouble`s exactly,
// i.e. `x == x.show-sum.ddouble`
public fun show-sum( x : ddouble, prec : int = -1 ) : string {
  if (!x.finite?) 
   then x.hi.show
   else x.hi.show(prec) + " + " + x.lo.show(prec)
}

// Show a `:ddouble` with an optional precision. 
public fun show( x : ddouble, prec : int = -1 ) : string {
  x.show-prec(prec)
}

// Show a `:ddouble` with an optional precision. 
public fun show-prec( x : ddouble, prec : int = -1 ) : string {
  if (!x.finite?) then return x.hi.show
  if (x.zero?) then return "0"
  if (x.in-double-range(prec)) then return x.hi.show(prec)
  // guess exponent
  val e = x.hi.abs.log10.floor.int  
  val p = if (prec.neg?) then dd-max-prec else max(1,prec)
  if (p > e && e >= -4 && e <= 20 && in-fixed-range(x,e))
   then show-fixed(x, (p - 1) - e, prec.neg? )
   else show-exp(x, prec)
}

// Show a `:ddouble` as a fixed point number with an optional precision `dprec` (=`0`) that
// specifies the number of digits after the dot. For some numbers `show-fixed` will still
// use exponential notation.
public fun show-fixed( x : ddouble, dprec : int = 0, trim-zeros : bool = False ) : string {
  if (!x.finite?) then return x.hi.show
  if (x.zero?) then return ("0" + (if (dprec.pos? && !trim-zeros) then "." + repeat("0",dprec) else ""))
  if (x.in-double-range(dprec)) then return x.hi.show-fixed(dprec)
  val (ds,e) = digits(x, dd-max-prec + 1 )
  // trace( " digits: " + ds.show-list(show) + ", e:" + e.show + ", len: " + ds.length.show)  

  // don't print too large a number; 10^28 and larger is problematic anyway
  if (!in-fixed-range(x,e)) return show-exp-digits(x,dd-max-prec,ds,e,trim-zeros)
  show-fixed-digits(x,dprec,ds,e,trim-zeros)
}

fun in-fixed-range( x : ddouble, e : int ) : bool {
  (e.abs <= 27 || (e.abs <= 30 && x.floor == x))
}

fun in-double-range( x : ddouble, prec : int  ) : bool {
  (x.lo.zero? && prec >= 0 && prec <= 15)
}

fun show-fixed-digits( x : ddouble, dprec : int, ds0 : list<int>, e0 : int, trim-zeros : bool ) : string {
  // round
  val (ds,e) = fixed-round( x.neg?, ds0, e0, dprec )
  // trace( " digits: " + ds.show-list(show) + ", e:" + e.show + ", len: " + ds.length.show)  

  // use fixed
  val (whole,frac) = if (e.neg?) then ([0], replicate(0, ~e - 1 ) + ds) else ds.split(e+1)
  val pre0 = (whole + replicate(0,e + 1 - whole.length)).map(show).join
  val pre1 = pre0.trim-left("0")
  val pre  = if (pre1.empty?) then "0" else pre1
  val frac0= if (dprec.zero?) then []
             elif (dprec.pos?) then frac.take(dprec) + replicate(0, dprec - frac.length)
             else frac
  val frac1= frac0.map(show).join
  val frac2= if (trim-zeros) then frac1.trim-right("0") else frac1
  val post = if (frac2.empty?) then "" else "." + frac2             
  val num  = pre + post
  val sign = if (x.neg? && num!="0") then "-" else ""
  sign + num
}

fun fixed-round( isneg : bool, ds : list<int>, e:int, dprec : int ) : (list<int>,int) {
  val pred = if (isneg) then odd? else even?
  val (pre,post) = ds.split( e + 1 + dprec )
  // trace( " fixed-round: " + pre.show-list(show) + " . " + post.show-list(show))
  match(post) {
    Cons(p) | p > 5 || (p==5 && pre.last.default(0).pred) -> {
      if (pre.nil?) then ([1],e.inc) 
      else pre.reverse.digits-carry(e,1)
    }
    _ -> (pre,e)
  }
}  

// Show a `:ddouble` in scientific exponential notation with an optional precision `prec` (=`-1`)
// and whether to trim trailing zeros (`trim-zeros` = `prec.neg?`).
public fun show-exp( x : ddouble, prec : int = -1, trim-zeros : bool = prec.neg? ) : string {
  if (!x.finite?) then return x.hi.show
  if (x.zero?) then return ("0" + (if (prec.pos?) then "." + repeat("0",prec) else ""))
  if (x.in-double-range(prec)) then return x.hi.show-exp(prec)
  val (ds,e) = digits(x, (if (prec.neg?) then dd-max-prec else prec)  )
  //trace( " digits: " + ds.show-list(show) + ", e:" + e.show + ", len: " + ds.length.show)  
  show-exp-digits(x,prec,ds,e,trim-zeros)
}

fun show-exp-digits( x : ddouble, prec : int, ds : list<int>, e : int, trim-zeros : bool = prec.neg? ) : string {    
  val pre  = ds.head.default(0).show
  val frac0= (if (!prec.zero?) then ds.tail else []).map(show).join
  val frac = if (trim-zeros) then frac0.trim-right("0") else frac0
  val post = if (frac.empty?) then "" else "." + frac
  val sign = if (x.neg?) then "-" else ""
  val exp  = if (e.zero?) then "" else ("e" + (if (e.pos?) then "+" else "") + e.show)
  sign + pre + post + exp
}


/*------------------------------------------------------
   Extract digits
------------------------------------------------------*/

// Extract significant digits and exponent from a `:ddouble`.
fun digits( x : ddouble, prec : int = dd-max-prec ) : (list<int>,int) {
  val n = prec + 1  // number of digits
  val (e0,r) = digits-to-exp(x)
  //trace(" digits-toexp: e: " + e0.show + ", r: " + r.show-debug )
  val ds0 = r.digits-extract(n)
  val (ds1,e1) = ds0.digits-carry(e0)
  (ds1.take(prec),e1)
}

// Return an exponent and normalized ddouble
fun digits-to-exp( x : ddouble ) : (int,ddouble) {
  // guess approximate exponent
  val e1 = x.hi.abs.log10.floor.int  
  // guess exponential
  val r0 = x.abs
  val r1 = if (e1 < -300) then (r0*pow10(300)) / pow10(e1+300)
           elif (e1 > 300) then ldexp(ldexp(r0,-53.0) / pow10(e1), 53.0)
           else r0 / pow10(e1)
  // fix off by one
  // trace("digits-toexp: r1: " + r1.show-debug + ", r1*10: " + (r1 * ten).show-debug + ", r1/ten: " + (r1 / ten).show-debug)
  if (r1 >= ten) then (e1.inc, r1 / ten)
  elif (r1 < one) then (e1.dec, r1 * ten)
  else (e1,r1)
}
 
// extract the first n digits from a normalized ddouble (in reverse)
fun digits-extract( r : ddouble, n : int, acc : list<int> = [] ) : list<int> {
  if (n <= 0) then digits-round(acc) else {
    val d = r.hi.trunc.int
    digits-extract( (r - ddouble(d))*ten, 
                    unsafe-decreasing(n.dec), Cons(d,acc) )
  }
}

// round on (and discard) the last digit before doing overflow correction
fun digits-round( rds : list<int> ) : list<int> {
  match(rds) {
    Cons(d0,Cons(d1,rds1) as rds0) 
      -> if (d0 >= 5) then Cons(d1.inc,rds1) else rds0
    _ -> rds
  }
}

// adjust overflow digits in a reversed list of digits
fun digits-carry( rds : list<int>, e : int, carry : int = 0, acc : list<int> = [] ) : (list<int>,int) {
  match(rds) {
    Cons(d0,ds) {
      // propagate carry while adjusting
      val d = d0 + carry
      if (d < 0)   then digits-carry( ds, e, -1, Cons(d + 10, acc))
      elif (d > 9) then digits-carry( ds, e, 1, Cons(d - 10, acc))
      else digits-carry( ds, e, 0, Cons(d,acc) )
    }
    Nil -> {
      (if (carry.pos?) then Cons(1,acc) else acc, e + carry) 
    }
  }
}

/*------------------------------------------------------
  Parsing
------------------------------------------------------*/

// Parse a floating point number with up to 31 digits precision. 
// Return `dd-nan` if the string is an invalid number.
public fun ddouble( s : string ) : ddouble {
  s.parse-ddouble.default(dd-nan)
}

// Parse a floating point number with up to 31 digits precision. 
// Returns `Nothing` on invalid input.
public fun parse-ddouble( s  : string ) : maybe<ddouble> {
  //parse-double(s).map(ddouble)
  match(s.find(rx-ddouble)) {
    Nothing -> {
      match(s.find(rx-special)) {
        Nothing -> Nothing
        Just(cap) -> {
          Just( if (!cap.groups[1].empty?) then dd-nan
                elif (cap.groups[2] == "-") then dd-neginf
                else dd-posinf
              )
        }
      }
    }
    Just(cap) -> 
      if (cap.groups[7].empty?) {  // single double-double float
        val sign  = cap.groups[2].parse-int-sign
        val whole = cap.groups[3]
        val frac  = (cap.groups[4] || "").trim-right("0")
        val esign = cap.groups[5].parse-int-sign
        val exp   = cap.groups[6].parse-int-default(0) * esign

        val w = (whole + frac).parse-int-default(0) * sign
        val e = exp - frac.count
        val x = ddouble-int-exp(w,e) 
        // trace("parse: s: " + s + "\n  w: " + w.show + ", e: " + e.show + "\n  wx: " + w.ddouble.show-sum + ", x: " + x.show + "\n  " + x.show-sum)
        Just(x)
      }
      else { // sum of two double's for precise round trips
        val d1 = cap.groups[1].parse-double.default(nan)
        val d2 = cap.groups[7].parse-double.default(nan)
        Just(d1.ddouble + d2.ddouble)
      }
  }
}

fun parse-int-sign( s : string ) : int {
  if (s=="-") then -1 else 1
}

val rx-ddouble = regex(@"^(([\+\-])?(\d+)(?:\.(\d*))?(?:[eE]([\+\-])?(\d+))?)(?:\s*\+\s*([\+\-]?\d+(?:\.\d*)?(?:[eE][\+\-]?\d+)?))?$")
val rx-special = regex(@"^(nan)|([\+\-])?inf(?:inity)?$")

/*------------------------------------------------------
  Advanced operations
------------------------------------------------------*/

/*------------------------------------------------------
  Roots
------------------------------------------------------*/


// The square root of a non-negative `:ddouble` `x`.
// For negative `x`, `dd-nan` is returned.
public fun sqrt( x : ddouble ) : ddouble {
  // From the QD library:
  // Use Karp's strategy: if a is an approximation to sqrt(x), then
  // sqrt(x) = x*a + (x - (x*a)^2) * a/2   
  if (x.zero?) return zero
  if (x.neg?)  return dd-nan
  val a  = 1.0 / sqrt(x.hi)
  val t1 = x.hi * a
  val t2 = (x - sqr(t1.ddouble)).hi * a * 0.5
  dsum(t1,t2)
}

// The `n`-th root of a `:ddouble` number `x`.
// `n` must be positive, and if `n` is even, then
// `x` must not be negative.
public fun nroot( x : ddouble, n : int ) : ddouble {
  if (n==2) then x.sqrt
  elif (n==1) then x
  elif (n<=0 || (n.even? && x.neg?)) then dd-nan
  elif (x.zero?) then zero
  else {
    val r = x.abs
    val a0 = exp( ~log(r.hi) / n.double ).ddouble
    val a1 = a0 + ((a0 * (one - r * pow(a0,n))) / n.ddouble)
    (if (x.hi.neg?) then ~one else one) / a1
  }
} 

// Multiply `x` by a `:double` `p` where `p` must be a power of 2.
fun mul-pwr2( x : ddouble, p : double ) : ddouble {
  Ddouble(x.hi * p, x.lo * p)
}


/*------------------------------------------------------
  Exponents and logarithms
------------------------------------------------------*/

// Return _e_ (`dd-e`) to the power of `x`.
public fun exp( x : ddouble ) : ddouble {
  /* From the QD library:
     Strategy:  We first reduce the size of x by noting that
     
          exp(kr + m * log(2)) = 2^m * exp(r)^k

     where m and k are integers.  By choosing m appropriately
     we can make |kr| <= log(2) / 2 = 0.347.  Then exp(r) is 
     evaluated using the familiar Taylor series.  Reducing the 
     argument substantially speeds up the convergence.  
  */
  val k : double = 512.0
  val inv-k = 1.0 / k

  if (x.hi <= -709.0) then zero 
  elif (x.hi >= 709.0) then dd-posinf
  elif (x.zero?) then one
  elif (x==one) then dd-e
  else {
    val m : double = floor( (x.hi / dd-log2.hi) + 0.5 )
    val r = mul-pwr2( x - (m.ddouble * dd-log2), inv-k )
    val p = r.sqr
    val t = r + mul-pwr2(p, 0.5)
    var acc := exp-approx( p, t, r, inv-k * dd-epsilon.hi, exp-factors )
    repeat(9) {
      acc := mul-pwr2(acc,2.0) + acc.sqr
    }
    acc := acc + one
    ldexp( acc, m )
  }
}

val exp-factors : list<ddouble> = [
  Ddouble(1.66666666666666657e-01,  9.25185853854297066e-18),
  Ddouble(4.16666666666666644e-02,  2.31296463463574266e-18),
  Ddouble(8.33333333333333322e-03,  1.15648231731787138e-19),
  Ddouble(1.38888888888888894e-03, -5.30054395437357706e-20),
  Ddouble(1.98412698412698413e-04,  1.72095582934207053e-22),
  Ddouble(2.48015873015873016e-05,  2.15119478667758816e-23)
]

fun exp-approx( p : ddouble, t : ddouble, 
                r : ddouble,
                eps : double,
                fs : list<ddouble>,
                s : ddouble = zero ) : ddouble {
  match(fs) {
    Nil -> s + t
    Cons(f,fs1) -> {
      val s1 = s + t
      val p1 = p * r
      val t1 = p1 * f
      if (t1.double.abs <= eps) 
       then s + t
       else exp-approx( p1, t1, r, eps, fs1, s1 )
    }
  }
}



// The natural logarithm (in base _e_) of `x`.
public fun log( x : ddouble ) : ddouble {
  /* From QD Library:
     Strategy.  The Taylor series for log converges much more
     slowly than that of exp, due to the lack of the factorial
     term in the denominator.  Hence this routine instead tries
     to determine the root of the function
         f(x) = exp(x) - a
     using Newton iteration.  The iteration is given by
         x' = x - f(x)/f'(x) 
            = x - (1 - a * exp(-x))
            = x + a * exp(-x) - 1.         
     Only one iteration is needed, since Newton's iteration
     approximately doubles the number of digits per iteration. 
  */
  if (x == one) then zero
  elif (x <= zero) then dd-nan
  elif (x == dd-e) then one
  else {
    val a0 = log(x.hi).ddouble // approximate
    val a1 = a0 + (x * exp(~a0) - one)
    a1
  }
}

// The logarithm in base 10 of `x`.
public fun log10( x : ddouble ) : ddouble {
  return log(x) / dd-log10
}


// The logarithm in base 2 of `x`.
public fun log2( x : ddouble ) : ddouble {
  return log(x) / dd-log2
}


/*------------------------------------------------------
  Trigonometry
------------------------------------------------------*/

// The sine function of a given angle in radians.
public fun sin( rad : ddouble ) : ddouble {
  rad.sincos.fst
}

// The cosine function of a given angle in radians.
public fun cos( rad : ddouble ) : ddouble {
  rad.sincos.snd
}

// The tangent of a given angle in radians.
public fun tan( rad : ddouble ) : ddouble {
  val (s,c) = rad.sincos
  s / c
}

val dd-pi16 = Ddouble(1.963495408493620697e-01,7.654042494670957545e-18)


// Calculate sine and cosine on an angle in radians.
public fun sincos( rad : ddouble ) : (ddouble,ddouble) {
  // quick approximation for small values
  if (rad.hi.abs < 1.0e-11) then {
    return (rad, one - 0.5.ddouble*sqr(rad))   
  }

  /*
  To compute sin(x), we choose integers a, b so that
       rad = s + a * (pi/2) + b * (pi/16)
  and |s| <= pi/32.  A precomputed sin(pi/16) and
  Chebyshev series, we can compute very precisely.
  */
  // find a, b, and s.
  val z  = (rad / dd-twopi).round
  val r  = rad - (dd-twopi * z)
  
  val qa : double = floor( (r.hi / dd-pi2.hi) + 0.5)
  val a  = qa.trunc.int
  val t  = r - (dd-pi2 * qa.ddouble)
  val qb : double = floor((t.hi / dd-pi16.hi) + 0.5)
  val b  = qb.trunc.int
  
  val s  = t - (dd-pi16 * qb.ddouble)
  val s2 = s.sqr
  
  // Use the Chebyshev series for best precision.
  val sins = s * ch-factors.foldl(zero) fun(acc,f) { f + acc*s2 }
  val coss = (one - sins.sqr).sqrt

  // sinb = sin(b*pi/16)
  val sinb = if (b >= 0) then sin16(b) else ~sin16(~b)
  val cosb = sin16(8 - b.abs)

  if (a==0) then (sins*cosb + coss*sinb, coss*cosb - sins*sinb)
  elif (a==1) then (coss*cosb - sins*sinb, ~coss*sinb - sins*cosb)
  elif (a== -1) then (sins*sinb -  coss*cosb, coss*sinb + sins*cosb)
  else (~sins*cosb - coss*sinb, sins*sinb - coss*cosb) // |a| == 2
}

public val ch-factors : list<ddouble> = [
  Ddouble( 1.6056491947130061858e-10, 6.1925234565562595936e-27 ),
  Ddouble( -2.5052108052208301379e-8, -3.6598195022865791876e-25 ),
  Ddouble( 0.0000027557319223964441402, -2.0315661398415506513e-22 ),
  Ddouble( -0.00019841269841269841253, 6.8577289081075077176e-21 ),
  Ddouble( 0.0083333333333333332177, 1.1563735775184918100e-19 ),
  Ddouble( -0.16666666666666665741, -9.2518585321663028924e-18 ),
  Ddouble( 1.0000000000000000000, -6.0239567712403467977e-31 )
]

// Return sin(i*pi/16) for 0 <= i <= 8
public fun sin16( i : int ) : ddouble {
  sin16-table.at(i).default(dd-nan)
}

val sin16-table : vector<ddouble> = [
  zero,
  Ddouble( 0.19509032201612827584, -7.9910790684617312634e-18 ),
  Ddouble( 0.38268343236508978178, -1.0050772696461587612e-17 ),
  Ddouble( 0.55557023301960217765, 4.7094109405616768214e-17 ),
  Ddouble( 0.70710678118654757274, -4.8336466567264561092e-17 ),
  Ddouble( 0.83146961230254523567, 1.4073856984728100930e-18 ),
  Ddouble( 0.92387953251128673848, 1.7645047084336683223e-17 ),
  Ddouble( 0.98078528040323043058, 1.8546939997825014970e-17 ),
  one
].vector


public fun with-sign-of( x : ddouble, y : ddouble ) : ddouble {
  if (y.neg?) then ~(x.abs) else x.abs
}



public fun asin( x : ddouble ) : ddouble {
  val a = x.abs
  if (a > one) then dd-nan
  elif (a == one) then dd-pi2.with-sign-of(x)
  else atan2( x, sqrt(one - x.sqr) )
}

public fun acos( x : ddouble ) : ddouble {
  val a = x.abs
  if (a > one) then dd-nan
  elif (a == one) then (if (x.pos?) then zero else dd-pi)
  else atan2( sqrt(one - x.sqr), x )
}

public fun atan( x : ddouble ) : ddouble {
  atan2(x,one)
}

public fun atan2( y : ddouble, x : ddouble ) : ddouble {
  if (x.zero?) {
    if (y.zero?) then zero /* dd-nan */ else dd-pi2.with-sign-of(y)
  }
  elif (y.zero?) then {
    if (x.pos?) then zero else dd-pi
  }
  elif (x == y) then {
    if (y.pos?) then dd-pi4 else ~dd-pi34
  }
  elif (x == ~y) then {
    if (y.pos?) then dd-pi34 else ~dd-pi4
  }
  else {
    val r = sqrt( x.sqr + y.sqr )
    val xr = x / r
    val yr = y / r

    val z = atan2( y.double, x.double ).ddouble // approximation
    val (sinz,cosz) = sincos(z)
    if (xr.hi.abs > yr.abs.hi) 
     then z + ((yr - sinz) / cosz)
     else z - ((xr - cosz) / sinz)
  }
}

public fun sinh( x : ddouble ) : ddouble {
  if (x.zero?) then zero
  elif (x.abs.double > 0.05) then {
    val ex = x.exp
    if (!ex.finite?) then ex else mul-pwr2( ex - (one / ex), 0.5 )
  }
  else {
    // small x so use Taylor series to avoid cancellation 
    val x2 = x.sqr    
    x*(one + (x2/6.ddouble)*(one + (x2/20.ddouble)*(one + (x2/42.ddouble))))
  }
}

public fun cosh( x : ddouble ) : ddouble {
  if (x.zero?) then one 
  elif (x.abs.double > 0.05) then {
    val ex = x.exp
    if (!ex.finite?) then ex else mul-pwr2(ex + (one / ex), 0.5)
  }
  else {
    val s = x.sinh
    sqrt( one + s.sqr )
  }
}

public fun tanh( x : ddouble ) : ddouble {
  if (x.zero?) then zero 
  elif (x.abs.double > 0.05) then {
    val ex = x.exp
    val iex = one / ex
    ((ex - iex) / (ex + iex))
  }
  else {
    val s = x.sinh
    val c = sqrt( one + s.sqr )
    return (s / c)
  }
}

public fun asinh( x : ddouble ) : ddouble {
  log( x + sqrt(x.sqr + one))
}

public fun acosh( x : ddouble ) : ddouble {
  if (x < one) then dd-nan else log(x + sqrt(x.sqr - one))
}

public fun atanh( x : ddouble ) : ddouble {
  if (x.abs >= one) then dd-nan 
   else mul-pwr2( log((one + x) / (one - x)), 0.5 )
}
