/*---------------------------------------------------------------------------
  Copyright 2017-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* 128-bit double-double floating point numbers.

The `:ddouble` type implements [double double][ddwiki] 128-bit floating point numbers
as a pair of IEEE `:float64` values. This extends the precision to 31 decimal digits
(versus 15 for `:float64`), but keeps the same range as
a `:float64` with a maximum  value of about 1.8&middot;10^308^ (`dd-max`). Because
`:float64`s usually have hardware support, a `:ddouble` is usually much faster
than arbitrary precision floating point numbers.

## Precision {-}

If you add two regular 64-bit `:float64` values you can quickly notice the imprecision
due to decimal fractions that cannot be represented precisely. For example:
```
> 0.1 + 0.2
0.30000000000000004
```

This happens because the decimal `0.1` and `0.2` cannot be represented exactly
by a `:float64` which is encoded in base 2. For example, if we show `0.1` to 20 digits, we get:
```
> 0.1.show(20)
"0.10000000000000000555"
```

When we convert the constant `0.1` to a `:ddouble` we can see with even more precision how it is
approximated:

```
> 0.1.ddouble
0.1000000000000000055511151231258
```

However, if we convert the number `0.1` directly to a `:ddouble` (instead of going through
a `:float64` constant), we can represent `0.1` more precisely:

```
> ddouble("0.1")   // for convenience; using `ddouble-exp(1,-1)` is more efficient
0.1
```

This is possible because a `:ddouble` uses two `:float64`s to represent numbers; in this
case the first (_hi_) float64 is (the inexact) `0.1` while the second one (_lo_) is the
correction to that number. The `show-sum`  (or `show-hex`) function shows this internal pair of numbers:

```
> ddouble("0.1").show-sum(20)
"0.10000000000000000555 + -5.55111512312578301027e-18"

> ddouble("0.1") + ddouble("0.2")
0.3
```

Generally, a `:ddouble` _d_ is represented by a pair of `:float64`s, _hi_ and _lo_,
such that _d_ equals _hi_+_lo_, where \|_lo_\| &le; 0.5&middot;ulp(_hi_).
Note that despite the extra precision, underflow/overflow and machine precision are
not as well-defined as with a regular `:float64` [@lin;@shewchuk;@Dekker].
Nevertheless, using a `:ddouble` often prevents instability in practice over
`:float64` computations due to  rounding errors or combining very large and
small numbers.

Take the "thin triangle" problem for example [@Goldberg:float;@Kahan:triangle].
The challenge is to compute
the area of a very thin triangle with sides $a$, $b$, and $c$. The short sides
 $b$ and $c$ are just 3 units in the last place (ulp) shorter than
the longest side $a$. Using the value $s = (a + b + c)/2$, the area of
a triangle is $A = \sqrt{s(s-a)(s-b)(s-c)}$. This is troublesome if $s$ is
close to $a$ since $s-a$ magnifies any rounding error. Let's take $a=7$,
and $b = c = 7/2 + 3\cdot2^{-111}$; according to [@gustafson:posit],
&ldquo;If the units are in light-years, then the short sides are only longer than
half the long side by $1/200$th the diameter of a proton. Yet that pops the triangle
up to about the width of a doorway at the thickest point&rdquo;. Using various
128-bit numbers we get:
````
exact          : 3.14784204874900425235885265494550774498...e-16
128-bit ddouble: 3.147842048749004252358852654945\([47]{color:#F88}\)e-16
128-bit posit  : 3.147842048749004252358852654945507744\([39]{color:#F88}\)e-16
128-bit ieee   : 3.\([63481490842332134725920516158057682878]{color:#F88}\)e-16
````
For this kind of example, a `:ddouble` has better precision than a
regular 128-bit IEEE float since it can combine very large and
small values. (Kahan [@Kahan:triangle] shows how to rewrite the equations
to avoid magnifying rounding errors -- in that case the result for
IEEE 128-bit floats becomes:
````
128-bit ieee x : 3.147842048749004252358852654945507\([92210]{color:#F88}\)e-16
````

The implementation is based closely on the [QD] C++ library [@Hida:qd;@Hida:qdlib],
and assumes proper 64-bit IEEE `:float64`s with correct rounding.
Integers can be represented precisely up to 30 decimal digits (and a bit more...
up to 2^106^ - 2).

## References {-}

~ Bibliography { caption:"0" }

~~ BibItem { #dekker; bibitem-label:"[1]"; searchterm:"Dekker+A+Floating-Point+Technique+for+Extending+the+Available+Precision" }
T.\ Dekker.
_A Floating-Point Technique for Extending the Available Precision_.
Numerische\ Mathematik, vol. 18 (3), June 1971, 224--242.
[pdf](http://gdz.sub.uni-goettingen.de/dms/resolveppn/?PPN=GDZPPN001170007).
~~

~~ BibItem { #Goldberg:float; bibitem-label:"[2]"}
David Goldberg.
_What Every Computer Scientist Should Know About Floating-point Arithmetic_.
ACM Computing Survey. vol. 23 (1), March 1991. doi: [10.1145/103162.103163](http://doi.org/10.1145/103162.103163).
[pdf](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.22.6768&rep=rep1&type=pdf).
~~

~~ BibItem { #Gustafson:posit; bibitem-label:"[3]"; searchterm:"Gustafson+John+Beating+floating+point+at+its+own+game+posit+arithmetic"; }
John\ L. Gustafson and Isaac\ Yonemoto.
_Beating Floating Point at its Own Game: Posit Arithmetic_. 2017.
[pdf](http://www.johngustafson.net/pdfs/BeatingFloatingPoint.pdf).
~~

~~ BibItem { #Hida:qd; bibitem-label:"[4]"; searchterm:"Hida+Quad-Double+Arithmetic:+Algorithms,+Implementation,+and+Application" }
Yozo Hida,\ Xiaoye S.\ Li, and David\ H.\ Bailey.
_Quad-Double Arithmetic: Algorithms, Implementation, and Application_.
Lawrence Berkeley National Laboratory Technical Report LBNL-46996. 2000. [pdf](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.4.5769).
~~

~~ BibItem { #Hida:qdlib; bibitem-label:"[5]"; searchterm:"Hida+Library+for+double-double+and+quad-double+arithmetic" }
Yozo Hida,\ Xiaoye\ S.\ Li, and David\ H.\ Bailey.
_Library for double-double and quad-double arithmetic._
(2007). [pdf](http://www.jaist.ac.jp/~s1410018/papers/qd.pdf).
~~

~~ Bibitem { #lin; bibitem-label:"[6]"; searchterm:'Linnainmaa+"Software+for+Doubled-Precision+Floating-Point+Computations"' }
Seppo\ Linnainmaa.
_Software for Doubled-Precision Floating-Point Computations_.
ACM Transactions on Mathematical Software (TOMS), vol. 7 (3), Sept. 1981, 272--283.
~~

~~ Bibitem { #Kahan:triangle; bibitem-label:"[7]"}
William\ Kahan.
_Miscalculating Area and Angles of a Needle-like Triangle_.
Lecture notes, 2004, [pdf](https://people.eecs.berkeley.edu/~wkahan/Triangle.pdf).
~~

~~ BibItem { #shewchuk; bibitem-label:"[8]"; }
Jonathan\ Richard\ Shewchuk.
_Adaptive Precision Floating-Point Arithmetic and Fast Robust Geometric Predicates_.
Discrete & Computational Geometry, vol. 18, 305--363, 1997. [pdf](https://people.eecs.berkeley.edu/~jrs/papers/robustr.pdf)
~~

~

[ddwiki]: https://en.wikipedia.org/wiki/Quadruple-precision_floating-point_format#Double-double_arithmetic
[qd]:     http://crd-legacy.lbl.gov/~dhbailey/mpdist

\/
*/
module std/num/ddouble

import std/num/float64
import std/num/decimal
import std/text/parse

/* The `:ddouble` type implements [float64 float64][ddwiki] 128-bit floating point numbers
as a pair of IEEE `:float64` values. This extends the precision to 31 decimal digits
(versus 15 for `:float64`), but keeps the same range as
a `:float64` with a maximum  value of about 1.8&middot;10^308^. Because
`:float64`s usually have hardware support, a `:ddouble` is usually much faster
than arbitrary precision floating point numbers.

Internally a `:ddouble` _d_ is represented as a pair of `:float64`s, _hi_ and _lo_,
such that the number represented by _d_ is _hi_+_lo_, where \|_lo_\| &le; 0.5&middot;ulp(_hi_).
*/
abstract struct ddouble
  hi : float64
  lo : float64

// Create a `:ddouble` from a `:float64`.
pub fun ddouble( d : float64 ) : ddouble
  Ddouble(d,0.0)

val maxprecise : int = 9007199254740991
val minprecise : int = ~maxprecise

fun is-precise(i : int ) : bool
  (i >= minprecise && i <= maxprecise)


// Create a `:ddouble` from an `:int`.
// A `:ddouble` can represent integers precisely up to 30 digits.
// If an integer is passed that is out of range an infinity is returned.
pub fun ddouble( i : int ) : ddouble
  ddouble-int-exp(i,0)

fun ddouble-int-exp( i : int, e : int ) : ddouble
  if i.is-precise then small-exp( i, e ) else
    // ensure we convert at least 31 (and at most 42) digits precisely
    val p    = i.count-digits
    val x    = i
    val px   = p - 14
    val (hi,y) = x.cdivmod-exp10(px)
    val py   = px - 14
    if py <= 0 then
      // if 28 or less digits we can convert with two doubles
      // trace("ddouble-i: " + i.show + ", hi:" + hi.show + ",  lo: " + y.show)
      small-exp(hi, px + e) + small-exp( y, e)

    else
      // otherwise use 2 initial 14 digits and a trailing value
      val (mid,z) = y.cdivmod-exp10(py)
      val pz = py - 14
      val (lo,plo) = if pz <= 0 then (z,0) else (z.cdiv-exp10(pz), pz)
      //trace("ddouble-i: " + i.show + ", hi:" + hi.show + ", mid: " + mid.show + ", lo: " + lo.show)
      //trace("  px: " + px.show + ", py : " + py.show + ", plo: " + plo.show + ", e: " + e.show )
      small-exp( hi, px + e) + (small-exp( mid, py + e) + small-exp( lo, plo + e))


fun small-exp( i : int, e : int )
  val dd = i.float64.ddouble
  if e.is-zero then dd else dd.mul-exp10(e)

// &pi;
pub val dd-pi     = Ddouble(0x1.921FB54442D18p+1, 0x1.1A62633145C07p-53) // 3.1415926535897932384626433832795028841971693993751
// pub val dd-pi-qd      = Ddouble(3.1415926535897931160,   1.2246467991473529607e-16)  // QD's pi constant is one bit off...

// 2&pi;
pub val dd-twopi   = Ddouble(0x1.921FB54442D18p+2, 0x1.1A62633145C07p-52)

// &pi;/2
pub val dd-pi2     = Ddouble(0x1.921FB54442D18p+0, 0x1.1A62633145C07p-54)

// &pi;/4
pub val dd-pi4     = Ddouble(0x1.921FB54442D18p-1, 0x1.1A62633145C07p-55)
// QD:                         7.853981633974482790e-01,3.061616997868383018e-17)

// &pi;/16
val dd-pi16 = Ddouble( 0x1.921FB54442D18p+1, 0x1.1A62633145C07p-53)

// 3&pi;/4
pub val dd-pi34    = Ddouble(0x1.2D97C7F3321D2p+1, 0x1.A79394C9E8A0Bp-54)
// QD:                          2.356194490192344837e+00,9.1848509936051484375e-17)  // QD is again one bit off

// The _e_ constant.
pub val dd-e       = Ddouble(0x1.5BF0A8B145769p+1, 0x1.4D57EE2B1013Ap-53) // 2.7182818284590452353602874713526624977572470937

// The natural logarithm of 2
pub val dd-ln2     = Ddouble(0x1.62E42FEFA39EFp-1, 0x1.ABC9E3B39803Fp-56) // 0.69314718055994530941723212145817656807550013436026

// The natural logarithm of 10
pub val dd-ln10    = Ddouble(0x1.26BB1BBB55516p+1, -0x1.F48AD494EA3E9p-53) // 2.3025850929940456840179914546843642076011014886288

// The base-2 logarithm of _e_.
pub val dd-log2e   = Ddouble(0x1.71547652B82FEp+0, 0x1.777D0FFDA0D23p-56) // 1.442695040888963407359924681001892137426645954153

// The base-10 logarithm of _e_.
pub val dd-log10e  = Ddouble(0x1.BCB7B1526E50Ep-2, 0x1.95355BAAAFAD3p-57) // 0.43429448190325182765112891891660508229439700580367

// The square-root of 2
pub val dd-sqrt2   = Ddouble(0x1.6A09E667F3BCDp+0, -0x1.BDD3413B26456p-54) // 1.4142135623730950488016887242096980785696718753769

// `1.0 / sqrt(2.0)`
pub val dd-sqrt12  = Ddouble(0x1.6A09E667F3BCDp-1, -0x1.BDD3413B26456p-55) // 0.70710678118654752440084436210484903928483593768847

// [Euler's constant](https://en.wikipedia.org/wiki/Euler%E2%80%93Mascheroni_constant)
pub val dd-euler   = Ddouble(0x1.2788CFC6FB619p-1, -0x1.6CB90701FBFABp-58) //0.57721566490153286060651209008240243104215933593992

// The 'machine epsilon': this is not well-defined for a `:ddouble` in general since
// the difference between 1.0 and the next representable `:ddouble` value is `dd-true-min`.
// Instead, we take the square of `flt-epsilon`, i.e. 2^-104^.
pub val dd-epsilon = Ddouble(0x1p-104,0.0)

// 8*dd-epsilon
val dd-epsilon8 = Ddouble(0x1.0p-101, 0x0.0p+0)

// The maximum representable `:ddouble`
pub val dd-max     = Ddouble( 0x1.FFFFFFFFFFFFFp+1023, 0x1.FFFFFFFFFFFFFp+969 )

// The smallest positive `:ddouble` that is still normalized
pub val dd-min     = Ddouble( flt-min, 0.0 )   // normalized = 2^-1022

// The smallest positive `:ddouble`  (which is subnormal).
pub val dd-true-min     = Ddouble( flt-true-min, 0.0 )

// Not-A-Number
pub val dd-nan     = Ddouble(nan,0.0)

// Positive infinity
pub val dd-posinf  = Ddouble(posinf,0.0)

// Negative infinity
pub val dd-neginf  = Ddouble(neginf,0.0)

// maximal precision in decimal digits of a `:ddouble`.
pub val dd-max-prec = 31

val dd-default-prec = -31

/*------------------------------------------------------
   Compare etc.
------------------------------------------------------*/

// Zero constant
pub val zero = ddouble(0.0)

// One
pub val one  = ddouble(1.0)

// Ten (`10.ddouble`)
pub val ten  = ddouble(10.0)

val two         = ddouble(2.0)

// Is this `:ddouble` equal to is-zero
pub fun is-zero( x : ddouble ) : bool
  x.hi.is-zero

// Is this `:ddouble` negative?
pub fun is-neg( x : ddouble ) : bool
  x.hi.is-neg

// Is this `:ddouble` positive?
pub fun is-pos( x : ddouble ) : bool
  x.hi.is-pos

// Return the sign of the `:ddouble`.
pub fun is-sign( x : ddouble ) : order
  if x.hi==0.0 then Eq
  elif x.hi < 0.0 then Lt
  else Gt

// Is this `:ddouble` not-a-number?
pub fun is-nan( x : ddouble ) : bool
  x.hi.is-nan || x.lo.is-nan

// Is this a finite `:ddouble`? (i.e. not `is-nan` or `is-inf`)
pub fun is-finite( x : ddouble ) : bool
  x.hi.is-finite && x.lo.is-finite

// Is this an infinite value.
pub fun is-inf( x : ddouble ) : bool
  x.hi.is-inf

// Does `x` equal positive infinity?
pub fun is-posinf( x : ddouble ) : bool
  x.hi.is-posinf

// Does `x` equal negative infinity?
pub fun is-neginf( x : ddouble ) : bool
  x.hi.is-neginf

// Compare two `:ddouble` values.
pub fun compare( x : ddouble, y : ddouble ) : order
  match compare(x.hi,y.hi)
    Eq  -> compare(x.lo,y.lo)
    ord -> ord


pub fun (>) (x : ddouble, y : ddouble) : bool { compare(x,y) == Gt }
pub fun (>=)(x : ddouble, y : ddouble) : bool { compare(x,y) != Lt }
pub fun (==)(x : ddouble, y : ddouble) : bool { compare(x,y) == Eq }
pub fun (!=)(x : ddouble, y : ddouble) : bool { compare(x,y) != Eq }
pub fun (<) (x : ddouble, y : ddouble) : bool { compare(x,y) == Lt }
pub fun (<=)(x : ddouble, y : ddouble) : bool { compare(x,y) != Gt }

// The minimum of `x` and `y`.
pub fun min( x : ddouble, y : ddouble ) : ddouble
  if x <= y then x else y

// The maximum of `x` and `y`
pub fun max( x : ddouble, y : ddouble ) : ddouble
  if x >= y then x else y

/*------------------------------------------------------
   Addition
------------------------------------------------------*/

struct edouble
  num : float64
  err : float64

// often called `twosum` in literature (see [@shewchuk])
fun sum( x : float64, y : float64 ) : edouble
  val z    = x + y
  val diff = z - x
  val err  = (x - (z - diff)) + (y - diff)
  Edouble(z, if z.is-finite then err else z)

fun dsum( x : float64, y : float64 ) : ddouble
  val z    = x + y
  val diff = z - x
  val err  = (x - (z - diff)) + (y - diff)
  Ddouble(z, if z.is-finite then err else z)

// As `sum` but with `x.abs >= y.abs`
fun quicksum( x : float64, y : float64 ) : edouble
  val z   = x + y
  val err = y - (z - x)
  Edouble(z, if z.is-finite then err else z)

fun dquicksum( x : float64, y : float64 ) : ddouble
  if !x.is-finite return ddouble(x)
  val z   = x + y
  val err = y - (z - x)
  Ddouble(z, if z.is-finite then err else z)

// Add two `:ddouble`s
pub fun (+)( x : ddouble, y : ddouble ) : ddouble
  val z1 = sum(x.hi,y.hi)
  val lo = sum(x.lo,y.lo)
  val e1 = z1.err + lo.num
  val z2 = quicksum(z1.num,e1)
  val e2 = z2.err + lo.err
  dquicksum(z2.num,e2)

// Negate a `:ddouble`.
pub fun (~)( x : ddouble ) : ddouble
  Ddouble(~x.hi,~x.lo)

// Subtract two values.
pub fun (-)( x : ddouble, y : ddouble ) : ddouble
  x + (~y)

// Return the absolute value.
pub fun abs( x : ddouble ) : ddouble
  if x.is-neg then ~x else x

// Increment by one.
pub fun inc( x : ddouble ) :ddouble
  x + one

// Decrement by one.
pub fun dec( x : ddouble ) :ddouble
  x - one


/*------------------------------------------------------
   Multiply and divide
------------------------------------------------------*/

/*
val splitter = 0x1.0000002p+27 // 134217729.0 = 2^27 + 1
val splitbound  =  0x1.0p996 // 6.696928794914171e+299  = 2^996   // Note, QD seems one bit off
val two28 = 0x1.0p28 // 268435456.0 = 2^28

fun split( x : float64 ) : (float64,float64)
  if x > splitbound || x < ~splitbound
    val y = x * 0x1.0p-28  // 3.7252902984619140625e-09 = 2^-28
    val t = y * splitter
    val hi = t - (t - y)
    val lo = y - hi
    (hi*two28, lo*two28)

  else
    val t = x * splitter
    val hi = t - (t - x)
    val lo = x - hi
    (hi,lo)


// often called `twoproduct` in literature (see [@shewchuk])
fun prod( x : float64, y : float64 ) : edouble
  val z = x*y
  val (xhi,xlo) = split(x)
  val (yhi,ylo) = split(y)
  val err = ((xhi*yhi - z) + (xhi*ylo + xlo*yhi)) + (xlo*ylo)
  Edouble(z,if z.is-finite then err else z)

fun prodsqr( x : float64 ) : edouble
  val z = x*x
  val (h,l) = split(x)
  val err = ((h*h - z) + (2.0*h*l)) + (l*l)
  Edouble(z,if z.is-finite then err else z)

*/

// often called `twoproduct` in literature (see [@shewchuk])
fun prod( x : float64, y : float64 ) : edouble
  val z = x*y
  val err = fmadd(x,y,~z)
  Edouble(z,err)

fun prodsqr( x : float64 ) : edouble
  val z = x*x
  val err = fmadd(x,x,~z)
  Edouble(z,err)

// Multiply two `:ddouble`s
pub fun (*)( x : ddouble, y : ddouble ) : ddouble
  val z  = prod(x.hi, y.hi)
  val e  = z.err + (x.hi*y.lo + x.lo*y.hi)
  dquicksum(z.num, e)

// Multiply `x` with itself.
pub fun sqr( x : ddouble ) : ddouble
  val z = prodsqr(x.hi)
  val e = (z.err + (2.0*x.hi*x.lo)) + (x.lo*x.lo)
  dquicksum(z.num,e)

// Divide two `:ddouble`s
pub fun (/)( x : ddouble, y : ddouble ) : ddouble
  val q1 = ddouble(x.hi / y.hi)
  if !q1.is-finite || !y.hi.is-finite return q1
  val r1 = x - (y * q1)
  val q2 = ddouble(r1.hi / y.hi)
  val r2 = r1 - (y * q2)
  val q3 = ddouble(r2.hi / y.hi)
  val q  = dquicksum(q1.hi,q2.hi)
  q + q3

// Remainder of two `:ddouble`s
pub fun (%)( x : ddouble, y : ddouble ) : ddouble
  val n = (x / y).round
  x - (n*y)

// Division and remainder of two `:ddouble`s
pub fun divrem( x : ddouble, y : ddouble ) : (ddouble,ddouble)
  val n = (x / y).round
  (n, x - (n*y))

/*------------------------------------------------------
   Rounding
------------------------------------------------------*/

// Convert a `:ddouble` to a `:float64` (losing precision)
pub fun float64( x : ddouble ) : float64
  x.hi

// Convert a `:ddouble` to the nearest integer (rounding to the nearest even number in case of a tie)
pub fun int( x : ddouble, nonfin : int = 0 ) : int
  if !x.is-finite then nonfin else x.round.decimal.int

// Round a `:ddouble` to the nearest integer, rounding to the nearest even number in case of a tie.
pub fun round( x : ddouble ) : ddouble
  val r = x.hi.round
  val diff = r - x.hi
  if diff == 0.0 then dquicksum(r,x.lo.round)
  elif diff == 0.5 && x.lo.is-neg then ddouble(r - 1.0)
  elif diff == -0.5 && x.lo.is-pos then ddouble(r + 1.0)
  else ddouble(r)

// Round to negative infinity.
pub fun floor( x : ddouble ) : ddouble
  val r = x.hi.floor
  if r == x.hi
   then dquicksum(r,x.lo.floor)
   else Ddouble(r,0.0)

// Round to positive infinity.
pub fun ceiling( x : ddouble ) : ddouble
  val r = x.hi.ceiling
  if r == x.hi
   then dquicksum(r,x.lo.ceiling)
   else Ddouble(r,0.0)

// Round towards zero.
pub fun truncate( x : ddouble ) : ddouble
  if x.is-neg then x.ceiling else x.floor

// The fraction of `x` such that `x.truncate + x.fraction == x`.
pub fun fraction( x : ddouble ) : ddouble
  x - x.truncate

// The _floored_ fraction of `x`. This is always positive, such that `x.floor + x.ffraction == x`.
pub fun ffraction( x : ddouble ) : ddouble
  x - x.floor

// Round a `:ddouble` to a specified precision.
// Uses `round` if the precision is smaller or equal to zero.
pub fun round-to-prec( x : ddouble, prec : int  ) : ddouble
  if prec <= 0 then x.round
  elif prec > dd-max-prec then x
  else
    val p : ddouble = powi10(prec)
    (x * p).round / p


/*------------------------------------------------------
   Exponents (needed for showing ddouble's)
------------------------------------------------------*/

// 'Load exponent': returns `x`&middot;2^`exp`^.
pub fun ldexp( x : ddouble, exp : int ) : ddouble
  Ddouble(ldexp(x.hi,exp),ldexp(x.lo,exp))

fun npwr-acc( x : ddouble, acc : ddouble, n : int ) : ddouble
  if n <= 0 then acc
  elif n.is-even then npwr-acc( sqr(x), acc, unsafe-decreasing(n/2) )
  else npwr-acc( x, x*acc, unsafe-decreasing(n.dec))

fun npwr( x : ddouble, n : int ) : ddouble
  if n==0 then (if x.is-zero then dd-nan else one)
  elif n==1 then x
  else x.npwr-acc(one,n)

// Return `x` to the power of `n`.
fun powi( x : ddouble, n : int ) : ddouble
  val p = npwr(x,n.abs)
  if n.is-neg then (one / p) else p

// Return 10 to the power of `exp`.
fun powi10( exp : int ) : ddouble
  powi(ten,exp)

// Return 10 to the power of `exp`.
pub fun exp10( exp : ddouble ) : ddouble
  pow(ten,exp)

// Return 2 to the power of `exp`.
pub fun exp2( exp : ddouble ) : ddouble
  pow(two,exp)

// Create a `:ddouble` `x` such that `x` equals `d`&middot;10^`e`^.
pub fun ddouble-exp( d : float64, e : int )
  d.ddouble.mul-exp10(e)

// Create a `:ddouble` `x` such that `x` equals `i`&middot;10^`e`^.
pub fun ddouble-exp( i : int, exp : int ) : ddouble
  i.ddouble.mul-exp10(exp)

fun mul-exp10( x : ddouble, exp : int ) : ddouble
  if exp.is-zero then x else x * powi10(exp)

/*------------------------------------------------------
   Show
------------------------------------------------------*/

// Decode a `:ddouble` `d` into two doubles `(hi,lo)` such that `d` equals  `hi`+`lo`,
// where `lo` &le; 0.5&middot;ulp(`hi`).
pub fun decode( d : ddouble ) : (float64,float64)
  (d.hi,d.lo)

// Encode a `:ddouble` `d` from two doubles `(hi,lo)` such that `d` equals  `hi`+`lo`.
pub fun encode( hi : float64, lo : float64 ) : ddouble
  hi.ddouble + lo.ddouble

// Convert a `:ddouble` to a `:decimal` up to a given precision `prec` (= `-1`).
// A negative precision converts precisely. Returns 0 for non-finite `:ddouble`'s.
pub fun decimal( x : ddouble, prec : int = -1 ) : decimal
  if !x.is-finite
    then 0.decimal
    else x.hi.decimal(prec) + x.lo.decimal(prec)

/* Show a `:ddouble` `x` precisely as the sum of two `:float64`s
in [hexadecimal notation](https://books.google.com/books?id=FgMsCwAAQBAJ&pg=PA41).
Use this if you need to guarantee that you can parse back `:ddouble`s exactly,
i.e. `x == x.show-hex.ddouble`.
```
> 0.1.ddouble.show-hex
"0x1.999999999999Ap-4 + 0x0.0p+0"
> "0.1".ddouble.show-hex
"0x1.999999999999Ap-4 + -0x1.999999999999Ap-58"
> dd-pi.show-hex
"0x1.921FB54442D18p+1 + 0x1.1A62633145C07p-53"
> dd-max.show-hex
"0x1.FFFFFFFFFFFFFp+1023 + 0x1.FFFFFFFFFFFFFp+969"
```
.
*/
pub fun show-hex( x : ddouble, width : int = 1, use-capitals : bool = True, pre : string = "0x" ) : string
  if !x.is-finite
    then x.hi.show
    else x.hi.show-hex(width,use-capitals,pre) ++ " + " ++ x.lo.show-hex(width,use-capitals,pre)

// Show a `:ddouble` `x` with a given precision `prec` (=`-31`).
// The precision specifies the  number of digits after the dot (in either scientific of fixed-point notation).
// If the precision is negative, _at most_ `prec` digits are displayed, while for a positive
// precision, exactly `prec` digits behind the dot are displayed.
// This uses `show-fixed` when the exponent of `x` in scientific notation is larger than -5
// and smaller than the precision (or 15 in case of a negative precision), otherwise it uses `show-exp`.
pub fun show( x : ddouble, prec : int = dd-default-prec ) : string
  if !x.is-finite
    then x.hi.show
    else x.decimal.show(prec)

/* Show a ddouble `x` with a given precision `prec` (=`-31`) in scientific notation.
The precision specifies the  number of digits after the dot, i.e.
the number of significant digits is `prec+1`.
If the precision is negative, _at most_ `prec` digits are displayed, and if
it is positive exactly `prec` digits are used.
```
> 1.1.ddouble.show-exp
"1.1000000000000000888178419700125"
> 1.1.ddouble.show-exp(-100)
"1.100000000000000088817841970012523233890533447265625"
> 1.1.ddouble.show-exp(5)
"1.10000"
> 1.1.ddouble.show-exp(-5)
"1.1"
```
.
*/
pub fun show-exp( x : ddouble, prec : int = dd-default-prec ) : string
  if !x.is-finite
    then x.hi.show
    else x.decimal.show-exp(prec)

/* Show a ddouble `x` with a given precision `prec` (=`-31`) in fixed-point notation.
The precision specifies the  number of digits after the dot.
If the precision is negative, _at most_  `prec` digits after the dot are displayed,
while for a positive precision, exactly `prec` digits are used.
```
> 0.1.ddouble.show-fixed
"0.1000000000000000055511151231258"
> 0.1.ddouble.show-fixed(-100)
"0.1000000000000000055511151231257827021181583404541015625"
> 0.1.ddouble.show-fixed(5)
"0.10000"
> 0.1.ddouble.show-fixed(-5)
"0.1"
```
.
*/
pub fun show-fixed( x : ddouble, prec : int = dd-default-prec ) : string
  if !x.is-finite
    then x.hi.show
    else x.decimal.show-fixed(prec)

// Show a `:ddouble` as the sum of  `:float64`s with an optional precision.
// Note: use `show-hex` for reliable round-trip parsing.
pub fun show-sum( x : ddouble, prec : int = -17 ) : string
  if !x.is-finite
    then x.hi.show
    else x.hi.show(prec) ++ " + " ++ x.lo.show(prec)


/*------------------------------------------------------
  Parsing
------------------------------------------------------*/

// Parse a floating point number with up to 31 digits precision.
// Return `dd-nan` if the string is an invalid number.
pub fun ddouble( s : string ) : ddouble
  s.parse-ddouble.default(dd-nan)

/*
// Parse a floating point number with up to 31 digits precision.
// Returns `Nothing` on invalid input.
pub fun parse-ddouble( s  : string ) : maybe<ddouble>
  val t = s.trim
  match(t.find(rx-ddouble))
    Just(cap) ->
      val whole	= cap.groups[1]
      val frac	= cap.groups[2].trim-right("0")
      val exp	= cap.groups[3].parse-int.default(0)
      val w	= (whole + frac).parse-int.default(0)
      val e	= exp - frac.count
      val x	= ddouble-int-exp(w,e)
      // trace("parse: s: " + s + "\n  w: " + w.show + ", e: " + e.show + "\n  wx: " + w.ddouble.show-sum + ", x: " + x.show + "\n  " + x.show-sum)
      Just(x)

    Nothing -> match(t.find(rx-special))
      Just(cap) ->
        val special =	if !cap.groups[1].is-empty then dd-nan
                       elif cap.groups[2] == "-" then dd-neginf
                       else dd-posinf
        Just(special)

      Nothing -> match(t.find(rx-float64-sum))
        Just(cap) ->
          match cap.groups[1].parse-float64
            Just(hi) -> match cap.groups[2].parse-float64
              Just(lo) -> Just( hi.ddouble + lo.ddouble )
              Nothing -> Nothing

            Nothing -> Nothing

        Nothing -> Nothing



val rx-ddouble = regex(r"^([\-\+]?\d+)(?:\.(\d*))?(?:[eE]([\-\+]?\d+))?$")
val rx-float64-sum = regex(r"^(\S*\d) *\+ *(\S+)$")
val rx-special = regex(r"^(nan)|([\+\-])?inf(?:inity)?$",ignorecase=True)
*/

pub fun parse-ddouble( s  : string ) : maybe<ddouble>
  s.trim.to-lower.slice.parse-eof(pddouble).maybe

pub fun pddouble() : parse ddouble
  (pddouble-sum || pddouble-normal)

fun pddouble-sum() : parse ddouble
  val hi = pdouble()
  if !hi.is-finite return hi.ddouble   // NaN or +-Infinity
  pstring(" + ")
  val lo = pdouble()
  return (hi.ddouble + lo.ddouble)

fun pddouble-normal() : parse ddouble
  val neg   = sign()
  val whole = digits()
  val frac  = optional("", { char('.'); digits() } ).trim-right("0")
  val exp   = optional(0, { char('e'); pint() } )
  val w = ((if neg then "-" else "") ++ whole ++ frac).parse-int.default(0)
  val e = exp - frac.count
  val x = ddouble-int-exp(w,e)
  // trace("parse-normal: w: " + w.show + ", e: " ++ e.show ++ "\n  wx: " ++ w.ddouble.show-sum ++ ", x: " ++ x.show ++ "\n  " ++ x.show-sum)
  x

/*------------------------------------------------------
  Advanced operations
------------------------------------------------------*/

/*------------------------------------------------------
  Roots
------------------------------------------------------*/

// The square root of a non-negative `:ddouble` `x`.
// For negative `x`, `dd-nan` is returned.
pub fun sqrt( x : ddouble ) : ddouble
  // From the QD library:
  // Use Karp's strategy: if a is an approximation to sqrt(x), then
  // sqrt(x) = x*a + (x - (x*a)^2) * a/2
  if x.is-zero then zero
  elif x.is-neg then dd-nan
  else
    val a  = 1.0 / sqrt(x.hi)
    val t1 = x.hi * a
    val t2 = (x - sqr(t1.ddouble)).hi * a * 0.5
    dsum(t1,t2)


// The `n`-th root of a `:ddouble` number `x`.
// `n` must be positive, and if `n` is even, then
// `x` must not be negative.
pub fun nroot( x : ddouble, n : int ) : ddouble
  if n==2 then x.sqrt
  elif n==1 then x
  elif (n<=0 || (n.is-even && x.is-neg)) then dd-nan
  elif x.is-zero then zero
  else
    val r = x.abs
    val a0 = exp( ~ln(r.hi) / n.float64 ).ddouble
    val a1 = a0 + ((a0 * (one - r * powi(a0,n))) / n.ddouble)
    (if x.hi.is-neg then ~one else one) / a1


// Multiply `x` by a `:float64` `p` where `p` must be a power of 2.
fun mul-pwr2( x : ddouble, p : float64 ) : ddouble
  Ddouble(x.hi * p, x.lo * p)

val one-half : ddouble = 0.5.ddouble

fun half( x : ddouble ) : ddouble
  mul-pwr2( x, 0.5 )

fun twice( x : ddouble ) : ddouble
  mul-pwr2( x, 2.0 )


/*------------------------------------------------------
  Exponents and logarithms
------------------------------------------------------*/

// Return _e_ (`dd-e`) to the power of `x`.
pub fun exp( x : ddouble ) : ddouble
  /* From the QD library:
     Strategy:  We first reduce the size of x by noting that

          exp(kr + m * ln(2)) = 2^m * exp(r)^k

     where m and k are integers.  By choosing m appropriately
     we can make |kr| <= ln(2) / 2 = 0.347.  Then exp(r) is
     evaluated using the familiar Taylor series.  Reducing the
     argument substantially speeds up the convergence.
  */
  val k : float64 = 512.0
  val inv-k = 1.0 / k

  if x.hi <= -709.0 then zero
  elif x.hi >= 709.0 then dd-posinf
  elif x.is-zero then one
  elif x==one then dd-e
  else
    val m : int = floor( (x.hi / dd-ln2.hi) + 0.5 ).int
    val r = mul-pwr2( x - (m.ddouble * dd-ln2), inv-k )
    val p = r.sqr
    val t = r + p.half
    var acc := exp-approx( p, t, r, inv-k * dd-epsilon.hi, exp-factors )
    repeat(9)
      acc := acc.twice + acc.sqr

    acc := acc + one
    ldexp( acc, m )


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
                eps : float64,
                fs : list<ddouble>,
                s : ddouble = zero ) : ddouble
  match fs
    Nil -> s + t
    Cons(f,fs1) ->
      val s1 = s + t
      val p1 = p * r
      val t1 = p1 * f
      if t1.float64.abs <= eps
       then s + t
       else exp-approx( p1, t1, r, eps, fs1, s1 )



// The natural logarithm (in base _e_) of `x`.
pub fun ln( x : ddouble ) : ddouble
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
  if x <= zero then (if x==zero then dd-neginf else dd-nan)
  elif x == one then zero
  elif x == dd-e then one
  elif x.is-posinf then x
  else
    val a0 = ln(x.hi).ddouble // approximate
    val a1 = a0 + (x * exp(~a0) - one)
    a1


// Return the logarithm in some base `b` of a `:ddouble` `x`
pub fun log( x : ddouble, base : ddouble ) : ddouble
  ln(x) / ln(base)

// The logarithm in base 10 of `x`.
pub fun log10( x : ddouble ) : ddouble
  ln(x) / dd-ln10

// The logarithm in base 2 of `x`.
pub fun log2( x : ddouble ) : ddouble
  ln(x) / dd-ln2

// `x` to the power of `y` both as `:ddouble`
pub fun pow( x : ddouble, y : ddouble ) : ddouble
  exp(y * ln(x))


// Return `ln(1.0 + x)`.
// Avoids potential imprecision for small `x` where adding `1.0` explicitly
// may lead to rounding errors.
pub fun ln1p( x : ddouble ) : ddouble
  if x.is-posinf then x else
    val y = one + x
    val z = y - one
    if z.is-zero then x else
      ln(y) * (x / z)


// Return `exp(x - 1.0)`.
// Avoids rounding errors for values of `x` very close to `1.0`.
pub fun expm1( x : ddouble ) : ddouble
  if x.is-posinf then x else
    val y = exp(x)
    if y==one then x else
      val ym = y - one
      if ym == ~one then ~one else ym * (x / ln(y))


fun log2p1( x : ddouble ) : ddouble
  dd-log2e * ln1p(x)

fun exp2m1( x : ddouble ) : ddouble
  expm1(dd-ln2 * x)

// Returns `ln(exp(x) + exp(y))`.
// Avoids overlow/underflow errors.
pub fun lnaddexp( x : ddouble, y : ddouble ) : ddouble
  if x==y then x + dd-ln2 else
    val z = x - y
    if z.is-pos then x + ln1p(exp(~z))
                else y + ln1p(exp(z))


// Returns `log2( exp2(x) + exp2(y) )`.
// Avoids overlow/underflow errors.
pub fun logaddexp2( x : ddouble, y : ddouble ) : ddouble
  if x==y then x + one else
    val z = x - y
    if z.is-pos then x + log2p1(exp2(~z))
                else y + log2p1(exp2(z))


// Return if two `:ddouble`s are nearly equal with respect to some `epsilon` (=`8*dd-epsilon`).
// The epsilon is the nearest difference for numbers around 1.0. The routine automatically
// scales the epsilon for larger and smaller numbers, and for numbers close to zero.
pub fun nearly-eq( x : ddouble, y : ddouble, epsilon : ddouble = dd-epsilon8 ) : bool 
  if x == y return True
  val diff = (x - y).abs
  if x.is-zero || y.is-zero || diff < dd-min then
    // very close to zero, scale the epsilon for subnormal numbers
    (two*diff < (epsilon * dd-min))
  else 
    val sum = x.abs + y.abs
    ((two*diff / (if sum > dd-max then dd-max else sum)) < epsilon)

// Return if two `:ddouble`s are nearly equal with respect to an `epsilon` of `10*dd-epsilon`.
// See also `nearly-eq` which takes an explicit `epsilon`.
pub fun (~=)(x : ddouble, y : ddouble ) : bool
  nearly-eq(x,y)

/*------------------------------------------------------
  Summing
------------------------------------------------------*/

// Return the sum of a list of doubles.
// Uses [Kahan-Babu&scaron;kan-Neumaier summation](https://en.wikipedia.org/wiki/Kahan_summation_algorithm#Further_enhancements)
// to minimize rounding errors. This
// is more precise as Kahan summation and about as fast.\
// `[1.0e3,1.0e97,1.0e3,-1.0e97].sum == 2000.0`\
// A. Neumaier, _Rundungsfehleranalyse einiger Verfahren zur Summation endlicher Summen_.
// Math. Mechanik, 54:39--51, 1974.
pub fun sum( xs : list<ddouble> ) : ddouble
  var total := zero
  var comp  := zero
  xs.foreach fn(x)
    val t = total + x
    val c = if total.abs >= x.abs then (total - t) + x else (x - t) + total
    comp  := comp + c
    total := t
  total + comp;

// The hypotenuse of `x` and `y`: `sqrt(x*x + y*y)`.
// Prevents overflow for large numbers.
pub fun hypot( x : ddouble, y : ddouble ) : ddouble
  val xx = abs(x)
  val yy = abs(y)
  val lo = min(xx,yy)
  val hi = max(xx,yy)
  if hi.is-zero then zero else
    val z  = lo / hi
    hi * sqrt( one + z*z )


// The square root of the sum of the squares of three doubles.
// Prevents overflow for large numbers.
pub fun hypot( x : ddouble, y : ddouble, z : ddouble ) : ddouble
  val xx = abs(x)
  val yy = abs(y)
  val zz = abs(z)
  val hi = max(max(xx,yy),zz)
  if hi.is-zero then zero else
    hi * sqrt( sqr(xx / hi) + sqr(yy / hi) + sqr(zz / hi) )


// The square root of the sum of squares of a list of doubles.
// Prevents overflow for large numbers and uses Kahan-Babu&scaron;kan-Neumaier summation
// for precision.
pub fun hypot( xs : list<ddouble> ) : ddouble
  val hi = xs.abs-max
  if hi.is-zero then zero else
    hi * xs.map(fn(x){ sqr(x / hi) }).sum.sqrt


// The maximum of the absolute values.
fun abs-max( x : ddouble, y : ddouble ) : ddouble
  max(abs(x),abs(y))

// The maximum of a list of absolute values.
fun abs-max( xs : list<ddouble> ) : ddouble
  xs.foldr(zero,fn(m, x){ max(abs(x),m) })

/*------------------------------------------------------
  Trigonometry
------------------------------------------------------*/

// The sine function of a given angle in radians.
pub fun sin( rad : ddouble ) : ddouble
  rad.sincos.fst

// The cosine function of a given angle in radians.
pub fun cos( rad : ddouble ) : ddouble
  rad.sincos.snd

// The tangent of a given angle in radians.
pub fun tan( rad : ddouble ) : ddouble
  val (s,c) = rad.sincos
  s / c

// Calculate sine and cosine on an angle in radians.
pub fun sincos( rad : ddouble ) : (ddouble,ddouble)
  // quick approximation for small values
  if rad.hi.abs < 1.0e-11 then
    // return (rad, one - 0.5.ddouble*sqr(rad))
    val s = rad * (one - (rad.sqr / 3.ddouble))
    val c = (one - s.sqr).sqrt
    return (s,c)

  /*
  To compute sin(x), we choose integers a, b so that
       rad = s + a * (pi/2) + b * (pi/16)
  and |s| <= pi/32.  A precomputed sin(pi/16) and
  Chebyshev series, we can compute very precisely.
  */
  // find a, b, and s.
  /*
  val z  = (rad / dd-twopi).round
  val r  = rad - (dd-twopi * z)

  val qa : float64 = floor( (r.hi / dd-pi2.hi) + 0.5)
  val a  = qa.truncate.int
  val t  = r - (dd-pi2 * qa.ddouble)
  val qb : float64 = floor((t.hi / dd-pi16.hi) + 0.5)
  val b  = qb.truncate.int

  val s  = t - (dd-pi16 * qb.ddouble)
  */

  val x1 = rad / dd-twopi
  val x3 = x1 - x1.round
  // s = x - a*pi/2 - b*pi/16
  val x32 = x3 + x3
  val x34 = x32 + x32 // 4.ddouble * x3
  val a   = x34.int
  val b   = (8.ddouble * (x34 - a.ddouble)).int
  val s   = dd-pi * (x32 - ((8*a + b).ddouble / 16.ddouble))
  val s2 = s.sqr

  // Use the Chebyshev series for best precision.
  val sins = s * ch-factors.foldl(zero) fn(acc,f) { f + acc*s2 }
  val coss = (one - sins.sqr).sqrt

  // sinb = sin(b*pi/16)
  val sinb = if b >= 0 then sin16(b) else ~sin16(~b)
  val cosb = sin16(8 - b.abs)

  if a==0 then (sins*cosb + coss*sinb, coss*cosb - sins*sinb)
  elif a==1 then (coss*cosb - sins*sinb, ~coss*sinb - sins*cosb)
  elif a== -1 then (sins*sinb -  coss*cosb, coss*sinb + sins*cosb)
  else (~sins*cosb - coss*sinb, sins*sinb - coss*cosb) // |a| == 2

val ch-factors : list<ddouble> = [
  Ddouble( 1.6056491947130061858e-10, 6.1925234565562595936e-27 ),
  Ddouble( -2.5052108052208301379e-8, -3.6598195022865791876e-25 ),
  Ddouble( 0.0000027557319223964441402, -2.0315661398415506513e-22 ),
  Ddouble( -0.00019841269841269841253, 6.8577289081075077176e-21 ),
  Ddouble( 0.0083333333333333332177, 1.1563735775184918100e-19 ),
  Ddouble( -0.16666666666666665741, -9.2518585321663028924e-18 ),
  Ddouble( 1.0000000000000000000, -6.0239567712403467977e-31 )
]

// Return sin(i*pi/16) for 0 <= i <= 8
fun sin16( i : int ) : ddouble
  sin16-table.at(i).default(dd-nan)

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

// Return `x` with the sign of `y`.
pub fun with-sign-of( x : ddouble, y : ddouble ) : ddouble
  if y.is-neg then ~(x.abs) else x.abs

// The arc-sine of `x`. Returns the angle in radians.
pub fun asin( x : ddouble ) : ddouble
  val a = x.abs
  if a > one then dd-nan
  elif a == one then dd-pi2.with-sign-of(x)
  else atan2( x, sqrt(one - x.sqr) )

// The arc-cosine of `x`. Returns the angle in radians.
pub fun acos( x : ddouble ) : ddouble
  val a = x.abs
  if a > one then dd-nan
  elif a == one then (if x.is-pos then zero else dd-pi)
  else atan2( sqrt(one - x.sqr), x )

// The arc-tangent of `x`. Returns the angle in radians.
pub fun atan( x : ddouble ) : ddouble
  atan2(x,one)

// The arc-tangent of a point (`x`,`y`). Returns the angle with respect to the x-axis in radians between -&pi; and &pi;.
pub fun atan2( y : ddouble, x : ddouble ) : ddouble 
  if x.is-zero then
    if y.is-zero then zero /* dd-nan */ else dd-pi2.with-sign-of(y)
  elif y.is-zero then 
    if x.is-pos then zero else dd-pi
  elif x == y then 
    if y.is-pos then dd-pi4 else ~dd-pi34
  elif x == ~y then 
    if y.is-pos then dd-pi34 else ~dd-pi4
  else 
    val r = sqrt( x.sqr + y.sqr )
    val xr = x / r
    val yr = y / r
    val z = atan2( y.float64, x.float64 ).ddouble // approximation
    val (sinz,cosz) = sincos(z)
    if xr.hi.abs > yr.abs.hi 
     then z + ((yr - sinz) / cosz)
     else z - ((xr - cosz) / sinz)
  

// The hyperbolic sine of `x`.
pub fun sinh( x : ddouble ) : ddouble
  if x.is-zero then zero
  elif x.abs.float64 > 0.05 then
    val ex = x.exp
    if !ex.is-finite then ex else ( ex - (one / ex)).half
  else
    // small x so use Taylor series to avoid cancellation
    val x2 = x.sqr
    x*(one + (x2/6.ddouble)*(one + (x2/20.ddouble)*(one + (x2/42.ddouble))))


// The hyperbolic cosine of `x`.
pub fun cosh( x : ddouble ) : ddouble
  if x.is-zero then one
  elif x.abs.float64 > 0.05 then
    val ex = x.exp
    if !ex.is-finite then ex else (ex + (one / ex)).half

  else
    val s = x.sinh
    sqrt( one + s.sqr )


// The hyperbolic tangent of `x`.
pub fun tanh( x : ddouble ) : ddouble
  if x.is-zero then zero
  elif x.abs.float64 > 0.05 then
    val ex = x.exp
    val iex = one / ex
    if ex.is-zero then ~one
    elif ex.is-posinf then one
    else ((ex - iex) / (ex + iex))

  else
    val s = x.sinh
    val c = sqrt( one + s.sqr )
    return (s / c)


// The area hyperbolic sine of `x`.
pub fun asinh( x : ddouble ) : ddouble
  ln( x + sqrt(x.sqr + one))

// The area hyperbolic cosine of `x`.
pub fun acosh( x : ddouble ) : ddouble
  if x < one then dd-nan else ln(x + sqrt(x.sqr - one))

// The area hyperbolic tangent of `x`.
pub fun atanh( x : ddouble ) : ddouble
  if x.abs > one then dd-nan
   else ln((one + x) / (one - x)).half

