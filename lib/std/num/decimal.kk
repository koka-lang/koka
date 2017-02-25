/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

// Operations on arbitrary precision decimal numbers.
module std/num/decimal

import std/text/regex

// Type of a decimal number. Decimals have arbitrary precision and range and 
// do exact decimal arithmetic and are well suited for financial calculations for 
// example. They are also used to represent precise [time](std_time.html).
abstract struct decimal (
  num: int,
  exp: int
)

// The decimal zero.
public val zero : decimal = Decimal(0,0)

// round exponents to specific intervals (7) to avoid too much rescaling
fun round-exp( exp : int ) : int {
  if (exp.zero?) then 0 else 7*(exp / 7)
}

// Create a decimal from an integer `i` with an optional
// exponent `exp` (=`0`) such that the result equals `i`&times;10^`exp`^.
public fun decimal-exp( i : int, exp : int = 0 ) : decimal {
  // use exponents only at specific intervals to avoid too much re-scaling
  val x = round-exp(exp)  // always x <= exp
  val diff = exp - x
  //trace("decimal-exp: " + i.show + "x" + exp.show + ", diff: " + diff.show + ", x: " + x.show)
  if (diff.zero?) then Decimal(i,exp) else Decimal(i.mul-pow10(diff),x)
}

// Create a decimal number with precision `prec` (=`~1`) from a 
// whole part `i` and fractional part `frac`. The result is `i + frac`&times;10^-`prec`^.
// i.e. `prec` determines how many digits there are behind the dot.
// `decimal(1,123,4).show == "1.0123"`\
// `decimal(1,123).show == "1.123"`\
// `decimal(123).show == "123"`\
// If `prec` is negative or smaller than the number of digits in `frac`, 
// the result will take the width of `frac` in digits as its precision.\
// `decimal(1,123,2).show == "1.123"`
public fun decimal( i : int, frac : int = 0, prec : int = ~1 ) : decimal {
  if (frac.zero? || prec.zero?) then return Decimal(i,0)
  val ds    = frac.count-digits
  val fprec = max(ds,prec) // always > 0
  decimal-exp( i.mul-pow10(fprec) + frac, ~fprec )
}


/* Create a decimal from a `:double` with a specified maximal precision (=`16` which is the maximal precision of a `:double`).
Note: creating a `:decimal` from a `:double` may loose precision and give surprising results as many decimal 
fractions cannot be represented precisely by a `:double`. Use `decimal-exp` or `parse-decimal` when exact representations
are required. For example:
```
> decimal(1.1)
1.1000000000000001
> decimal(1.2)
1.2
```
*/
public fun decimal( d : double, max-prec : int = 16 ) : decimal {
  // todo: optimize 
  val fl = d.floor
  val fr = d - fl
  if (fr.zero?) then return decimal(fl.int) 
  val p = fr.show-fixed(max-prec).count - 2
  decimal(fl.int, (fr * pow10(p.double)).int, p)
}


// Parse a `:decimal` number where a dot (`.`) separates the factional part.
public fun parse-decimal( s : string ) : maybe<decimal> {
  match(s.find(rx-decimal)) {
    Nothing -> Nothing
    Just(cap) -> {
      val sign  = cap.groups[1]
      val whole = cap.groups[2]
      val frac  = cap.groups[3] 
      val fracx = cap.groups[4]
      val prec  = frac.count  // can be zero
      val f = decimal( whole.parse-int-default(0), fracx.parse-int-default(0), prec )
      if (sign=="-") then Just(~f) else Just(f)
    }
  }
}
val rx-decimal = regex(@"^([\-\+]?)(\d+)(?:\.(0*([1-9]\d*))?)$")


// Ensure a decimal `x` has an exponent such that `x.exp <= e`.
fun expand( x : decimal, e : int ) : decimal {
  if (x.exp <= e) then x else {
    //trace("expand: " + x.show + " to exp " + p.show)
    decimal-exp( x.num.mul-pow10( x.exp - e ), e )
  }
}

// Choose an exponent that minimizes memory usage.
public fun reduce( x : decimal ) : decimal {
  val p   = x.num.pow10?
  if (!p.pos?) return x
  val expp= x.exp + p
  // trace("reduce: x:" + x.showx + ", p:" + p.show + ", expp:" + expp.show)
  if (round-exp(expp)==x.exp) then x else decimal-exp(x.num.cdiv-pow10(p), expp)
}

// Add two decimals.
public fun (+)( x : decimal, y : decimal ) : decimal {
  val e  = min(x.exp,y.exp)
  val xx = x.expand(e)
  val yy = y.expand(e)
  Decimal(xx.num + yy.num, e)
}

// Negate a decimal.
public fun (~)( x : decimal ) : decimal {
  Decimal(~x.num, x.exp)
}

// Subtract two decimals.
public fun (-)( x : decimal, y : decimal ) : decimal {
  x + (~y)
}

// Increment a decimal
public fun inc( x : decimal ) : decimal {
  Decimal(x.num.inc, x.exp)
}

// Decrement a decimal
public fun dec( x : decimal ) : decimal {
  Decimal(x.num.dec, x.exp)
}

// Is this an even decimal?
public fun even?(x : decimal ) : bool {
  x.num.even?
}

// Is this an odd decimal?
public fun odd?( x :decimal ) : bool {
  x.num.odd?
}


// Is this decimal zero?
public fun zero?( x : decimal ) : bool {
  x.num.zero? 
}

// Is the decimal positive?
public fun pos?( x : decimal ) : bool {
  x.num.pos?
}

// Is the decimal negative?
public fun neg?( x : decimal ) : bool {
  x.num.neg?
}

// Show a decimal to a specified maximum precision (=`~1`). use a negative value to 
// use maximum precision required.
public fun show( x : decimal, max-prec : int = ~1 ) : string {
  // trace("show: " + x.num.show + "x" + x.exp.show)
  val cx   = if (max-prec.neg?) then x.reduce else x.round-to-prec(max-prec)
  cx.showx()
}

public fun showx( cx : decimal ) : string {
  //trace("showx: " + cx.num.show + "x" + cx.exp.show)
  val sign = if (cx.num.neg?) then "-" else ""
  val s    = cx.num.abs.show
  if (!cx.exp.neg?) then {
    sign + s + repeat("0",cx.exp)
  }
  else {
    val prec  = ~cx.exp
    val sfrac = s.last(prec)
    val frac  = (repeat("0", prec - sfrac.count ) + sfrac.string).trim-right("0")
    val whole = sfrac.before.string
    sign + (if (whole.empty?) then "0" else "") + whole 
          + (if (frac.empty?) then "" else "." + frac)
  }
}

// Multiply two decimals with full precision.
public fun (*)( x : decimal, y : decimal ) : decimal {
  val z = decimal-exp(x.num * y.num, x.exp + y.exp)
  if (z.exp.neg?) then z.reduce else z
}

// Rounding modes.
public type round {
  Even
  HalfUp
  HalfDown
  Floor
  Ceil
  Up
  Down
}

// Round a `:decimal` number to an integer an optional rounding mode `rnd` (=`Even`).
public fun int( x : decimal, rnd : round = Even) : int {
  val y = x.round(rnd)  // y.exp >= 0
  if (y.exp.pos?) then y.num.mul-pow10(y.exp) else y.num
}

// Round a `:decimal` number to a whole number with an optional rounding mode (=`Even`).
public fun round( x : decimal, rnd : round = Even ) : decimal {
  x.round-to-prec(0,rnd)
}

// Round the decimal-point number `x` to 
// to a specified number of digits behind the dot `prec` (=`0`) with an optional
// rounding mode `rnd` (=`Even`). The precision can be negative.\
// `decimal(1,485).round-to-prec(2).show == "1.48"` \
// `decimal(112,49).round-to-prec(~1).show == "110"`
public fun round-to-prec( x : decimal, prec : int = 0, rnd : round = Even ) : total decimal {
  if (x.exp >= ~prec) return x
  val cx = x.reduce
  val p  = ~cx.exp - prec
  if (!p.pos?) return cx  // already less than prec precision

  val (q,r) = cx.num.divmod-pow10(p)
    
  fun round-half( keep-on-eq : bool ) {
    val half = p.pow10 / 2
    match(compare(r,half)) {
      Eq -> if (keep-on-eq) then q else q.inc
      Gt -> q.inc
      Lt -> q
    }
  }

  val q1 = match(rnd) {
    Even     -> round-half(q.even?)
    HalfUp   -> round-half(q.neg?)
    HalfDown -> round-half(q.pos?)
    Floor    -> q
    Ceil     -> q.inc
    Up       -> if (q.neg?) then q else q.inc
    Down     -> if (q.pos?) then q else q.inc
  }

  //trace("round: " + x.showx + ", q: " + q.show + ", r: " + r.show + ", q1: " + q1.show + "e-" + prec.show )
  decimal-exp(q1,~prec)
}

// Optimize: Use double division when within precision bounds.
val maxexp     : int = 308 
val maxpd      : double = 1.0e15
val maxprecise : int = int(maxpd)
val minprecise : int = ~maxprecise

fun precise?(i : int ) : bool {
  (i > minprecise && i < maxprecise)
}


/* Divide two decimals with a given extra precision `min-prec` (=`15`).
The `min-prec` is the number of extra digits used to calculate inexact 
divisions.

Note: the division uses up to `min-prec` precision using `Floor` rounding
for the last digit if the result is  inexact. To round differently, you can
for example divide with larger precision and use `round-to-prec`. 
```
> div( decimal(2), decimal(3), 0 )
0
> div( decimal(2), decimal(3), 1 )
0.6
> div( decimal(2), decimal(3) )  default precision is 15
0.6666666666666666
> div( decimal(2), decimal(3) ).round-to-prec(6)
0.666667
```
*/
public fun div( x : decimal, y : decimal, min-prec : int = 15 ) : decimal {
  if (x.zero? || y.zero?) then return zero
  val e = x.exp - y.exp
  // use double division if within precision bounds..
  if (min-prec <= 15 && e.abs <= 15 && x.num.precise? && y.num.precise?) {
    val r = x.num.double / y.num.double
    if (e.zero?) then decimal(r) else decimal(r * pow10(e.double))
  }  
  else {
    // otherwise use long integer division
    val mp = max(min-prec,0)
    val z = decimal-exp( x.num.mul-pow10(mp) / y.num, e - mp ).reduce
    // trace("div: " + x.show + " / " + y.show + " = " + z.show)
    z
  }
}

// Divide two decimals using 15 digits of extra precision.
public fun (/)( x : decimal, y : decimal ) : decimal {
  div(x,y)
}


// Compare decimals.
public fun compare( x : decimal, y : decimal ) : order {
  // todo: optimize further to avoid allocations.
  val e  = min(x.exp,y.exp)
  val xx = x.expand(e)
  val yy = y.expand(e)
  compare(xx.num,yy.num)
}


public fun (>) (x : decimal, y : decimal) : bool { compare(x,y) == Gt }
public fun (>=)(x : decimal, y : decimal) : bool { compare(x,y) != Lt }
public fun (==)(x : decimal, y : decimal) : bool { compare(x,y) == Eq }
public fun (!=)(x : decimal, y : decimal) : bool { compare(x,y) != Eq }
public fun (<) (x : decimal, y : decimal) : bool { compare(x,y) == Lt }
public fun (<=)(x : decimal, y : decimal) : bool { compare(x,y) != Gt }

// The sign of a decimal number.
public fun sign( x : decimal ) : order {
  x.num.sign
}

// The minimum of `x` and `y`.
public fun min( x : decimal, y : decimal ) : decimal {
  if (x <= y) then x else y
}

// The maximum of `x` and `y`
public fun max( x : decimal, y : decimal ) : decimal {
  if (x >= y) then x else y
}

// Take the sum of a list of decimal numbers (0 for the empty list).
public fun sum( ds : list<decimal> ) : decimal {
  ds.foldr(zero,(+))
}

// The absolute value of a decimal
public fun abs( x : decimal ) : decimal {
  if (x.neg?) then ~x else x
}


// Round a `:decimal` to an integer using `Floor` (i.e. round to minus infinity)
public fun floor( x : decimal ) : int {
  x.int(Floor)
}


// Truncate a `:decimal` to an integer using `Down` (i.e. round to zero)
public fun trunc( x : decimal ) : int {
  x.int(Down)
}


// Convert a decimal to a `:double`. This may lose precision.
public fun double( x : decimal ) : double {
  // todo: handle decimals with many digits better
  if (!x.exp.neg?) {
    x.num.double * pow(10.0,x.exp.double)
  }
  else {
    val (q,r) = x.num.divmod-pow10(~x.exp)
    q.double + r.double * pow(10.0,x.exp.double)
  }
}

// Return the 'floored' fraction, always in the range [`0`,`1.0`] (can be `1.0` on precision loss):\
// `x.floor.decimal + x.ffraction.decimal == x` 
public fun ffraction( x : decimal ) : double {
  if (!x.exp.neg?) return 0.0
  val (_,r) = x.num.divmod-pow10(~x.exp)
  r.double * pow(10.0,x.exp.double)
  //if (x.frac.zero? || x.prec.zero?) then 0.0 else x.frac.double * pow10(~x.prec.double)
}

// Return the 'truncated' fraction, always in the range [`~1.0`,`1.0`] (can reach extremes on precision loss):\
// `x.trunc.decimal + x.fraction.decimal == x`
public fun fraction( x : decimal ) : double {
  val f = x.ffraction
  if (x.neg? && !f.zero?) then f - 1.0 else f
}
