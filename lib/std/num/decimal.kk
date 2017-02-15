/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

// Operations on arbitrary precision decimal numbers.
module std/num/decimal

import std/text/regex

// Type of a decimal number.
public struct decimal (
  whole: int,
  frac : int,  // always >= 0
  prec : int   // always >= 0                     
)

// The decimal zero.
public val zero : decimal = Decimal(0,0,0)


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
  if (frac.zero? || prec.zero?) then return Decimal(i,0,0)
  val ds = frac.count-digits
  val fprec = max(ds,prec)
  if (frac.neg?) then {
    val base = pow10(fprec)
    Decimal(i.dec, base + frac, fprec)  
  }
  else {
    Decimal(i, frac, fprec)
  }
}


// Create a decimal from a `:double` with a specified maximal precision (=`16` which is the maximal precision of a `:double`).
public fun decimal( d : double, max-prec : int = 16 ) : decimal {
  val fl = d.floor
  val fr = d - fl
  if (fr.zero?) then return decimal(fl.int) 
  val p = fr.show-fixed(max-prec).count - 2
  decimal(fl.int, (fr * pow10(p.double)).int, p)
}

// Create a decimal from an integer `i` by specifying the exponent `exp` (=`0`) such 
// that the resulting decimal equals `i`&times;10^^`exp`^^\
// `decimal-exp(123).show == "123"`\
// `decimal-exp(123,-2).show == "1.23"`\
// `decimal-exp(123,2).show == "12300"`\
public fun decimal-exp( i : int, exp : int = 0 ) : decimal {
  decimal-at(i,~exp)
}

// Create a decimal from an integer `i` by specifying the number of digits right of the dot, eg. the precision `prec` (=`0`).
// The resulting decimal has the value `i`&times;10^^-`prec`^^.\
// `decimal-at(123).show == "123"`\
// `decimal-at(123,2).show == "1.23"`
// `decimal-at(123,~2).show == "12300"`
public fun decimal-at( i : int, prec : int = 0 ) : decimal {
  if (prec.zero? || i.zero?) then {
    Decimal(i,0,0)
  }
  elif (prec.neg?) {
    decimal(i.mul-pow10(~prec))
  }
  else {
    val q = i.cdiv-pow10(prec)
    val r = i - q.mul-pow10(prec)
    decimal(q,r,prec)
  }
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

// Ensure a decimal has at least `p` precision.
fun expand( x : decimal, p : int ) : decimal {
  if (x.prec >= p) then x else {
    //trace("expand: " + x.show + " to prec " + p.show)
    Decimal(x.whole, x.frac.mul-pow10(p - x.prec), p)
  }
}

// Return an int `i` such that `x * pow10(p) == i`. Only works for `p` where `p <= x.prec`!
fun unsafe-expand-int( x : decimal, p : int ) : int {
  if (p.zero?) then return x.whole
  val xx = x.expand(p)
  xx.whole.mul-pow10(p) + xx.frac
}


fun reduce( x : decimal ) : total decimal {
  if (x.prec.zero?) then return x
  if (x.frac.zero?) then return Decimal(x.whole,0,0)
  val z = x.frac.pow10?
  val p = x.prec - z
  //trace("reduce " + x.show + " to prec " + p.show)
  if (!p.pos?) then return Decimal(x.whole,0,0) 
  val factor = pow10(z)
  Decimal(x.whole, x.frac / factor, p)
}

// Add two decimals.
public fun (+)( x : decimal, y : decimal ) : decimal {
  val p  = max(x.prec,y.prec)
  if (p.zero?) then return decimal(x.whole + y.whole)
  val xx = x.expand(p)
  val yy = y.expand(p)
  val f  = xx.frac + yy.frac
  if (f.count-digits > p) then {  // overflow, add the carry
    val base = pow10(p)
    Decimal(x.whole + y.whole + 1, f - base, p)
  }
  else Decimal(x.whole + y.whole, f, p)
}

// Negate a decimal.
public fun (~)( x : decimal ) : decimal {
  decimal(~x.whole,~x.frac,x.prec)
}

// Subtract two decimals.
public fun (-)( x : decimal, y : decimal ) : decimal {
  x + (~y)
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

// Round a `:decimal` number to an integer with `Even` rounding.
public fun int( x : decimal) : int {
  x.round(Even)
}

// Round a `:decimal` number to an integer with an optional rounding mode (=`Even`).
public fun round( x : decimal, rnd : round = Even ) : total int {
  val q = x.whole
  if (x.prec.zero? || x.frac.zero?) return q

  fun round-half( keep-on-eq : bool ) {
    val half = x.prec.pow10 / 2
    match(compare(x.frac,half)) {
      Eq -> if (keep-on-eq) then q else q.inc
      Gt -> q.inc
      Lt -> q
    }
  }

  match(rnd) {
    Even     -> round-half(q.even?)
    HalfUp   -> round-half(q.neg?)
    HalfDown -> round-half(q.pos?)
    Floor    -> q
    Ceil     -> q.inc
    Up       -> if (q.neg?) then q else q.inc
    Down     -> if (q.pos?) then q else q.inc
  }
}

// Round the decimal-point number `x` to a specified precision `p` with a possible rounding mode `rnd` (=`Even`).
public fun round-to-prec( x : decimal, p : int, rnd : round = Even) : total decimal {
  if (p.neg? || x.prec <= p || x.frac.zero?) return x 
  val d   = x.prec - p 
  val q   = x.frac.cdiv-pow10(d)  
  val r   = x.frac - q.mul-pow10(d)
  val y   = x.whole.mul-pow10(d)
  val z   = Decimal( y + q,r,d).round(rnd)
  val zq  = z.cdiv-pow10(d)
  val zr  = z - zq.mul-pow10(d)
  val res = Decimal(zq, zr, p)
  // trace("round-to-prec: " + p.show + ", d: " + d.show +  ", q: " + q.show + ", x: " + xr.fix.show + ", res: " + res.fix.show )// q1: " + q1.show + ", r:" + r.show + ", half:" + half.show)                  
  res
}


// Multiply two decimals up to a given precision `max-prec` (=`~1`).
// Use a negative precision to compute to the current maximum precision of either argument.
public fun mul( x : decimal, y : decimal, max-prec : int = ~1, rnd : round = Even ) : decimal {
  if (x.zero? || y.zero?) then decimal(0) else {
    val p  = max(x.prec,y.prec)
    val xi = x.unsafe-expand-int(p)
    val yi = y.unsafe-expand-int(p)
    //trace("mul: " + xi.show  + " * " + yi.show)
    val z  = decimal-at( xi*yi, 2*p ).reduce
    z.round-to-prec( if (max-prec.neg?) then p else max-prec, rnd )
  }  
}

// Multiply two decimals using the maximum precision of either argument and using `Even` rounding.
public fun (*)( x : decimal, y : decimal ) : decimal {
  mul(x,y)
}

// Divide two decimals up to a given precision `max-prec` (=`~1`) and rounding mode `rnd` (=`Even`).
// Use a negative precision to compute to the maximum precision either argument.
public fun div( x : decimal, y : decimal, max-prec : int = ~1, rnd : round = Even ) : decimal {
  if (x.zero? || y.zero?) then return zero
  val p  = max(x.prec,y.prec)
  val xi = x.unsafe-expand-int(p)
  val yi = y.unsafe-expand-int(p)
  val mp = if (max-prec.neg?) then p else max-prec
  val dp = 2*mp + 1
  val q  = xi.mul-pow10(dp) / yi
  decimal-at(q,dp).round-to-prec(mp,rnd)
}

// Divide two decimals up to the maximum precision of either argument using `Even` rounding.
public fun (/)( x : decimal, y : decimal ) : decimal {
  div(x,y)
}



// Show a decimal to a specified maximum precision (=`~1`). use a negative value to 
// use maximum precision required.
public fun show( f : decimal, max-prec : int = ~1 ) : string {
  //trace("show: " + f.whole.show + "," + f.frac.show + " prec:" + f.prec.show)
  val fp      = f.reduce.round-to-prec(max-prec)
  val (tr,fr) = fp.truncfrac
  val sign    = if (f.neg?) then "-" else ""
  sign + tr.abs.show + (if (fr.zero?) then "" else "." + fr.abs.show.pad-left(fp.prec,'0'))
}

fun truncfrac( f : decimal ) : (int,int) {
  if (f.neg? && f.frac.pos?) then {
    val base = pow10(f.prec)
    (f.whole.inc, f.frac - base)
  }
  else (f.whole,f.frac)
}


// Is this a negative decimal?
public fun neg?( f : decimal ) : bool {
  f.whole.neg?
}

// Is this a positive decimal?
public fun pos?( f : decimal ) : bool {
  f.whole.pos? || (f.whole.zero? && f.frac.pos?)
}

// Is this a zero?
public fun zero?( f : decimal ) : bool {
  f.whole.zero? && f.frac.zero?
}

// Compare decimals.
public fun compare( x : decimal, y : decimal ) : order {
  match(compare(x.whole,y.whole)) {
    Eq  -> {
      val p = max(x.prec,y.prec)
      val xx = x.expand(p)
      val yy = y.expand(p)
      compare(xx.frac,yy.frac)
    }
    ord -> ord
  }
}


public fun (>) (x : decimal, y : decimal) : bool { compare(x,y) == Gt }
public fun (>=)(x : decimal, y : decimal) : bool { compare(x,y) != Lt }
public fun (==)(x : decimal, y : decimal) : bool { compare(x,y) == Eq }
public fun (!=)(x : decimal, y : decimal) : bool { compare(x,y) != Eq }
public fun (<) (x : decimal, y : decimal) : bool { compare(x,y) == Lt }
public fun (<=)(x : decimal, y : decimal) : bool { compare(x,y) != Gt }

// The sign of a decimal number.
public fun sign( x : decimal ) : order {
  if (x.neg?) then Lt
  elif (x.zero?) then Eq
  else Gt
}

val one = decimal(1)
val minus-one = decimal(~1)

// Increment a decimal number.
public fun inc( f : decimal ) : decimal  -> f + one

// Decrement a decimal point number
public fun dec( f : decimal ) : decimal  -> f + minus-one

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

// Convert a decimal to a `:double`. This may lose precision.
public fun double( x : decimal ) : double {
  if (x.frac.zero? || x.prec.zero?) return x.whole.double
  x.whole.double + (x.frac.double * pow10(~x.prec.double))
}

// Round a `:decimal` to an integer using `Floor` (i.e. round to minus infinity)
public fun floor( x : decimal ) : int {
  x.round(Floor)
}

// Return the 'floored' fraction, always in the range [`0`,`1.0`] (can be `1.0` on precision loss):\
// `x.floor.fixed + x.ffraction == x` 
public fun ffraction( x : decimal ) : double {
  if (x.frac.zero? || x.prec.zero?) then 0.0 else x.frac.double * pow10(~x.prec.double)
}

// Truncate a `:decimal` to an integer using `Down` (i.e. round to zero)
public fun trunc( x : decimal ) : int {
  x.round(Down)
}

// Return the 'truncated' fraction, always in the range [`~1.0`,`1.0`] (can reach extremes on precision loss):\
// `x.trunc.fixed + x.fraction == x`
public fun fraction( x : decimal ) : double {
  if (x.whole.neg? && !x.frac.zero?) then x.ffraction - 1.0 else x.ffraction
}
