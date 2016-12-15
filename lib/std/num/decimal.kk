/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

// Operations on arbitrary precision decimal numbers.
//
// Note: this module is work in progress. 
module std/num/decimal

import std/text/regex

// Type of a decimal number.
public struct decimal (
  whole: int,
  frac : int,
  prec : int                        
)

// The decimal zero.
val zero : decimal = Decimal(0,0,0)

// Create a decimal number with maximal precision from a 
// whole part `i` and fractional part `frac`. The result is `i + frac`&times;10^-`prec`^.
// i.e. `prec` determines how far to shift the fraction. By default `prec` is negative which
// means it will take the width of `frac` in digits as its precision.\
// `decimal(1,123,4).show == "1.0123"`\
// `decimal(1,123).show == "1.123"`\
// `decimal(123).show == "123"`
public fun decimal( i : int, frac : int = 0, prec : int = ~1 ) : decimal {
  if (frac.zero? || prec.zero?) then return Decimal(i,0,0)
  val fprec = max(frac.count-digits,prec)
  if (frac.neg?) then {
    val base = pow10(fprec)
    Decimal(i.dec, base + frac, fprec)
  }
  else {
    Decimal(i, frac, fprec)
  }
}

// Create a decimal from a `:double` with a specified maximal precision (=`16`).
public fun decimal( d : double, max-prec : int = 16 ) : decimal {
  val fl = d.floor
  val fr = d - fl
  if (fr.zero?) then return decimal(fl.int) 
  val p = fr.show-fixed(max-prec).count - 2
  decimal(fl.int, (fr * pow10(p.double)).int, p)
}

// Create a decimal from an integer by specifying how far to shift the dot (`prec`)\
// `decimal-at(123).show == "123"`\
// `decimal-at(123,2).show == "1.23"`
public fun decimal-at( i : int, prec : int = 0 ) : decimal {
  if (prec.zero? || i.zero?) then return Decimal(i,0,0)
  val q = i.cdiv-pow10(prec)
  val r = i - q.mul-pow10(prec)
  decimal(q,r,prec)
}

fun expand( x : decimal, p : int ) : decimal {
  if (x.prec >= p) then x else {
    trace("expand: " + x.show + " to prec " + p.show)
    Decimal(x.whole, x.frac.mul-pow10(p - x.prec), p)
  }
}

fun expand-int( x : decimal, p : int ) : int {
  if (p.zero?) then return x.whole
  val xx = x.expand(p)
  xx.whole.mul-pow10(p) + xx.frac
}

fun reduce( x : decimal ) : decimal {
  if (x.prec.zero?) then return x
  if (x.frac.zero?) then return Decimal(x.whole,0,0)
  val z = x.frac.pow10?
  val p = x.prec - z
  trace("reduce " + x.show + " to prec " + p.show)
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
  if (f.count-digits > p) then {
    val base = pow10(p)
    Decimal(x.whole + y.whole + 1, f - base, p).reduce
  }
  else Decimal(x.whole + y.whole, f, p).reduce
}

// Negate a decimal.
public fun (~)( x : decimal ) : decimal {
  decimal(~x.whole,~x.frac,x.prec)
}

// Subtract two decimals.
public fun (-)( x : decimal, y : decimal ) : decimal {
  x + (~y)
}

// Multiply two decimals up to a given precision `max-prec` (=`~1`).
// Use a negative precision to compute to the current maximum precision of either argument.
public fun mul( x : decimal, y : decimal, max-prec : int = ~1 ) : decimal {
  if (x.zero? || y.zero?) then decimal(0) else {
    val p  = max(x.prec,y.prec)
    val xi = x.expand-int(p)
    val yi = y.expand-int(p)
    trace("mul: " + xi.show  + " * " + yi.show)
    val z  = decimal-at( xi*yi, 2*p ).reduce      // TODO limit precision
    z.reduce
  }  
}

// Multiply two decimals up to a given precision `max-prec` (=`~1`).
// Use a negative precision to compute to the current maximum precision of either argument.
public fun (*)( x : decimal, y : decimal, max-prec : int = ~1 ) : decimal {
  mul(x,y,max-prec)
}

// Divide two decimals up to a given precision `max-prec` (=`~1`).
// Use a negative precision to compute to the precision of the first argument.
public fun div( x : decimal, y : decimal, max-prec : int = ~1 ) : decimal {
  if (x.zero? || y.zero?) then return zero
  val p  = max(x.prec,y.prec)
  val xi = x.expand-int(p)
  val yi = y.expand-int(p)
  val mp = if (max-prec >= 0) then max-prec else x.prec
  val dp = 2*mp + 1
  val q  = xi.mul-pow10(dp) / yi
  decimal-at(q,dp).round-to-prec(mp)
}

// Divide two decimals up to a given precision `max-prec` (=`~1`).
// Use a negative precision to compute to the precision of the first argument.
public fun (/)( x : decimal, y : decimal, max-prec : int = ~1 ) : decimal {
  div(x,y,max-prec)
}

// Rounding modes.
type round {
  Even
  Floor
  Ceil
  Up
  Down
}

// Round a decimal point number to an integer
public fun int( x : decimal, rnd : round = Even ) : int {
  match(rnd) {
    Even -> {
      val half= x.prec.pow10 / 2
      val q   = x.whole
      match(compare(x.frac, half)) {
          Eq -> if (q.even?) then q else q.inc
          Gt -> q.inc
          Lt -> q
      }
    }
    Floor -> 
      x.whole
    Ceil -> 
      if (x.frac.zero?) then x.whole else x.whole.inc
    Up -> 
      if (x.frac.zero? || x.whole.neg?) then x.whole 
      else x.whole.inc
    Down -> 
      if (x.frac.zero? || x.whole.pos?) then x.whole 
      else x.whole.inc
  }
}

// Round the decimal-point number `x` to a specified precision `p` with a possible rounding mode `rnd` (=`Even`).
public fun round-to-prec( x : decimal, p : int, rnd : round = Even) : decimal {
  if (p.neg? || x.prec <= p) then x else {
    val d   = x.prec - p 
    val q   = x.frac.cdiv-pow10(d)  
    val r   = x.frac - q.mul-pow10(d)
    val qr  = Decimal(q,r,d).int(rnd)
    val res = Decimal(x.whole, qr, p)
    // trace("round-to-prec: " + p.show + ", d: " + d.show +  ", q: " + q.show + ", x: " + x.fix.show + ", res: " + res.fix.show )// q1: " + q1.show + ", r:" + r.show + ", half:" + half.show)                  
    res
  }
}

// Show a decimal to a specified maximum precision (=`~1`). use a negative value to 
// use maximum precision required.
public fun show( f : decimal, max-prec : int = ~1 ) : string {
  trace("show: " + f.whole.show + "," + f.frac.show + " prec:" + f.prec.show)
  val fp      = f.round-to-prec(max-prec)
  val (tr,fr) = fp.truncfrac
  val sign    = if (f.neg?) then "-" else ""
  sign + tr.abs.show + "." + fr.abs.show.pad-left(fp.prec,'0')
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

fun truncfrac( f : decimal ) : (int,int) {
  if (f.neg? && f.frac.pos?) then {
    val base = pow10(f.prec)
    (f.whole.inc, f.frac - base)
  }
  else (f.whole,f.frac)
}

