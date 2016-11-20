/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

// Operations on arbitrary precision decimal-point numbers.
public module std/num/decimal

import std/regex

// Type of a decimal number 
public struct decimal (
  whole: int,
  frac : int,
  prec : int                        
)


val zero : decimal = Decimal(0,0,0)

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

public fun decimal( d : double, max-prec : int = 16 ) : decimal {
  val fl = d.floor
  val fr = d - fl
  if (fr.zero?) then return decimal(fl.int) 
  val p = fr.show-fixed(max-prec).count - 2
  decimal(fl.int, (fr * pow10(p.double)).int, p)
}


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

public fun (~)( x : decimal ) : decimal {
  decimal(~x.whole,~x.frac,x.prec)
}

public fun (-)( x : decimal, y : decimal ) : decimal {
  x + (~y)
}

  
public fun mul( x : decimal, y : decimal, max-prec : int = ~1 ) : decimal {
  if (x.zero? || y.zero?) then decimal(0) else {
    val p  = max(x.prec,y.prec)
    val xi = x.expand-int(p)
    val yi = y.expand-int(p)
    trace("mul: " + xi.show  + " * " + yi.show)
    val z  = decimal-at( xi*yi, 2*p ).reduce
    z.reduce
  }  
}

public fun (*)( x : decimal, y : decimal, max-prec : int = ~1 ) : decimal {
  mul(x,y,max-prec)
}

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

// Round the decimal-point number `x` to a specified precision `p`.
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

public fun show( f : decimal, max-prec : int = ~1 ) : string {
  trace("show: " + f.whole.show + "," + f.frac.show + " prec:" + f.prec.show)
  val fp      = f.round-to-prec(max-prec)
  val (tr,fr) = fp.truncfrac
  val sign    = if (f.neg?) then "-" else ""
  sign + tr.abs.show + "." + fr.abs.show.pad-left(fp.prec,'0')
}

public fun neg?( f : decimal ) : bool {
  f.whole.neg?
}

public fun pos?( f : decimal ) : bool {
  f.whole.pos? || (f.whole.zero? && f.frac.pos?)
}

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

/*
// Convert a `:decimal` to a string
fun show( f0 : decimal, max-prec : int = ~1 ) : string { 
  val f1 = f0.reduce
  val f  = if (max-prec >= 0) then f1.round-to-prec(max-prec) else f1
  if (!f.prec.pos?) then return f.fix.show

  val s     = f.fix.abs.show.pad-left(f.prec,'0')
  val frac  = s.last(f.prec)
  val whole = frac.before.string
  (if (f.fix.neg?) then "-" else "") +
   (if (whole.empty?) then "0" else whole) + 
     "." + frac.string  // + " (prec=" + f.prec.show + ")"
}

// Parse a decimal-point number.
fun parse-decimal( s : string ) : maybe<decimal> {
  match(s.find(rx-decimal)) {
    Nothing -> Nothing
    Just(cap) -> {
      val whole = cap.groups[1]
      val frac  = cap.groups[2]
      val i = (whole + frac).parse-int-default(0)
      Just(decimal(i,frac.count))
    }
  }
}
private val rx-decimal = regex(@"^([\-\+]?\d+)(?:\.(\d+))?$")


// Create a new decimal-point number from an integer `i` putting the decimal point at `prec` (=`0`) digits from the right.
fun decimal( i : int, prec : int = 0) : decimal {
  if (prec <= 0) then Decimal(i,0) else {
    Decimal(i,prec).reduce
  }
}

// Create a new decimal-point number from an integer `i` and a fraction as a double, with
// at most `max-prec` precision.
fun decimal-frac( i : int, frac : double, max-prec : int = 20 ) : decimal {
  if (frac.zero?) then Decimal(i,0) else Decimal(i,0) + decimal(frac,max-prec)
}

// Create a new decimal-point number from a double `d` with a maximum precision of `max-prec` (=`20`) digits.
fun decimal( d : double, max-prec : int = 20 ) : decimal {
  if (d.zero?) then return zero
  if (d.fraction.zero?) then return Decimal(d.int,0)
  val s = d.show-decimal(max-prec)
  match(s.find(rx-fraction)) {
    Nothing   -> Decimal(d.int,0)
    Just(cap) -> {
      val frac = cap.groups[1]
      val p    = frac.count
      val f    = frac.parse-int-default(0)
      Decimal( d.trunc.int * pow10(p) + (if (d.neg?) then ~f else f), p)
    }
  }
}
val rx-fraction = regex(@"\.(\d*[1-9])0*$")

// Convert to a double. Returns `nan` for out-of-bounds numbers.
public fun double( x : decimal ) : double {
  x.show.parse-double.default(nan)
}

// Possibly reduce a decimal-point number to use less space while maintaining precision.
fun reduce( x : decimal ) : decimal {
  if (x.zero?) return zero
  if (!x.prec.pos?) return x
  val z = x.fix.pow10?
  if (z.pos? && z <= x.prec) then {
    //trace("reduce: " + x.fix.show + ", prec: " + x.prec.show + ", z:" + z.show + ", r: " + x.fix.cdiv-pow10(6).show)
    Decimal( x.fix.cdiv-pow10(z), x.prec - z)
  }
  else x
}

// Expand the precision of a decimal-point number `x` to at least `p`.
private fun expand( x : decimal, p : int ) : decimal {
  if (x.prec >= p) then x else {
    Decimal( x.fix.mul-pow10(p - x.prec), p )
  }
}




// Negate a decimal-point number.
fun (~)(x : decimal ) : decimal {
  Decimal( ~x.fix, x.prec )
}

// Subtract two decimal-point numbers.
fun (-)( x : decimal, y : decimal ) : decimal {
  x + ~y
}

// Add two decimal-point numbers.
fun (+)( x : decimal, y : decimal ) : decimal {
  if (x.zero?) then y
  elif (y.zero?) then x 
  else {
    val p  = max(x.prec,y.prec)
    val xp = x.expand(p)
    val yp = y.expand(p)
    Decimal(xp.fix + yp.fix, p)
  }
}

// Multiply two decimal-point numbers with an optional maximum precision `max-prec` (=`~1`). 
// If the maximum precision is negative, the multiplication will be precise.
fun (*)( x : decimal, y : decimal, max-prec : int = ~1 )  : decimal {
  mul(x,y,max-prec)
}

// Multiply two decimal-point numbers with an optional maximum precision `max-prec` (=`~1`). 
// If the maximum precision is negative, the multiplication will be precise.
fun mul( x : decimal, y : decimal, max-prec : int = ~1 )  : decimal {
  if (x.zero? || y.zero?) then decimal(0) else {
    val p  = max(x.prec,y.prec)
    val xp = x.expand(p)
    val yp = y.expand(p)
    val z  = Decimal(xp.fix * yp.fix, 2*p).reduce
    if (max-prec>=0) then z.round-to-prec(max-prec) else z.round-to-prec(p)
  }
}

// Euclidean division and modulus up to a specified maximum precision (=`20` digits).
fun divmod( x : decimal, y : decimal, max-prec : int = 20) : (decimal,decimal) {
  if (x.zero? || y.zero?) then (zero,zero)
  else {
    val p  = max(x.prec,y.prec)
    val xp = x.expand(p)
    val yp = y.expand(p)
    val dp = if (max-prec >= 0) then max-prec else 2*p
    val q  = xp.fix.mul-pow10(dp) / yp.fix
    val qq = Decimal(q,dp).reduce
    val rr = xp - mul(qq,yp)
    (qq,rr.reduce)
  }
}

fun show( t : (decimal,decimal) ) : string {
  t.fst.show + ", " + t.snd.show
}

fun div( x : decimal, y : decimal, max-prec : int = 20 ) : decimal {
  divmod(x,y,max-prec).fst
}

fun (/)( x : decimal, y : decimal, max-prec : int = 20 ) : decimal {
  div(x,y,max-prec)
}


fun mod( x : decimal, y : decimal, max-prec : int = 20 ) : decimal {
  divmod(x,y,max-prec).snd
}

fun (%)( x : decimal, y : decimal, max-prec : int = 20 ) : decimal {
  mod(x,y,max-prec)
}

// Round the decimal-point number `x` to a specified precision `p`.
fun round-to-prec( x : decimal, p : int ) : decimal {
  if (p < 0 || x.prec <= p) then x else {
    val d   = x.prec - p 
    val f   = x.fix.abs
    val q   = f.cdiv-pow10(d)  
    val r   = f - q.mul-pow10(d)
    val half= d.pow10 / 2
    val q1  = match(compare(r, half)) {
                Eq -> if (q.even?) then q else q.inc
                Gt -> q.inc
                Lt -> q
            }
    val res = Decimal(q1, p)
    // trace("round-to-prec: " + p.show + ", d: " + d.show +  ", q: " + q.show + ", x: " + x.fix.show + ", res: " + res.fix.show )// q1: " + q1.show + ", r:" + r.show + ", half:" + half.show)                  
    if (x.neg?) then ~res else res
  }
}

// Convert a decimal-point number to an integer by rounding.
fun int( f : decimal ) : int {
  f.round.fix
}

// Round a decimal-point number to the nearest integer.
fun round( f : decimal ) : decimal {
  f.round-to-prec(0)
}

// Truncate a decimal-point number to a certain precision.
fun trunc-to-prec( x : decimal, p : int ) : decimal {
  if (x.zero?) return zero
  if (x.prec.neg?) return x
  val z = x.prec - p
  if (z.pos? && z <= x.prec) then {
    Decimal( x.fix.cdiv-pow10(z), x.prec - z)
  }
  else x
}

// Return the fraction of a decimal-point number from a specified precision.
fun fraction-to-prec( f : decimal, p : int ) : decimal {
  f - f.trunc-to-prec(f.prec - p)
}

// Truncate a decimal-point number.
fun trunc( f : decimal ) : decimal {
  f.trunc-to-prec(0)
}

// Floor a decimal point number
fun floor( f : decimal ) : decimal {
  val ft = f.trunc
  val fr = f - ft
  if (f.neg? && !fr.zero?) then ft.dec else ft
}

// Return the fraction of a decimal-point number.
fun fraction( f : decimal ) : decimal {
  f.fraction-to-prec(f.prec)
}

// The zero decimal-point number.
val zero : decimal = Decimal(0,0)

fun zero?( x : decimal ): bool  {x.fix.zero?}
fun pos?( x : decimal ) : bool  {x.fix.pos?}
fun neg?( x : decimal ) : bool  {x.fix.neg?}

// Compare two decimal-point numbers.
fun compare( x : decimal, y : decimal ) : order {
  // cheaper to first reduce the terms to the minimum precision
  val p = min(x.prec,y.prec)
  val rx = x.trunc-to-prec(p)
  val ry = y.trunc-to-prec(p)
  match(compare(rx.fix,ry.fix)) {
    Eq -> {
      // only if equal, we need to expand the other to do a full comparison
      val q = max(x.prec,y.prec)
      compare(x.expand(p).fix, y.expand(p).fix)
    }
    ord -> ord
  }
}

fun (>) (x : decimal, y : decimal) : bool { compare(x,y) == Gt }
fun (>=)(x : decimal, y : decimal) : bool { compare(x,y) != Lt }
fun (==)(x : decimal, y : decimal) : bool { compare(x,y) == Eq }
fun (!=)(x : decimal, y : decimal) : bool { compare(x,y) != Eq }
fun (<) (x : decimal, y : decimal) : bool { compare(x,y) == Lt }
fun (<=)(x : decimal, y : decimal) : bool { compare(x,y) != Gt }

// The sign of a decimal point number.
fun sign( x : decimal ) : order  -> x.fix.sign

// Increment a decimal point number.
fun inc( x : decimal ) : decimal  -> x + decimal(1)

// Decrement a decimal point number
fun dec( x : decimal ) : decimal  -> x - decimal(1)

// The absolute value of a decimal-point number
fun abs( x : decimal ) : decimal  -> Decimal(x.fix.abs,x.prec)

// The minimum of `x` and `y`.
fun min( x : decimal, y : decimal ) : decimal {
  if (x <= y) then x else y
}

// The maximum of `x` and `y`
fun max( x : decimal, y : decimal ) : decimal {
  if (x >= y) then x else y
}
*/