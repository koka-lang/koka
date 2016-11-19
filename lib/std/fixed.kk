/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

// Operations on arbitrary precision fixed-point numbers.
public module std/fixed

import std/regex

// Type of a fixed number 
public struct fixed (
  fix  : int,
  prec : int                        
)

// Convert a `:fixed` to a string
fun show( f0 : fixed, max-prec : int = ~1 ) : string { 
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

// Parse a fixed-point number.
fun parse-fixed( s : string ) : maybe<fixed> {
  match(s.find(rx-fixed)) {
    Nothing -> Nothing
    Just(cap) -> {
      val whole = cap.groups[1]
      val frac  = cap.groups[2]
      val i = (whole + frac).parse-int-default(0)
      Just(fixed(i,frac.count))
    }
  }
}
private val rx-fixed = regex(@"^([\-\+]?\d+)(?:\.(\d+))?$")


// Create a new fixed-point number from an integer `i` putting the decimal point at `prec` (=`0`) digits from the right.
fun fixed( i : int, prec : int = 0) : fixed {
  if (prec <= 0) then Fixed(i,0) else {
    Fixed(i,prec).reduce
  }
}

// Create a new fixed-point number from an integer `i` and a fraction as a double, with
// at most `max-prec` precision.
fun fixed-frac( i : int, frac : double, max-prec : int = 20 ) : fixed {
  if (frac.zero?) then Fixed(i,0) else Fixed(i,0) + fixed(frac,max-prec)
}

// Create a new fixed-point number from a double `d` with a maximum precision of `max-prec` (=`20`) digits.
fun fixed( d : double, max-prec : int = 20 ) : fixed {
  if (d.zero?) then return zero
  if (d.fraction.zero?) then return Fixed(d.int,0)
  val s = d.show-fixed(max-prec)
  match(s.find(rx-fraction)) {
    Nothing   -> Fixed(d.int,0)
    Just(cap) -> {
      val frac = cap.groups[1]
      val p    = frac.count
      val f    = frac.parse-int-default(0)
      Fixed( d.trunc.int * pow10(p) + (if (d.neg?) then ~f else f), p)
    }
  }
}
val rx-fraction = regex(@"\.(\d*[1-9])0*$")

// Convert to a double. Returns `nan` for out-of-bounds numbers.
public fun double( x : fixed ) : double {
  x.show.parse-double.default(nan)
}

// Possibly reduce a fixed-point number to use less space while maintaining precision.
fun reduce( x : fixed ) : fixed {
  if (x.zero?) return zero
  if (!x.prec.pos?) return x
  val z = x.fix.pow10?
  if (z.pos? && z <= x.prec) then {
    //trace("reduce: " + x.fix.show + ", prec: " + x.prec.show + ", z:" + z.show + ", r: " + x.fix.cdiv-pow10(6).show)
    Fixed( x.fix.cdiv-pow10(z), x.prec - z)
  }
  else x
}

// Expand the precision of a fixed-point number `x` to at least `p`.
private fun expand( x : fixed, p : int ) : fixed {
  if (x.prec >= p) then x else {
    Fixed( x.fix.mul-pow10(p - x.prec), p )
  }
}




// Negate a fixed-point number.
fun (~)(x : fixed ) : fixed {
  Fixed( ~x.fix, x.prec )
}

// Subtract two fixed-point numbers.
fun (-)( x : fixed, y : fixed ) : fixed {
  x + ~y
}

// Add two fixed-point numbers.
fun (+)( x : fixed, y : fixed ) : fixed {
  if (x.zero?) then y
  elif (y.zero?) then x 
  else {
    val p  = max(x.prec,y.prec)
    val xp = x.expand(p)
    val yp = y.expand(p)
    Fixed(xp.fix + yp.fix, p)
  }
}

// Multiply two fixed-point numbers with an optional maximum precision `max-prec` (=`~1`). 
// If the maximum precision is negative, the multiplication will be precise.
fun (*)( x : fixed, y : fixed, max-prec : int = ~1 )  : fixed {
  mul(x,y,max-prec)
}

// Multiply two fixed-point numbers with an optional maximum precision `max-prec` (=`~1`). 
// If the maximum precision is negative, the multiplication will be precise.
fun mul( x : fixed, y : fixed, max-prec : int = ~1 )  : fixed {
  if (x.zero? || y.zero?) then fixed(0) else {
    val p  = max(x.prec,y.prec)
    val xp = x.expand(p)
    val yp = y.expand(p)
    val z  = Fixed(xp.fix * yp.fix, 2*p).reduce
    if (max-prec>=0) then z.round-to-prec(max-prec) else z.round-to-prec(p)
  }
}

// Euclidean division and modulus up to a specified maximum precision (=`20` digits).
fun divmod( x : fixed, y : fixed, max-prec : int = 20) : (fixed,fixed) {
  if (x.zero? || y.zero?) then (zero,zero)
  else {
    val p  = max(x.prec,y.prec)
    val xp = x.expand(p)
    val yp = y.expand(p)
    val dp = if (max-prec >= 0) then max-prec else 2*p
    val q  = xp.fix.mul-pow10(dp) / yp.fix
    val qq = Fixed(q,dp).reduce
    val rr = xp - mul(qq,yp)
    (qq,rr.reduce)
  }
}

fun show( t : (fixed,fixed) ) : string {
  t.fst.show + ", " + t.snd.show
}

fun div( x : fixed, y : fixed, max-prec : int = 20 ) : fixed {
  divmod(x,y,max-prec).fst
}

fun (/)( x : fixed, y : fixed, max-prec : int = 20 ) : fixed {
  div(x,y,max-prec)
}


fun mod( x : fixed, y : fixed, max-prec : int = 20 ) : fixed {
  divmod(x,y,max-prec).snd
}

fun (%)( x : fixed, y : fixed, max-prec : int = 20 ) : fixed {
  mod(x,y,max-prec)
}

// Round the fixed-point number `x` to a specified precision `p`.
fun round-to-prec( x : fixed, p : int ) : fixed {
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
    val res = Fixed(q1, p)
    // trace("round-to-prec: " + p.show + ", d: " + d.show +  ", q: " + q.show + ", x: " + x.fix.show + ", res: " + res.fix.show )// q1: " + q1.show + ", r:" + r.show + ", half:" + half.show)                  
    if (x.neg?) then ~res else res
  }
}

// Convert a fixed-point number to an integer by rounding.
fun int( f : fixed ) : int {
  f.round.fix
}

// Round a fixed-point number to the nearest integer.
fun round( f : fixed ) : fixed {
  f.round-to-prec(0)
}

// Truncate a fixed-point number to a certain precision.
fun trunc-to-prec( x : fixed, p : int ) : fixed {
  if (x.zero?) return zero
  if (x.prec.neg?) return x
  val z = x.prec - p
  if (z.pos? && z <= x.prec) then {
    Fixed( x.fix.cdiv-pow10(z), x.prec - z)
  }
  else x
}

// Return the fraction of a fixed-point number from a specified precision.
fun fraction-to-prec( f : fixed, p : int ) : fixed {
  f - f.trunc-to-prec(f.prec - p)
}

// Truncate a fixed-point number.
fun trunc( f : fixed ) : fixed {
  f.trunc-to-prec(0)
}

// Floor a fixed point number
fun floor( f : fixed ) : fixed {
  val ft = f.trunc
  val fr = f - ft
  if (f.neg? && !fr.zero?) then ft.dec else ft
}

// Return the fraction of a fixed-point number.
fun fraction( f : fixed ) : fixed {
  f.fraction-to-prec(f.prec)
}

// The zero fixed-point number.
val zero : fixed = Fixed(0,0)

fun zero?( x : fixed ): bool  {x.fix.zero?}
fun pos?( x : fixed ) : bool  {x.fix.pos?}
fun neg?( x : fixed ) : bool  {x.fix.neg?}

// Compare two fixed-point numbers.
fun compare( x : fixed, y : fixed ) : order {
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

fun (>) (x : fixed, y : fixed) : bool { compare(x,y) == Gt }
fun (>=)(x : fixed, y : fixed) : bool { compare(x,y) != Lt }
fun (==)(x : fixed, y : fixed) : bool { compare(x,y) == Eq }
fun (!=)(x : fixed, y : fixed) : bool { compare(x,y) != Eq }
fun (<) (x : fixed, y : fixed) : bool { compare(x,y) == Lt }
fun (<=)(x : fixed, y : fixed) : bool { compare(x,y) != Gt }

// The sign of a fixed point number.
fun sign( x : fixed ) : order  -> x.fix.sign

// Increment a fixed point number.
fun inc( x : fixed ) : fixed  -> x + fixed(1)

// Decrement a fixed point number
fun dec( x : fixed ) : fixed  -> x - fixed(1)

// The absolute value of a fixed-point number
fun abs( x : fixed ) : fixed  -> Fixed(x.fix.abs,x.prec)

// The minimum of `x` and `y`.
fun min( x : fixed, y : fixed ) : fixed {
  if (x <= y) then x else y
}

// The maximum of `x` and `y`
fun max( x : fixed, y : fixed ) : fixed {
  if (x >= y) then x else y
}