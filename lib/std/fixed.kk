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
  if (f.prec <= 0) then return f.fix.show

  val s     = f.fix.abs.show.pad-left(f.prec,'0')
  val frac  = s.last(f.prec)
  val whole = frac.before.string
  (if (f.fix.neg?) then "-" else "") +
   (if (whole.empty?) then "0" else whole) + 
     "." + frac.string // + " (prec=" + f.prec.show + ", fix=" + f1.fix.show + ")"
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
val rx-fixed = regex(@"^([\-\+]?\d+)(?:\.(\d+))?$")

// Create a new fixed-point number from an integer `i` putting the decimal point at `prec` (=`0`) digits from the right.
fun fixed( i : int, prec : int = 0) : fixed {
  if (prec <= 0) then Fixed(i,0) else {
    Fixed(i,prec).reduce
  }
}

// Create a new fixed-point number from a double `d` with a maximum precision of `max-prec` (=`20`) digits.
fun fixed( d : double, max-prec : int = 20 ) : fixed {
  if (d.fraction==0.0) then return Fixed(d.int,0)
  val s = d.show-fixed(max-prec)
  match(s.find(rx-fraction)) {
    Nothing   -> Fixed(d.int,0)
    Just(cap) -> {
      val frac = cap.groups[1]
      val p    = frac.count
      Fixed( d.trunc.int * pow10(p) + frac.parse-int-default(0), p)
    }
  }
}
val rx-fraction = regex(@"\.(\d*[1-9])0*$")

// Possibly reduce a fixed-point number to use less space while maintaining precision.
fun reduce( x : fixed ) : fixed {
  if (!x.prec.pos?) return x
  val z = x.fix.pow10?
  if (z.pos? && z <= x.prec) then {
    Fixed( x.fix / pow10(z), x.prec - z)
  }
  else x
}

// Expand the precision of a fixed-point number `x` to at least `p`.
private fun expand( x : fixed, p : int ) : fixed {
  if (x.prec >= p) then x else {
    Fixed( x.fix * pow10(p - x.prec), p )
  }
}

// Return the number of ending `0` digits of `i`. Return `0` when `i==0`.
fun pow10?( i : int ) : int {
  if (i.zero?) then return 0
  match(i.show.find(rx-endzeros)) {
    Nothing   -> 0
    Just(cap) -> cap.groups[1].count
  }
}
val rx-endzeros = regex(@"[1-9](0+)$")

// Count the number of decimal digits in `i`.
private fun count-digits( i : int ) : int {
  if (i.zero?) then 0 else i.abs.show.count
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
  val p  = max(x.prec,y.prec)
  val xp = x.expand(p)
  val yp = y.expand(p)
  Fixed(xp.fix + yp.fix, p)
}

// Multiply two fixed-point numbers with an optional maximum precision `max-prec` (=`~1`). 
// If the maximum precision is negative, the multiplication will be precise.
fun (*)( x : fixed, y : fixed, max-prec : int = ~1 )  : fixed {
  val p  = max(x.prec,y.prec)
  val xp = x.expand(p)
  val yp = y.expand(p)
  val z  = Fixed(xp.fix * yp.fix, 2*p).reduce
  if (max-prec>=0) then z.round-to-prec(max-prec) else z
}

// Euclidean division and modulus up to a specified maximum precision (=`20` digits).
fun divmod( x : fixed, y : fixed, max-prec : int = 20) : (fixed,fixed) {
  val p  = max(x.prec,y.prec)
  val xp = x.expand(p)
  val yp = y.expand(p)
  val dp = if (max-prec >= 0) then max-prec else 2*(p+1)
  val (q,r) = divmod(xp.fix * pow10(dp), yp.fix)
  (Fixed(q,dp).reduce, Fixed(r,dp + p).reduce)
}

fun (/)( x : fixed, y : fixed, max-prec : int = 20 ) : fixed {
  divmod(x,y,max-prec).fst
}

fun (%)( x : fixed, y : fixed, max-prec : int = 20 ) : fixed {
  divmod(x,y,max-prec).snd
}

// Round the fixed-point number `x` to a specified precision `p`.
fun round-to-prec( x : fixed, p : int ) : fixed {
  if (p < 0 || x.prec <= p) then x else {
    val d = x.prec - p 
    val d10 = pow10(d)
    val (q,r) = divmod(x.fix,d10)
    val q1  = match(compare(r,d10/2)) {
                Eq -> if (q.even?) then q else q.inc
                Lt -> q
                Gt -> q.inc
            }
    Fixed(q1, p)
  }
}

// The zero fixed-point number.
val zero = Fixed(0,0)

fun zero?( x : fixed ): bool  {x.fix.zero?}
fun pos?( x : fixed ) : bool  {x.fix.pos?}
fun neg?( x : fixed ) : bool  {x.fix.neg?}

// Compare two fixed-point numbers.
fun compare( x : fixed, y : fixed ) : order {
  val p = max(x.prec,y.prec)
  compare(x.expand(p).fix, y.expand(p).fix)
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