/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

// Operations on 'fixed'ed point numbers. 
//
// These have infinite range and precision for the whole part, and a precision of up to 15 digits after the dot.
// Internally they are represented by a infinite range `:int` for the whole
// part and a `:double` to represent the fraction. 
// A `:fixed` generally has better performance than arbitrary precision [`:decimal`](std_num_decimal.html)s.
module std/num/fixed

import std/text/regex
  
// A `:fixed` has infinite range and a precision of up to 15 digits after the dot.
// Internally they are represented by a infinite range `:int` for the whole
// part and a `:double` to represent the fraction. 
// A `:fixed` generally has better performance than arbitrary precision [`:decimal`](std_num_decimal.html)s.
abstract struct fixed (
  public floor: int,
  frac        : double  // 'floored fraction':  0 <= frac < 1.0                      
)

// The `:fixed` point zero.
public val zero : fixed = fixed(0,0.0)


// Create a `:fixed` point number from a `:double`.
public fun fixed( d : double ) : fixed {
  fixed(0,d)
}

// Create a `:fixed` point number from a whole part `i` and a fractional part `frac`. 
// The fixed point number is equal to `i + frac` and correctly deals with negative numbers.
// This is the most precise way to create a fixed number.
public fun fixed( i : int, frac : double = 0.0 ) : fixed {
  // trace("fixed( " + i.show + ", " + frac.show + ")")
  if (0.0 <= frac && frac < 1.0) then return Fixed(i,frac)
  if (frac.nan?) then return Fixed(i,0.0)
  val ff = frac.floor
  Fixed( i + ff.int, frac - ff )
}

// Parse a `:fixed` point number where a dot (`.`) separates the factional part.
public fun parse-fixed( s : string ) : maybe<fixed> {
  match(s.find(rx-fixed)) {
    Nothing -> Nothing
    Just(cap) -> {
      val sign  = cap.groups[1]
      val whole = cap.groups[2]
      val frac  = cap.groups[3] || "0"
      val f = fixed( whole.parse-int-default(0), ("0." + frac).parse-double-default(0.0) )
      if (sign=="-") then Just(~f) else Just(f)
    }
  }
}
val rx-fixed = regex(@"^([\-\+]?)(\d+)(?:\.(\d*))?$")

// Convert a fixed point number to a double. This may lose precision.
public fun double( f : fixed ) : double {
  f.floor.double + f.frac
}

// Round a fixed point number to an integer.
public fun int( f : fixed ) : int {
  f.round
}

// Truncate a fixed point number to an integer.
public fun trunc( f : fixed ) : int {
  if (f.floor.neg? && f.frac != 0.0) then f.floor + 1 else f.floor
}

// Return the 'truncated' fraction, always in the range <`~1.0`,`1.0`>:\
// `x.trunc.fixed + x.fraction == x`
public fun fraction( f : fixed ) : double {
  if (f.floor.neg? && f.frac != 0.0) then f.frac - 1.0 else f.frac
}

// Return the 'floored' fraction, always in the range [`0`,`1.0`>:\
// `x.floor.fixed + x.ffraction == x` 
public fun ffraction( f : fixed ) : double {
  f.frac
}

// Round a fixed point number to a specified precision of digits after the dot.
public fun round-to-prec( f : fixed, prec : int ) : fixed {
  if (prec.pos?) then fixed(f.floor, f.frac.round-to-prec(prec))
                 else fixed(f.floor + f.frac.int, 0.0)
}

// Round a fixed point number to a whole integer.
public fun round( f : fixed ) : int {
  f.round-to-prec(0).floor
}

// Show a fixed point number rounded to a specified maximum precision (`max-prec`, =`15`), 
// showing at most `prec` digits after the dot (=`~1`). Use a negative number to use the 
// minimal digits to show the required precision.
public fun show( f : fixed, prec : int = ~1, max-prec : int = 15) : string {
  // trace("show: " + f.floor.show + " + " + f.frac.show-fixed)
  if (prec.zero? || f.frac.zero?) then return f.int.show
  val fprec = if (prec.neg?) then max-prec else min(prec,max-prec)
  val r     = f.round-to-prec(fprec)
  val rfrac = r.fraction.abs
  if (rfrac.zero?) then return r.floor.show
  val strf  = rfrac.show-fixed(fprec).first(2).after.string
  val padf  = if (prec.neg?) then strf else strf.first(prec).string.pad-right(prec,'0') 
  val sign  = if (f.neg?) then "-" else ""
  sign + r.trunc.abs.show + "." + padf
}

// Negate a `:fixed` point number.
public fun (~)( f : fixed ) : fixed {
  fixed( ~f.floor, ~f.frac )
}

// The absolute value of a fixed number
public fun abs( f : fixed ) : fixed {
  if (f.neg?) then ~f else f
}

// Add two fixed point numbers.
public fun (+)( q1 : fixed, q2 : fixed ) : fixed {
  fixed(q1.floor + q2.floor, q1.frac + q2.frac )
}

// Subtract two fixed point numbers.
public fun (-)( q1 : fixed, q2 : fixed ) : fixed {
  fixed(q1.floor - q2.floor, q1.frac - q2.frac )
}

// Take the sum of a list of fixed point numbers (0 for the empty list).
public fun sum( fs : list<fixed> ) : fixed {
  fs.foldr(zero,(+))
}

val maxpd      : double = 1.0e15
val maxprecise : int = int(maxpd)
val minprecise : int = ~maxprecise

fun precise?(i : int ) : bool {
  (i > minprecise && i < maxprecise)
}

fun fmul( i : int, frac : double ) : fixed {
  // assert: 0 <= frac < 1.0
  if (i.zero? || frac == 0.0) then return zero 
  if (i.precise?) then {
    fixed( i.double * frac )
  }
  else {
    // we are going to split 
    val (hi,lo) = divmod(i,maxprecise)
    val f  = hi*(frac * maxpd).int  // round?? Yes, because rounds to 15 digits precision
    val ff = lo.double * frac 
    fixed(f,ff) 
  }
}

fun dmul( i : int, d : double ) : fixed {
  // i*d == i*(t + f)  == i*t + i*f 
  if (i.zero? || d.zero?) then return zero
  if (d==1.0) then return i.fixed
  val fl = d.floor
  val fr = d - fl
  fixed( i * fl.int ) + fmul( i, fr )
}

fun ddiv( i : int, d : double ) : fixed {
  // i / d == i * (1/d)
  if (i.zero? || d.inf? || d.zero?) then return zero
  dmul(i, 1.0 / d)
}

// Multiply two fixed point numbers.
public fun (*)( q1 : fixed, q2 : fixed ) : fixed {
  // (t1 + f1)*(t2 + f2) == t1*t2 + t1*f2 + f1*t2 + f1*f2
  Fixed(q1.floor*q2.floor,0.0) 
    + fmul(q1.floor, q2.frac)
      + fmul(q2.floor, q1.frac)
        + Fixed(0,q1.frac*q2.frac)
}

// Divide two fixed point numbers.
public fun (/)( q1 : fixed, q2 : fixed ) : fixed {
  // (t1 + f1)/(t2 + f2) 
  //  == t1/(t2 + f2) + f1/(t2 + f2)
  if (q1.zero? || q2.zero?) then return zero
  val d = q2.double // approximate up to 15 digits
  if (q2.frac.zero? || !q2.floor.precise?) then { // ignore the fraction
    // (t1 + f1) / t2 == t1/t2 + f1/t2
    val (q,r) = divmod(q1.floor,q2.floor) // full int division
    fixed(q, (r.double / d) + (q1.frac / d))
  }
  else {
    ddiv(q1.floor, d) + fixed(q1.frac / d)
  }
}

// Is a fixed point number negative?
public fun neg?( f : fixed ) : bool  { f.floor.neg? }

// Is a fixed point number positive?
public fun pos?( f : fixed ) : bool  { f.floor.pos? || (f.floor.zero? && f.frac.pos?) }

// Is a fixed point number zero?
public fun zero?( f : fixed ) : bool { f.floor.zero? && f.frac.zero? }

// Compare two fixed point numbers.
public fun compare( q1 : fixed, q2 : fixed ) : order {
  match(compare(q1.floor,q2.floor)) {
    Eq  -> compare(q1.frac,q2.frac)
    ord -> ord
  }
}

public fun (>) (x : fixed, y : fixed) : bool { compare(x,y) == Gt }
public fun (>=)(x : fixed, y : fixed) : bool { compare(x,y) != Lt }
public fun (==)(x : fixed, y : fixed) : bool { compare(x,y) == Eq }
public fun (!=)(x : fixed, y : fixed) : bool { compare(x,y) != Eq }
public fun (<) (x : fixed, y : fixed) : bool { compare(x,y) == Lt }
public fun (<=)(x : fixed, y : fixed) : bool { compare(x,y) != Gt }

// The sign of a fixed point number.
public fun sign( x : fixed ) : order {
  if (x.neg?) then Lt
  elif (x.zero?) then Eq
  else Gt
}

// Increment a fixed point number.
public fun inc( f : fixed ) : fixed  -> f + fixed(1)

// Decrement a fixed point number
public fun dec( f : fixed ) : fixed  -> f - fixed(1)

// The minimum of `x` and `y`.
public fun min( x : fixed, y : fixed ) : fixed {
  if (x <= y) then x else y
}

// The maximum of `x` and `y`
public fun max( x : fixed, y : fixed ) : fixed {
  if (x >= y) then x else y
}

fun  test( op : string, d1 : double, d2 : double ) {
  val (f,g) : ((fixed,fixed) -> fixed, (double,double) -> double) 
    = if   (op=="*") then ((*) : (fixed,fixed) -> fixed,(*) : (double,double) -> double)
      elif (op=="/") then ((/) : (fixed,fixed) -> fixed,(/) : (double,double) -> double)
      else ((+) : (fixed,fixed) -> fixed,(+) : (double,double) -> double)
  println( d1.show + op + d2.show + "\n == " + f(d1.fixed,d2.fixed).show + "\n == " + g(d1,d2).show-fixed() )
}