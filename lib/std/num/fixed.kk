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
  // value == trunc + frac * 1.0e15                      
  public trunc: int,
  frac        : double  // 'fraction':  0 <= frac < 1.0                      
)

val scale = 16
val base = 1.0e16

// The `:fixed` point zero.
public val zero : fixed = Fixed(0,0.0)


public fun fixed( i : int, prec : int = 0 ) : fixed {
  val p = max(0,prec)
  if (p.zero?) then Fixed(i,0.0) else {
    val q = cdiv-pow10(i,p)
    val r = i - q.mul-pow10(p)
    val rr = if (p > scale) then r.cdiv-pow10(p - scale) else r.mul-pow10(scale - p)
    Fixed(q,rr.double)
  }
}

// Create a `:fixed` point number from a `:double`.
public fun fixed( d : double ) : fixed {
  if (d.nan? || d.inf?) then zero else {
    val fl = d.trunc
    val fr = ((d - fl)*base).round
    fixed-frac-sign(fl.int,fr)
  }
}

fun fixed-frac-ovf( tr : int, fr : double ) : fixed {
  if (fr >= base) then fixed-frac-sign(tr.inc,fr - base)
  elif (fr <= ~base) then fixed-frac-sign(tr.dec,fr + base)
  else fixed-frac-sign(tr,fr)
}

fun fixed-frac-sign( tr : int, fr : double ) : fixed {
  if (tr.neg? && fr.pos?) then Fixed(tr.inc, fr - base)
  elif (tr.pos? && fr.neg?) then Fixed(tr.dec, base + fr)
  else Fixed(tr,fr)
}


// Parse a `:fixed` point number where a dot (`.`) separates the factional part.
public fun parse-fixed( s : string ) : maybe<fixed> {
  match(s.find(rx-fixed)) {
    Nothing -> Nothing
    Just(cap) -> {
      val sign  = cap.groups[1] 
      val whole = cap.groups[2]
      val frac  = cap.groups[3].pad-right(scale,'0').first(scale).string.trim-left("0") + ".0"
      val f     = Fixed(whole.parse-int-default(0),frac.parse-double-default(0.0))
      if (sign=="-") then Just(~f) else Just(f)
    }
  }
}
val rx-fixed = regex(@"^([\-\+]?)(\d+)(?:\.(\d*))?$")

// Convert a fixed point number to a double. This may lose precision.
public fun double( f : fixed ) : double {
  f.trunc.double + (f.frac / base)
}

// Round a fixed point number to an integer.
public fun int( f : fixed ) : int {
  val base2  = base/2.0
  if (f.frac < ~base2) then f.trunc.dec elif (f.frac > base2) then f.trunc.inc else f.trunc
}

// Floor a fixed point number to an integer (i.e. round to minus infinity)
public fun floor( x : fixed ) : int {
  if (x.trunc.neg? && !x.frac.zero?) then x.trunc.dec else x.trunc
}

// Return the 'truncated' fraction, always in the range <`~1.0`,`1.0`>:\
// `x.trunc.fixed + x.fraction == x`
public fun fraction( x : fixed ) : double {
  x.frac / base
}

// Return the 'floored' fraction, always in the range [`0`,`1.0`>:\
// `x.floor.fixed + x.ffraction == x` 
public fun ffraction( x : fixed ) : double {
  if (x.trunc.neg? && !x.frac.zero?) then x.fraction + 1.0 else x.fraction
}

/*
// Round a fixed point number to a specified precision of digits after the dot.
public fun round-to-prec( f : fixed, prec : int ) : fixed {
  if (prec.pos?) then fixed-frac(f.floor, f.frac.round-to-prec(prec))
                 else fixed-frac(f.floor + f.frac.int, 0.0)
}

// Round a fixed point number to a whole integer.
public fun round( f : fixed ) : int {
  f.round-to-prec(0).floor
}
*/

// Show a fixed point number rounded to a specified maximum precision (`max-prec`, =`15`), 
// showing at most `prec` digits after the dot (=`~1`). Use a negative number to use the 
// minimal digits to show the required precision.
public fun show( f : fixed, prec : int = ~1 ) : string {
  trace("show: " + f.trunc.show + " + " + f.frac.show-fixed)
  if (prec.zero? || f.frac.zero?) then return f.int.show
  val fr = f.frac.abs.show-fixed(scale).last(2).before.string
  val sign = if (f.neg?) then "-" else ""
  sign + f.trunc.abs.show + "." + fr.pad-left(scale,'0').trim-right("0")  
}



// Is a fixed point number negative?
public fun neg?( f : fixed ) : bool  { f.trunc.neg? || f.frac.neg? }

// Is a fixed point number positive?
public fun pos?( f : fixed ) : bool  { f.trunc.pos? || f.frac.pos? }

// Is a fixed point number zero?
public fun zero?( f : fixed ) : bool { f.trunc.zero? && f.frac.zero? }

// Compare two fixed point numbers.
public fun compare( q1 : fixed, q2 : fixed ) : order {
  match(compare(q1.trunc,q2.trunc)) {
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

// Negate a `:fixed` point number.
public fun (~)( f : fixed ) : fixed {
  Fixed(~f.trunc,~f.frac)
}

// The absolute value of a fixed number
public fun abs( f : fixed ) : fixed {
  if (f.neg?) then ~f else f
}

// Add two fixed point numbers.
public fun (+)( x : fixed, y : fixed ) : fixed {
  fixed-frac-ovf(x.trunc + y.trunc, x.frac + y.frac)
}

// Subtract two fixed point numbers.
public fun (-)( x : fixed, y : fixed ) : fixed {
  fixed-frac-ovf(x.trunc - y.trunc, x.frac - y.frac )
}

// Take the sum of a list of fixed point numbers (0 for the empty list).
public fun sum( fs : list<fixed> ) : fixed {
  fs.foldr(zero,(+))
}

fun expand( x : fixed ) : (int, int ) {
  if (x.frac.zero?) return (x.trunc,0)
  val fi = x.frac.int
  val p = fi.pow10?
  val f = fi.cdiv-pow10(p)
  trace("expand: " + x.show + ", f: " + f.show + ", p:" + p.show )
  (x.trunc.mul-pow10(scale - p) + f, scale - p)
}

// Multiply two `:fixed` numbers.
public fun (*)( x : fixed, y : fixed ) : fixed {
  val (xx,px) = x.expand
  val (yy,py) = y.expand
  fixed( xx*yy, px + py )
}

// Divide two `:fixed` numbers.
public fun (/)( x : fixed, y : fixed ) : fixed {
  val (yy,py) = y.expand
  val xx = x.trunc.mul-pow10(scale + py) + x.frac.int.mul-pow10(py)
  fixed( xx / yy, scale )
}


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
