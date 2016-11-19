/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

// Operations on 'fixed'ed point numbers. 
// These have infinite range and a precision of up to 15 digits after the dot.
// Internally they are represented by a infinite range `:int` for the whole
// part and a `:double` to represent the fraction.
public module std/num/fixed

import std/regex

// Type of a fixed.
abstract struct fixed (
  public floor     : int,
  public ffraction : double  // 'floored fraction':  0 <= ffraction < 1.0                      
)

val zero : fixed = fixed(0,0.0)


public fun fixed( d : double ) : fixed {
  fixed(0,d)
}

public fun fixed( i : int, frac : double = 0.0 ) : fixed {
  if (0.0 <= frac && frac < 1.0) then return Fixed(i,frac)
  if (frac.nan?) then return Fixed(i,0.0)
  val ff = frac.floor
  Fixed( i + ff.int, frac - ff )
}

public fun double( f : fixed ) : double {
  f.floor.double + f.ffraction
}

public fun int( f : fixed ) : int {
  f.round.floor
}

public fun trunc( f : fixed ) : int {
  if (f.floor.neg? && f.ffraction != 0.0) then f.floor + 1 else f.floor
}

public fun fraction( f : fixed ) : double {
  if (f.floor.neg? && f.ffraction != 0.0) then f.ffraction - 1.0 else f.ffraction
}

public fun round-to-prec( f : fixed, prec : int ) : fixed {
  if (prec.pos?) then fixed(f.floor, f.ffraction.round-to-prec(prec))
                 else fixed(f.floor + f.ffraction.int, 0.0)
}

public fun round( f : fixed ) : fixed {
  f.round-to-prec(0)
}

public fun show( f : fixed, prec : int = ~1 ) : string {
  trace("show: " + f.floor.show + " + " + f.ffraction.show-fixed)
  if (prec==0) then return f.int.show
  val sign  = if (f.neg?) then "-" else ""
  val fprec = if (prec.neg?) then 15 else min(prec,15)
  val frac  = f.fraction.abs.show-fixed(fprec).first(2).after.string
  val frac0 = if (prec.neg?) then frac else frac.first(prec).string.pad-right(prec,'0') 
  sign + f.trunc.abs.show + "." + frac0
}

public fun (~)( f : fixed ) : fixed {
  fixed( ~f.floor, ~f.ffraction )
}

// The absolute value of a fixed number
public fun abs( f : fixed ) : fixed {
  if (f.neg?) then ~f else f
}

public fun (+)( q1 : fixed, q2 : fixed ) : fixed {
  fixed(q1.floor + q2.floor, q1.ffraction + q2.ffraction )
}

public fun (-)( q1 : fixed, q2 : fixed ) : fixed {
  fixed(q1.floor - q2.floor, q1.ffraction - q2.ffraction )
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

public fun (*)( q1 : fixed, q2 : fixed ) : fixed {
  // (t1 + f1)*(t2 + f2) == t1*t2 + t1*f2 + f1*t2 + f1*f2
  Fixed(q1.floor*q2.floor,0.0) 
    + fmul(q1.floor, q2.ffraction)
      + fmul(q2.floor, q1.ffraction)
        + Fixed(0,q1.ffraction*q2.ffraction)
}

public fun (/)( q1 : fixed, q2 : fixed ) : fixed {
  // (t1 + f1)/(t2 + f2) 
  //  == t1/(t2 + f2) + f1/(t2 + f2)
  if (q1.zero? || q2.zero?) then return zero
  val d = q2.double // approximate up to 15 digits
  if (q2.ffraction.zero? || !q2.floor.precise?) then { // ignore the fraction
    // (t1 + f1) / t2 == t1/t2 + f1/t2
    val (q,r) = divmod(q1.floor,q2.floor) // full int division
    fixed(q, (r.double / d) + (q1.ffraction / d))
  }
  else {
    ddiv(q1.floor, d) + fixed(q1.ffraction / d)
  }
}

public fun neg?( f : fixed ) : bool  { f.floor.neg? }
public fun pos?( f : fixed ) : bool  { f.floor.pos? || (f.floor.zero? && f.ffraction.pos?) }
public fun zero?( f : fixed ) : bool { f.floor.zero? && f.ffraction.zero? }

public fun compare( q1 : fixed, q2 : fixed ) : order {
  match(compare(q1.floor,q2.floor)) {
    Eq  -> compare(q1.ffraction,q2.ffraction)
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

public fun  test( op : string, d1 : double, d2 : double ) {
  val (f,g) : ((fixed,fixed) -> fixed, (double,double) -> double) 
    = if   (op=="*") then ((*) : (fixed,fixed) -> fixed,(*) : (double,double) -> double)
      elif (op=="/") then ((/) : (fixed,fixed) -> fixed,(/) : (double,double) -> double)
      else ((+) : (fixed,fixed) -> fixed,(+) : (double,double) -> double)
  println( d1.show + op + d2.show + "\n == " + f(d1.fixed,d2.fixed).show + "\n == " + g(d1,d2).show-fixed() )
}