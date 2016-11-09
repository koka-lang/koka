/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

// Operations on arbitrary precision fixed numbers 
public module std/fixed

import std/regex

// Type of a fixed number 
abstract struct fixed (
  fix  : int,
  prec : int                        
)

// Convert a `:fixed` to a string
fun show( f0 : fixed, max-prec : int = ~1 ) : string { 
  val f = f0.reduce
  if (f.prec <= 0) then return f.fix.show

  val s    = f.fix.abs.show.pad-left(f.prec,'0')
  val frac = s.last(f.prec)
  val p    = if (max-prec >= 0) then min(max-prec,f.prec) else f.prec
  val whole = frac.before.string
  (if (f.fix.neg?) then "-" else "") +
   (if (whole.empty?) then "0" else whole) + 
     "." + frac.extend(p - f.prec).string + " (prec=" + f.prec.show + ", fix=" + f0.fix.show + ")"
}

fun fixed( whole : int, prec : int = 0) : fixed {
  if (prec <= 0) then Fixed(whole,0) else {
    Fixed(whole,prec).reduce
  }
}

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
val rx-fraction = regex(@"\.(\d+[1-9])0*$")

fun reduce( x : fixed ) : fixed {
  if (!x.prec.pos?) return x
  val z = x.fix.pow10?
  if (z.pos? && z < x.prec) then {
    Fixed( x.fix / pow10(z), x.prec - z)
  }
  else x
}


private fun expand( x : fixed, p : int ) : fixed {
  if (x.prec >= p) then x else {
    Fixed( x.fix * pow10(p - x.prec), p )
  }
}


private fun pow10?( i : int ) : int {
  if (i.zero?) then return 0
  match(i.show.find(rx-endzeros)) {
    Nothing   -> 0
    Just(cap) -> cap.groups[1].count
  }
}
val rx-endzeros = regex(@"[1-9](0+)$")


private fun count-digits( i : int ) : int {
  if (i.zero?) then 0 else i.abs.show.count
}



fun (+)( x : fixed, y : fixed ) : fixed {
  match-prec(x,y,(+))
}

fun (-)( x : fixed, y : fixed ) : fixed {
  match-prec(x,y,(-))
}

fun (~)(x : fixed ) : fixed {
  Fixed( ~x.fix, x.prec )
}

private fun match-prec( x : fixed, y : fixed, f : (int,int) -> int ) : fixed {
  val p  = max(x.prec,y.prec)
  val xp = x.expand(p)
  val yp = y.expand(p)
  Fixed(f(xp.fix,yp.fix),p)
}

fun (*)( x : fixed, y : fixed, max-prec : int = ~1 )  : fixed {
  val p  = max(x.prec,y.prec)
  val xp = x.expand(p)
  val yp = y.expand(p)
  val z  = Fixed(xp.fix * yp.fix, 2*p).reduce
  if (max-prec>=0) then z.round-to-prec(max-prec) else z
}

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


/* 


// Convert an integer to an `:int32`. The number is _clamped_ to the maximal or minimum `:int32`
// value if it is outside the range of an `:int32`.
extern inline int32  : (int) -> int32   { 
  cs "Primitive.IntToInt32"
  js "$std_core._int_to_int32" 
}

extern inline int32 : (double) -> int32 {
  cs "Primitive.DoubleToInt32"
  js inline "(#1|0)"
}

// Convert an `:int32` to an `:int`.
extern inline int( i : int32 ) : int { 
  cs inline "(new BigInteger(#1))"
  js "$std_core._int_double" 
}

// Convert an `:int32` to a boolean.
function bool( i : int32 ) : bool {
  (i!=zero)
}

// Convert a boolean to an `:int32`.
function int32( b : bool ) : int32 {
  if (b) then one else zero
}


// Show an `:int32` in hexadecimal notation
// The `width`  parameter specifies how wide the hex value is where `'0'`  is used to align.\
// The `use-capitals` parameter (= `True`) determines if captical letters should be used to display the hexadecimal digits.\
// The `pre` (=`"0x"`) is an optional prefix for the number (goes between the sign and the number).
fun show-hex( i : int32, width : int = 1, use-capitals : bool = True, pre : string = "0x" ) : string {
  std/core/show-hex(i.int,width,use-capitals,pre)
}


extern inline (<=) : (int32,int32) -> bool  { inline "(#1 <= #2)"; js inline "(#1 <= #2)" }
extern inline (==) : (int32,int32) -> bool  { inline "(#1 == #2)"; js inline "(#1 === #2)" }
extern inline (!=) : (int32,int32) -> bool  { inline "(#1 != #2)"; js inline "(#1 !== #2)" }
extern inline (>=) : (int32,int32) -> bool  { inline "(#1 >= #2)"; js inline "(#1 >= #2)"}
extern inline (<)  : (int32,int32) -> bool  { inline "(#1 < #2)"; js inline "(#1 < #2)" }
extern inline (>)  : (int32,int32) -> bool  { inline "(#1 > #2)"; js inline "(#1 > #2)" }

extern inline (+)  : (int32,int32) -> int32 { inline "(#1 + #2)"; js inline "((#1 + #2)|0)" }
extern inline (-)  : (int32,int32) -> int32 { inline "(#1 - #2)"; js inline "((#1 - #2)|0)" }

fun pos?( i : int32 ) : bool { 
  i>0.int32
}

fun neg?( i : int32 ) : bool { 
  i<0.int32
}

fun zero?( i : int32 ) : bool { 
  i==zero
}

val zero = 0.int32
val one  = 1.int32

fun sign( i : int32 ) : order {
  if (i.pos?) then Gt elif (i.neg?) then Lt else Eq
}

// Returns `true` if the integer `i`  is an odd number.
fun odd?( i : int32 ) : bool {
  return (i&1.int32==1.int32);
}

// Returns `true` if the integer `i`  is an even number.
fun even?( i : int32 ) : bool {
  return (i&1.int32==0.int32);
}

// Increment a 32-bit integer.
fun inc( i : int32 ) : int32 {
  i + 1.int32
}

// Decrement a 32-bit integer.
fun dec( i : int32 ) : int32 {
  i - 1.int32
}

// Multiply two 32-bit integers.
extern inline (*) : (int32,int32) -> int32 { 
  cs inline "#1*#2"; 
  js "$std_core._int32_multiply"
}

fun compare( x : int32, y : int32) : order
{
  if (x < y) then Lt
  elif (x > y) then Gt
  else Eq
}

// Return the absolute value of an integer.
// Raises an exception if the `:int32` is `min-int32`
// (since the negation of `min-int32` equals itself and is still negative)
fun abs( i : int32 ) : exn int32
{
  if (!i.neg?) then i
  elif (i > min-int32.int32) then negate(i)
  else error( "system.core.abs: cannot make min-int32 int32o a positive int32 without overflow" )
}

// Return the absolute value of an integer.
// Returns 0 if the `:int32` is `min-int32`
// (since the negation of `min-int32` equals itself and is still negative)
fun abs0( i : int32 ) : int32
{
  if (!i.neg?) then i
  elif (i > min-int32.int32) then negate(i)
  else 0.int32
}


// Take the bitwise _and_ of two `:int32`s
extern inline (&) : (int32,int32) -> int32 { 
  cs inline "(#1 & #2)";
  js inline "(#1 & #2)"
}
  
// Take the bitwise _or_ of two `:int32`s
extern inline (|) : (int32,int32) -> int32 { 
  cs inline "(#1 | #2)";
  js inline "(#1 | #2)";
}

// Take the bitwise _xor_ of two `:int32`s
extern inline (^) : (int32,int32) -> int32 { 
  cs inline "(#1 ^ #2)";
  js inline "(#1 ^ #2)"
}

// Bitwise _not_ of an `:int32`, i.e. flips all bits.
extern inline not : ( i : int32 ) -> int32  { 
  cs inline "(~#1)";
  js inline "(~#1)"
}

private extern inline prim-shl : (int32,int32) -> int32 { 
  cs inline "(#1 << #2)";
  js inline "(#1 << #2)";
}

private extern inline prim-shr : (int32,int32) -> int32 { 
  cs inline "(Int32)(((UInt32)#1)>>#2)";
  js inline "(#1>>>#2)";
}

private extern inline prim-sar : (int32,int32) -> int32 { 
  cs inline "(#1>>#2)";
  js inline "(#1>>#2)";
}

// Bitwise rotate an `:int32` `n` bits to the left.
// Does not rotate when `n` is negative.
fun rotl( i : int32, n : int ) : int32 {
  if (n.neg?) return i;
  val r = n.int32 & (bits-int32 - 1).int32  // modulo bits in an int32
  (prim-shl(i,r) | prim-shr(i,bits-int32.int32 - r))
}

// Bitwise rotate an `:int32` `n` bits to the right.
// Does not rotate when `n` is negative.
fun rotr( i : int32, n : int ) : int32 {
  if (n.neg?) return i;
  val r = n.int32 & (bits-int32 - 1).int32  // modulo bits in an int32
  (prim-shr(i,r) | prim-shl(i,bits-int32.int32 - r))
}

// Shift an `:int32` `i` to the left by `n` bits.
// Returns `i` when `n` is negative, or `0` when `n >= bitsInt`.
fun shl( i : int32, n : int ) : int32 {
  if (n.neg?) return i
  if (n >= bits-int32) return 0.int32
  prim-shl(i,n.int32)
}

// Logical shift an `:int32` to the right by `n` bits. Shift in zeros from the left.
// Returns `i` when `n` is negative, or `0` when `n >= bitsInt`.
fun shr( i : int32, n : int ) : int32 {
  if (n.neg?) return i;
  if (n >= bits-int32) return 0.int32;
  prim-shr(i,n.int32);
}

// Arithmetic shift an `:int32` to the right by `n` bits. Preserves the sign bit.
// Returns `i` when `n` is negative. When `n >= bitsInt` returns either `-1` when `i<0`, or `0` when `i>=0`.
fun sar( i : int32, n : int ) {
  if (n.neg?) return i;
  if (n >= bits-int32) return (if (i.neg?) then ~1.int32 else 0.int32);
  prim-sar(i,n.int32);
}



// Return the minimum of two integers
fun min( i : int32, j : int32 ) : int32  -> if (i <= j) then i else j
// Return the maximum of two integers
fun max( i : int32, j : int32 ) : int32  -> if (i >= j) then i else j


// Truncated division (as in C). See also `(/):(x : int32, y : int32) -> int32`.
extern inline cdiv : (int32,int32) -> exn int32 { 
  cs inline "(#1 / #2)";
  js "$std_core._int32_cdiv"
}

// Truncated modulus (as in C). See also `(%):(x : int32, y : int32) -> int32`.
extern inline cmod : (int32,int32) -> exn int32 { 
  cs inline "(#1 % #2)";
  js "$std_core._int32_cmod"
}

// Truncated division (as in C). See also `(/):(x : int32, y : int32) -> int32`.
private extern inline unsafe-cdiv : (int32,int32) -> int32 {
  cs inline "(#1 / #2)";
  js inline "((#1/#2)|0)"
}

// Truncated modulus (as in C). See also `(%):(x : int32, y : int32) -> int32`.
private extern inline unsafe-cmod : (int32,int32) -> int32 { 
  cs inline "(#1 % #2)";
  js inline "((#1 % #2)|0)"
}

// Convert an 32-bit integer to a double.
extern inline double : (int32) -> double { cs inline "(double)(#1)"; js inline "(#1)" }

// Negate a 32-bit integer
fun negate( i : int32 ) : int32 {
  return (0.int32 - i)
}

// Negate an 32-bit integer
fun (~)(i : int32) : total int32 {
  return (0.int32 - i)
}

// Euclidean-0 division.
// Euclidean division is defined as: For any `D`  and `d`  where `d!=0` , we have:
//
// 1. `D == d*(D/d) + (D%d)` 
// 2. `D%d`  is always positive where `0 <= D%d < abs(d)` 
//
// Moreover, Euclidean-0 is a total function, for the case where `d==0`  we have
// that `D%0 == D`  and `D/0 == 0` . So property (1) still holds, but not property (2).
// 
// Useful laws that hold for Euclidean-0 division:
//
// * `D/(-d) == -(D/d)` 
// * `D%(-d) == D%d` 
// * `D/(2^n) == sar(D,n)         `  (with `0 <= n <= 31`  and `2^n`  means `2`  to the power of `n` )
// * `D%(2^n) == D & ((2^n) - 1)  `  (with `0 <= n <= 31`  and `2^n`  means `2`  to the power of `n` )
//
// Note that an int32eresting edge case is `minInt / -1` which equals `minInt` since in modulo 32-bit 
// arithmetic `minInt == -1 * minInt == -1 * (minInt / -1) + (minInt % -1)` satisfying property (1).
// Of course `(minInt + 1) / -1` is again positive (namely `maxInt`).  
// 
// See also _Division and modulus for computer scientists, Daan Leijen, 2001_ for further information
// available at: <http://research.microsoft.com/pubs/151917/divmodnote.pdf> .
fun (/)( x : int32, y : int32 ) : int32
{
  if (y == 0.int32) then return 0.int32
  val q = unsafe-cdiv(x,y)
  val r = unsafe-cmod(x,y)
  if (r >= 0.int32) then q
  elif (y > 0.int32) then q - 1.int32
  else q+1.int32
}

// Euclidean-0 modulus. See `(/):(x : int32, y : int32) -> int32` division for more information.
fun (%)( x : int32, y : int32 ) : int32
{
  if (y == 0.int32) then return x
  val r = unsafe-cmod(x,y)
  if (r >= 0.int32) then r
  elif (y > 0.int32) then (r+y)
  else (r - y)
}

fun divmod(x:int32,y:int32) : (int32,int32) {
  if (y.zero?) then return (zero,zero)
  val q = unsafe-cdiv(x,y)
  val r = unsafe-cmod(x,y)
  if (!r.neg?) then (q,r)
  elif (y.pos?) then (q.dec, r+y)
  else (q.inc,r - y)
}

*/