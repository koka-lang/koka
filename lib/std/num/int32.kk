/*---------------------------------------------------------------------------
  Copyright 2012-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
 
// 32-bit signed integers.
//
// Using 32-bit signed two's complement representation with wrapping on overflow, 
// e.g. `max-int32 + 1.int32 == min-int32`.
module std/num/int32

// The maximal integer value before overflow happens
pub val max-int32 : int32 = 0x7FFF_FFFF.int32

// The minimal integer value before underflow happens
pub val min-int32 : int32 = -0x8000_0000.int32

// The number of bits in an `:int32` (always 32)
pub val bits-int32 : int32 = 32.int32

// Convert an `:int32` to a string
pub fun show( i : int32 ) : string 
  show(i.int)


// Convert a `:double` to an `:int32`. The double is clamped to the
// minimum or maximum `:int32` value if it is outside the range. A `nan` is
// mapped to `0`.
pub inline extern int32 : (double) -> int32 
  cs "Primitive.DoubleToInt32"
  js "$std_core._double_to_int32"


// Convert an `:int32` to a boolean.
pub fun bool( i : int32 ) : bool 
  (i!=zero)


// Convert a boolean to an `:int32`.
pub fun int32( b : bool ) : int32 
  if (b) then one else zero



// Show an `:int32` in hexadecimal notation
// The `width`  parameter specifies how wide the hex value is where `'0'`  is used to align.\
// The `use-capitals` parameter (= `True`) determines if captical letters should be used to display the hexadecimal digits.\
// The `pre` (=`"0x"`) is an optional prefix for the number (goes between the sign and the number).
pub fun show-hex( i : int32, width : int = 1, use-capitals : bool = True, pre : string = "0x" ) : string 
  std/core/show-hex(i.int,width,use-capitals,pre)


// Convert an `:int` to `:int32` but interpret the `int` as an unsigned 32-bit value.
// `i` is clamped between `0` and `0xFFFFFFFF`.\
// `0x7FFF_FFFF.uint32 == 0x7FFF_FFFF.int32 == max-int32`\
// `0x8000_0000.uint32 == -0x8000_0000.int32 == min-int32`\
// `0xFFFF_FFFF.uint32 == -1.int32`\
pub fun uint32( i : int ) : int32 
  if (i > max-int32.int) then (i - 0x1_0000_0000).int32 else i.int32


// Convert an `:int32` to an `:int` but interpret the `:int32` as a 32-bit unsigned value.
pub fun uint( i : int32 ) : int 
  if (i.is-neg) then 0x1_0000_0000 + i.int else i.int


// Show an `:int32` in hexadecimal notation interpreted as an unsigned 32-bit value.
// The `width`  parameter specifies how wide the hex value is where `'0'`  is used to align.\
// The `use-capitals` parameter (= `True`) determines if captical letters should be used to display the hexadecimal digits.\
// The `pre` (=`"0x"`) is an optional prefix for the number.
pub fun show-hex32( i : int32, width : int = 8, use-capitals : bool = True, pre : string = "0x" ) : string 
  std/core/show-hex(i.uint,width,use-capitals,pre)


pub inline extern (==) : (int32,int32) -> bool  { inline "(#1 == #2)"; js inline "(#1 === #2)" }
pub inline extern (!=) : (int32,int32) -> bool  { inline "(#1 != #2)"; js inline "(#1 !== #2)" }
pub inline extern (<=) : (int32,int32) -> bool  { inline "(#1 <= #2)" }
pub inline extern (>=) : (int32,int32) -> bool  { inline "(#1 >= #2)" }
pub inline extern (<)  : (int32,int32) -> bool  { inline "(#1 < #2)"  } 
pub inline extern (>)  : (int32,int32) -> bool  { inline "(#1 > #2)"  }

pub inline extern (+)  : (int32,int32) -> int32 
  c inline "(int32_t)((uint32_t)#1 + (uint32_t)#2)" // avoid UB
  js inline "((#1 + #2)|0)" 

pub inline extern (-)  : (int32,int32) -> int32 
  c inline "(int32_t)((uint32_t)#1 - (uint32_t)#2)" // avoid UB
  js inline "((#1 - #2)|0)" 


pub inline extern is-neg( i : int32 ) : bool 
  inline "0 > #1"


pub inline extern is-pos( i : int32 ) : bool 
  inline "0 < #1"


pub inline extern is-zero( i : int32 ) : bool 
  inline "0 == #1"
  js inline "0 === #1"


pub val zero = 0.int32
pub val one  = 1.int32

pub fun sign( i : int32 ) : order 
  if (i.is-pos) then Gt 
  elif (i.is-neg) then Lt 
  else Eq


// Returns `true` if the integer `i`  is an odd number.
pub fun is-odd( i : int32 ) : bool 
  and(i,1.int32) == 1.int32


// Returns `true` if the integer `i`  is an even number.
pub fun is-even( i : int32 ) : bool 
  and(i,1.int32) == 0.int32


// Increment a 32-bit integer.
pub fun inc( i : int32 ) : int32 
  i + 1.int32


// Decrement a 32-bit integer.
pub fun dec( i : int32 ) : int32 
  i - 1.int32


// Multiply two 32-bit integers.
pub inline extern (*) : (int32,int32) -> int32 
  inline "(int32_t)((uint32_t)#1 * (uint32_t)#2)" // avoid UB
  js "$std_core._int32_multiply"


pub fun compare( x : int32, y : int32) : order
  if (x < y) then Lt
  elif (x > y) then Gt
  else Eq


// Return the absolute value of an integer.
// Raises an exception if the `:int32` is `min-int32`
// (since the negation of `min-int32` equals itself and is still negative)
pub fun abs( i : int32 ) : exn int32
  if (!i.is-neg) then i
  elif (i > min-int32) then negate(i)
  else throw( "std/num/int32/abs: cannot make min-int32 into a positive int32 without overflow" )


// Return the absolute value of an integer.
// Returns 0 if the `:int32` is `min-int32`
// (since the negation of `min-int32` equals itself and is still negative)
pub fun abs0( i : int32 ) : int32
  if (!i.is-neg) then i
  elif (i > min-int32) then negate(i)
  else 0.int32



// Take the bitwise _and_ of two `:int32`s
pub inline extern and : (int32,int32) -> int32 
  inline "(#1 & #2)";


// Take the bitwise _or_ of two `:int32`s
pub inline extern or : (int32,int32) -> int32 
  inline "(#1 | #2)";


// Take the bitwise _xor_ of two `:int32`s
pub inline extern (^) : (int32,int32) -> int32 
  inline "(#1 ^ #2)";


// Bitwise _not_ of an `:int32`, i.e. flips all bits.
pub inline extern not : ( i : int32 ) -> int32  
  inline "(~#1)"


// Shift an `:int32` `i` to the left by `n & 31` bits.
pub inline extern shl : (int32,int32) -> int32 
  c  inline "kk_shl32(#1,#2)"
  js inline "#1 << #2"       // javascript masks the shift already


// Logical shift an `:int32` to the right by `n % 32` bits. Shift in zeros from the left.
pub inline extern shr : (int32,int32) -> int32 
  c  inline "(int32_t)kk_shr32(#1,#2)"
  cs inline "(Int32)(((UInt32)#1)>>#2)"
  js inline "#1 >>> #2"


// Arithmetic shift an `:int32` to the right by `n % 32` bits. Preserves the sign bit.
pub inline extern sar : (int32,int32) -> int32 
  c  inline "kk_sar32(#1,#2)"
  cs inline "(#1>>#2)"
  js inline "#1 >> #2"


// Bitwise rotate an `:int32` `n % 32` bits to the left.
pub inline extern rotl( i : int32, n : int32 ) : int32 
  c inline "(int32_t)kk_bits_rotl32(#1,#2)"
  js "$std_core._int32_rotl"


// Bitwise rotate an `:int32` `n % 32` bits to the right.
pub inline extern rotr( i : int32, n : int32 ) : int32 
  c inline "(int32_t)kk_bits_rotr32(#1,#2)"
  js "$std_core._int32_rotr"


// Return the minimum of two integers
pub fun min( i : int32, j : int32 ) : int32 
  if (i <= j) then i else j 

// Return the maximum of two integers
pub fun max( i : int32, j : int32 ) : int32 
  if (i >= j) then i else j 



// Truncated division (as in C). See also `(/):(x : int32, y : int32) -> int32`.
pub fun cdiv(i:int32, j:int32) : exn int32 
  if (j.is-zero) then throw("std/num/int32/cdiv: modulus by zero")
  elif (j == -1.int32 && i==min-int32) then throw("std/num/int32/cdiv: modulus overflow in cdiv(min-int32, -1.int32)")
  else unsafe-cdiv(i,j)


// Truncated modulus (as in C). See also `(%):(x : int32, y : int32) -> int32`.
pub fun cmod(i:int32, j:int32) : exn int32 
  if (j.is-zero) then throw("std/num/int32/(%): modulus by zero")
  elif (j == -1.int32 && i==min-int32) then throw("std/num/int32/cmod: modulus overflow in cmod(min-int32, -1.int32)")
  else unsafe-cmod(i,j)


// Truncated division (as in C). See also `(/):(x : int32, y : int32) -> int32`.
pub inline extern unsafe-cdiv : (int32,int32) -> int32 
  inline "(#1 / #2)"
  js inline "((#1/#2)|0)"


// Truncated modulus (as in C). See also `(%):(x : int32, y : int32) -> int32`.
pub inline extern unsafe-cmod : (int32,int32) -> int32 
  inline "(#1 % #2)"
  js inline "((#1 % #2)|0)"


// Convert an 32-bit integer to a double.
pub inline extern double : (int32) -> double 
  c inline "(double)(#1)"
  cs inline "(double)(#1)"
  js inline "(#1)" 


// Negate a 32-bit integer
pub fun negate( i : int32 ) : int32 
  (0.int32 - i)


// Negate an 32-bit integer
pub fun (~)(i : int32) : total int32 
  (0.int32 - i)


/* 
Euclidean-0 division.
Euclidean division is defined as: For any `D`  and `d`  where `d!=0` , we have:

1. `D == d*(D/d) + (D%d)`
2. `D%d`  is always positive where `0 <= D%d < abs(d)`

Moreover, Euclidean-0 is a total function, for the case where `d==0`  we have
that `D%0 == D`  and `D/0 == 0` . So property (1) still holds, but not property (2).
Useful laws that hold for Euclidean-0 division:

* `D/(-d) == -(D/d)`
* `D%(-d) == D%d`
* `D/(2^n) == sar(D,n)         `  (with `0 <= n <= 31`)
* `D%(2^n) == D & ((2^n) - 1)  `  (with `0 <= n <= 31`)

Note that an interesting edge case is `min-int32 / -1` which equals `min-int32` since in modulo 32-bit
arithmetic `min-int32 == -1 * min-int32 == -1 * (min-int32 / -1) + (min-int32 % -1)` satisfying property (1).
Of course `(min-int32 + 1) / -1` is again positive (namely `max-int32`).

See also _Division and modulus for computer scientists, Daan Leijen, 2001_ 
[pdf](http://research.microsoft.com/pubs/151917/divmodnote.pdf) .
*/
pub fun (/)( x : int32, y : int32 ) : int32
  if (y == 0.int32) then return 0.int32
  if (y == -1.int32 && x==min-int32) return x
  val q = unsafe-cdiv(x,y)
  val r = unsafe-cmod(x,y)
  if (r >= 0.int32) then q
  elif (y > 0.int32) then q - 1.int32
  else q + 1.int32


// Euclidean-0 modulus. See `(/):(x : int32, y : int32) -> int32` division for more information.
pub fun (%)( x : int32, y : int32 ) : int32
  if (y == 0.int32) then return x
  if (y == -1.int32 && x==min-int32) return 0.int32
  val r = unsafe-cmod(x,y)
  if (r >= 0.int32) then r
  elif (y > 0.int32) then (r+y)
  else (r - y)


pub fun divmod(x:int32,y:int32) : (int32,int32) 
  if (y.is-zero) then return (zero,x)
  if (y == -1.int32 && x==min-int32) return (x,0.int32)
  val q = unsafe-cdiv(x,y)
  val r = unsafe-cmod(x,y)
  if (!r.is-neg) then (q,r)
  elif (y.is-pos) then (q.dec, r+y)
  else (q.inc,r - y)


pub fun fold-int32( start : int32, end : int32, init : a, f : (int32,a) -> e a ) : e a 
  if (start >= end) then init else 
    val x = f(start,init)
    fold-int32(unsafe-decreasing(start.inc), end, x, f)

