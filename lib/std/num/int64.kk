/*---------------------------------------------------------------------------
  Copyright 2012-2023, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// 64-bit signed integers.
//
// Using 64-bit signed two's complement representation
// with wrapping on overflow, e.g. `max-int64 + 1.int64 == min-int64`
module std/num/int64

import std/core/undiv

extern import
  c  file "int64-inline.c"

// The maximal integer value before overflow happens
pub val max-int64 : int64 =  0x7FFF_FFFF_FFFF_FFFF.int64

// The minimal integer value before underflow happens
pub val min-int64 : int64 = -0x8000_0000_0000_0000.int64

// The number of bits in an `:int64` (always 64)
pub val bits-int64 : int64 = 64.int64

// Convert an `:int64` to a string
pub fun show( i : int64 ) : string
  show(i.int)


// clamp an `:int` to fit in an `:int64_t`.
pub fip extern int64( i : int) : int64
  c  "kk_integer_clamp64"
  cs "Primitive.IntToInt64"
  js "$std_core_types._int_clamp64"

// Convert an `:int64_t` to an `:int`.
pub inline fip extern int( i : int64 ) : int
  c  "kk_integer_from_int64"
  cs inline "(new BigInteger(#1))"
  js "$std_core_types._int_from_int64"

// Convenient shorthand to `int64`, e.g. `1234.i64`
pub inline fip fun i64( i : int ) : int64
  i.int64


// Convert an `:int64` to a boolean.
pub fun bool( i : int64 ) : bool
  (i!=zero)


// Convert a boolean to an `:int64`.
pub fip fun bool/int64( b : bool ) : int64
  if b then one else zero

// Convert an `:int32` to an `:int64` (using sign extension).
pub inline fip extern int32/int64( i : int32 ) : int64
  c inline "(int64_t)(#1)"
  js "$std_core_types._int64_from_int32"

// Convert an `:int32` to an `:int64` interpreting the `:int32` as unsigned.
pub inline fip extern uint64( i : int32 ) : int64
  c inline "(int64_t)((uint32_t)(#1))"
  js "$std_core_types._int64_from_uint32"

// Clamp an `:int64` to an `:int32`
// `-1.int64.int32 == -1.int32`
// `0x8000_0000.int64.int32 == 0x7FFF_FFFF.int32` (clamped)
pub inline fip extern int32( i : int64 ) : int32
  c  "kk_int64_clamp_int32"
  js "$std_core_types._int64_clamp_int32"

// Clamp an `:int64` to an `:int32` but interpreting the `:int32` as unsigned
// (and thus clamp between 0 and 0xFFFFFFFF).
// `-1.int64.uint32 == 0.int32` (clamped)
// `0xFFFFFFFF.int64.uint32 == -1.int32`
pub inline fip extern uint32( i : int64 ) : int32
  c  "kk_int64_clamp_uint32"
  js "$std_core_types._int64_clamp_uint32"

// Create an `:int64` `i` from the bits of `lo` and `hi` such
// that `i.int = hi.int * 0x1_0000_0000 + lo.uint`.
pub inline fip extern hilo32/int64( hi : int32, lo : int32 ) : int64
  c inline "kk_int64_hi_lo(#1,#2)"
  js "$std_core_types._int64_hi_lo"

// Return the top 32-bits of an `:int64`.
// Preserves the sign.
pub fip fun hi( i : int64 ) : int64
  sar64(i,32.int64)

// Return the low 32-bits of an `:int64`.
pub fip fun lo( i : int64 ) : int64
  and(i,0xFFFFFFFF.int64)

// Create an `:int64` from the given `hi` and `lo` numbers lowest 32-bits.
// Preserves the sign of `hi`.
pub fip fun hilo/int64( hi : int64, lo : int64 ) : int64
  or( shl64(hi,32.int64), and(lo,0xFFFF_FFFF.int64) )

// Convert an `:int` to `:int64` but interpret the `int` as an unsigned 64-bit value.
// `i` is clamped between `0` and `0xFFFF_FFFF_FFFF_FFFF`.\
// `0x7FFF_FFFF_FFFF_FFFF.uint64 == 0x7FFF_FFFF_FFFF_FFFF.int64 == max-int64`\
// `0x8000_0000_0000_0000.uint64 == -0x8000_0000_0000_0000.int64 == min-int64`\
// `0xFFFF_FFFF_FFFF_FFFF.uint64 == -1.int64`\
pub fun int/uint64( i : int ) : int64
  if i > max-int64.int then (i - 0x1_0000_0000_0000_0000).int64 else i.int64


// Convert an `:int64` to an `:int` but interpret the `:int64` as a 64-bit unsigned value.
pub fip fun int64/uint( i : int64 ) : int
  if i.is-neg then 0x1_0000_0000_0000_0000 + i.int else i.int


// Show an `:int64` in hexadecimal notation
// The `width`  parameter specifies how wide the hex value is where `'0'`  is used to align.\
// The `use-capitals` parameter (= `True`) determines if captical letters should be used to display the hexadecimal digits.\
// The `pre` (=`"0x"`) is an optional prefix for the number (goes between the sign and the number).
pub fun show-hex( i : int64, width : int = 1, use-capitals : bool = True, pre : string = "0x" ) : string
  std/core/show/show-hex(i.int,width,use-capitals,pre)

// Show an `:int64` in hexadecimal notation interpreted as an unsigned 64-bit value.
// The `width`  parameter specifies how wide the hex value is where `'0'`  is used to align.\
// The `use-capitals` parameter (= `True`) determines if captical letters should be used to display the hexadecimal digits.\
// The `pre` (=`"0x"`) is an optional prefix for the number.
pub fun show-hex64( i : int64, width : int = 16, use-capitals : bool = True, pre : string = "0x" ) : string
  std/core/show/show-hex(i.uint,width,use-capitals,pre)


// Are two 64-bit integers equal?
pub inline fip extern (==)(x : int64, y : int64) : bool
  inline "(#1 == #2)"
  js inline "(#1 === #2)"

// Are two 64-bit integers not equal?
pub inline fip extern (!=)(x : int64, y : int64) : bool
  inline "(#1 != #2)"
  js inline "(#1 !== #2)"

// Is the first 64-bit integer smaller or equal to the second?
pub inline fip extern (<=)(x : int64, y : int64) : bool
  inline "(#1 <= #2)"

// Is the first 64-bit integer larger or equal to the second?
pub inline fip extern (>=)(x : int64, y : int64) : bool
  inline "(#1 >= #2)"

// Is the first 64-bit integer smaller than the second?
pub inline fip extern (<)(x : int64, y : int64) : bool
  inline "(#1 < #2)"

// Is the first 64-bit integer larger than the second?
pub inline fip extern (>)(x : int64, y : int64) : bool
  inline "(#1 > #2)"

// Add two 64-bit integers.
pub inline fip extern (+)(x : int64, y : int64) : int64
  c inline "(int64_t)((uint64_t)#1 + (uint64_t)#2)"  // avoid UB
  js inline "BigInt.asIntN(64,#1 + #2)"

// Subtract two 64-bit integers.
pub inline fip extern (-)(x : int64, y : int64) : int64
  c inline "(int64_t)((uint64_t)#1 - (uint64_t)#2)" // avoid UB
  js inline "BigInt.asIntN(64,#1 - #2)"

// Is the 64-bit integer negative?
pub inline fip extern is-neg( i : int64 ) : bool
  inline "0 > #1"
  js inline "0n > #1"

// Is the 64-bit integer positive? (i.e. larger than zero)
pub inline fip extern is-pos( i : int64 ) : bool
  inline "0 < #1"
  js inline "0n < #1"

// Is the 64-bit integer zero?
pub inline fip extern is-zero( i : int64 ) : bool
  inline "0 == #1"
  js inline "0n === #1"

// The 64-bit integer for zero.
pub val zero = 0.int64

// The 64-bit integer for 1.
pub val one  = 1.int64

// Compare a 64-integer to zero.
pub fip fun sign( i : int64 ) : order
  if i.is-pos then Gt
  elif i.is-neg then Lt
  else Eq


// Returns `true` if the integer `i`  is an odd number.
pub fip fun is-odd( i : int64 ) : bool
  and(i,one)==one


// Returns `true` if the integer `i`  is an even number.
pub fip fun is-even( i : int64 ) : bool
  and(i,one)==zero


// Increment a 64-bit integer.
pub fip fun inc( i : int64 ) : int64
  i + 1.int64


// Decrement a 64-bit integer.
pub fip fun dec( i : int64 ) : int64
  i - 1.int64


// Multiply two 64-bit integers.
pub inline fip extern (*) : (int64,int64) -> int64
  c inline "(int64_t)((uint64_t)#1 * (uint64_t)#2)"; // avoid UB
  js inline "BigInt.asIntN(64,#1 * #2)"


pub fip fun cmp( x : int64, y : int64) : order
  if x < y then Lt
  elif x > y then Gt
  else Eq


// Return the absolute value of an integer.
// Raises an exception if the `:int64` is `min-int64`
// (since the negation of `min-int64` equals itself and is still negative)
pub fun abs( i : int64 ) : exn int64
  if !i.is-neg then i
  elif i > min-int64 then negate(i)
  else throw( "std/num/int64/abs: cannot make min-int64 into a positive int64 without overflow" )


// Return the absolute value of an integer.
// Returns 0 if the `:int64` is `min-int64`
// (since the negation of `min-int64` equals itself and is still negative)
pub fip fun abs0( i : int64 ) : int64
  if !i.is-neg then i
  elif i > min-int64 then negate(i)
  else 0.int64



// Take the bitwise _and_ of two `:int64`s
pub inline fip extern and : (int64,int64) -> int64
  inline "#1 & #2"

// Take the bitwise _or_ of two `:int64`s
pub inline fip extern or : (int64,int64) -> int64
  inline "#1 | #2"

// Take the bitwise _xor_ of two `:int64`s
pub inline fip extern xor : (int64,int64) -> int64
  inline "#1 ^ #2";

// Take the bitwise _xor_ of two `:int64`s
pub fip fun (^)( x : int64, y : int64) : int64
  xor(x,y)

// Bitwise _not_ of an `:int64`, i.e. flips all bits.
pub inline fip extern not : ( i : int64 ) -> int64
  inline "~#1"
  js inline "BigInt.asIntN(64, ~#1)"


// Shift an `:int64` `i` to the left by `n % 64` bits.
inline fip extern shl64 : (int64,int64) -> int64
  c inline "kk_shl64(#1,#2)"
  js "$std_core_types._int64_shl"

// Shift an `:int64` `i` to the left by `n % 64` bits.
pub fip fun shl( i : int64, shift : int) : int64
  shl64(i,shift.int64)

// Logical shift an `:int64` to the right by `n % 64` bits. Shift in zeros from the left.
inline fip extern shr64 : (int64,int64) -> int64
  c inline "(int64_t)kk_shr64(#1,#2)"
  cs inline "(int64)(((Uint64)#1)>>#2)"
  js "$std_core_types._int64_shr"

// Logical shift an `:int64` to the right by `n % 64` bits. Shift in zeros from the left.
pub fip fun shr( i : int64, shift : int) : int64
  shr64(i,shift.int64)


// Arithmetic shift an `:int64` to the right by `n % 64` bits. Preserves the sign bit.
inline fip extern sar64 : (int64,int64) -> int64
  c inline "kk_sar64(#1,#2)"
  js "$std_core_types._int64_sar"

// Arithmetic shift an `:int64` to the right by `n % 64` bits. Shift in the sign bit from the left.
pub fip fun sar( i : int64, shift : int) : int64
  sar64(i,shift.int64)

// Bitwise rotate an `:int64` `n % 64` bits to the left.
inline fip extern rotl64( i : int64, n : int64 ) : int64
  c inline "(int64_t)kk_bits_rotl64(#1,(int)#2)"
  js "$std_core_types._int64_rotl"

// Bitwise rotate an `:int64` `n % 64` bits to the left.
pub fun rotl( i : int64, shift : int) : int64
  rotl64(i,shift.int64)

// Bitwise rotate an `:int64` `n % 64` bits to the right.
inline fip extern rotr64( i : int64, n : int64 ) : int64
  c inline "(int64_t)kk_bits_rotr64(#1,(int)#2)"
  js "$std_core_types._int64_rotr"

// Bitwise rotate an `:int64` `n % 64` bits to the right.
pub fip fun rotr( i : int64, shift : int) : int64
  rotr64(i,shift.int64)


// Count trailing zero bits. Returns 64 if `i` is zero.
pub inline fip extern ctz( i : int64 ) : int
  c inline "kk_integer_from_small(kk_bits_ctz64(#1))"
  js "$std_core_types._int64_ctz"

// Count leading zero bits. Returns 64 if `i` is zero.
pub inline fip extern clz( i : int64 ) : int
  c inline "kk_integer_from_small(kk_bits_clz64(#1))"
  js "$std_core_types._int64_clz"


// Count leading redundant sign bits (i.e. the number of bits
// following the most significant bit that are identical to it).
pub inline fip extern clrsb( i : int64 ) : int
  c inline "kk_integer_from_small(kk_bits_clrsb64(#1))"
  js "$std_core_types._int64_clrsb"


// Count number of 1-bits.
inline fip extern popcount64( i : int64 ) : int64
  c inline "(int64_t)kk_bits_popcount64(#1)"
  js "$std_core_types._int64_popcount"

// Count number of 1-bits.
pub fip fun popcount( i : int64 ) : int
  popcount64(i).int

// Is the `popcount` even?
pub inline fip extern parity( i : int64 ) : bool
  c inline "kk_bits_parity64(#1)"
  js "$std_core_types._int64_parity"

// Reverse the bytes in an `:int64`.
pub inline fip extern byte-reverse( i : int64 ) : int64
  c inline "kk_bits_bswap64(#1)"
  js "$std_core_types._int64_bswap"

// Reverse the bits in an `:int64`.
pub inline fip extern bit-reverse( i : int64 ) : int64
  c inline "kk_bits_reverse64(#1)"
  js "$std_core_types._int64_breverse"

// Bit gather (also known as _pext_ or parallel bit extract).
// For each 1-bit in mask `m`, extract the corresponding bit from `i` and write it
// into contiguous lower bits in the result. The remaining bits in the result are zero.
//
// `bit-gather(0x1234.int64, 0x0F0F.int64).show-hex == "0x24"`
pub inline fip extern bit-gather( i : int64, m : int64 ) : int64
  c inline "kk_bits_gather64(#1,#2)"

// Bit scatter (also known as _pdep_ or parallel bit deposit).
// For each 1-bit in mask `m`, set the corresponding bit in the result from the
// contiguous lower bits of `i`. Any bits not set according to the mask are set to zero.
//
// `bit-scatter(0x1234.int64, 0x0F0F.int64).show-hex == "0x304"`
pub inline fip extern bit-scatter( i : int64, m : int64 ) : int64
  c inline "kk_bits_scatter64(#1,#2)"

// Interleave the hi 32-bits with the lo 32-bits of the argument `i` such
// that the hi bits are spread over the odd bits and the lo bits over the even bits
// of the result (starting at bit 0).
// `bit-zip(x.and(0xFFFF_FFFF.int64)) == bit-scatter(x,0x5555_5555_5555_5555.int64)`
pub inline fip extern bit-zip( i : int64 ) : int64
  c inline "kk_bits_zip64(#1)"

// De-interleave the bits in `i` such that the odd bits are gathered in the
// hi 32-bits of the result, and the even bits (starting at bit 0)
// end up in the lo 32-bits of the result.
// `bit-unzip(x).and(0xFFFF_FFFF.int64) == bit-gather(x,0x5555_5555_5555_5555.int64)`
pub inline fip extern bit-unzip( i : int64 ) : int64
  c inline "kk_bits_unzip64(#1)"

// Or-combine: for every byte `b` in the argument `i`, the corresponding
// byte in the result becomes 0 if `b==0`, and `0xFF` otherwise.
pub inline fip extern orc( i : int64 ) : int64
  c inline "kk_bits_orc64(#1)"

// Carry-less multiplication (or _xor_ multiplication).
// See also <https://en.wikipedia.org/wiki/Carry-less_product>
// Interesting cases:
// - `clmul(x,x)` : _bit spread_, where a zero bit is put in between each input bit.
//   `clmul(x,x) == bit-scatter(x,0x5555_5555_5555_5555.int64) == bit-zip(and(x,0xFFFF_FFFF.int64))`
// - `clmul(x,-1.int32)`: _prefix xor_, where each bit is the parity of the corresponding input bit
//    and all input bits before it (the _xor sum_).
//    `clmul(x,-1.int64).shr(63).bool == x.parity`
pub inline fip extern clmul( x : int64, y : int64 ) : int64
  c inline "(int64_t)kk_clmul64((uint64_t)#1,(uint64_t)#2)"

// Wide carry-less multiplication (or _xor_ multiplication) to `(hi,lo)`,
// where `(hi,lo).int == hi.int * 0x1_0000_0000_0000_0000 + lo.uint`.
// See also <https://en.wikipedia.org/wiki/Carry-less_product>.
pub fip extern clmul-wide( x : int64, y : int64 ) : (int64,int64)
  c "kk_clmul64x_wide"

// Reverse carry-less multiplication.
// Defined as `clmulr(x,y) == clmul(x.bit-reverse,y.bitreverse).bitreverse`
// Interesting cases:
// - `clmulr(x,x) == zip(x.and(0xFFFF_FFFF_0000_0000.int64)`, and
// - `zip(x) == clmul(x,x) | clmulr(x,x)`.
pub inline fip extern clmulr( x : int64, y : int64 ) : int64
  c inline "(int64_t)kk_clmulr64((uint64_t)#1,(uint64_t)#2)"


// Return the minimum of two integers
pub fip fun min( i : int64, j : int64 ) : int64
  if i <= j then i else j

// Return the maximum of two integers
pub fip fun max( i : int64, j : int64 ) : int64
  if i >= j then i else j


// Full 64x64 to 128-bit unsigned multiply to `(hi,lo)`.
// where `umul(x,y).uint == x.uint * y.uint`
pub fip extern umul( i : int64, j : int64 ) : (int64,int64)
  c "kk_umul64x_wide"
  js "$std_core_types._int64_umul"

// Full 64x64 to 128-bit signed multiply to `(hi,lo)`.
// where `imul(x,y).int == x.int * y.int`
pub fip extern imul( i : int64, j : int64 ) : (int64,int64)
  c "kk_imul64x_wide"
  js "$std_core_types._int64_imul"

// Convert a pair `(hi,lo)` to an signed integer,
// where `(hi,lo).int == hi.int * 0x1_0000_0000_0000_0000 + lo.uint`
pub fun hilo/int( (hi,lo) : (int64,int64) ) : int
  hi.int * 0x1_0000_0000_0000_0000 + lo.uint

// Convert a pair `(hi,lo)` to an unsigned integer,
// where `(hi,lo).uint == hi.uint * 0x1_0000_0000_0000_0000 + lo.uint`
pub fun hilo/uint( (hi,lo) : (int64,int64) ) : int
  hi.uint * 0x1_0000_0000_0000_0000 + lo.uint

// Truncated division (as in C). See also `(/):(x : int64, y : int64) -> int64`.
pub fun cdiv(i:int64, j:int64) : exn int64
  if j.is-zero then throw("std/num/int64/cdiv: division by zero")
  elif j == -1.int64 && i==min-int64 then throw("std/num/int64/cdiv: division overflow in cdiv(min-int64, -1.int64)")
  else unsafe-cdiv(i,j)


// Truncated modulus (as in C). See also `(%):(x : int64, y : int64) -> int64`.
pub fun cmod(i:int64, j:int64) : exn int64
  if j.is-zero then throw("std/num/int64/cmod: modulus by zero")
  elif j == -1.int64 && i==min-int64 then throw("std/num/int64/cmod: modulus overflow in cmod(min-int64, -1.int64)")
  else unsafe-cmod(i,j)


// Truncated division (as in C). See also `(/):(x : int64, y : int64) -> int64`.
inline fip extern unsafe-cdiv : (int64,int64) -> int64
  inline "#1 / #2"


// Truncated modulus (as in C). See also `(%):(x : int64, y : int64) -> int64`.
inline fip extern unsafe-cmod : (int64,int64) -> int64
  inline "#1 % #2"


// Negate a 64-bit integer
pub fip fun negate( i : int64 ) : int64
  0.int64 - i


// Negate an 64-bit integer
pub fip fun (~)(i : int64) : total int64
  0.int64 - i


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
* `D/(2^n) == sar(D,n)         `  (with `0 <= n <= 63`)
* `D%(2^n) == D & ((2^n) - 1)  `  (with `0 <= n <= 63`)

Note that an interesting edge case is `min-int64 / -1` which equals `min-int64` since in modulo 64-bit
arithmetic `min-int64 == -1 * min-int64 == -1 * (min-int64 / -1) + (min-int64 % -1)` satisfying property (1).
Of course `(min-int64 + 1) / -1` is again positive (namely `max-int64`).

See also _Division and modulus for computer scientists, Daan Leijen, 2001_
[pdf](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf) .
*/
pub fip fun (/)( x : int64, y : int64 ) : int64
  if y == 0.int64 return 0.int64
  if y == -1.int64 && x==min-int64 return x
  val q = unsafe-cdiv(x,y)
  val r = unsafe-cmod(x,y)
  if r >= 0.int64 then q
  elif y > 0.int64 then q.dec
  else q.inc


// Euclidean-0 modulus. See `(/):(x : int64, y : int64) -> int64` division for more information.
pub fip fun (%)( x : int64, y : int64 ) : int64
  if y == 0.int64 return x
  if y == -1.int64 && x==min-int64 return 0.int64
  val r = unsafe-cmod(x,y)
  if r >= 0.int64 then r
  elif y > 0.int64 then r + y
  else r - y

pub fip fun divmod( x :int64, y :int64 ) : (int64,int64)
  if y.is-zero return (zero,x)
  if y == -1.int64 && x==min-int64 return (x,0.int64)
  val q = unsafe-cdiv(x,y)
  val r = unsafe-cmod(x,y)
  if r >= 0.int64 then (q, r)
  elif y > 0.int64 then (q.dec, r + y)
  else (q.inc, r - y)


pub fun range/fold-int64( start : int64, end : int64, init : a, f : (int64,a) -> e a ) : e a
  if start > end then init else
    val x = f(start,init)
    range/fold-int64(pretend-decreasing(start.inc), end, x, f)

pub fun fold-int64( n : int64, init : a, f : (int64,a) -> e a ) : e a
  range/fold-int64(zero,n.dec,init,f)


// Executes `action`  for each integer between `start`  upto `end`  (including both `start`  and `end` ).
// If `start > end`  the function returns without any call to `action` .
// If `action` returns `Just`, the iteration is stopped and the result returned
pub fun range/for-while64( start: int64, end : int64, ^action : (int64) -> e maybe<a> ) : e maybe<a>
  fun rep( i : int64 )
    if i <= end then
      match action(i)
        Nothing -> rep(pretend-decreasing(i.inc))
        Just(x) -> Just(x)
    else Nothing
  rep(start)

pub fun for-while64( n : int64, ^action : (int64) -> e maybe<a> ) : e maybe<a>
  range/for-while64(zero,n.dec,action)


pub fun list64( lo: int64, hi: int64 ) : total list<int64>
  if lo <= hi
    then Cons( lo, list64( pretend-decreasing(lo.inc), hi ) )
    else Nil

pub fun sum64( xs : list<int64> ) : int64
  // xs.foldl( 0.int64, fn(x,y) x + y )
  sumacc64(xs,0.int64)

fun sumacc64( xs : list<int64>, acc : int64 ) : int64
  match xs
    Cons(x,xx) -> sumacc64(xx,acc+x)
    Nil -> acc


// carry-less multiply by -1 gives the _prefix sum_
fun test-prefix-sum()
  val x = 0x050F.int64  // = 0b0000010100001111
  clmul(x, -1.int64).show-hex // = 0x305


// carry-less multiply of x by x spreads the bits
fun test-bit-spread1()
  val x = 0x05FF.int64  // = 0b0000010111111111
  clmul(x,x).show-hex   // = 0x115555 = 0b00000000000100010101010101010101

fun test-bit-spread2()
  val x = 0x05FB.int64  // = 0b0000010111111011
  clmul(x,x).show-hex   // = 0x115545 = 0b00000000000100010101010101000101

fun zip-clmul(x : int64) : int64
  or(clmul(x,x),clmulr(x,x))

fun test-clmulr1()
  val x = 0x33.int64
  bit-zip(x).show-hex ++ " == " ++ zip-clmul(x).show-hex  // 0x505

fun test-clmulr2()
  val x = 0x12346789ABCDEF0.int64
  bit-zip(x).show-hex ++ " == " ++ zip-clmul(x).show-hex  // 0x41464D5A717C7F80

fun clmulrev(x : int64, y : int64) : int64
  clmul(x.bit-reverse, y.bit-reverse).bit-reverse

fun test-clmulrev1()
  val x = 0x12345678.int64
  val y = 0x89ABCDEF.int64.byte-reverse
  clmulr(x,y).show-hex ++ " == " ++ clmulrev(x,y).show-hex  // 0x1E6799EC