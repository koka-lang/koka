/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/* Random numbers.
*/
module std/num/random

import std/num/int32
import std/num/int64
import std/num/float64

extern import
  js file "random-inline.js"

pub effect random
  fun random-int32() : int32

pub fun ".default-random"(action : () -> <random,ndet|e> a) : <ndet|e> a
  strong-random(action)

// Pick random numbers from a the best strong random source in the OS.
// (e.g. like `/dev/urandom`, `arc4random` etc.). Use `srandom-is-strong` to test if the 
// numbers are indeed based on a strong random source.
pub fun strong-random(action : () -> <random,ndet|e> a) : <ndet|e> a
  with fun random-int32() srandom-int32() 
  action()

// Pseudo random number using sfc32 by Chris Doty-Humphrey.
// It is a "chaotic" pseudo random generator that uses 32-bit operations only 
// (so we can be deterministic across architectures in results and performance).
// It has good statistical properties and passes PractRand and Big-crush.
// It uses a 32-bit counter to guarantee a worst-case cycle
// of 2^32. It has a 96-bit state, so the average period is 2^127.
// The chance of a cycle of less than 2^(32+max(96-k,0)) is 2^-(32+k), 
// (e.g. the chance of a cycle of less than 2^48 is 2^-80).
// <http://pracrand.sourceforge.net/RNG_engines.txt>
abstract value struct sfc(x:int32, y:int32, z:int32, cnt:int32)

pub value struct sfc-result( rnd : int32, rstate : sfc )

pub fun sfc-step( sfc : sfc ) : sfc-result
  match sfc 
    Sfc(x,y,z,cnt) -> 
      val res = x + y + cnt
      Sfc-result( res, Sfc( y ^ shr(y,9), 
                            z + shl(z,3), 
                            rotl(z,21) + res, 
                            cnt + 1.int32 ))      

pub fun sfc-init32( seed1 : int32, seed2 : int32 ) : sfc
  val sfc0 = Sfc(0.int32, seed1, seed2, 1.int32)
  fold-int32( 0.int32, 12.int32, sfc0, fn(_,s){ sfc-step(s).rstate } )  // step 12 times


pub fun sfc-init( seed : int ) : sfc
  sfc-init32(seed.int32, (seed / 0x100000000).int32)
  

// Use pseudo random numbers given some initial `seed`. At most
// 64-bits of the initial seed are used. Do not use this for
// cryptographic applications (use `strong-random` instead).
// Uses _sfc32_ by Chris Doty-Humphrey which is a fast random
// number generator with a 128-bit internal state which 
// passes PractRand and BigCrush. The worst case minimum cycle
// is 2^^32^^, where a potential cycle of 2^^48^^ has a chance 
// of 2^^-80^^.
pub fun pseudo-random( seed : int, action : () -> <random|e> a) : e a
  var s := sfc-init(seed)
  with fun random-int32()
    val sfc = sfc-step(s)
    s := sfc.rstate
    sfc.rnd
  action()


// Return a random boolean
pub fun random-bool() : random bool
  (random-int32() >= 0.int32)

// Return a random integer in the range [-2^^31^^, 2^^31^^).
pub fun random-int() : random int
  random-int32().int

pub fun random-int64() : random int64
  int64( random-int32(), random-int32()) // todo: add native random-int64?
  
// Return a random float64 in the range [0,1) using 52-bits of randomness
pub fun random-float64() : random float64
  val mag = random-int64().shr(12).or(0x3FF0_0000_0000_0000.int64)
  (float64-from-bits(mag) - 1.0)


// Returns one of its arguments `x`  or `y`  based on a non-deterministic choice.
pub fun choose( x: a, y: a) : ndet a
  if srandom-bool() then x else y

// Return a strong random boolean
pub fun srandom-bool() : ndet bool
  (srandom-int32() >= 0.int32)

// Return a strong random integer in the range [-2^^31^^, 2^^31^^).
pub fun srandom-int() : ndet int
  srandom-int32().int

// Return a strong random `:int32`
pub extern srandom-int32: () -> ndet int32
  c  inline "(int32_t)kk_srandom_uint32(kk_context())"
  js "_srandom_int32"

// Return a strong random `:float64` in the range [0,1) using 52-bits of randomness
pub extern srandom-float64 : () -> ndet float64
  c  "kk_srandom_double"
  js "_srandom_double"

// Are the strong random numbers generated from a strong random source? (like /dev/urandom)
pub extern srandom-is-strong: () -> ndet bool
  c  "kk_srandom_is_strong"
  js "_srandom_is_strong"

// Return a strong random `:int32` uniformly distributed in the range [lo,hi)
pub extern srandom-int32-range(lo : int32, hi : int32) : ndet int32
  c  "kk_srandom_range_int32"
  js "_srandom_range_int32"

// Generate a strong random float64 uniformly distributed in the range [lo, hi)
pub fun srandom-float64-range(lo : float64, hi : float64) : ndet float64
  val low  = if lo <= hi then lo else hi
  val high = if lo <= hi then hi else lo
  val x = ((high - low) * srandom-float64()) + low
  if x >= high then low else x  // can happen due to rounding

