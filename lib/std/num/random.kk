/*---------------------------------------------------------------------------
  Copyright 2020, Daan Leijen, Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/* Random numbers.
*/
public module std/num/random

import std/num/int32
import std/num/double

include extern {
  js file "random-inline.js"
}

effect random {
  fun random-int32() : int32
}

fun ".default-random"(action : () -> <random,ndet|e> a) : <ndet|e> a {
  strong-random(action)
}

// Pick random numbers from a the best strong random source in the OS.
// (e.g. like `/dev/urandom`, `arc4random` etc.). Use `srandom-strong?` to test if the 
// numbers are indeed based on a strong random source.
fun strong-random(action : () -> <random,ndet|e> a) : <ndet|e> a {
  with fun random-int32() { srandom-int32() }
  action()
}

// Pseudo random number using sfc32 by Chris Doty-Humphrey.
// It is a "chaotic" pseudo random generator that uses 32-bit operations only 
// (so we can be deterministic across architectures in results and performance).
// It has good statistical properties and passes PractRand and Big-crush.
// It uses a 32-bit counter to guarantee a worst-case cycle
// of 2^32. It has a 96-bit state, so the average period is 2^127.
// The chance of a cycle of less than 2^(32+max(96-k,0)) is 2^-(32+k), 
// (e.g. the chance of a cycle of less than 2^48 is 2^-80).
// <http://pracrand.sourceforge.net/RNG_engines.txt>
private struct sfc(x:int32, y:int32, z:int32, cnt:int32)

private fun sfc-step( sfc : sfc ) : (int32,sfc) {
  match(sfc) {
    Sfc(x,y,z,cnt) {
      val res = x + y + cnt
      (res, Sfc( y ^ shr(y,9.int32), 
                 z + shl(z,3.int32), 
                 rotl(z,21.int32) + res, 
                 cnt + 1.int32 ))      
    }
  }
}

private fun sfc-init( seed : int ) : sfc {
  val sfc0 = Sfc(0.int32, seed.int32, (seed / 0x100000000).int32, 1.int32)
  fold-int32( 0.int32, 12.int32, sfc0, fun(_,s){ sfc-step(s).snd } )  // step 12 times
}

// Use pseudo random numbers given some initial `seed`. At most
// 64-bits of the initial seed are used. Do not use this for
// cryptographic applications (see `strong-random`).
// Uses _sfc32_ by Chris Doty-Humphrey which is a fast random
// number generator with a 128-bit internal state which 
// passes PractRand and BigCrush. The worst case minimum cycle
// is 2^^32^^, where a potential cycle of 2^^48^^ has a chance 
// of 2^^-80^^.
fun pseudo-random( seed : int, action : () -> <random|e> a) : e a {
  var s := sfc-init(seed)
  with fun random-int32() { 
    val (x,sfc) = sfc-step(s)
    s := sfc
    x
  }
  mask<local>{ action() }
}

// Return a random boolean
fun random-bool() : random bool {
  (random-int32() >= 0.int32)
}

// Return a random integer in the range [-2147483648,2147483647] (32-bits).
fun random-int() : random int {
  random-int32().int
}

// Return a random double in the range [0,1) using 48-bits of randomness
fun random-double() : random double {
  val lo = random-int32()
  val hi = random-int32().shr(16.int32).or(0x3FF00000.int32)
  (double-from-bits(lo,hi) - 1.0)
}


// Returns one of its arguments `x`  or `y`  based on a non-deterministic choice.
fun choose( x: a, y: a) : ndet a {
  if (srandom-bool()) then x else y
}

// Return a strong random boolean
fun srandom-bool() : ndet bool {
  (srandom-int32() >= 0.int32)
}

// Return a strong random integer in the range [-2147483648,2147483647] (32 bits).
fun srandom-int() : ndet int {
  srandom-int32().int
}

// Return a strong random `:int32`
extern srandom-int32: () -> ndet int32  {
  c  inline "(int32_t)kk_srandom_uint32(kk_context())"
  js "_srandom_int32"
}

// Return a strong random `:double` in the range [0,1) using 48-bits of randomness
extern srandom-double : () -> ndet double   {
  c  inline "kk_srandom_double(kk_context())"
  js "_srandom_double"
}

// Are the strong random numbers generated from a strong random source? (like /dev/urandom)
extern srandom-strong?: () -> ndet bool  {
  c  "kk_srandom_is_strong"
  js "_srandom_is_strong"
}
