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

extern include {
  js file "random-inline.js"
}

effect random {
  fun random-int32() : int32
}

fun ".default-random"(action : () -> <random,ndet|e> a) : <ndet|e> a {
  strong-random(action)
}

// Pick random numbers from a the best strong random source in the OS.
// (e.g. like `/dev/urandom`). Use `srandom-strong?` to test if the 
// numbers are indeed strong.
fun strong-random(action : () -> <random,ndet|e> a) : <ndet|e> a {
  with fun random-int32() { srandom-int32() }
  action()
}

// Return a random boolean
fun random-bool() : random bool {
  (random-int32() >= 0.int32)
}

// Return a random integer in the range -2147483648 to 2147483647.
fun random-int() : random int {
  random-int32().int
}

// Return a random double in the range [0,1) using 48-bits of randomness
fun random-double() : random double {
  val lo = random-int32().shl(4.int32)
  val hi = random-int32().shr(12.int32) | 0x3FF00000.int32
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

// Return a strong random number equal or larger than 0.0 and smaller than 1.0
extern srandom-double : () -> ndet double   {
  c  "srandom_double"
  js "_srandom_double"
}

// Return a strong random `int32`
extern srandom-int32: () -> ndet int32  {
  c  inline "(int32_t)srandom_uint32(current_context())"
  js "_srandom_int32"
}

// Are the strong random numbers generated from a strong random source? (like /dev/urandom)
extern srandom-strong?: () -> ndet bool  {
  c  "srandom_is_strong"
  js "_srandom_is_strong"
}
