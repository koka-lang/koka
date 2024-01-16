/*---------------------------------------------------------------------------
  Copyright 2012-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// 8-bit signed integers.
//
// 8-bit signed two's complement representation
// Provides no operations and used only for storage and interaction with external code.
module std/num/int8

// clamp an `:int` to fit in an `:int8`.
pub fip extern int8( i : int) : int8
  c  "kk_integer_clamp_int8"
  cs "Primitive.IntToInt8"
  js "$std_core_types._int_clamp8"

// Convert an `:int8` to an `:int`.
pub inline fip extern int( i : int8 ) : int
  c  "kk_integer_from_int8"
  cs inline "(new BigInteger(#1))"
  js "$std_core_types._int_from_int32"


// clamp an `:int` to fit in an `:int8` but interpret the `:int` as an unsigned 8-bit value,
// and clamp between 0 and 255.
pub fip extern uint8( i : int) : int8
  c  "kk_integer_clamp_byte"
  cs "Primitive.IntToUInt8"
  js "$std_core_types._int_clamp_byte"

// Convert an `:int8` to an `:int` but interpret the `:int8` as an unsigned 8-bit value between 0 and 255.
pub inline fip extern uint( i : int8 ) : int
  c  "kk_integer_from_uint8"
  cs inline "(new BigInteger(#1 >= 0 ? #1 : 256 + #1))"
  js "$std_core_types._int_from_int32"

