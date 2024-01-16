/*---------------------------------------------------------------------------
  Copyright 2012-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// 16-bit signed integers.
//
// 16-bit signed two's complement representation.
// Provides no operations and used only for storage and interaction with external code.


// clamp an `:int` to fit in an `:int16`.
pub fip extern int16( i : int) : int16
  c  "kk_integer_clamp_int16"
  cs "Primitive.IntToInt16"
  js "$std_core_types._int_clamp16"

// Convert an `:int16` to an `:int`.
pub inline fip extern int16/int( i : int16 ) : int
  c  "kk_integer_from_int16"
  cs inline "(new BigInteger(#1))"
  js "$std_core_types._int_from_int32"

