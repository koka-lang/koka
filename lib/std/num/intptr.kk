/*---------------------------------------------------------------------------
  Copyright 2012-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// `intptr_t` signed integer.
//
// A signed two's complement integer equal to an `intptr_t` type in C, i.e.
// it has the same bit-size as a native pointer (`void*`).
// Provides no operations and used only for storage and interaction with external code.
// The Javascript backend uses `int64` for this.
// See also: `:ssize_t`
module std/num/intptr

// clamp an `:int` to fit in an `:intptr_t`.
pub fip extern intptr_t( i : int) : intptr_t
  c  "kk_integer_clamp_intptr_t"
  cs "Primitive.IntToInt64"
  js "$std_core_types._int_clamp64"

// Convert an `:intptr_t` to an `:int`.
pub inline fip extern int( i : intptr_t ) : int
  c  "kk_integer_from_intptr_t"
  cs inline "(new BigInteger(#1))"
  js "$std_core_types._int_from_int64"

