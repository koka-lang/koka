/*---------------------------------------------------------------------------
  Copyright 2023-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Unsafe primitives to dismiss divergence.
module std/core/undiv

import std/core/types

// _Unsafe_. Mark a function parameter as decreasing to suppress the non-termination effect (`:div`).
// The compiler has a very limited termination analysis, only recognizing direct recursion on
// inductive datatypes. When it is clear a function has a decreasing parameter but it is not recognized
// as such by the compliler, you can wrap the arguments in `pretend-decreasing` to inform the
// termination analysis to consider that parameter decreasing on a recursive call. Use with care
// to not accidentally allow actual divergence (without having that reflected in the effect type).
pub inline fip extern pretend-decreasing( x : a ) : a
  inline "#1"

// _Unsafe_. This function pretends the give action is terminating.
pub fun pretend-no-div( action : () -> <div|e> a ) : e a
  unsafe-total-cast( action )()

inline fip extern unsafe-total-cast : forall<e,a> ( action : () -> e a ) -> (() -> a)
  inline "#1"

