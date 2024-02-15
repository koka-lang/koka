/*---------------------------------------------------------------------------
  Copyright 2023-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Unsafe primitives to dismiss effects.
module std/core/unsafe

import std/core/types

// _Unsafe_. This function calls a function and pretends it did not have any effect at all.
// Use with utmost care as it should not be used to dismiss user-defined effects that need
// a handler and can cause a segfault at runtime in such cases!
//
// You can use `unsafe-total` to dismiss built-in effects without handlers which include:
//
// - behavioral: `:div` (non-termination/divergence), `:ndet` (non-determinism)
// - state: `:alloc`, `:read`, `:write`, `:st`
// - external: `:ui`, `:fsys`, `:net`, `:blocking`
// - combinations: `:io-total` and `:io-noexn`
//
// Do _not_ dismiss `:io` since it has the `:exn` effect that should be handled (and an evidence
// vector should be passed in).
//
// Try to avoid using `unsafe-total` to initialize global values that have a side-effect, but
// use `std/core/delayed/delay` instead:
// ```
// val myglobal = delay( fn() initialize() )
// fun get-global() : e int
//   myglobal.force
// ```
pub fun unsafe-total( action : () -> e a ) : a
  unsafe-total-cast( action )()

inline fip extern unsafe-total-cast : forall<e,a> ( action : () -> e a ) -> (() -> a)
  inline "#1"

// _Unsafe_. This function pretends the given `action` is deterministic
pub fun pretend-no-ndet( action : () -> <ndet|e> a ) : e a
  unsafe-total-cast( action )()

// _Unsafe_. This function checks if two objects have the same address in the heap.
// The results may vary on the backend or compiler optimizations but the function pretends to be `:total`.
// Note also that value types always compare unequal since each value will be boxed fresh in the heap
// when calling this function.
pub inline extern unsafe-ptr-eq( x : a, y : a ) : bool
  c  inline "kk_box_eq(#1,#2)"
  cs inline "Object.ReferenceEquals(#1,#2)"
  js inline "Object.is(#1,#2)"

