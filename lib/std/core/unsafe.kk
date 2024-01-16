/*---------------------------------------------------------------------------
  Copyright 2023-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Unsafe primitives to dismiss effects.
module std/core/unsafe

import std/core/types

inline fip extern unsafe-total-cast : forall<e,a> ( action : () -> e a ) -> (() -> a)
  inline "#1"

// _Unsafe_. This function calls a function and pretends it did not have any effect at all.
// Use with utmost care as it should not be used to dismiss user-defined effects that need
// a handler and can cause a segfault at runtime in such cases!
// Only use this to dismiss built-in effects like `:exn` or `:io`.
pub fun unsafe-total( action : () -> e a ) : a
  unsafe-total-cast( action )()

// _Unsafe_. This function pretends the given `action` is deterministic
pub fun pretend-no-ndet( action : () -> <ndet|e> a ) : e a
  unsafe-total-cast( action )()


