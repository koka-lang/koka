/*---------------------------------------------------------------------------
  Copyright 2023-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Delayed computations.
module std/core/delayed

import std/core/types
import std/core/hnd
import std/core/unsafe

// ----------------------------------------------------------------------------
// Delayed values
// ----------------------------------------------------------------------------

// Delayed (or _lazy_) values are computed (with effect `:e`) only the first time
// `force` is called and cached afterwards.
abstract value type delayed<e,a>
  con XDelay( dref : ref<global,either<() -> e a,a>> )

// Create a new `:delayed` value.
pub fun delay( action : () -> e a ) : delayed<e,a>
  unsafe-total
    val r : ref<global,either<_,_>> = ref(Left(action))
    XDelay(r)

// Force a delayed value; the value is computed only on the first
// call to `force` and cached afterwards.
pub fun force( delayed : delayed<e,a> ) : e a
  unsafe-total
    val r = delayed.dref
    match !r
      Right(x) -> x
      Left(action) ->
        val x = mask-st{ mask<div>(action) }()
        r := Right(x)
        x

// Given a total function to calculate a value `:a`, return
// a total function that only calculates the value once and then
// returns the cached result.
pub fun once( calc : () -> a ) : (() -> a)
  unsafe-total
    val r = ref(Nothing)
    return fn()
      unsafe-total
        match !r
          Just(x) -> x
          Nothing ->
            val x = calc()
            r := Just(x)
            x
