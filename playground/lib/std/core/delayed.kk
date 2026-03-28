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

reference type computation<e, a>
  con XComputation( action : () -> e a )
  con XDone( value : a )
  con XBlocking

// Delayed (or _lazy_) values are computed (with effect `:e`) only the first time
// `force` is called and cached afterwards.
abstract value type delayed<e,a>
  con XDelay( dref : ref<global,computation<e,a>> )

// Create a new `:delayed` value.
pub fun delay( action : () -> e a ) : delayed<e,a>
  unsafe-total
    val r : ref<global,computation<_,_>> = ref(XComputation(action))
    XDelay(r)

// Create a new `:delayed` value.
pub fun memo( value : a ) : delayed<e, a>
  unsafe-total
    val r : ref<global,computation<_,_>> = ref(XDone(value))
    XDelay(r)

pub fun force/go( delayed : delayed<e,a> ) : <div,read<global>,write<global>,alloc<global>|e> a
  val r = delayed.dref
  match !r
    XDone(x) -> x
    XBlocking -> delayed.go()
    XComputation(action) ->
      r := XBlocking // ensure that action can be unique
      val x = mask-st{ mask<div>(action) }()
      r := XDone(x)
      x

fun unsafe-no-state-div(f : () -> <st<global>,div|e> a): e a
  unsafe-no-state-div-cast(f)()

extern unsafe-no-state-div-cast(f : () -> <st<global>,div|e> a): (() -> e a)
  inline "#1"

// Force a delayed value; the value is computed only on the first
// call to `force` and cached afterwards.
pub fun force( delayed : delayed<e,a> ) : e a
  unsafe-no-state-div
    force/go(delayed)

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
