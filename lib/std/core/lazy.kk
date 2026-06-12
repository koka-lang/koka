/*---------------------------------------------------------------------------
  Copyright 2012-2025, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Internal functions to support lazy constructors
//
module std/core/lazy

import std/core/types

extern import 
  js file "inline/lazy.js"

// note: all of the following definitions will be made hidden (prefix with `@`)
// in the future and are only exposed for now for experimentation

// internal: marker for lazy values that will be memoized (used to for correct reuse and reference counting)
pub inline fip extern memoize-target(^target : a, size : int, scan-size : int) : ()
  c  inline "/**/"      // optimized away
  js inline "null"

// internal: explicitly force update-in-place for lazy values
pub fip extern memoize(target : a, x : a) : a
  c inline "#1"         // optimized away during codegen (reuse analysis)
  js "kk_lazy_memoize"  // todo: optimize this as part of codegen

// internal: atomically set a lazy value to locked (or return `false` if already memoized)
pub inline fip extern atomic-enter(^target : a, indirect-tag: int32 ) : bool
  c  "kk_lazy_atomic_enter"
  js "$std_core_lazy.kk_lazy_atomic_enter"

// internal: release an atomically memoized lazy value
pub inline fip extern atomic-leave(target : a) : ()
  c  "kk_lazy_atomic_leave"
  js "$std_core_lazy.kk_lazy_atomic_leave"


// internal: test if a lazy value is in whnf
pub inline fip extern datatype-is-whnf(^x:a, indirect-tag:int32) : bool
  c "kk_datatype_is_whnf"
  js inline "(#1._tag < #2)"

// internal: test if a heap allocated lazy value is in whnf
pub inline fip extern datatype-ptr-is-whnf(^x:a, indirect-tag:int32) : bool
  c  "kk_datatype_ptr_is_whnf"
  js inline "(#1._tag < #2)"


// internal: test if a heap allocated data type is unique
pub inline fip extern datatype-ptr-is-unique(^x:a) : bool
  c "kk_datatype_ptr_is_unique"
  js inline "false"

// internal: test if a heap allocated data type is thread-shared
pub inline fip extern datatype-ptr-is-thread-shared(^x : a) : bool
  c "kk_datatype_ptr_is_thread_shared"
  js inline "false"


// internal: compress indirection chains
pub fip extern indirect-compress(value : a, indirect-tag:int32) : a
  c  "kk_indirect_compress"
  js "kk_indirect_compres"

val internal = 42
