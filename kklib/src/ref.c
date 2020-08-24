/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
#include "kklib.h"


// Atomic path for mutable references
kk_decl_export kk_box_t kk_ref_get_thread_shared(kk_ref_t r, kk_context_t* ctx) {
  // careful: we cannot first read and then dup the read value as it may be 
  // overwritten and _dropped_ by another thread in between. To avoid this
  // situation we first atomically swap with a guard value 0, then dup, and 
  // write back the old value again.
again: ;
  kk_box_t b; 
  b.box = kk_atomic_load_relaxed(&r->value);
  do {
    if (kk_box_is_value(b)) return b;  // optimize: if it is a raw value (that is not heap allocated), we can immediately return
    if (b.box == 0) { b.box = 1; }     // expect any value but 0
  } while (!kk_atomic_cas_weak_relaxed(&r->value, &b.box, 0));
  // we got it, and hold the "locked" reference (`r->value == 0`)
  kk_box_dup(b);
  // and release our lock by writing back `b`    
  uintptr_t guard = 0;
  while (!kk_atomic_cas_strong_relaxed(&r->value, &guard, b.box)) { 
    assert(false); 
    // should never happen! as a last resort, restart the operation
    kk_box_drop(b,ctx);
    goto again;
  }
  kk_ref_drop(r, ctx);
  return b;
}

kk_decl_export kk_box_t kk_ref_swap_thread_shared(kk_ref_t r, kk_box_t value, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  // atomically swap, but not if guarded with 0 (to not interfere with a `ref_get`)
  uintptr_t exp = kk_atomic_load_relaxed(&r->value);
  do {
    if (exp==0) { exp = 1; }  // any value but 0
  } while (!kk_atomic_cas_weak_relaxed(&r->value, &exp, value.box));
  kk_ref_drop(r, ctx);
  return _kk_box_new(exp);
}


