/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
#include "kklib.h"


// Atomic path for mutable references
kk_decl_export kk_box_t kk_ref_get_thread_shared(struct kk_ref_s* r, kk_context_t* ctx) {
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
  kk_box_dup(b,ctx);
  // and release our lock by writing back `b`    
  kk_intb_t guard = 0;
  while (!kk_atomic_cas_strong_relaxed(&r->value, &guard, b.box)) {
    assert(false); 
    // should never happen! as a last resort, restart the operation
    kk_box_drop(b,ctx);
    goto again;
  }
  kk_block_drop(&r->_block, ctx);
  return b;
}

kk_decl_export kk_box_t kk_ref_swap_thread_shared_borrow(struct kk_ref_s* r, kk_box_t value, kk_context_t* ctx) {
  // once stored, `value` is reachable from other threads, so it must be marked
  // thread-shared before the swap publishes it: a reader can dup it right after
  kk_box_mark_shared(value, ctx);
  // atomically swap, but not if guarded with 0 (to not interfere with a `ref_get`)
  kk_box_t b; 
  b.box = kk_atomic_load_relaxed(&r->value);
  //uintptr_t exp = kk_atomic_load_relaxed(&r->value);
  do {
    if (b.box==0) { b.box = 1; }  // any value but 0
  } while (!kk_atomic_cas_weak_relaxed(&r->value, &b.box, value.box));
  return b;
}

// Replace the value of a reference with `update(value)`. The value is handed to
// `update` with its ownership, so a unique value can be updated in place; the
// cell holds the moved value until the store, which is safe because `update` is
// total and cannot read the reference.
kk_decl_export kk_unit_t kk_ref_update_borrow(kk_ref_t _r, kk_function_t update, kk_context_t* ctx) {
  struct kk_ref_s* r = kk_datatype_as_assert(struct kk_ref_s*, _r, KK_TAG_REF, ctx);
  if kk_likely(!kk_block_is_thread_shared(&r->_block)) {
    kk_box_t b;
    b.box = kk_atomic_load_relaxed(&r->value);
    kk_box_t newb = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), update, (update, b, ctx), ctx);
    kk_atomic_store_relaxed(&r->value, newb.box);
  }
  else {
    // `update` cannot be applied atomically to a reference shared between threads
    kk_function_drop(update, ctx);
    kk_unsupported_external("kk_ref_update_borrow with a thread-shared reference");
  }
  return kk_Unit;
}
