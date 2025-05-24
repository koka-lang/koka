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

kk_decl_export kk_box_t kk_ref_swap_thread_shared_borrow(struct kk_ref_s* r, kk_box_t value) {
  // atomically swap, but not if guarded with 0 (to not interfere with a `ref_get`)
  kk_box_t b; 
  b.box = kk_atomic_load_relaxed(&r->value);
  //uintptr_t exp = kk_atomic_load_relaxed(&r->value);
  do {
    if (b.box==0) { b.box = 1; }  // any value but 0
  } while (!kk_atomic_cas_weak_relaxed(&r->value, &b.box, value.box));
  return b;
}

// TODO: inline this function?
// Update is owned since it is likely a closure, copy is borrowed since it is likely a static function.
kk_decl_export kk_unit_t kk_ref_update_borrow(kk_ref_t _r, kk_function_t update, kk_context_t* ctx) {
  struct kk_ref_s* r = kk_datatype_as_assert(struct kk_ref_s*, _r, KK_TAG_REF, ctx);
  if kk_likely(!kk_block_is_thread_shared(&r->_block)) {
    // fast path
    kk_box_t b; 
    b.box = kk_atomic_load_relaxed(&r->value);    
    // kk_assert_internal(kk_datatype_is_unique(v,ctx)); The returned value should be unique (in place update), but `update` is not constrained to do that
    kk_box_t newb = kk_function_call(kk_box_t, (kk_function_t, kk_box_t, kk_context_t*), update, (update, b, ctx), ctx);
    // Reference types should not change the box (in place updates)
    // However, `update` could be used for other purposes (allocating a new value, extending a current one, etc.)
    kk_atomic_store_relaxed(&r->value, newb.box);
    return kk_Unit;
  }
  else {
    // Update is dropped in the other branch (since ownership is passed to the called function) but we need to drop it here as well.
    kk_function_drop(update, ctx);
    // It doesn't matter anyways, because we don't support updates to a thread-shared reference.
    // thread shared
    kk_unsupported_external("kk_ref_update_borrow with a thread-shared reference");
  }
  return kk_Unit;
}