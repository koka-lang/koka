/*---------------------------------------------------------------------------
  Copyright 2024, Microsoft Research, Daan Leijen, Anton Lorenzen

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

#include "kklib.h"
#include "kklib/lazy.h"

// We cannot assume the platform support byte sized atomic operations
// To access the `_field_idx` atomically, we access the entire header atomically as a `uint64_t`.
// Hopefully this interacts well with the overlapping atomic refcount operations (as a `uint32_t`).
// Otherwise we need to revise this to use a bit from the reference count as the blocking bit.

static void kk_header_atomic_load_relaxed(kk_header_t* dest /*out*/, kk_header_t* src /*in*/) {
  *((kk_uint_header_t*)dest) = kk_atomic_load_relaxed((const _Atomic(kk_uint_header_t)*)src);
}

static void kk_header_atomic_load_acquire(kk_header_t* dest /*out*/, kk_header_t* src /*in*/) {
  *((kk_uint_header_t*)dest) = kk_atomic_load_acquire((const _Atomic(kk_uint_header_t)*)src);
}

// static void kk_header_atomic_store_release(kk_header_t* dest /*out*/, kk_header_t* src /*in*/) {
//   kk_atomic_store_release((_Atomic(kk_uint_header_t)*)dest, *((kk_uint_header_t*)src));
// }

static void kk_header_copy(kk_header_t* dest /*out*/, kk_header_t* src /*in*/) {
  *((kk_uint_header_t*)dest) = *((kk_uint_header_t*)src);
}

static bool kk_header_compare_and_swap(kk_header_t* dest /*out*/, kk_header_t* expected /*in-out*/, kk_header_t* desired /*in*/) {
  return kk_atomic_cas_strong_acq_rel((_Atomic(kk_uint_header_t)*)dest, (kk_uint_header_t*)expected, *((kk_uint_header_t*)desired));
}


static void kk_lazy_atomic_wait(kk_block_t* b, int32_t indirect_tag, kk_context_t* ctx) {
  // TODO: if we eval recursively, we could busy wait on ourselves
  kk_assert(kk_block_is_thread_shared(b));
  kk_unused(indirect_tag); kk_unused(ctx);
  kk_header_t header;
  do {
    kk_atomic_yield(); // TODO: improve the busy wait
    kk_header_atomic_load_acquire(&header, &b->header);
  } while (header._field_idx == KK_FIELD_IDX_LAZY_BLOCKED);
  kk_assert(header.tag <= indirect_tag);
}

kk_decl_export bool kk_lazy_atomic_thread_enter(kk_block_t* b /* borrow */, int32_t indirect_tag, kk_context_t* ctx) {
  kk_assert(kk_block_is_thread_shared(b));
  kk_header_t blocked_header;
  kk_header_t header;
  kk_header_atomic_load_relaxed(&header, &b->header);
  do {
    if (header.tag <= indirect_tag) {
      // already eval'd
      return false;
    }
    else if (header._field_idx == KK_FIELD_IDX_LAZY_BLOCKED) {
      // todo: if it is the same thread, change this in to a Lazy_blocked constructor and return?
      kk_lazy_atomic_wait(b,indirect_tag,ctx);
      return false;
    }
    kk_assert(header.tag <= KK_TAG_MAX);
    kk_header_copy(&blocked_header, &header);
    blocked_header._field_idx = KK_FIELD_IDX_LAZY_BLOCKED;
  } while (!kk_header_compare_and_swap(&b->header, &header, &blocked_header));
  return true;
}

kk_decl_export void kk_lazy_atomic_thread_leave(kk_block_t* b /* own */, kk_context_t* ctx) {
  kk_assert(kk_block_is_thread_shared(b));
  kk_header_t unblocked_header;
  kk_header_t header;
  kk_header_atomic_load_relaxed(&header, &b->header);
  do {
    kk_assert(header._field_idx == KK_FIELD_IDX_LAZY_BLOCKED);
    kk_header_copy(&unblocked_header, &header);
    unblocked_header._field_idx = 0;
  } while (!kk_header_compare_and_swap(&b->header, &header, &unblocked_header));
  kk_block_drop(b,ctx);
}


kk_decl_export kk_datatype_t kk_indirect_compress_all( kk_datatype_t root, int32_t indirect_tag, kk_context_t* ctx ) {
  kk_assert(kk_datatype_is_indirection(root,indirect_tag,ctx));
  // walk the indirections to find the final value `val`
  kk_block_t* b = kk_datatype_as_ptr(root,ctx);
  kk_block_t* last;
  kk_box_t val;
  do {
    val = kk_block_field(b,0);
    b = (kk_box_is_ptr(val) ? kk_box_to_ptr(val,ctx) : NULL);
  } while( b!=NULL && kk_block_tag(b) == (kk_tag_t)indirect_tag);

  // walk again and update all indirections with `val`
  b = kk_datatype_as_ptr(root,ctx);
  do {
    kk_box_t next = kk_block_field(b,0);
    // free or update-in-place as we traverse
    if (kk_block_is_unique(b)) {
      kk_block_free(b,ctx);
    }
    else {
      kk_block_decref(b,ctx);
      kk_block_field_set(b,0,kk_box_dup(val,ctx));
    }
    b = (kk_box_is_ptr(next) ? kk_box_to_ptr(next,ctx) : NULL);
  } while( b!=NULL && kk_block_tag(b) == (kk_tag_t)indirect_tag);
  return kk_datatype_unbox(val);
}


/*
kk_decl_export kk_datatype_t kk_lazy_atomic_eval(kk_box_t lazy, kk_context_t* ctx) {
  kk_block_t* b = kk_datatype_as_ptr(kk_datatype_unbox(lazy), ctx);
  kk_assert(kk_block_is_thread_shared(b));
  kk_header_t prep_header;
  kk_header_t header;
  kk_header_atomic_load_relaxed(&header, &b->header);
  do {
    if (header.tag >= KK_TAG_LAZY_EVAL) {
      return kk_datatype_Nothing();  // already being claimed (Lazy-wait)
    }
    kk_header_copy(&prep_header, &header);
    prep_header.tag = KK_TAG_LAZY_PREP;
  } while (!kk_header_compare_and_swap(&b->header, &header, &prep_header));

  // now make it a blackhole copying the object and changing the tag
  kk_block_t* x = kk_block_alloc_copy(b, ctx);
  x->header.tag = header.tag;               // restore the tag
  kk_block_field_set(b, 0, kk_box_null());  // todo: initial wait mutex

  // set the header it to eval (to signify field[0] (the wait mutex) can be accessed )
  kk_header_t eval_header;
  kk_header_copy(&eval_header,&prep_header);
  eval_header.tag = KK_TAG_LAZY_EVAL;
  eval_header.scan_fsize = 1;          // the wait mutex
  kk_header_atomic_store_release(&b->header,&eval_header);
  // return the copy
  return kk_datatype_from_ptr(x,ctx);       // return Lazy-eval(copy)
}

kk_decl_export kk_box_t kk_lazy_atomic_wait(kk_box_t lazy, kk_context_t* ctx) {
  kk_block_t* b = kk_datatype_as_ptr(kk_datatype_unbox(lazy), ctx);
  kk_assert(kk_block_is_thread_shared(b));
  kk_header_t header;
  do {
    kk_atomic_yield();
    kk_header_atomic_load_acquire(&header, &b->header);
  } while (header.tag >= KK_TAG_LAZY_EVAL);
  return lazy;
}

kk_decl_export void kk_lazy_atomic_set_header(kk_header_t* dest, kk_ssize_t scan_fsize, kk_tag_t tag, kk_context_t* ctx) {
  kk_assert(scan_fsize >= 0 && scan_fsize <= KK_SCAN_FSIZE_MAX);
  kk_header_t new_header;
  kk_header_t header;
  kk_header_atomic_load_relaxed(&header, dest);
  do {
    kk_assert(header.tag >= KK_TAG_LAZY_EVAL);
    kk_header_copy(&new_header, &header);
    new_header.scan_fsize = (uint8_t)scan_fsize;
    new_header.tag = tag;
  } while (!kk_header_compare_and_swap(dest, &header, &new_header));
}
*/