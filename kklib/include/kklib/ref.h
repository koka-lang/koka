#pragma once
#ifndef KK_REF_H
#define KK_REF_H
/*---------------------------------------------------------------------------
  Copyright 2020-2022, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------------------
  Mutable references cells
--------------------------------------------------------------------------------------*/

struct kk_ref_s {
  kk_block_t         _block;
  _Atomic(kk_intb_t) value;   
};
typedef kk_datatype_ptr_t kk_ref_t;

kk_decl_export kk_box_t  kk_ref_get_thread_shared(struct kk_ref_s* r, kk_context_t* ctx);
kk_decl_export kk_box_t  kk_ref_swap_thread_shared_borrow(struct kk_ref_s* r, kk_box_t value);
kk_decl_export kk_unit_t kk_ref_vector_assign_borrow(kk_ref_t r, kk_integer_t idx, kk_box_t value, kk_context_t* ctx);

static inline kk_decl_const kk_box_t kk_ref_box(kk_ref_t r, kk_context_t* ctx) {
  kk_unused(ctx);
  return kk_datatype_ptr_box(r);
}

static inline kk_decl_const kk_ref_t kk_ref_unbox(kk_box_t b, kk_context_t* ctx) {
  kk_unused(ctx);
  return kk_datatype_ptr_unbox_assert(b, KK_TAG_REF, ctx);
}

static inline void kk_ref_drop(kk_ref_t r, kk_context_t* ctx) {
  kk_datatype_ptr_drop_assert(r, KK_TAG_REF, ctx);
}

static inline kk_ref_t kk_ref_dup(kk_ref_t r, kk_context_t* ctx) {
  return kk_datatype_ptr_dup_assert(r, KK_TAG_REF, ctx);
}

static inline kk_ref_t kk_ref_alloc(kk_box_t value, kk_context_t* ctx) {
  struct kk_ref_s* r = kk_block_alloc_as(struct kk_ref_s, 1, KK_TAG_REF, ctx);
  kk_atomic_store_relaxed(&r->value,value.box);
  return kk_datatype_from_base(r,ctx);
}

static inline kk_box_t kk_ref_get(kk_ref_t _r, kk_context_t* ctx) {
  struct kk_ref_s* r = kk_datatype_as_assert(struct kk_ref_s*, _r, KK_TAG_REF, ctx);
  if kk_likely(!kk_block_is_thread_shared(&r->_block)) {
    // fast path
    kk_box_t b; b.box = kk_atomic_load_relaxed(&r->value);
    kk_box_dup(b,ctx);
    kk_block_drop(&r->_block,ctx);    // TODO: make references borrowed (only get left)
    return b;
  }
  else {
    // thread shared
    return kk_ref_get_thread_shared(r,ctx);
  }  
}

static inline kk_box_t kk_ref_swap_borrow(kk_ref_t _r, kk_box_t value, kk_context_t* ctx) {
  struct kk_ref_s* r = kk_datatype_as_assert(struct kk_ref_s*, _r, KK_TAG_REF, ctx);
  if kk_likely(!kk_block_is_thread_shared(&r->_block)) {
    // fast path
    kk_box_t b; b.box = kk_atomic_load_relaxed(&r->value);
    kk_atomic_store_relaxed(&r->value, value.box);
    return b;
  }
  else {
    // thread shared
    return kk_ref_swap_thread_shared_borrow(r, value);
  }
}


static inline kk_unit_t kk_ref_set_borrow(kk_ref_t r, kk_box_t value, kk_context_t* ctx) {
  kk_box_t b = kk_ref_swap_borrow(r, value, ctx);
  kk_box_drop(b, ctx);
  return kk_Unit;
}

// In Koka we can constrain the argument of f to be a local-scope reference.
static inline kk_box_t kk_ref_modify(kk_ref_t r, kk_function_t f, kk_context_t* ctx) {
  return kk_function_call(kk_box_t,(kk_function_t,kk_ref_t,kk_context_t*),f,(f,r,ctx),ctx);
}



#endif // KK_REF_H
