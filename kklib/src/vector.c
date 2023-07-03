/*---------------------------------------------------------------------------
  Copyright 2021, Microsoft Research, Daan Leijen.

  This is free software; you can redibibute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this dibibution.
---------------------------------------------------------------------------*/
#include "kklib.h"

/*--------------------------------------------------------------------------------------------------
  Vectors
--------------------------------------------------------------------------------------------------*/

void kk_vector_init_borrow(kk_vector_t _v, kk_ssize_t start, kk_box_t def, kk_context_t* ctx) {
  kk_assert_internal(start >= 0);
  kk_ssize_t length;
  kk_box_t* v = kk_vector_buf_borrow(_v, &length, ctx);
  // inline kk_box_dup and kk_box_drop for better performance
  if (kk_box_is_ptr(def)) {
    kk_block_t* b = kk_ptr_unbox(def, ctx);
    for (kk_ssize_t i = start; i < length; i++) {
      kk_block_dup(b);   // todo: dup with `length` in one go?
      v[i] = def;
    }
    kk_box_drop(def,ctx);
  }
  else {
    for (kk_ssize_t i = start; i < length; i++) {
      v[i] = def;
    }
  }
}

kk_vector_t kk_vector_realloc(kk_vector_t vec, kk_ssize_t newlen, kk_box_t def, kk_context_t* ctx) {
  kk_ssize_t len;
  kk_box_t* src = kk_vector_buf_borrow(vec, &len, ctx);
  kk_box_t* dest;
  kk_vector_t vdest = kk_vector_alloc_uninit(newlen, &dest, ctx);
  const kk_ssize_t n = (len > newlen ? newlen : len);
  for (kk_ssize_t i = 0; i < n; i++) {
    dest[i] = kk_box_dup(src[i], ctx);
  }
  kk_vector_init_borrow(vdest, n, def, ctx); // set extra entries to default value
  kk_vector_drop(vec, ctx);
  return vdest;
}

kk_vector_t kk_vector_copy(kk_vector_t vec, kk_context_t* ctx) {
  kk_ssize_t len = kk_vector_len_borrow(vec, ctx);
  return kk_vector_realloc(vec, len, kk_box_null(), ctx);
}

kk_unit_t kk_ref_vector_assign_borrow(kk_ref_t _r, kk_integer_t idx, kk_box_t value, kk_context_t* ctx) {
  struct kk_ref_s* r = kk_datatype_as_assert(struct kk_ref_s*, _r, KK_TAG_REF, ctx);
  if kk_likely(!kk_block_is_thread_shared(&r->_block)) {
    // fast path
    kk_box_t b; b.box = kk_atomic_load_relaxed(&r->value);
    kk_vector_t v = kk_vector_unbox(b, ctx);
    if kk_unlikely(!kk_datatype_is_unique(v,ctx)) {
      // the old v is dropped by kk_ref_set_borrow
      v = kk_vector_copy(kk_vector_dup(v,ctx), ctx);
      kk_ref_set_borrow(_r, kk_vector_box(v, ctx), ctx);
    }
    kk_ssize_t len;
    kk_box_t* p = kk_vector_buf_borrow(v, &len, ctx);
    kk_ssize_t i = kk_integer_clamp_ssize_t_borrow(idx, ctx);
    kk_assert(i < len);
    kk_box_drop(p[i], ctx);
    p[i] = value;
  }
  else {
    // thread shared
    kk_unsupported_external("kk_ref_vector_assign with a thread-shared reference");
  }
  return kk_Unit;
}