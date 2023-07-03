#pragma once
#ifndef KK_VECTOR_H
#define KK_VECTOR_H
/*---------------------------------------------------------------------------
  Copyright 2020-2022, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/


/*--------------------------------------------------------------------------------------
  Vectors : arrays of boxed values
--------------------------------------------------------------------------------------*/

typedef struct kk_vector_large_s {  // always use a large block for a vector so the offset to the elements is fixed
  struct kk_block_large_s _base;
  kk_box_t                vec[1];               // vec[(large_)scan_fsize - 1]
} *kk_vector_large_t;


static inline kk_decl_const kk_vector_t kk_vector_empty(void) {
  return kk_datatype_from_tag((kk_tag_t)1);
}

static inline kk_decl_pure kk_vector_large_t kk_vector_as_large_borrow(kk_vector_t v, kk_context_t* ctx) {
  if (kk_datatype_is_singleton(v)) {
    return NULL;
  }
  else {
    return kk_datatype_as_assert(kk_vector_large_t, v, KK_TAG_VECTOR, ctx);
  }
}

static inline void kk_vector_drop(kk_vector_t v, kk_context_t* ctx) {
  kk_datatype_drop(v, ctx);
}

static inline kk_vector_t kk_vector_dup(kk_vector_t v, kk_context_t* ctx) {
  return kk_datatype_dup(v,ctx);
}

static inline kk_vector_t kk_vector_alloc_uninit(kk_ssize_t length, kk_box_t** buf, kk_context_t* ctx) {
  if kk_unlikely(length<=0) {
    if (buf != NULL) *buf = NULL;
    return kk_vector_empty();
  }
  else {
    kk_vector_large_t v = (kk_vector_large_t)kk_block_large_alloc(
        kk_ssizeof(struct kk_vector_large_s) + (length-1)*kk_ssizeof(kk_box_t),  // length-1 as the vector_large_s already includes one element 
        length + 1, // +1 to include the kk_large_scan_fsize field itself 
        KK_TAG_VECTOR, ctx);
    if (buf != NULL) *buf = &v->vec[0];
    return kk_datatype_from_base(&v->_base,ctx);
  }
}

kk_decl_export void        kk_vector_init_borrow(kk_vector_t _v, kk_ssize_t start, kk_box_t def, kk_context_t* ctx);
kk_decl_export kk_vector_t kk_vector_realloc(kk_vector_t vec, kk_ssize_t newlen, kk_box_t def, kk_context_t* ctx);
kk_decl_export kk_vector_t kk_vector_copy(kk_vector_t vec, kk_context_t* ctx);

static inline kk_vector_t kk_vector_alloc(kk_ssize_t length, kk_box_t def, kk_context_t* ctx) {
  kk_vector_t v = kk_vector_alloc_uninit(length, NULL, ctx);
  kk_vector_init_borrow(v, 0, def, ctx);
  return v;
}

static inline kk_box_t* kk_vector_buf_borrow(kk_vector_t vd, kk_ssize_t* len, kk_context_t* ctx) {
  kk_vector_large_t v = kk_vector_as_large_borrow(vd,ctx);
  if kk_unlikely(v==NULL) {
    if (len != NULL) *len = 0;
    return NULL;
  }
  else {
    if (len != NULL) {
      *len = (kk_ssize_t)kk_intf_unbox(v->_base.large_scan_fsize) - 1;  // exclude the large scan_fsize field itself
      kk_assert_internal(*len + 1 == kk_block_scan_fsize(&v->_base._block));
      kk_assert_internal(*len > 0);
    }
    return &(v->vec[0]);
  }
}

static inline kk_decl_pure kk_ssize_t kk_vector_len_borrow(const kk_vector_t v, kk_context_t* ctx) {
  kk_ssize_t len;
  kk_vector_buf_borrow(v, &len, ctx);
  return len;
}

static inline kk_ssize_t kk_vector_len(const kk_vector_t v, kk_context_t* ctx) {
  kk_ssize_t len = kk_vector_len_borrow(v,ctx);
  kk_vector_drop(v, ctx);
  return len;
}

static inline kk_box_t kk_vector_at_borrow(const kk_vector_t v, kk_ssize_t i, kk_context_t* ctx) {
  kk_assert(i < kk_vector_len_borrow(v,ctx));
  kk_box_t res = kk_box_dup(kk_vector_buf_borrow(v, NULL, ctx)[i],ctx);
  return res;
}

static inline kk_decl_const kk_box_t kk_vector_box(kk_vector_t v, kk_context_t* ctx) {
  kk_unused(ctx);
  return kk_datatype_box(v);
}

static inline kk_decl_const kk_vector_t kk_vector_unbox(kk_box_t v, kk_context_t* ctx) {
  kk_unused(ctx);
  return kk_datatype_unbox(v);
}



#endif // KK_VECTOR_H
