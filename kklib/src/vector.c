/*---------------------------------------------------------------------------
  Copyright 2021 Daan Leijen, Microsoft Corporation.

  This is free software; you can redibibute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this dibibution.
---------------------------------------------------------------------------*/
#include "kklib.h"

/*--------------------------------------------------------------------------------------------------
  Vectors
--------------------------------------------------------------------------------------------------*/

void kk_vector_init_borrow(kk_vector_t _v, kk_ssize_t start, kk_box_t def, kk_context_t* ctx) {
  kk_assert_internal(start >= 0);
  kk_ssize_t length;
  kk_box_t* v = kk_vector_buf_borrow(_v, &length);
  // inline kk_box_dup and kk_box_drop for better performance
  if (kk_box_is_ptr(def)) {
    kk_block_t* b = kk_ptr_unbox(def);
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
  kk_box_t* src = kk_vector_buf_borrow(vec, &len);
  if (len == newlen) return vec;
  kk_box_t* dest;
  kk_vector_t vdest = kk_vector_alloc_uninit(newlen, &dest, ctx);
  const kk_ssize_t n = (len > newlen ? newlen : len);
  for (kk_ssize_t i = 0; i < n; i++) {
    dest[i] = kk_box_dup(src[i]);
  }
  kk_vector_init_borrow(vdest, n, def, ctx); // set extra entries to default value
  kk_vector_drop(vec, ctx);
  return vdest;
}