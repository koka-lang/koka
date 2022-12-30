/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
#include "kklib.h"

/*----------------------------------------------------------------
  Value type boxing
----------------------------------------------------------------*/

void kk_valuetype_unbox_from_any(kk_box_t* p, size_t size, kk_box_t box, kk_context_t* ctx) {
  const size_t max_scan_fsize = size / sizeof(kk_box_t);
  for (size_t i = 0; i < max_scan_fsize; i++) {
    p[i] = kk_box_any(ctx);
  }
  kk_block_decref(kk_block_unbox(box, KK_TAG_BOX_ANY, ctx), ctx);
}

/*----------------------------------------------------------------
  Integer boxing
----------------------------------------------------------------*/

typedef struct kk_boxed_intptr_s {
  kk_block_t  _block;
  intptr_t     value;
} *boxed_intptr_t;

intptr_t kk_intptr_unbox(kk_box_t v, kk_context_t* ctx) {
  if kk_likely(kk_box_is_value(v)) {
    kk_intf_t i = kk_intf_unbox(v);
    return (intptr_t)i;
  }
  else {
    kk_assert_internal((kk_box_is_ptr(v) && kk_block_tag(kk_ptr_unbox(v,ctx)) == KK_TAG_INTPTR) || kk_box_is_any(v));
    boxed_intptr_t bi = kk_block_assert(boxed_intptr_t, kk_ptr_unbox(v,ctx), KK_TAG_INTPTR);
    intptr_t i = bi->value;
    if (ctx!=NULL) { kk_block_drop(&bi->_block, ctx); }
    return i;
  }
}

kk_box_t kk_intptr_box(intptr_t i, kk_context_t* ctx) {
  if (i >= KK_INTF_BOX_MIN(0) && i <= KK_INTF_BOX_MAX(0)) {
    return kk_intf_box((kk_intf_t)i);
  }
  else {
    boxed_intptr_t bi = kk_block_alloc_as(struct kk_boxed_intptr_s, 0, KK_TAG_INTPTR, ctx);
    bi->value = i;
    return kk_ptr_box(&bi->_block,ctx);
  }
}


#if (KK_INTF_SIZE <= 8) 
typedef struct kk_boxed_int64_s {
  kk_block_t  _block;
  int64_t     value;
} *boxed_int64_t;

int64_t kk_int64_unbox(kk_box_t v, kk_context_t* ctx) {
  if kk_likely(kk_box_is_value(v)) {
    kk_intf_t i = kk_intf_unbox(v);
    return (int64_t)i;
  }
  else {
    kk_assert_internal((kk_box_is_ptr(v) && kk_block_tag(kk_ptr_unbox(v,ctx)) == KK_TAG_INT64) || kk_box_is_any(v));
    boxed_int64_t bi = kk_block_assert(boxed_int64_t, kk_ptr_unbox(v,ctx), KK_TAG_INT64);
    int64_t i = bi->value;
    if (ctx!=NULL) { kk_block_drop(&bi->_block, ctx); }
    return i;
  }
}

kk_box_t kk_int64_box(int64_t i, kk_context_t* ctx) {
  if (i >= KK_INTF_BOX_MIN(0) && i <= KK_INTF_BOX_MAX(0)) {
    return kk_intf_box((kk_intf_t)i);
  }
  else {
    boxed_int64_t bi = kk_block_alloc_as(struct kk_boxed_int64_s, 0, KK_TAG_INT64, ctx);
    bi->value = i;
    return kk_ptr_box(&bi->_block,ctx);
  }
}
#endif


#if (KK_INTF_SIZE <= 4)
typedef struct kk_boxed_int32_s {
  kk_block_t  _block;
  int32_t  value;
} *boxed_int32_t;

int32_t kk_int32_unbox(kk_box_t v, kk_context_t* ctx) {
  if kk_likely(kk_box_is_value(v)) {
    kk_intf_t i = kk_intf_unbox(v);
    kk_assert_internal((i >= INT32_MIN && i <= INT32_MAX) || kk_box_is_any(v));
    return (int32_t)i;
  }
  else {
    kk_assert_internal((kk_box_is_ptr(v) && kk_block_tag(kk_ptr_unbox(v,ctx)) == KK_TAG_INT32) || kk_box_is_any(v));
    boxed_int32_t bi = kk_block_assert(boxed_int32_t, kk_ptr_unbox(v,ctx), KK_TAG_INT32);
    int32_t i = bi->value;
    if (ctx!=NULL) { kk_block_drop(&bi->_block, ctx); }
    return i;
  }
}

kk_box_t kk_int32_box(int32_t i, kk_context_t* ctx) {
  if (i >= KK_INTF_BOX_MIN(0) && i <= KK_INTF_BOX_MAX(0)) {
    return kk_intf_box(i);
  }
  else {
    boxed_int32_t bi = kk_block_alloc_as(struct kk_boxed_int32_s, 0, KK_TAG_INT32, ctx);
    bi->value = i;
    return kk_ptr_box(&bi->_block,ctx);
  }
}
#endif

#if (KK_INTF_SIZE <= 2)
typedef struct kk_boxed_int16_s {
  kk_block_t  _block;
  int16_t  value;
} *boxed_int16_t;

int16_t kk_int16_unbox(kk_box_t v, kk_context_t* ctx) {
  if kk_likely(kk_box_is_value(v)) {
    kk_intf_t i = kk_intf_unbox(v);
    kk_assert_internal((i >= int16_MIN && i <= int16_MAX) || kk_box_is_any(v));
    return (int16_t)i;
  }
  else {
    kk_assert_internal((kk_box_is_ptr(v) && kk_block_tag(kk_ptr_unbox(v,ctx)) == KK_TAG_INT16) || kk_box_is_any(v));
    boxed_int16_t bi = kk_block_assert(boxed_int16_t, kk_ptr_unbox(v,ctx), KK_TAG_INT16);
    int16_t i = bi->value;
    if (ctx!=NULL) { kk_block_drop(&bi->_block, ctx); }
    return i;
  }
}

kk_box_t kk_int16_box(int16_t i, kk_context_t* ctx) {
  if (i >= KK_INTF_BOX_MIN && i <= KK_INTF_BOX_MAX) {
    return kk_intf_box(i);
  }
  else {
    boxed_int16_t bi = kk_block_alloc_as(struct kk_boxed_int16_s, 0, KK_TAG_INT16, ctx);
    bi->value = i;
    return kk_ptr_box(&bi->_block);
  }
}
#endif

#if KK_SSIZE_SIZE == KK_INTF_SIZE
kk_box_t kk_ssize_box(kk_ssize_t i, kk_context_t* ctx) {
  return kk_intptr_box(i, ctx);
}
kk_ssize_t kk_ssize_unbox(kk_box_t b, kk_context_t* ctx) {
  return kk_intptr_unbox(b, ctx);
}
#elif KK_SSIZE_SIZE == 8
kk_box_t kk_ssize_box(kk_ssize_t i, kk_context_t* ctx) {
  return kk_int64_box(i, ctx);
}
kk_ssize_t kk_ssize_unbox(kk_box_t b, kk_context_t* ctx) {
  return kk_int64_unbox(b, ctx);
}
#elif KK_SSIZE_SIZE == 4
kk_box_t kk_ssize_box(kk_ssize_t i, kk_context_t* ctx) {
  return kk_int32_box(i, ctx);
}
kk_ssize_t kk_ssize_unbox(kk_box_t b, kk_context_t* ctx) {
  return kk_int32_unbox(b, ctx);
}
#elif KK_SSIZE_SIZE == 2
kk_box_t kk_ssize_box(kk_ssize_t i, kk_context_t* ctx) {
  return kk_int16_box(i, ctx);
}
kk_ssize_t kk_ssize_unbox(kk_box_t b, kk_context_t* ctx) {
  return kk_int16_unbox(b, ctx);
}
#else
#error "platform size_t must be 16, 32, 64, or 128 bits"
#endif


/*----------------------------------------------------------------
  Pointers
----------------------------------------------------------------*/


// C pointers
kk_box_t kk_cptr_raw_box(kk_free_fun_t* freefun, void* p, kk_context_t* ctx) {
  kk_cptr_raw_t raw = kk_block_alloc_as(struct kk_cptr_raw_s, 0, KK_TAG_CPTR_RAW, ctx);
  raw->free = freefun;
  raw->cptr = p;
  return kk_ptr_box(&raw->_block,ctx);
}

void* kk_cptr_raw_unbox(kk_box_t b, kk_context_t* ctx) {
  kk_cptr_raw_t raw = kk_block_unbox_as(kk_cptr_raw_t, b, KK_TAG_CPTR_RAW, ctx);
  return raw->cptr;
}

kk_box_t kk_cptr_box(void* p, kk_context_t* ctx) {
  intptr_t i = (intptr_t)p;
  if kk_likely(i >= KK_INTF_BOX_MIN(0) && i <= KK_INTF_BOX_MAX(0)) {
    // box as value
    return kk_intf_box((kk_intf_t)i);
  }
  else {
    // allocate 
    return kk_cptr_raw_box(&kk_free_fun_null, p, ctx);
  }
}

void* kk_cptr_unbox(kk_box_t b, kk_context_t* ctx) {
  if (kk_box_is_value(b)) {
    return (void*)((intptr_t)kk_intf_unbox(b));
  }
  else {
    return kk_cptr_raw_unbox(b,ctx);
  }
}



/*----------------------------------------------------------------
  Maybe type support
----------------------------------------------------------------*/

kk_box_t kk_unbox_Just_block( kk_block_t* b, kk_context_t* ctx ) {
  kk_assert_internal(kk_block_has_tag(b,KK_TAG_JUST));
  kk_box_t res = kk_block_as(kk_just_t*, b)->value;
  if (ctx != NULL) {
    if (kk_block_is_unique(b)) {
      kk_block_free(b,ctx);  
    }
    else {
      kk_box_dup(res,ctx);
      kk_block_decref(b, ctx);
    }
  }
  return res;
}


/*----------------------------------------------------------------
  Double boxing on 64-bit systems
----------------------------------------------------------------*/

#if (KK_INTF_SIZE == 8) && KK_BOX_DOUBLE64
// Generic double allocation in the heap
typedef struct kk_boxed_double_s {
  kk_block_t _block;
  double  value;
} *kk_boxed_double_t;

static double kk_double_unbox_heap(kk_box_t b, kk_context_t* ctx) {
  kk_boxed_double_t dt = kk_block_assert(kk_boxed_double_t, kk_ptr_unbox(b,ctx), KK_TAG_DOUBLE);
  double d = dt->value;
  if (ctx != NULL) { kk_base_type_drop(dt, ctx); }
  return d;
}

static kk_box_t kk_double_box_heap(double d, kk_context_t* ctx) {
  kk_boxed_double_t dt = kk_block_alloc_as(struct kk_boxed_double_s, 0, KK_TAG_DOUBLE, ctx);
  dt->value = d;
  return kk_ptr_box(&dt->_block, ctx);
}


#if (KK_BOX_DOUBLE64 == 2)  // heap allocate when negative
kk_box_t kk_double_box(double d, kk_context_t* ctx) {
  kk_unused(ctx);
  uint64_t u = kk_bits_from_double(d);
  //if (isnan(d)) { kk_debugger_break(ctx); }
  if (u <= KK_UINTF_BOX_MAX) {  // fits in a boxed value?  (i.e. is the double positive)
    return kk_uintf_box(u);
  }
  else {
    // heap allocate
    return kk_double_box_heap(d, ctx);
  }
}

double kk_double_unbox(kk_box_t b, kk_context_t* ctx) {
  kk_unused(ctx);
  double d;
  if (kk_box_is_value(b)) {
    // positive double
    d = kk_bits_to_double(kk_uintf_unbox(b));
  }
  else {
    // heap allocated
    d = kk_double_unbox_heap(b, ctx);
  }
  // if (isnan(d)) { kk_debugger_break(ctx); }
  return d;
}
#else  // heap allocate when the exponent is between 0x200 and 0x5FF.
kk_box_t kk_double_box(double d, kk_context_t* ctx) {
  kk_unused(ctx);
  uint64_t u = kk_bits_from_double(d);
  u = kk_bits_rotl64(u, 12);
  uint64_t exp = u & 0x7FF;
  u -= exp;
  // adjust to 10-bit exponent (if possible)
  if (exp==0) { // zero or subnormal
    // already good
  }
  else if (exp==0x7FF) { // infinity or NaN
    exp = 0x3FF;
  }
  else if (exp > 0x200 && exp < 0x5FF) {  // absolute value between [2^-510,2^512)
    exp -= 0x200;
  }
  else {
    // outside our range, heap allocate (outside [2^-510,2^512) and not 0, subnormal, NaN or Inf)
    return kk_double_box_heap(d, ctx);
  }
  kk_assert_internal(exp <= 0x3FF);
  kk_assert_internal((kk_shr64(u,1) & 0x3FF) == 0);
  return kk_uintf_box( kk_shr64(u,1) | exp );  
}

double kk_double_unbox(kk_box_t b, kk_context_t* ctx) {
  kk_unused(ctx);
  if (kk_box_is_value(b)) {
    // expand 10-bit exponent to 11-bits again
    uint64_t u = kk_uintf_unbox(b);
    uint64_t exp = u & 0x3FF;
    u -= exp;    // clear lower 10 bits        
    if (exp == 0) {
      // ok
    }
    else if (exp==0x3FF) {
      exp = 0x7FF;
    }
    else {
      exp += 0x200;
    }
    kk_assert_internal(exp <= 0x7FF);
    u = kk_bits_rotr64( kk_shl64(u,1) | exp, 12);
    double d = kk_bits_to_double(u);
    return d;
  }
  else {
    // heap allocated
    return kk_double_unbox_heap(b, ctx);
  }
}
#endif
#endif


/*----------------------------------------------------------------
  Float boxing on 32-bit systems
----------------------------------------------------------------*/

#if (KK_INTF_SIZE == 4) 
// Generic float allocation in the heap
typedef struct kk_boxed_float_s {
  kk_block_t _block;
  float      value;
} *kk_boxed_float_t;

static float kk_float_unbox_heap(kk_box_t b, kk_context_t* ctx) {
  kk_boxed_float_t ft = kk_block_assert(kk_boxed_float_t, kk_ptr_unbox(b,ctx), KK_TAG_FLOAT);
  float f = ft->value;
  if (ctx != NULL) { kk_base_type_drop(ft, ctx); }
  return f;
}

static kk_box_t kk_float_box_heap(float f, kk_context_t* ctx) {
  kk_boxed_float_t ft = kk_block_alloc_as(struct kk_boxed_float_s, 0, KK_TAG_FLOAT, ctx);
  ft->value = f;
  return kk_ptr_box(&ft->_block,ctx);
}

kk_box_t kk_float_box(float f, kk_context_t* ctx) {
  kk_unused(ctx);
  uint32_t u = kk_bits_from_float(f);
  if (u <= KK_UINTF_BOX_MAX(0)) {  // fits in a boxed value?  (i.e. is the double positive)
    return kk_uintf_box(u);
  }
  else {
    // heap allocate
    return kk_float_box_heap(f, ctx);
  }
}

float kk_float_unbox(kk_box_t b, kk_context_t* ctx) {
  kk_unused(ctx);
  float f;
  if (kk_box_is_value(b)) {
    // positive float
    f = kk_bits_to_float(kk_uintf_unbox(b));
  }
  else {
    // heap allocated
    f = kk_float_unbox_heap(b, ctx);
  }
  // if (isnan(f)) { kk_debugger_break(ctx); }
  return f;
}
#endif