/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
#include "kklib.h"

#if !KK_USE_NAN_BOX
// Generic double allocation in the heap
typedef struct kk_boxed_double_s {
  kk_block_t _block;
  double  value;
} *boxed_kk_double_t;

double kk_double_unbox_heap(kk_box_t b, kk_context_t* ctx) {
  boxed_kk_double_t dt = kk_block_assert(boxed_kk_double_t, kk_ptr_unbox(b), KK_TAG_DOUBLE);
  double d = dt->value;
  if (ctx != NULL) { kk_basetype_drop(dt, ctx); }
  return d;
}

kk_box_t kk_double_box_heap(double d, kk_context_t* ctx) {
  boxed_kk_double_t dt = kk_block_alloc_as(struct kk_boxed_double_s, 0, KK_TAG_DOUBLE, ctx);
  dt->value = d;
  return kk_ptr_box(&dt->_block);
}
#endif

#if (KK_INTPTR_SIZE==8) && !KK_USE_NAN_BOX

#if defined(BOX_DOUBLE_IF_NEG)
kk_box_t kk_double_box(double d, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  int64_t i;
  memcpy(&i, &d, sizeof(i));  // safe for C aliasing
  if (i >= 0) {  // positive?
    return kk_enum_box((uint64_t)i);
  }
  else {
    // heap allocate
    return kk_double_box_heap(d, ctx);
  }
}

double kk_double_unbox(kk_box_t b, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  if (kk_box_is_value(b)) {
    // positive double
    double d;
    uint64_t u = kk_shr(b.box, 1);
    memcpy(&d, &u, sizeof(d)); // safe for C aliasing: see <https://gist.github.com/shafik/848ae25ee209f698763cffee272a58f8#how-do-we-type-pun-correctly>
    return d;
  }
  else {
    // heap allocated
    return kk_double_unbox_heap(b, ctx);
  }
}
#else
kk_box_t kk_double_box(double d, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  uint64_t u;
  memcpy(&u, &d, sizeof(u));  // safe for C aliasing
  u = bits_rotl64(u, 12);
  uint64_t exp = u & 0x7FF;
  u -= exp;
  // adjust to 10-bit exponent (if possible)
  if (exp==0) {
    // already good
  }
  else if (exp==0x7FF) {
    exp = 0x3FF;
  }
  else if (exp > 0x200 && exp < 0x5FF) {
    exp -= 0x200;
  }
  else {
    // outside our range, heap allocate (outside [2^-510,2^512) and not 0, subnormal, NaN or Inf)
    return kk_double_box_heap(d, ctx);
  }
  kk_assert_internal(exp <= 0x3FF);
  kk_box_t b = { (u | (exp<<1) | 1) };
  return b;
}

double kk_double_unbox(kk_box_t b, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  if (kk_box_is_value(b)) {
    // expand 10-bit exponent to 11-bits again
    uint64_t u = b.box;
    uint64_t exp = u & 0x7FF;
    u -= exp;    // clear lower 11 bits
    exp >>= 1;
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
    u = bits_rotr64(u | exp, 12);
    double d;
    memcpy(&d, &u, sizeof(d)); // safe for C aliasing: see <https://gist.github.com/shafik/848ae25ee209f698763cffee272a58f8#how-do-we-type-pun-correctly>
    return d;
  }
  else {
    // heap allocated
    return kk_double_unbox_heap(b, ctx);
  }
}
#endif

#elif (KK_INTPTR_SIZE==8) && KK_USE_NAN_BOX 

static inline double kk_double_unbox(kk_box_t v, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  kk_assert_internal(_is_double(v) || kk_box_is_any(v));
  double d;
  uint64_t u = v.box;
  if (kk_likely(_kk_double_is_normal(v))) {
    // regular double
    if ((int64_t)u >= 0) { u -= (KU64(1) << 52); } // subtract 0x0010 0000 0000 0000 to positive doubles    
    memcpy(&d, &u, sizeof(d)); // safe for C aliasing: see <https://gist.github.com/shafik/848ae25ee209f698763cffee272a58f8#how-do-we-type-pun-correctly>
    kk_assert_internal(isfinite(d));
  }
  else {
    // NaN or infinity
    kk_assert_internal(kk__is_double_special(v) || kk_box_is_any(v));
    u = (v.box^1) | ((v.box >> 1) & 1);  // invert:  v.box = u | 1 | ((u & 1) << 1);    
    u |= (KU64(0x7FF) << 52);             // restore exponent to 0x7FF (only needed for positive u but this avoids an if)
    memcpy(&d, &u, sizeof(d)); // safe for C aliasing
    kk_assert_internal(!isfinite(d));
  }
  return d;
}


static inline kk_box_t kk_double_box(double d, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  uint64_t u;
  kk_box_t v;
  memcpy(&u, &d, sizeof(u));  // safe for C aliasing
  uint64_t exp = (u >> 52) & 0x7FF;
  if (kk_likely(exp != 0x7FF)) {
    // finite double
    if ((int64_t)u >= 0) { u += (KU64(1) << 52); }  // add 0x0010 0000 0000 0000 to positive doubles (use signbit to encode -0.0 properly)
    v.box = u;
    kk_assert_internal(_kk_double_is_normal(v));
    kk_assert_internal(kk_double_unbox(v, ctx) == d);
  }
  else {
    // NaN or infinity
    if ((int64_t)u >= 0) {
      u = ((u << 12) >> 12);  // clear upper 12 bits if >= 0, (so upper the 12 bits are either 0xFFF or 0x000)
    }
    v.box = u | 1 | ((u & 1) << 1);  // merge bit 0 with bit 1 (to avoid non-zero NaN payload on unbox)
    kk_assert_internal(!_kk_double_is_normal(v) && kk__is_double_special(v));
#if (DEBUG>=3)
    double dx = kk_double_unbox(v, ctx);
    uint64_t ux;
    memcpy(&ux, &dx, sizeof(double));
    kk_assert_internal(u == ux);  // (may fail due to bits 2-3 of a NaN payload)
#endif
  }
  kk_assert_internal(_is_double(v));
  return v;
}

#endif
