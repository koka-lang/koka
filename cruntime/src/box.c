/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
#include "runtime.h"

#if !USE_NAN_BOX
// Generic double allocation in the heap
typedef struct boxed_double_s {
  block_t _block;
  double  value;
} *boxed_double_t;

double unbox_double_heap(box_t b, context_t* ctx) {
  boxed_double_t dt = block_as_assert(boxed_double_t, unbox_ptr(b), TAG_DOUBLE);
  double d = dt->value;
  drop_datatype(dt, ctx);
  return d;
}

box_t box_double_heap(double d, context_t* ctx) {
  boxed_double_t dt = block_alloc_as(struct boxed_double_s, 0, TAG_DOUBLE, ctx);
  dt->value = d;
  return box_ptr(&dt->_block);
}
#endif

#if (INTPTR_SIZE==8) && !USE_NAN_BOX

#if defined(BOX_DOUBLE_IF_NEG)
box_t box_double(double d, context_t* ctx) {
  UNUSED(ctx);
  int64_t i;
  memcpy(&i, &d, sizeof(i));  // safe for C aliasing
  if (i >= 0) {  // positive?
    return box_enum((uint64_t)i);
  }
  else {
    // heap allocate
    return box_double_heap(d, ctx);
  }
}

double unbox_double(box_t b, context_t* ctx) {
  UNUSED(ctx);
  if (is_value(b)) {
    // positive double
    double d;
    uint64_t u = shr(b.box, 1);
    memcpy(&d, &u, sizeof(d)); // safe for C aliasing: see <https://gist.github.com/shafik/848ae25ee209f698763cffee272a58f8#how-do-we-type-pun-correctly>
    return d;
  }
  else {
    // heap allocated
    return unbox_double_heap(b, ctx);
  }
}
#else
box_t box_double(double d, context_t* ctx) {
  UNUSED(ctx);
  uint64_t u;
  memcpy(&u, &d, sizeof(u));  // safe for C aliasing
  u = rotl64(u, 12);
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
    return box_double_heap(d, ctx);
  }
  assert_internal(exp <= 0x3FF);
  box_t b = { (u | (exp<<1) | 1) };
  return b;
}

double unbox_double(box_t b, context_t* ctx) {
  UNUSED(ctx);
  if (is_value(b)) {
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
    assert_internal(exp <= 0x7FF);
    u = rotr64(u | exp, 12);
    double d;
    memcpy(&d, &u, sizeof(d)); // safe for C aliasing: see <https://gist.github.com/shafik/848ae25ee209f698763cffee272a58f8#how-do-we-type-pun-correctly>
    return d;
  }
  else {
    // heap allocated
    return unbox_double_heap(b, ctx);
  }
}
#endif

#elif (INTPTR_SIZE==8) && USE_NAN_BOX 

static inline double unbox_double(box_t v, context_t* ctx) {
  UNUSED(ctx);
  assert_internal(_is_double(v) || is_box_any(v));
  double d;
  uint64_t u = v.box;
  if (likely(_is_double_normal(v))) {
    // regular double
    if ((int64_t)u >= 0) { u -= (U64(1) << 52); } // subtract 0x0010 0000 0000 0000 to positive doubles    
    memcpy(&d, &u, sizeof(d)); // safe for C aliasing: see <https://gist.github.com/shafik/848ae25ee209f698763cffee272a58f8#how-do-we-type-pun-correctly>
    assert_internal(isfinite(d));
  }
  else {
    // NaN or infinity
    assert_internal(_is_double_special(v) || is_box_any(v));
    u = (v.box^1) | ((v.box >> 1) & 1);  // invert:  v.box = u | 1 | ((u & 1) << 1);    
    u |= (U64(0x7FF) << 52);             // restore exponent to 0x7FF (only needed for positive u but this avoids an if)
    memcpy(&d, &u, sizeof(d)); // safe for C aliasing
    assert_internal(!isfinite(d));
  }
  return d;
}


static inline box_t box_double(double d, context_t* ctx) {
  UNUSED(ctx);
  uint64_t u;
  box_t v;
  memcpy(&u, &d, sizeof(u));  // safe for C aliasing
  uint64_t exp = (u >> 52) & 0x7FF;
  if (likely(exp != 0x7FF)) {
    // finite double
    if ((int64_t)u >= 0) { u += (U64(1) << 52); }  // add 0x0010 0000 0000 0000 to positive doubles (use signbit to encode -0.0 properly)
    v.box = u;
    assert_internal(_is_double_normal(v));
    assert_internal(unbox_double(v, ctx) == d);
  }
  else {
    // NaN or infinity
    if ((int64_t)u >= 0) {
      u = ((u << 12) >> 12);  // clear upper 12 bits if >= 0, (so upper the 12 bits are either 0xFFF or 0x000)
    }
    v.box = u | 1 | ((u & 1) << 1);  // merge bit 0 with bit 1 (to avoid non-zero NaN payload on unbox)
    assert_internal(!_is_double_normal(v) && _is_double_special(v));
#if (DEBUG>=3)
    double dx = unbox_double(v, ctx);
    uint64_t ux;
    memcpy(&ux, &dx, sizeof(double));
    assert_internal(u == ux);  // (may fail due to bits 2-3 of a NaN payload)
#endif
  }
  assert_internal(_is_double(v));
  return v;
}

#endif