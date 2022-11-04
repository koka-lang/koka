#pragma once
#ifndef KK_RANDOM_H
#define KK_RANDOM_H

/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Strong random state based on chacha20.
typedef struct kk_random_ctx_s {
  uint32_t output[16]; // current output
  uint32_t input[16];  // current state
  int32_t  used;       // how many output fields are already used?
  bool     is_strong;  // initialized from strong random source?
} kk_random_ctx_t;

kk_decl_export kk_random_ctx_t* kk_srandom_round(kk_context_t* ctx);

// Strong random number using chacha20 (by Daniel J. Bernstein)
// Initial randomness comes from the OS.
static inline uint32_t kk_srandom_uint32(kk_context_t* ctx) {
  kk_random_ctx_t* rnd = ctx->srandom_ctx;
  if kk_unlikely(rnd == NULL || rnd->used >= 16) {
    rnd = kk_srandom_round(ctx);
    kk_assert_internal(rnd != NULL && rnd->used >= 0 && rnd->used < 16);
  }
  uint32_t x = rnd->output[rnd->used];
  rnd->output[rnd->used++] = 0; // clear after use
  return x;
}

static inline uint64_t kk_srandom_uint64(kk_context_t* ctx) {
  // return (((uint64_t)kk_srandom_uint32(ctx) << 32) | kk_srandom_uint32(ctx));
  kk_random_ctx_t* rnd = ctx->srandom_ctx;
  if kk_unlikely(rnd == NULL || rnd->used >= 15) {
    rnd = kk_srandom_round(ctx);
    kk_assert_internal(rnd != NULL && rnd->used >= 0 && rnd->used < 15);
  }
  uint64_t* p = (uint64_t*)&rnd->output[rnd->used];
  uint64_t x = *p;
  *p = 0;
  rnd->used += 2;
  return x;
}

static inline kk_integer_t kk_srandom_int(kk_context_t* ctx) {
  return kk_integer_from_int32((int32_t)kk_srandom_uint32(ctx), ctx);
}

kk_decl_export bool     kk_srandom_is_strong(kk_context_t* ctx);
kk_decl_export int32_t  kk_srandom_range_int32(int32_t min, int32_t max, kk_context_t* ctx);  // unbiased range
kk_decl_export uint32_t kk_srandom_range_uint32(uint32_t max, kk_context_t* ctx);             // unbiased range
kk_decl_export double   kk_srandom_double(kk_context_t* ctx);


#endif // include guard
