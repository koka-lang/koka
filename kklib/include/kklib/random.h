#pragma once
#ifndef RANDOM_H_
#define RANDOM_H_

/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

// Strong random state based on chacha20.
typedef struct kk_random_ctx_s {
  uint32_t output[16]; // current output
  uint32_t input[16];  // current state
  int32_t  used;       // how many output fields are already used?
  bool     kk_is_strong;  // initialized from strong random source?
} kk_random_ctx_t;

kk_decl_export kk_random_ctx_t* srandom_round(kk_context_t* ctx);

// Strong random number using chacha20 (by Daniel J. Bernstein)
// Initial randomness comes from the OS.
static inline uint32_t srandom_uint32(kk_context_t* ctx) {
  kk_random_ctx_t* rnd = ctx->srandom_ctx;
  if (kk_unlikely(rnd == NULL || rnd->used >= 16)) {
    rnd = srandom_round(ctx);
  }
  uint32_t x = rnd->output[rnd->used];
  rnd->output[rnd->used++] = 0; // clear after use
  return x;
}

static inline kk_integer_t srandom_int(kk_context_t* ctx) {
  return kk_integer_from_int32((int32_t)srandom_uint32(ctx), ctx);
}

static inline uint64_t srandom_uint64(kk_context_t* ctx) {
  return (((uint64_t)srandom_uint32(ctx) << 32) | srandom_uint32(ctx));
}

kk_decl_export bool     kk_srandom_is_strong(kk_context_t* ctx);
kk_decl_export uint32_t srandom_range32(uint32_t max, kk_context_t* ctx);  // unbiased range
kk_decl_export double   srandom_double(kk_context_t* ctx);


#endif // include guard
