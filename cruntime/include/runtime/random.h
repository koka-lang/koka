#pragma once
#ifndef __RANDOM_H__
#define __RANDOM_H__

/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------
  Randomness
  prandom is deterministic and can be seeded
  random and srandom are secure and always seeded from the OS best random source.
  Use `random_is_strong` to check if it was initialized successfully
  from a strong random source.
--------------------------------------------------------------------------------------*/

decl_export random_ctx_t* random_round(context_t* ctx);
decl_export random_ctx_t* srandom_round(context_t* ctx);
decl_export void prandom_init(context_t* ctx);
decl_export void prandom_seed(uint64_t seed, context_t* ctx);
decl_export bool random_is_strong(context_t* ctx);
decl_export bool rrandom_is_strong(context_t* ctx);

// Pseudo random number using PCG, fast but not secure
// By Melissa O'Neill <https://www.pcg-random.org/>
static inline uint32_t pcg_uint32(pcg_ctx_t* rnd) {
  const uint64_t state0 = rnd->state;
  rnd->state = (state0 * U64(6364136223846793005)) + rnd->stream;
  const uint32_t x = (uint32_t)(((state0 >> 18) ^ state0) >> 27);
  const uint32_t rot = (uint32_t)(state0 >> 59);
  return rotr32(x, rot);
}
static inline uint32_t prandom_uint32(context_t* ctx) {
  pcg_ctx_t* const rnd = &ctx->random_pcg;
  return pcg_uint32(rnd);
}
  

// Secure random number using chacha8 (by Daniel J. Bernstein)
// Initial randomness comes from the OS.
static inline uint32_t random_uint32(context_t* ctx) {
  random_ctx_t* rnd = ctx->random_ctx;
  if (unlikely(rnd == NULL || rnd->used >= RANDOM_FIELDS)) {
    rnd = random_round(ctx);
  }
  return rnd->output[rnd->used++];
}

// Secure strong random number using chacha20 (by Daniel J. Bernstein)
// Initial randomness comes from the OS.
static inline uint32_t srandom_uint32(context_t* ctx) {
  random_ctx_t* rnd = ctx->random_strong;
  if (unlikely(rnd == NULL || rnd->used >= RANDOM_FIELDS)) {
    rnd = srandom_round(ctx);
  }
  return rnd->output[rnd->used++];
}


/*--------------------------------------------------------------------------------------
  Select in a range
--------------------------------------------------------------------------------------*/

#define unbiased_range32(max,rand32) \
  /* Select unbiased integer in the range [0,max) by Daniel Lemire <https://arxiv.org/pdf/1805.10941.pdf> */ \
  uint32_t x = rand32; \
  uint64_t m = (uint64_t)x * (uint64_t)max; \
  uint32_t l = (uint32_t)m; \
  if (unlikely(l < max)) {  \
    uint32_t threshold = (~max+1) % max;  /* 2^32 % max == (2^32 - max) % max == -max % max */ \
    while (l < threshold) { \
      x = rand32;  \
      m = (uint64_t)x * (uint64_t)max; \
      l = (uint32_t)m; \
    } \
  } \
  return (uint32_t)(m >> 32);

static inline uint32_t prandom_range32(uint32_t max, context_t* ctx) { unbiased_range32(max, prandom_uint32(ctx)) }
static inline uint32_t random_range32(uint32_t max, context_t* ctx)  { unbiased_range32(max, random_uint32(ctx)) }
static inline uint32_t srandom_range32(uint32_t max, context_t* ctx) { unbiased_range32(max, srandom_uint32(ctx)) }

// Random 64 bits
#define rand_uint64(rand32) \
  return ((((uint64_t)(rand32)) << 32) | (rand32));

static inline uint64_t prandom_uint64(context_t* ctx) { rand_uint64(prandom_uint32(ctx)); }
static inline uint64_t random_uint64(context_t* ctx)  { rand_uint64(random_uint32(ctx)); }
static inline uint64_t srandom_uint64(context_t* ctx) { rand_uint64(srandom_uint32(ctx)); }

// Use 48 random bits to generate a double in the range [0,1)
#define rand_double(rand32) \
  const uint32_t lo = (rand32 << 4);            /* clear lower 4 bits  */ \
  const uint32_t hi = (rand32 & U32(0xFFFFF));  /* use only lower 20 bits (for bits 32 to 51) */ \
  const uint64_t x = U64(0x3FF0000000000000) | (uint64_t)hi << 32 | (uint64_t)lo; \
  double d; \
  memcpy(&d, &x, sizeof(double)); /* alias safe: <https://gist.github.com/shafik/848ae25ee209f698763cffee272a58f8#how-do-we-type-pun-correctly> */ \
  return (d - 1.0);

static inline double prandom_double(context_t* ctx) { rand_double(prandom_uint32(ctx)); }
static inline double random_double(context_t* ctx)  { rand_double(random_uint32(ctx)); }
static inline double srandom_double(context_t* ctx) { rand_double(srandom_uint32(ctx)); }

// Use 32 bits for a random integer 
static inline integer_t prandom_int(context_t* ctx) {
  return integer_from_int32((int32_t)prandom_uint32(ctx), ctx);
}
static inline integer_t random_int(context_t* ctx) {
  return integer_from_int32((int32_t)random_uint32(ctx), ctx);
}
static inline integer_t rrandom_int(context_t* ctx) {
  return integer_from_int32((int32_t)srandom_uint32(ctx), ctx);
}

#endif // include guard
