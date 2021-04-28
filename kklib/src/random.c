/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
#include "kklib.h"
#include <string.h> // memset


/* -----------------------------------------------------------
  Deterministic pseudo random number generation. Fast but not secure.
----------------------------------------------------------- */

// pseudo random number context (using pcg)
typedef struct kk_pcg_ctx_s {
  uint64_t state;
  uint64_t stream; // must be odd
} kk_pcg_ctx_t;

// Pseudo random number using PCG by Melissa E. O'Neill.
// It combines a linear congruential generator (CG) with an output permutation
// function (P) and has good statictical properties (and passes PractRand and Big-crush[2]).
// Note: another good multiplier is KU32(0xF13283AD) [1] which is more efficient on
// 32-bit architectures as that can be implemented in 64x32-bit multiply instead
// of a full 64x64-bit multiply.
// [1]: https://www.pcg-random.org/posts/critiquing-pcg-streams.html#changing-the-multiplier
// [2]: https://www.pcg-random.org/pdf/hmc-cs-2014-0905.pdf
static inline uint32_t pcg_uint32(kk_pcg_ctx_t* rnd) {
  const uint64_t state0 = rnd->state;
  rnd->state = (state0 * KU64(0x5851F42D4C957F2D)) + rnd->stream;
  const uint32_t x = (uint32_t)(((state0 >> 18) ^ state0) >> 27);
  const uint32_t rot = (uint32_t)(state0 >> 59);
  return kk_bits_rotr32(x, rot);
}

static void pcg_init(uint64_t init, uint64_t stream, kk_pcg_ctx_t* rnd) {
  rnd->state = 0;
  rnd->stream = (2*stream) | 1; // ensure it is odd
  pcg_uint32(rnd);
  rnd->state += init;
  for (int i = 0; i < 8; i++) { pcg_uint32(rnd); }
}


/*
// pseudo random number context (using sfc32)
typedef struct kk_sfc_ctx_s {
  uint32_t a;
  uint32_t b;
  uint32_t c;
  uint32_t counter;
} kk_sfc_ctx_t;

// Pseudo random number using sfc32 by Chris Doty-Humphrey.
// It is a "chaotic" pseudo random generator that uses 32-bit operations only
// (so we can be deterministic across architectures in results and performance).
// It has good statistical properties and passes PractRand and Big-crush.
// It uses a 32-bit counter to guarantee a worst-case cycle
// of 2^32. It has a 96-bit state, so the average period is 2^127.
// The chance of a cycle of less than 2^(32+max(96-k,0)) is 2^-(32+k),
// (e.g. the chance of a cycle of less than 2^48 is 2^-80).
// <http://pracrand.sourceforge.net/RNG_engines.txt>
static inline uint32_t sfc_uint32(kk_sfc_ctx_t* rnd) {
  uint32_t x = rnd->a + rnd->b + rnd->counter;
  rnd->counter++;
  rnd->a = rnd->b ^ (rnd->b >> 9);
  rnd->b = rnd->c + (rnd->c << 3);
  rnd->c = kk_bits_rotl32(rnd->c, 21) + x;
  return x;
}

static void sfc_init(uint64_t seed, kk_sfc_ctx_t* rnd) {
  rnd->a = 0;
  rnd->b = (uint32_t)(seed);
  rnd->c = (uint32_t)(seed >> 32);
  rnd->counter = 1;
  for (size_t i = 0; i < 12; i++) {
    sfc_uint32(rnd);
  }
}
*/


/* ----------------------------------------------------------------------------
Secure pseudo random numbers based on chacha-20/8
We use our own PRNG to keep predictable performance of random number generation
and to avoid implementations that use a lock. We only use the OS provided
random source to initialize the initial seeds.
-----------------------------------------------------------------------------*/

/* ----------------------------------------------------------------------------
Chacha20/8 implementation as the original algorithm with a 64-bit nonce
and counter: https://en.wikipedia.org/wiki/Salsa20 by Daniel J. Bernstein
The input matrix has sixteen 32-bit values:
Position  0 to  3: constant key
Position  4 to 11: the key
Position 12 to 13: the counter.
Position 14 to 15: the nonce.

The implementation uses regular C code which compiles very well on modern compilers.
(gcc x64 has no register spills, and clang 6+ uses SSE instructions)
-----------------------------------------------------------------------------*/

static inline void qround(uint32_t x[16], size_t a, size_t b, size_t c, size_t d) {
  x[a] += x[b]; x[d] = kk_bits_rotl32(x[d] ^ x[a], 16);
  x[c] += x[d]; x[b] = kk_bits_rotl32(x[b] ^ x[c], 12);
  x[a] += x[b]; x[d] = kk_bits_rotl32(x[d] ^ x[a], 8);
  x[c] += x[d]; x[b] = kk_bits_rotl32(x[b] ^ x[c], 7);
}

static inline void kk_chacha_shuffle(const size_t rounds, uint32_t* x)
{
  for (size_t i = 0; i < rounds; i += 2) {
    qround(x, 0, 4, 8, 12);
    qround(x, 1, 5, 9, 13);
    qround(x, 2, 6, 10, 14);
    qround(x, 3, 7, 11, 15);
    qround(x, 0, 5, 10, 15);
    qround(x, 1, 6, 11, 12);
    qround(x, 2, 7, 8, 13);
    qround(x, 3, 4, 9, 14);
  }
}

static inline void chacha_block(const size_t rounds, uint32_t* input, uint32_t* output)
{
  // copy into `x`
  uint32_t x[16];
  for (size_t i = 0; i < 16; i++) {
    x[i] = input[i];
  }

  // shuffle bits
  kk_chacha_shuffle(rounds, x);

  // add scrambled data to the initial state into the output
  for (size_t i = 0; i < 16; i++) {
    output[i] = x[i] + input[i];
  }

  // increment the counter for the next round
  input[12] += 1;
  if (input[12] == 0) {
    input[13] += 1;
    if (input[13] == 0) {  // and keep increasing into the nonce
      input[14] += 1;
    }
  }
}

static kk_decl_noinline void chacha20(kk_random_ctx_t* rnd) {
  chacha_block(20, rnd->input, rnd->output);
  rnd->used = 0;
}
/*
static kk_decl_noinline void chacha8(kk_random_ctx_t* rnd) {
  chacha_block(8, rnd->input, rnd->output);
  rnd->used = 0;
}
*/

static inline uint32_t read32(const uint8_t* p, size_t idx32) {
  const size_t i = 4*idx32;
  return ((uint32_t)p[i+0] | (uint32_t)p[i+1] << 8 | (uint32_t)p[i+2] << 16 | (uint32_t)p[i+3] << 24);
}

static void chacha_init(kk_random_ctx_t* rnd, const uint8_t key[32], uint64_t nonce)
{
  // read the 32-bit values as little-endian
  memset(rnd, 0, sizeof(*rnd));
  for (size_t i = 0; i < 4; i++) {
    const uint8_t* sigma = (uint8_t*)"expand 32-byte k";
    rnd->input[i] = read32(sigma,i);
  }
  for (size_t i = 0; i < 8; i++) {
    rnd->input[i + 4] = read32(key,i);
  }
  rnd->input[12] = 0;
  rnd->input[13] = 0;
  rnd->input[14] = (uint32_t)nonce;
  rnd->input[15] = (uint32_t)(nonce >> 32);
  rnd->used = 128;
}

/*
static void kk_chacha_split(kk_random_ctx_t* rnd, uint64_t nonce, kk_random_ctx_t* ctx_new) {
  memset(ctx_new, 0, sizeof(*ctx_new));
  memcpy(ctx_new->input, rnd->input, sizeof(ctx_new->input));
  ctx_new->input[12] = 0;
  ctx_new->input[13] = 0;
  ctx_new->input[14] = (uint32_t)nonce;
  ctx_new->input[15] = (uint32_t)(nonce >> 32);
  kk_assert_internal(rnd->input[14] != ctx_new->input[14] || rnd->input[15] != ctx_new->input[15]); // do not reuse nonces!
  chacha20(ctx_new);
}
*/

/* ----------------------------------------------------------------------------
Secure random: split
-----------------------------------------------------------------------------*/
#if !defined(NDEBUG) && defined(KK_DEBUG_FULL)
static bool random_is_initialized(kk_random_ctx_t* rnd) {
  return (rnd->input[0] != 0);
}
#endif

/*
static void kk_random_split(kk_random_ctx_t* rnd, kk_random_ctx_t* ctx_new) {
  kk_assert_internal(random_is_initialized(rnd));
  kk_assert_internal(rnd != ctx_new);
  kk_chacha_split(rnd, (uintptr_t)ctx_new, ctx_new); // nonce = ctx_new
}
*/


/*--------------------------------------------------------------------------------------
  Secure random: select in a range without bias
--------------------------------------------------------------------------------------*/

uint32_t kk_srandom_range_uint32(uint32_t max, kk_context_t* ctx) {
  /* Select unbiased integer in the range [0,max) by Daniel Lemire <https://arxiv.org/pdf/1805.10941.pdf> */
  uint32_t x = kk_srandom_uint32(ctx);
  uint64_t m = (uint64_t)x * (uint64_t)max;
  uint32_t l = (uint32_t)m;
  if (kk_unlikely(l < max)) {
    uint32_t threshold = (~max+1) % max;  /* 2^32 % max  ==  (2^32 - max) % max  ==  -max % max */
    while (l < threshold) {
      x = kk_srandom_uint32(ctx);
      m = (uint64_t)x * (uint64_t)max;
      l = (uint32_t)m;
    }
  }
  return (uint32_t)(m >> 32);
}

int32_t kk_srandom_range_int32(int32_t min, int32_t max, kk_context_t* ctx) {
  if (min > max) {
    int32_t x = min;
    min = max;
    max = x;
  }
  uint32_t delta = (uint32_t)(max - min);
  if (delta == 0) return 0;
  uint32_t x = kk_srandom_range_uint32(delta, ctx);
  return (min + (int32_t)(x));
}

/*--------------------------------------------------------------------------------------
  Secure random: get a double
--------------------------------------------------------------------------------------*/

// Use 52 random bits to generate a double in the range [0,1)
double kk_srandom_double(kk_context_t* ctx) {
  const uint64_t x = KU64(0x3FF0000000000000) | kk_shr64(kk_srandom_uint64(ctx), 12);
  double d;
  memcpy(&d, &x, sizeof(double)); /* alias safe: <https://gist.github.com/shafik/848ae25ee209f698763cffee272a58f8#how-do-we-type-pun-correctly> */
  return (d - 1.0);
}



/* ----------------------------------------------------------------------------
To get an initial secure random context we rely on the OS:
- Windows     : RtlGenRandom (or BCryptGenRandom)
- OSX,bsd,wasi: arc4random_buf
- Linux       : getrandom (or /dev/urandom)
If we cannot get good randomness, we fall back to weak randomness based on a timer and ASLR.
-----------------------------------------------------------------------------*/

#if defined(WIN32)
/*
#pragma comment (lib,"bcrypt.lib")
#include <windows.h>
#include <bcrypt.h>
static bool os_random_buf(void* buf, size_t buf_len) {
  return (BCryptGenRandom(NULL, (PUCHAR)buf, (ULONG)buf_len, BCRYPT_USE_SYSTEM_PREFERRED_RNG) >= 0);
}
*/
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#define RtlGenRandom  SystemFunction036
#ifdef __cplusplus
extern "C" {
#endif
  BOOLEAN NTAPI RtlGenRandom(PVOID RandomBuffer, ULONG RandomBufferLength);
#ifdef __cplusplus
}
#endif
static bool os_random_buf(void* buf, size_t buf_len) {
  kk_assert_internal(buf_len >= sizeof(uintptr_t));
  memset(buf, 0, buf_len);
  RtlGenRandom(buf, (ULONG)buf_len);
  return (((uintptr_t*)buf)[0] != 0);  // sanity check (but RtlGenRandom should never fail)
}
#elif defined(ANDROID) || defined(XP_DARWIN) || defined(__APPLE__) || defined(__DragonFly__) || \
      defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__) || \
      defined(__wasi__)
#include <stdlib.h>
static bool os_random_buf(void* buf, size_t buf_len) {
  arc4random_buf(buf, buf_len);
  return true;
}
#elif defined(__linux__)
#include <unistd.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
static bool os_random_buf(void* buf, size_t buf_len) {
  // Modern Linux provides `getrandom` but different distributions either use `sys/random.h` or `linux/random.h`
  // and for the latter the actual `getrandom` call is not always defined.
  // (see <https://stackoverflow.com/questions/45237324/why-doesnt-getrandom-compile>)
  // We therefore use a syscall directly and fall back dynamically to /dev/urandom when needed.
#ifdef SYS_getrandom
  #ifndef GRND_NONBLOCK
  #define GRND_NONBLOCK (1)
  #endif
  static volatile uintptr_t no_getrandom; // = 0
  if (no_getrandom == 0) {
    ssize_t ret = syscall(SYS_getrandom, buf, buf_len, GRND_NONBLOCK);
    if (ret >= 0) return (buf_len == (size_t)ret);
    if (ret != ENOSYS) return false;
    no_getrandom = 1; // don't call again, and fall back to /dev/urandom
  }
#endif
  int flags = O_RDONLY;
  #if defined(O_CLOEXEC)
  flags |= O_CLOEXEC;
  #endif
  int fd = open("/dev/urandom", flags, 0);
  if (fd < 0) return false;
  size_t count = 0;
  while(count < buf_len) {
    ssize_t ret = read(fd, (char*)buf + count, buf_len - count);
    if (ret<=0) {
      if (errno!=EAGAIN && errno!=EINTR) break;
    }
    else {
      count += ret;
    }
  }
  close(fd);
  return (count==buf_len);
}
#else
static bool os_random_buf(void* buf, size_t buf_len) {
  return false;
}
#endif

#if defined(WIN32)
#include <Windows.h>
#elif defined(__APPLE__)  
#include <mach/mach_time.h>
#else
#include <time.h>
#endif


static uint64_t os_random_weak(void) {
  uint64_t x = (uint64_t)&os_random_weak ^ KU64(0x853C49E6748FEA9B); // hopefully, ASLR makes the address random
  do {
  #if defined(WIN32)
    LARGE_INTEGER pcount;
    QueryPerformanceCounter(&pcount);
    x ^= (uint64_t)(pcount.QuadPart);
  #elif defined(__APPLE__)
    x ^= mach_absolute_time();
  #else
    struct timespec time;
    #if defined(CLOCK_MONOTONIC)
    clock_gettime(CLOCK_MONOTONIC, &time);
    #else
    clock_gettime(CLOCK_REALTIME, &time);
    #endif  
    x ^= kk_bits_rotl64((uint64_t)time.tv_sec, 32);
    x ^= (uint64_t)time.tv_nsec;
  #endif
  } while (x == 0);  
  return x;
}

static kk_random_ctx_t* random_init(kk_context_t* ctx) {
  kk_random_ctx_t* rnd = (kk_random_ctx_t*)kk_zalloc(sizeof(kk_random_ctx_t), ctx);
  uint8_t key[32];
  const bool strong = os_random_buf(key, sizeof(key));
  if (!strong) {
    // if we fail to get random data from the OS, we fall back to a
    // weak random source based on the C library `rand()`, the current (high precision) time, and ASLR.
    kk_warning_message("unable to use strong randomness\n");
    kk_pcg_ctx_t pcg;
    pcg_init(os_random_weak(), (uint64_t)(rand()), &pcg);
    for (size_t i = 0; i < 8; i++) {  // key is eight 32-bit words.
      uint32_t x = pcg_uint32(&pcg);
      ((uint32_t*)key)[i] = x;
    }
  }
  chacha_init(rnd, key, (uintptr_t)&random_init /*nonce*/ );
  rnd->is_strong = strong;
  return rnd;
}

kk_random_ctx_t* kk_srandom_round(kk_context_t* ctx) {
  // initialize on demand
  kk_random_ctx_t* rnd = ctx->srandom_ctx;
  if (rnd == NULL) {
    ctx->srandom_ctx = rnd = random_init(ctx);
  }
  chacha20(rnd);
  return rnd;
}

bool kk_srandom_is_strong(kk_context_t* ctx) {
  kk_random_ctx_t* rnd = ctx->srandom_ctx;
  if (rnd == NULL) {
    rnd = kk_srandom_round(ctx);
  }
  return rnd->is_strong;
}
