/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
#include "runtime.h"
#include <string.h> // memset

// Secure random context (using chacha-20)
typedef struct random_cxt_s {
  uint32_t input[16];
  uint32_t output[16];
  int      output_available;
} random_ctx_t;

// Pseudo random context (not secure, based on PCG)
typedef struct pcg_ctx_s {
  uint64_t state;
  uint64_t stream; // must be odd
} pcg_ctx_t;

bool     random_init(random_ctx_t* rnd);
void     random_split(random_ctx_t* rnd, random_ctx_t* ctx_new);
uint32_t random_next32(random_ctx_t* rnd);
uint64_t random_next64(random_ctx_t* rnd);
uintx_t  random_next(random_ctx_t* rnd);
double   random_next_double(random_ctx_t* rnd);
uint32_t random_range32(uint32_t max, random_ctx_t* rnd);

void     pcg_init(uint64_t init, uint64_t stream, pcg_ctx_t* rnd);
void     pcg_init_default(uint64_t stream, pcg_ctx_t* rnd);
void     pcg_split(pcg_ctx_t* rnd, pcg_ctx_t* ctx_new);
static inline uint32_t pcg_next32(pcg_ctx_t* rnd);
static inline uint64_t pcg_next64(pcg_ctx_t* rnd);
static inline uintx_t  pcg_next(pcg_ctx_t* rnd);
double   pcg_next_double(pcg_ctx_t* rnd);
uint32_t pcg_range32(uint32_t max, pcg_ctx_t* rnd);


/* -----------------------------------------------------------
  Generic helpers: unbiased selection and random double
----------------------------------------------------------- */

#define unbiased_range32(max,rand) \
  /* Select unbiased integer in the range [0,max) by Daniel Lemire <https://arxiv.org/pdf/1805.10941.pdf> */ \
  uint32_t x = rand; \
  uint64_t m = (uint64_t)x * (uint64_t)max; \
  uint32_t l = (uint32_t)m; \
  if (unlikely(l < max)) {  \
    uint32_t threshold = (~max+1) % max;  /* 2^32 % max == (2^32 - max) % max == -max % max */ \
    while (l < threshold) { \
      x = rand;  \
      m = (uint64_t)x * (uint64_t)max; \
      l = (uint32_t)m; \
    } \
  } \
  return (uint32_t)(m >> 32);


uint32_t pcg_range32(uint32_t max, pcg_ctx_t* rnd) {
  unbiased_range32(max, pcg_next32(rnd))
}

uint32_t random_range32(uint32_t max, random_ctx_t* rnd) {
  unbiased_range32(max, random_next32(rnd))
}


#define next_double(rand32) \
  const uint32_t lo = (rand32 << 4);            /* clear lower 4 bits  */ \
  const uint32_t hi = (rand32 & U32(0xFFFFF));  /* use only lower 20 bits (for bits 32 to 51) */ \
  const uint64_t x = U64(0x3FF0000000000000) | (uint64_t)hi << 32 | (uint64_t)lo; \
  double d; \
  memcpy(&d, &x, sizeof(double)); /* alias safe: <https://gist.github.com/shafik/848ae25ee209f698763cffee272a58f8#how-do-we-type-pun-correctly> */ \
  return (d - 1.0);

double pcg_next_double(pcg_ctx_t* rnd) {
  next_double(pcg_next32(rnd));
}

double random_next_double(random_ctx_t* rnd) {
  next_double(random_next32(rnd));
}


/* -----------------------------------------------------------
  PCG pseudo random number generation. Fast but not secure.
  By Melissa O'Neill <https://www.pcg-random.org/>
----------------------------------------------------------- */

static inline uint32_t pcg_next32(pcg_ctx_t* rnd) {
  const uint64_t state0 = rnd->state;
  rnd->state = (state0 * U64(6364136223846793005)) + rnd->stream;
  const uint32_t x   = (uint32_t)(((state0 >> 18) ^ state0) >> 27);
  const uint32_t rot = (uint32_t)(state0 >> 59);
  return rotr32(x, rot);
}

static inline uint64_t pcg_next64(pcg_ctx_t* rnd) {
  return ((((uint64_t)pcg_next32(rnd)) << 32) | pcg_next32(rnd));
}

static inline uintx_t pcg_next(pcg_ctx_t* rnd) {
#if (INTX_SIZE == 4) 
  return pcg_next32(rnd);
#else
  return pcg_next64(rnd);
#endif
}

void pcg_init(uint64_t init, uint64_t stream, pcg_ctx_t* rnd) {
  if (stream == 0) stream = (uintptr_t)rnd; // each context has its own stream based on its address
  rnd->state  = 0;
  rnd->stream = (2*stream) | 1; // ensure it is odd
  pcg_next(rnd);
  rnd->state += init;
  pcg_next(rnd);
}

void pcg_init_default(uint64_t stream, pcg_ctx_t* rnd) {
  pcg_init(U64(0x853C49E6748FEA9B), stream, rnd);
}

void pcg_split(pcg_ctx_t* rnd, pcg_ctx_t* rng_new) {
  pcg_init(rnd->state, (uintptr_t)rng_new, rng_new);
  assert_internal(rnd->stream != rng_new->stream);
  pcg_next32(rnd); // update state
}



/* ----------------------------------------------------------------------------
Secure pseudo random numbers based on chacha-20
We use our own PRNG to keep predictable performance of random number generation
and to avoid implementations that use a lock. We only use the OS provided
random source to initialize the initial seeds. 
-----------------------------------------------------------------------------*/

#define CHACHA_ROUNDS (20)   // perhaps use 12 for better performance?


/* ----------------------------------------------------------------------------
Chacha20 implementation as the original algorithm with a 64-bit nonce
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
  x[a] += x[b]; x[d] = rotl32(x[d] ^ x[a], 16);
  x[c] += x[d]; x[b] = rotl32(x[b] ^ x[c], 12);
  x[a] += x[b]; x[d] = rotl32(x[d] ^ x[a], 8);
  x[c] += x[d]; x[b] = rotl32(x[b] ^ x[c], 7);
}

static void chacha_block(random_ctx_t* rnd)
{
  // scramble into `x`
  uint32_t x[16];
  for (size_t i = 0; i < 16; i++) {
    x[i] = rnd->input[i];
  }
  for (size_t i = 0; i < CHACHA_ROUNDS; i += 2) {
    qround(x, 0, 4,  8, 12);
    qround(x, 1, 5,  9, 13);
    qround(x, 2, 6, 10, 14);
    qround(x, 3, 7, 11, 15);
    qround(x, 0, 5, 10, 15);
    qround(x, 1, 6, 11, 12);
    qround(x, 2, 7,  8, 13);
    qround(x, 3, 4,  9, 14);
  }

  // add scrambled data to the initial state
  for (size_t i = 0; i < 16; i++) {
    rnd->output[i] = x[i] + rnd->input[i];
  }
  rnd->output_available = 16;

  // increment the counter for the next round
  rnd->input[12] += 1;
  if (rnd->input[12] == 0) {
    rnd->input[13] += 1;
    if (rnd->input[13] == 0) {  // and keep increasing into the nonce
      rnd->input[14] += 1;
    }
  }
}

static uint32_t chacha_next32(random_ctx_t* rnd) {
  if (rnd->output_available <= 0) {
    chacha_block(rnd);
    rnd->output_available = 16; // (assign again to suppress static analysis warning)
  }
  const uint32_t x = rnd->output[16 - rnd->output_available];
  rnd->output[16 - rnd->output_available] = 0; // reset once the data is handed out
  rnd->output_available--;
  return x;
}

static inline uint32_t read32(const uint8_t* p, size_t idx32) {
  const size_t i = 4*idx32;
  return ((uint32_t)p[i+0] | (uint32_t)p[i+1] << 8 | (uint32_t)p[i+2] << 16 | (uint32_t)p[i+3] << 24);
}

static void chacha_init(random_ctx_t* rnd, const uint8_t key[32], uint64_t nonce)
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
}

static void chacha_split(random_ctx_t* rnd, uint64_t nonce, random_ctx_t* ctx_new) {
  memset(ctx_new, 0, sizeof(*ctx_new));
  memcpy(ctx_new->input, rnd->input, sizeof(ctx_new->input));
  ctx_new->input[12] = 0;
  ctx_new->input[13] = 0;
  ctx_new->input[14] = (uint32_t)nonce;
  ctx_new->input[15] = (uint32_t)(nonce >> 32);
  assert_internal(rnd->input[14] != ctx_new->input[14] || rnd->input[15] != ctx_new->input[15]); // do not reuse nonces!
  chacha_block(ctx_new);
}


/* ----------------------------------------------------------------------------
Random interface
-----------------------------------------------------------------------------*/

#if MI_DEBUG>1
static bool random_is_initialized(random_ctx_t* rnd) {
  return (rnd != NULL && rnd->input[0] != 0);
}
#endif

void random_split(random_ctx_t* rnd, random_ctx_t* ctx_new) {
  assert_internal(random_is_initialized(rnd));
  assert_internal(rnd != ctx_new);
  chacha_split(rnd, (uintptr_t)ctx_new /*nonce*/, ctx_new);
}

uint32_t random_next32(random_ctx_t* rnd) {
  assert_internal(random_is_initialized(rnd));
  return chacha_next32(rnd);
}

uint64_t random_next64(random_ctx_t* rnd) {
  return (((uint64_t)random_next32(rnd) << 32) | random_next32(rnd));
}

uintx_t random_next(random_ctx_t* rnd) {
#if INTX_SIZE <= 4
  return random_next32(rnd);
#elif INTX_SIZE == 8
  return random_next64(rnd);
#else
# error "define random_next for this platform"
#endif
}


/* ----------------------------------------------------------------------------
To get an initial secure random context we rely on the OS:
- Windows     : BCryptGenRandom
- osX,bsd,wasi: arc4random_buf
- Linux       : getrandom,/dev/urandom
If we cannot get good randomness, we fall back to weak randomness based on a timer and ASLR.
-----------------------------------------------------------------------------*/

#if defined(_WIN32)
#pragma comment (lib,"bcrypt.lib")
#include <windows.h>
#include <bcrypt.h>
static bool os_random_buf(void* buf, size_t buf_len) {
  return (BCryptGenRandom(NULL, (PUCHAR)buf, (ULONG)buf_len, BCRYPT_USE_SYSTEM_PREFERRED_RNG) >= 0);
}
/*
#define SystemFunction036 NTAPI SystemFunction036
#include <NTSecAPI.h>
#undef SystemFunction036
static bool os_random_buf(void* buf, size_t buf_len) {
  RtlGenRandom(buf, (ULONG)buf_len);
  return true;
}
*/
#elif defined(ANDROID) || defined(XP_DARWIN) || defined(__APPLE__) || defined(__DragonFly__) || \
      defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__) || \
      defined(__wasi__)
#include <stdlib.h>
static bool os_random_buf(void* buf, size_t buf_len) {
  arc4random_buf(buf, buf_len);
  return true;
}
#elif defined(__linux__)
#include <sys/syscall.h>
#include <unistd.h>
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

#if defined(_WIN32)
#include <windows.h>
#elif defined(__APPLE__)
#include <mach/mach_time.h>
#else
#include <time.h>
#endif


static uint64_t os_random_weak(uint64_t extra_seed) {
  uint64_t x = (uint64_t)&os_random_weak ^ extra_seed; // hopefully, ASLR makes the address random
  #if defined(_WIN32)
    LARGE_INTEGER pcount;
    QueryPerformanceCounter(&pcount);
    x ^= (uint64_t)(pcount.QuadPart);
  #elif defined(__APPLE__)
    x ^= (uint64_t)mach_absolute_time();
  #else
    struct timespec time;
    clock_gettime(CLOCK_MONOTONIC, &time);
    x ^= rotl64((uint64_t)time.tv_sec, 32);
    x ^= (uint64_t)time.tv_nsec;
  #endif  
  assert_internal(x != 0);
  return x;
}

bool random_init(random_ctx_t* rnd) {
  uint8_t key[32];
  const bool secure = os_random_buf(key, sizeof(key));
  if (!secure) {
    // if we fail to get random data from the OS, we fall back to a
    // weak random source based on the current time and ASLR.
    warning_message("unable to use secure randomness\n");
    pcg_ctx_t pcg;
    pcg_init(os_random_weak(rand()), 0, &pcg);
    for (size_t i = 0; i < 8; i++) {  // key is eight 32-bit words.
      uint32_t x = pcg_next32(&pcg);
      ((uint32_t*)key)[i] = x;
    }
  }
  chacha_init(rnd, key, (uintptr_t)rnd /*nonce*/ );
  return secure;
}
