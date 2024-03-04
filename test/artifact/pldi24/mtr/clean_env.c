#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>

#define ITER 100
#define SCALEDOWN 1
#define USE_SFC 1

// euclidean modulus
static int modE( int i, int j ) {
  int m = i%j;
  if (i < 0 && m < 0) { m += (j < 0 ? -j : j); } 
  return m;
}

struct rnd_s;
typedef struct rnd_s rnd_t;

static inline int32_t rnd_step(rnd_t* rnd);
static void rnd_init32(int32_t seed1, int32_t seed2, rnd_t* rnd);

#if USE_SFC

// --------------------------------------------------------------------
// pseudo random number context (using sfc32)
// --------------------------------------------------------------------

struct rnd_s {
  int32_t a;
  int32_t b;
  int32_t c;
  int32_t counter;
};

static inline int32_t rotl32(int32_t x, int32_t shift) {
  shift &= 31;
  return (x << shift) | (int32_t)((uint32_t)x >> (32 - shift));
}

// Pseudo random number using sfc32 by Chris Doty-Humphrey.
// It is a "chaotic" pseudo random generator that uses 32-bit operations only
// (so we can be deterministic across architectures in results and performance).
// It has good statistical properties and passes PractRand and Big-crush.
// It uses a 32-bit counter to guarantee a worst-case cycle
// of 2^32. It has a 96-bit state, so the average period is 2^127.
// The chance of a cycle of less than 2^(32+max(96-k,0)) is 2^-(32+k),
// (e.g. the chance of a cycle of less than 2^48 is 2^-80).
// <http://pracrand.sourceforge.net/RNG_engines.txt>
static inline int32_t rnd_step(rnd_t* rnd) {
  int32_t x = rnd->a + rnd->b + rnd->counter;
  rnd->counter++;
  rnd->a = rnd->b ^ (int32_t)((uint32_t)(rnd->b) >> 9);
  rnd->b = rnd->c + (rnd->c << 3);
  rnd->c = rotl32(rnd->c, 21) + x;
  return x;
}

static void rnd_init32(int32_t seed1, int32_t seed2, rnd_t* rnd) {
  rnd->a = 0;
  rnd->b = seed1;
  rnd->c = seed2;
  rnd->counter = 1;
  for (size_t i = 0; i < 12; i++) {
    rnd_step(rnd);
  }
}

#else

// --------------------------------------------------------------------
// pseudo random number context (using sfc32)
// --------------------------------------------------------------------

struct rnd_s {
  int64_t seed;
};


static inline int64_t rotr64(int64_t x, int shift) {
  shift &= 63;
  return ((int64_t)((uint64_t)x >> shift)) | (x << (64 - shift));
}

static inline int32_t rnd_step(rnd_t* rnd) {
  int64_t s = rnd->seed * 134775813 + 1;
  int32_t x = (int32_t)(rotr64(s,17) >> 32);
  rnd->seed = s;
  return x;
}

static void rnd_init32(int32_t seed1, int32_t seed2, rnd_t* rnd) {
  int64_t s = (int64_t)seed1 * (int64_t)seed2;
  rnd->seed = s;
  for (int i = 0; i < 12; i++) {
    rnd_step(rnd);
  }
}

#endif

// --------------------------------------------------------------------
// interface for clean
// --------------------------------------------------------------------

rnd_t* global_rnd;

int rnd_create_c(int seed1, int seed2) {
  global_rnd = (rnd_t*) malloc(sizeof(rnd_t));
  rnd_init32(seed1, seed2, global_rnd);
  return 0;
}

int rnd_next_c(int n) {
    return modE(rnd_step(global_rnd), n);
}

int report_result_c(long sum, long max_height, long min_height, long top) {
  const long x = rnd_step(global_rnd);
  printf("sum: %ld, height: %ld/%ld, top: %ld, final access: %ld\n", sum, max_height, min_height, top, x);
  return 0;
}

int report_int_c(int sum) {
  printf("sum: %d\n", sum);
  return 0;
}

// --------------------------------------------------------------------
// hash a key pseudo randomly
// --------------------------------------------------------------------

#if defined(__GNUC__)
#if (LONG_MAX == INT32_MAX) 
 #define __builtin32(name)  __builtin_##name##l
#else
 #define __builtin32(name)  __builtin_##name
#endif
static inline int ctz32(int32_t x) {
  return (x==0 ? 32 : __builtin32(ctz)(x));
}
#else
#error "define ctz32"
#endif

static inline int32_t rotr32(int32_t x, uint32_t shift) {
  shift &= 31;
  return (int32_t)(((uint32_t)x >> shift) | ((uint32_t)x << (32 - shift)));
}

static inline int32_t shr(int32_t x, uint32_t shift) {
  shift &= 31;
  return (int32_t)((uint32_t)x >> shift);
}

int32_t rank_of_c( long k ) {
  int32_t x = (int32_t)k + 1;
  x ^= shr(x,16);
  x *= 0x297a2d39;
  x ^= shr(x,16);
  return (int32_t)ctz32(x);
}