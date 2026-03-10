#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>

#define ITER 100
#define SCALEDOWN 1
#define USE_SFC 1

#ifndef ADDHEADER
//#define ADDHEADER 1
#endif

#ifndef ADDPARENT
//#define ADDPARENT 1
#endif

typedef intptr_t tkey_t;
typedef int32_t  rank_t;

typedef struct ztree_s {
    #if ADDHEADER
    int64_t header;
    #endif
    #if ADDPARENT
    struct ztree_s* parent;
    #endif
    rank_t        rank;
    struct ztree_s* left;
    tkey_t         key;
    struct ztree_s* right;
} ztree_t;

static ztree_t* access(ztree_t* t, tkey_t i);


// --------------------------------------------------------------------
// ztree
// --------------------------------------------------------------------
#ifdef ALLOCSTATS
static long alloc_count = 0;
#endif

static ztree_t* node( ztree_t* left, tkey_t key, ztree_t* right ) {
  #ifdef ALLOCSTATS
  alloc_count++;
  #endif
  ztree_t* t = malloc(sizeof(ztree_t));
  #if ADDHEADER
  t->header = 0;
  #endif
  t->left = left;
  t->key = key;
  t->right = right;
  #if ADDPARENT
  t->parent = NULL;
  #endif
  return t;
}

static inline ztree_t* leaf(void) {
  return NULL;
}

static inline bool is_node( const ztree_t* t ) {
  return (t != NULL);
}

static inline bool is_leaf( const ztree_t* t ) {
  return (t == NULL);
}


// euclidean modulus
static int modE( int i, int j ) {
  int m = i%j;
  if (i < 0 && m < 0) { m += (j < 0 ? -j : j); } 
  return m;
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

static inline rank_t rank_of1( tkey_t k ) {
  // by Chris Wellons, see: <https://nullprogram.com/blog/2018/07/31/>
  int32_t x = (int32_t)k + 1;
  x ^= shr(x,15);
  x *= 0x2c1b3c6d; 
  x ^= shr(x,12);
  x *= 0x297a2d39;
  x ^= shr(x,15);
  return (rank_t)ctz32(x);
}

static inline rank_t rank_of( tkey_t k ) {
  int32_t x = (int32_t)k + 1;
  x ^= shr(x,16);
  x *= 0x297a2d39;
  x ^= shr(x,16);
  return (rank_t)ctz32(x);
}


// --------------------------------------------------------------------
// pseudo random number context (using sfc32)
// --------------------------------------------------------------------

struct rnd_s;
typedef struct rnd_s rnd_t;

static inline int32_t rnd_step(rnd_t* rnd);
static void rnd_init32(int32_t seed1, int32_t seed2, rnd_t* rnd);

#if USE_SFC

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
// benchmark
// --------------------------------------------------------------------

static long long sum(ztree_t* t) {
  long long n = 0;
  while( !is_leaf(t) ) {        
    n += t->key;
    n += sum(t->right);
    t = t->left;
  }
  return n;
}

static inline long max(long x, long y) { return (x >= y ? x : y); }
static inline long min(long x, long y) { return (x <= y ? x : y); }

static long max_height(ztree_t* t ) {
  if (is_leaf(t)) return 0;
  return 1 + max(max_height(t->left), max_height(t->right));    
}

static long min_height(ztree_t* t ) {
  if (is_leaf(t)) return 0;
  return 1 + min(min_height(t->left), min_height(t->right));   
}

static long top(ztree_t* t) {
  if (is_leaf(t)) return 0;
            else return t->key;
}

static void tfree(ztree_t* t) {
  while( !is_leaf(t) ) {
    tfree(t->right);
    ztree_t* l = t->left;
    free(t);
    t = l;
  }
}



static void test(int n) {
  ztree_t* t = leaf();
  rnd_t rnd; 
  rnd_init32(42,43,&rnd);
  for(int iter = 0; iter < ITER/SCALEDOWN; iter++) {
    for(int i = 0; i < n; i++) {
      tkey_t j = rnd_step(&rnd);
      t = access(t, modE(j,n));
    }
  }
  const long x = rnd_step(&rnd);  
  printf("sum: %lld, height: %ld/%ld, top: %ld, final access: %ld\n", sum(t), max_height(t), min_height(t), top(t), x);
  tfree(t);
}

int main(int argc, char** argv) {
  int n = 100000;
  if (argc > 1) {
      n = atoi(argv[1]);
  }
  test(n/SCALEDOWN);
  #ifdef ALLOCSTATS
  printf("node allocations: %ld\n\n", alloc_count);
  #endif
  return 0;
}
