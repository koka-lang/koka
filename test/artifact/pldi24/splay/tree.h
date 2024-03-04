#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>

#define ITER 100
#define SCALEDOWN 1
#define USE_SFC 1

#ifndef ADDHEADER
// #define ADDHEADER 1
#endif

#ifndef ADDPARENT
// #define ADDPARENT 1
#endif

typedef long tkey_t;

typedef struct tree_s {
    #if ADDHEADER
    int64_t header;
    #endif
    #if ADDPARENT
    struct tree_s* parent;
    #endif
    struct tree_s* left;
    tkey_t         key;
    struct tree_s* right;
} tree_t;

static tree_t* access(tree_t* t, tkey_t i);


// --------------------------------------------------------------------
// Tree
// --------------------------------------------------------------------
#ifdef ALLOCSTATS
static long alloc_count = 0;
#endif

static tree_t* node( tree_t* left, tkey_t key, tree_t* right ) {
  #ifdef ALLOCSTATS
  alloc_count++;
  #endif
  tree_t* t = malloc(sizeof(tree_t));
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

static inline tree_t* leaf(void) {
  return NULL;
}

static inline bool is_node( const tree_t* t ) {
  return (t != NULL);
}

static inline bool is_leaf( const tree_t* t ) {
  return (t == NULL);
}


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
// benchmark
// --------------------------------------------------------------------

static long long sum(tree_t* t) {
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

static long max_height(tree_t* t ) {
  if (is_leaf(t)) return 0;
  return 1 + max(max_height(t->left), max_height(t->right));    
}

static long min_height(tree_t* t ) {
  if (is_leaf(t)) return 0;
  return 1 + min(min_height(t->left), min_height(t->right));   
}

static long top(tree_t* t) {
  if (is_leaf(t)) return 0;
            else return t->key;
}

static void tfree(tree_t* t) {
  while( !is_leaf(t) ) {
    tfree(t->right);
    tree_t* l = t->left;
    free(t);
    t = l;
  }
}



static void test(int n) {
  tree_t* t = leaf();
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
