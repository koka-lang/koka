// 2023, Daan Leijen
//
// Using standard STL to test the red-black tree in C++
// In glibc++ this uses <https://github.com/gcc-mirror/gcc/tree/master/libstdc++-v3/src/c++98/tree.cc>
// With the LLVM libc++ this uses <https://github.com/llvm/llvm-project/blob/main/libcxx/include/__tree>
// In glibc this uses eventually: <https://sourceware.org/git/?p=glibc.git;a=blob;f=misc/tsearch.c>
// (Highly optimized in-place red-black tree using the low pointer bit to encode color information.)

#define ITER 100
#define SCALEDOWN 1
#define USE_SFC 1

#include <iostream>
#include <map>
#include <algorithm>
using std::for_each;

typedef int    tkey_t;
typedef tkey_t tval_t;

struct lt_fn {
    bool operator()(tkey_t const & n1, tkey_t const & n2) const { return n1 < n2; }
};

typedef std::map<tkey_t, tval_t, lt_fn> tree;

/*
map mk_map(unsigned n) {
    map m;
    while (n > 0) {
        --n;
        m.insert(std::make_pair(nat(n), n%10 == 0));
    }
    return m;
}

nat fold(map const & m) {
    nat r(0);
    for_each(m.begin(), m.end(), [&](std::pair<nat, bool> const & p) { if (p.second) r = r + nat(1); });
    return r;
}

int main(int argc, char ** argv) {
    unsigned n = 4200000;
    if (argc == 2) {
      n = atoi(argv[1]);
    }
    map m = mk_map(n);
    std::cout << fold(m) << "\n";
    return 0;
}
*/


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

static long long sum(tree const& t)
{
  long long count(0);
  for_each(t.begin(), t.end(), [&](std::pair<tkey_t, tval_t> const & p) { count += p.second; });
  return count;
}

static void access(tree& t, tkey_t k) {
  t.insert(std::make_pair(k,k));  
}

// euclidean modulus
static int modE( int i, int j ) {
  int m = i%j;
  if (i < 0 && m < 0) { m += (j < 0 ? -j : j); } 
  return m;
}



static void test(int n) {
  tree t;
  rnd_t rnd; 
  rnd_init32(42,43,&rnd);
  for(int iter = 0; iter < ITER/SCALEDOWN; iter++) {
    for(int i = 0; i < n; i++) {
      tkey_t j = rnd_step(&rnd);
      access(t, modE(j,n));
    }
  }
  const long x = rnd_step(&rnd);
  printf("sum: %lld, final access: %ld\n", sum(t), x);
  //tfree(t);
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
