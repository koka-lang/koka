// 2023, Daan Leijen
//
// Bottom up red-black tree insertion as in 'Introduction to Algorithms', Cormen, Leiserson, Rivest, Stein

//#if ADDPARENT  
//#define BAILOUT_EARLY 0
//#endif

#ifndef BAILOUT_EARLY
#define BAILOUT_EARLY 1
#endif

#define ITER 100
#define SCALEDOWN 1
#define USE_SFC 1

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
 
typedef intptr_t tkey_t;
typedef tkey_t tval_t;

#define MAX(a,b) (((a)>(b))?(a):(b))

enum Color { RED, BLACK };

typedef struct Node {
    tkey_t key;
    tval_t value;
    enum Color color;
    struct Node *left;
    struct Node *right;
    struct Node *parent;
} Node;

static Node nnil = { 0, 0, BLACK, NULL, NULL, NULL };
static Node* nil = &nnil;

static bool is_leaf( Node* x ) { return x == nil; }
static bool is_node( Node* x ) { return x != nil; }

static void left_rotate(Node** root, Node *x) {
    Node *y = x->right;
    x->right = y->left;
    if (y->left != nil) {
        y->left->parent = x;
    }
    y->parent = x->parent;
    if (x->parent == nil) {
        *root = y;
    } else if (x == x->parent->left) {
        x->parent->left = y;
    } else {
        x->parent->right = y;
    }
    y->left = x;
    x->parent = y;
}

void right_rotate(Node** root, Node *x) {
    Node *y = x->left;
    x->left = y->right;
    if (y->right != nil) {
        y->right->parent = x;
    }
    y->parent = x->parent;
    if (x->parent == nil) {
        *root = y;
    } else if (x == x->parent->right) {
        x->parent->right = y;
    } else {
        x->parent->left = y;
    }
    y->right = x;
    x->parent = y;
}

static void fixup(Node** root, Node *z) {
    while (z->parent != nil) {
      if (z->parent->color == RED && z->color == RED) {
        if (z->parent == z->parent->parent->left) {
            Node *y = z->parent->parent->right;
            if (y->color == RED) {
                z->parent->color = BLACK;
                y->color = BLACK;
                z->parent->parent->color = RED;
                z = z->parent->parent;
            } else {
                if (z == z->parent->right) {
                    z = z->parent;
                    left_rotate(root, z);
                }
                z->parent->color = BLACK;
                z->parent->parent->color = RED;
                right_rotate(root, z->parent->parent);
            }
        } else {
            Node *y = z->parent->parent->left;
            if (y->color == RED) {
                z->parent->color = BLACK;
                y->color = BLACK;
                z->parent->parent->color = RED;
                z = z->parent->parent;
            } else {
                if (z == z->parent->left) {
                    z = z->parent;
                    right_rotate(root, z);
                }
                z->parent->color = BLACK;
                z->parent->parent->color = RED;
                left_rotate(root, z->parent->parent);
            }
        }    
      }
      else {  // no red-red violation anymore
        #if BAILOUT_EARLY
        break;
        #else
        z = z->parent;
        #endif
      }
  };
  if (z->parent == nil) { *root = z; }
  (*root)->color = BLACK;
}

void access(Node** root, tkey_t key, tval_t value) {
    Node* p = nil;
    Node* x = *root;
    while (x != nil) {
        p = x;
        if (x->key < key) {
          x = x->right;
        } else if (x->key > key) {
          x = x->left;
        }
        else {
          return;  // already present
        }
    }

    // create fresh node
    x = (Node*)malloc(sizeof(Node));
    x->key = key;
    x->value = value;
    x->parent = p;
    x->left = x->right = nil;
    x->color = RED;

    // link back parent
    if (p == nil) {
        *root = x;
    } else if (p->key < key) {
        p->right = x;
    } else {
        p->left = x;
    }

    // rebalance up
    fixup(root, x);
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

static long long sum(Node* t) {
  long long n = 0;
  while( !is_leaf(t) ) {        
    n += t->value;
    n += sum(t->right);
    t = t->left;
  }
  return n;
}


static inline long max(long x, long y) { return (x >= y ? x : y); }
static inline long min(long x, long y) { return (x <= y ? x : y); }

static long max_height(Node* t ) {
  if (is_leaf(t)) return 0;
  return 1 + max(max_height(t->left), max_height(t->right));    
}

static long min_height(Node* t ) {
  if (is_leaf(t)) return 0;
  return 1 + min(min_height(t->left), min_height(t->right));   
}

static long top(Node* t) {
  if (is_leaf(t)) return 0;
            else return t->key;
}


static void tfree(Node* t) {
  if (t != nil) {
    tfree(t->left);
    tfree(t->right);
    free(t);  
  }
}

// euclidean modulus
static int modE( int i, int j ) {
  int m = i%j;
  if (i < 0 && m < 0) { m += (j < 0 ? -j : j); } 
  return m;
}



static void test(int n) {
  Node* t = nil;
  rnd_t rnd; 
  rnd_init32(42,43,&rnd);
  for(int iter = 0; iter < ITER/SCALEDOWN; iter++) {
    for(int i = 0; i < n; i++) {
      tkey_t j = rnd_step(&rnd);
      tkey_t k = modE(j,n);
      access(&t, k, k);
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
