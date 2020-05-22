#pragma once

#include "runtime.h"

/*--------------------------------------------------------------------------------------------------
  Integers are always boxed: and either a pointer to a `bigint_t`, or a boxed (small) int.
  The boxed small int is restricted in size to SMALLINT_BITS such that we can do
  efficient arithmetic on the boxed representation of a small int directly where
  `boxed(n) == 4*n + 1`. The `smallint_t` size is chosen to allow efficient overflow detection.
--------------------------------------------------------------------------------------------------*/


#if INTPTR_SIZE==8                           // always us 32-bits on 64-bit platforms
typedef int32_t smallint_t;
#define SMALLINT_BITS       (32)
#elif (INTPTR_SIZE==4) && defined(__GNUC__)  // with overflow builtins we can use the full 32 bits
typedef int32_t smallint_t;
#define SMALLINT_BITS       (32)
#elif INTPTR_SIZE==4                         // otherwise generic overflow detect uses 16-bit operands with 32-bit operations
typedef int16_t smallint_t;
#define SMALLINT_BITS       (16)
#else
# error "platform must be 32 or 64 bits."
#endif

#define SMALLINT_MAX  ((intptr_t)(((uintptr_t)INTPTR_MAX >> (INTPTR_BITS - SMALLINT_BITS)) >> 2))  // use unsigned shift to avoid UB
#define SMALLINT_MIN  (-SMALLINT_MAX - 1)

static inline bool is_integer(integer i) {
  return ((is_int(i)  && unbox_int(i) >= SMALLINT_MIN && unbox_int(i) <= SMALLINT_MAX) 
         || (is_ptr(i) && ptr_tag(unbox_ptr(i)) == TAG_BIGINT));
}

static inline bool is_bigint(integer i) {
  assert(is_integer(i));
  return is_ptr_fast(i);
}

static inline bool is_smallint(integer i) {
  assert(is_integer(i));
  return is_int_fast(i);
}

static inline bool are_smallints(integer i, integer j) {
  assert(is_integer(i) && is_integer(j));
  return ((i&j)&1)!=0;
}

static inline integer integer_from_small(intptr_t i) {   // use for known small ints (under 14 bits)
  assert(i >= SMALLINT_MIN && i <= SMALLINT_MAX);
  return box_int(i);
}

/*---------------------------------------------------------------------------------
  Generic operations on integers
-----------------------------------------------------------------------------------*/

static inline box_t   box_integer(integer i) { return i; }
static inline integer unbox_integer(box_t b) { return b; }

static inline void    integer_incref(integer x) { boxed_incref(x); }
static inline void    integer_decref(integer x) { boxed_decref(x); }
static inline integer integer_dup(integer x)    { return boxed_dup(x); }

decl_export integer  integer_parse(const char* num);
decl_export integer  integer_from_str(const char* num); // for known correct string number
decl_export integer  integer_from_big(intptr_t i);      // for possibly large i

decl_export integer  integer_add_generic(integer x, integer y);
decl_export integer  integer_sub_generic(integer x, integer y);
decl_export integer  integer_mul_generic(integer x, integer y);
decl_export integer  integer_div_generic(integer x, integer y);
decl_export integer  integer_mod_generic(integer x, integer y);
decl_export integer  integer_div_mod_generic(integer x, integer y, integer* mod);

decl_export int      integer_cmp_generic(integer x, integer y);
decl_export integer  integer_neg_generic(integer x);
decl_export integer  integer_sqr_generic(integer x);
decl_export integer  integer_pow(integer x, integer p);

decl_export bool     integer_is_even_generic(integer x);
decl_export int      integer_signum_generic(integer x);

decl_export integer  integer_ctz(integer x);           // count trailing zero digits
decl_export integer  integer_count_digits(integer x);  // count decimal digits
decl_export integer  integer_mul_pow10(integer x, integer p);  // x*(10^p)
decl_export integer  integer_div_pow10(integer x, integer p);  // x/(10^p)

decl_export void     integer_fprint(FILE* f, integer x);
decl_export void     integer_print(integer x);


/*---------------------------------------------------------------------------------
  Inlined operation to allow for fast operation on small integers
-----------------------------------------------------------------------------------*/

static inline integer integer_from_int(intptr_t i) {
  return (likely(i >= SMALLINT_MIN && i <= SMALLINT_MAX) ? integer_from_small(i) : integer_from_big(i));
}

#if defined(__GNUC__) && (SMALLINT_BITS >= 32)  

// Use the overflow detecting primitives

static inline bool smallint_add_ovf(intptr_t h, intptr_t y, intptr_t* r) {
  smallint_t i;
  const bool ovf = __builtin_add_overflow((smallint_t)h, (smallint_t)y, &i);
  *r = (intptr_t)i; // sign extend
  return ovf;
}

static inline bool smallint_sub_ovf(intptr_t h, intptr_t y, intptr_t* r) {
  smallint_t i;
  const bool ovf = __builtin_sub_overflow((smallint_t)h, (smallint_t)y, &i);
  *r = (intptr_t)i; // sign extend
  return ovf;
}

static inline bool smallint_mul_ovf(intptr_t h, intptr_t y, intptr_t* r) {
  smallint_t i;
  const bool ovf = __builtin_mul_overflow((smallint_t)h, (smallint_t)y, &i);
  *r = (intptr_t)i; // sign extend
  return ovf;
}

#else 
// Generic overflow detection, still quite good.

static inline bool smallint_add_ovf(intptr_t x, intptr_t y, intptr_t* r) {
  intptr_t z = x + y; // do a full add
  *r = z;
  uintptr_t s = (uintptr_t)(sar(z, SMALLINT_BITS - 1)); // cast to unsigned to avoid UB for s+1 on overflow
  return (s + 1 > 1); // are the top bits not 0 or -1 ?
}

static inline bool smallint_sub_ovf(intptr_t x, intptr_t y, intptr_t* r) {
  intptr_t z = x - y; // do a full sub
  *r = z;
  uintptr_t s = (uintptr_t)(sar(z, SMALLINT_BITS - 1)); // cast to unsigned to avoid UB for s+1 on overflow
  return (s + 1 > 1); // are the top bits not 0 or -1 ?
}

static inline bool smallint_mul_ovf(intptr_t x, intptr_t y, intptr_t* r) {
  intptr_t z = x * y; // do a full multiply (as smallint is at most half the intptr_t size)
  *r = z;
  uintptr_t s = (uintptr_t)(sar(z, SMALLINT_BITS - 1)); // cast to unsigned to avoid UB for s+1 on overflow
  return (s + 1 > 1); // are the top bits not 0 or -1 ?
}

#endif


/* Fast addition on small integers. Since `boxed(n) = n*4 + 1`, we can add as:
      boxed(n) + (boxed(m) - 1)
      = (n*4 + 1) + ((m*4 + 1) - 1)
      = n*4 + m*4 + 1
      = 4*(n + m) + 1
      = boxed(n+m)
    (we use (^ 1) instead of (- 1) to reduce register stalls (since we know the bottom bits of `y` are 01)
*/
static inline integer integer_add_small(integer x, integer y) {
  assert(are_smallints(x, y));
  intptr_t i;
  if (likely(!smallint_add_ovf((intptr_t)x, (intptr_t)(y^1), &i))) return (integer)i;
  return integer_add_generic(x, y);
}


/*
static inline integer integer_add(integer x, integer y) {
  if (likely(are_smallints(x, y))) return integer_add_small(x, y);
  return integer_add_generic(x, y);
}
*/

/* We further optimize addition on integers, if we add directly, our lowest 2 bits are :
    x + y = z
    00  00  00    ptr + ptr
    00  01  01    ptr + int
    01  00  01    int + ptr
    01  01  10    int + int
  and we can detect afterwards if it was correct to assume these were smallint's
*/
static inline integer integer_add(integer x, integer y) {
  assert(is_integer(x) && is_integer(y));
  intptr_t i;
  if (likely(!smallint_add_ovf((intptr_t)x, (intptr_t)y, &i) && (i&2)!=0)) {
    integer z = (integer)(i) ^ 3;  // == i - 1
    assert(is_int(z));
    return z;
  }
  return integer_add_generic(x, y);
}


/* Fast subtract on small integers. Since `boxed(n) = n*4 + 1`, we can subtract as:
      boxed(n) - (boxed(m) - 1)
      = (n*4 + 1) - ((m*4 + 1) - 1)
      = n*4 + 1 - m*4
      = (n - m)*4 + 1
      = boxed(n-m)
*/
static inline integer integer_sub_small(integer x, integer y) {
  assert(are_smallints(x, y));
  intptr_t i;
  if (likely(!smallint_sub_ovf((intptr_t)x, (intptr_t)(y^1), &i))) return (integer)(i);
  return integer_sub_generic(x, y);
}

static inline integer integer_sub(integer x, integer y) {
  if (likely(are_smallints(x, y))) return integer_sub_small(x, y);
  return integer_sub_generic(x, y);
}

/* Fast multiply on small integers. Since `boxed(n) = n*4 + 1`, we can multiply as:
    (boxed(n)/2) * (boxed(m)/2) + 1
    = (n*4+1)/2 * (m*4+1)/2 + 1
    = (n*2) * (m*2) + 1
    = (n*m*4) + 1
    = boxed(n*m)
*/
static inline integer integer_mul_small(integer x, integer y) {
  assert(are_smallints(x, y));
  intptr_t i = sar((intptr_t)x, 1);
  intptr_t j = sar((intptr_t)y, 1);
  intptr_t k;
  if (likely(!smallint_mul_ovf(i, j, &k))) {
    integer z = (integer)(k)|1;
    assert(is_int(z));
    return z;
  }
  return integer_mul_generic(x, y);
}

static inline integer integer_mul(integer x, integer y) {
  if (likely(are_smallints(x, y))) return integer_mul_small(x, y);
  return integer_mul_generic(x, y);
}

/* Fast divide on small integers. Since `boxed(n) = n*4 + 1`, we can divide as:
    4*((boxed(n)/2)/((boxed(m)/2) + 1
    = 4*((n*2)/(m*2)) + 1
    = 4*(n/m) + 1
    = boxed(n/m)
*/
static inline integer integer_div_small(integer x, integer y) {
  assert(are_smallints(x, y));
  intptr_t i = sar((intptr_t)x, 1);
  intptr_t j = sar((intptr_t)y, 1);
  return (shr(i/j, 2)|1);
}

/* Fast modulus on small integers. Since `boxed(n) = n*4 + 1`, we can divide as:
    2*((boxed(n)/2)/((boxed(m)/2) + 1
    = 2*((n*2)%(m*2)) + 1
    = 2*2*(n%m) + 1
    = boxed(n%m)
*/
static inline integer integer_mod_small(integer x, integer y) {
  assert(are_smallints(x, y));
  intptr_t i = sar((intptr_t)x, 1);
  intptr_t j = sar((intptr_t)y, 1);
  return (shr(i%j, 1)|1);
}

static inline integer integer_div_mod_small(integer x, integer y, integer* mod) {
  assert(are_smallints(x, y)); assert(mod!=NULL);
  intptr_t i = sar((intptr_t)x, 1);
  intptr_t j = sar((intptr_t)y, 1);
  *mod = shr(i%j, 1)|1;
  return (shr(i/j, 2)|1);
}

static inline integer integer_div(integer x, integer y) {
  if (likely(are_smallints(x, y))) return integer_div_small(x, y);
  return integer_div_generic(x, y);
}

static inline integer integer_mod(integer x, integer y) {
  if (likely(are_smallints(x, y))) return integer_mod_small(x, y);
  return integer_mod_generic(x, y);
}

static inline integer integer_div_mod(integer x, integer y, integer* mod) {
  assert(mod!=NULL);
  if (likely(are_smallints(x, y))) return integer_div_mod_small(x, y, mod);
  return integer_div_mod_generic(x, y, mod);
}


static inline integer integer_sqr(integer x) {
  if (likely(is_smallint(x))) return integer_mul_small(x,x);
  return integer_sqr_generic(x);
}

static inline integer integer_neg(integer x) {
  if (likely(is_smallint(x))) return integer_sub_small(box_int(0),x);
  return integer_neg_generic(x);
}

static inline integer integer_dec(integer x) {
  if (likely(is_smallint(x))) return integer_sub(x,integer_from_small(1));
  return integer_sub_generic(x, integer_from_small(1));
}

static inline integer integer_inc(integer x) {
  if (likely(is_smallint(x))) return integer_add(x, integer_from_small(1));
  return integer_add_generic(x, integer_from_small(1));
}

static inline int integer_cmp(integer x, integer y) {
  if (likely(are_smallints(x, y))) return (x == y ? 0 : (x > y ? 1 : -1));
  return integer_cmp_generic(x, y);
}

static inline bool integer_lt(integer x, integer y) {
  if (likely(are_smallints(x, y))) return (x < y);
  return (integer_cmp(x, y) == -1);
}

static inline bool integer_lte(integer x, integer y) {
  if (likely(are_smallints(x, y))) return (x <= y);
  return (integer_cmp(x, y) <= 0);
}

static inline bool integer_gt(integer x, integer y) {
  if (likely(are_smallints(x, y))) return (x > y);
  return (integer_cmp(x, y) == 1);
}

static inline bool integer_gte(integer x, integer y) {
  if (likely(are_smallints(x, y))) return (x >= y);
  return (integer_cmp(x, y) >= 0);
}

static inline bool integer_eq(integer x, integer y) {
  if (likely(are_smallints(x, y))) return (x == y);
  return (integer_cmp(x, y) == 0);
}

static inline bool integer_neq(integer x, integer y) {
  if (likely(are_smallints(x, y))) return (x != y);
  return (integer_cmp(x, y) != 0);
}

static inline bool integer_is_zero(integer x) {
  if (likely(is_smallint(x))) return (x == integer_from_small(0));
  integer_decref(x);
  return false;
}

static inline bool integer_is_one(integer x) {
  if (likely(is_smallint(x))) return (x == integer_from_small(1));
  integer_decref(x);
  return false;
}

static inline bool integer_is_minus_one(integer x) {
  if (likely(is_smallint(x))) return (x == integer_from_small(-1));
  integer_decref(x);
  return false;
}

static inline bool integer_is_even(integer x) {
  if (likely(is_smallint(x))) return ((x&0x08)==0);
  return integer_is_even_generic(x);
}

static inline bool integer_is_odd(integer x) {
  if (likely(is_smallint(x))) return ((x&0x08)!=0);
  return !integer_is_even_generic(x);
}

static inline int integer_signum(integer x) {
  if (likely(is_smallint(x))) return (((intptr_t)x>1)-((intptr_t)x<0));
  return integer_signum_generic(x);
}

static inline bool integer_is_neg(integer x) {
  if (likely(is_smallint(x))) return ((intptr_t)x<0);
  return (integer_signum_generic(x) < 0);
}

static inline bool integer_is_pos(integer x) {
  if (likely(is_smallint(x))) return ((intptr_t)x>1);
  return (integer_signum_generic(x) > 0);
}

static inline integer integer_max(integer x, integer y) {
  if (likely(are_smallints(x, y))) return (x>=y ? x : y);
  integer_incref(x); integer_incref(y);
  if (integer_gte(x,y)) {
    integer_decref(y); return x;
  }
  else {
    integer_decref(x); return y;
  }
}

static inline integer integer_min(integer x, integer y) {
  if (likely(are_smallints(x, y))) return (x<=y ? x : y);
  integer_incref(x); integer_incref(y);
  if (integer_lte(x, y)) {
    integer_decref(y); return x;
  }
  else {
    integer_decref(x); return y;
  }
}