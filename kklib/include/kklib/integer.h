#pragma once
#ifndef INTEGER_H_
#define INTEGER_H_
/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------------------------------
Integers are either a pointer to a `kk_bigint_t` (with lowest bit == 0),  
or a _small_ int (with lowest bit == 1).

The small int is restricted in size to KK_SMALLINT_BITS such that we can do
efficient arithmetic on the representation of a small int `n` directly where
`boxed(n) == 4*n + 1`. The `kk_smallint_t` size is chosen to allow efficient overflow detection.
By using `4*n + 1` we always have the lowest two bits of a pointer as `00` 
while those of a kk_smallint_t are always `01`.

This way we can do more efficient basic arithmetic where we can for example directly add 
and test afterwards if we actually added two small integers (and not two pointers or smallint and pointer)
and whether there was an overflow. For example,

    intptr_t kk_integer_add(intptr_t x, intptr_t y) {
      intptr_t z;
      if (unlikely(__builtin_add_overflow(x,y,&z) || (z&2)==0)) return kk_integer_add_generic(x,y);
      return (z^3);  // or `z - 1`
    }

Here we test afterwards if we actually added 2 integers and not two pointers.
If we add, the last 2 bits are:

     x + y = z
    00  00  00    ptr + ptr
    00  01  01    ptr + int
    01  00  01    int + ptr
    01  01  10    int + int

so the test `(z&0x02) == 0` checks if we added 2 integers.
Finally, we subtract 1 (== ^3 in this case) to normalize the integer again. 
With gcc on x86-64 we get:

kk_integer_add(long x, long y)
        mov     rax, rdi   // move  `x` to eax
        add     rax, rsi   // add `y`
        jo      .L7        // on overflow goto slow
        and     rax, 2     // check bit 1 of the result
        je      .L7        // if it is zero, goto slow
        xor     rax, 3     // normalize back to an integer (clear bit 1, set bit 0)
        ret
.L7:
        jmp     kk_integer_add_generic(long, long)

However, not all compilers have a `__builtin_add_overflow`, and it is not always 
compiled well either. For example, the Intel C compiler generates:

kk_integer_add(long x, long y)
        mov       rax, rdi                                      #22.20
        xor       edx, edx                                      #22.20
        add       rax, rsi                                      #22.20
        seto      dl                                            #22.20
        test      dl, dl                                        #56.8
        jne       ..B2.4        # Prob 9%                       #56.8
        test      rax, 2                                        #56.41
        je        ..B2.4        # Prob 29%                      #56.45
        xor       rax, 3                                        #56.58
        ret                                                     #56.58
..B2.4:                         # Preds ..B2.2 ..B2.1
        jmp       kk_integer_add_generic(long, long)


However, we can also test in portable way, and do it with just a single test!
We can do that by limiting kk_smallint_t to a half-word. 
We then use a full-word add and see if the sign-extended lower half-word 
equals the full-word (and thus didn't overflow). 
This also allows us to combine that test with testing
if we added two small integers, (where bit 1 must ==1 after an addition):

    intptr_t kk_integer_add(intptr_t x, intptr_t y) {
      intptr_t z = x + y;
      if (kk_likely(z == (int32_t)(z|2))) return (z^3);
                                  else return kk_integer_add_generic(x,y);
    }

Now we have just one test that test both for overflow, as well as for the
small integers. This gives with clang (and gcc/msvc/icc) on x86-64:

kk_integer_add(long x, long y)
        lea     rax, [rdi+rsi]        // add into rax
        movsxd  rcx, eax              // sign extend lower 32-bits to rcx
        or      rcx, 2                // set bit 1 to 1
        cmp     rax, rcx              // rax == rcx ?
        jne     .L28                  // if not, we have an overflow or added a pointer
        xor     rax, 3                // clear bit 1, set bit 0
        ret
.L28:
        jmp     kk_integer_add_generic

on RISC-V with gcc we get:

kk_integer_add(long x, long y)
        add     a4,a0,a1       // add into a4
        ori     a5,a4,2        // a5 = a4|2
        sext.w  a5,a5          // sign extend
        bne     a5,a4,.L30     // a5 == a4 ? if not, overflow or pointer add
        xori    a0,a5,3        // clear bit 1, set bit 0
        ret
.L30:
        tail    kk_integer_add_generic


on ARM-v8 with gcc; using __builtin_add_overflow, we have:

        add     x2, x0, x1    // x2 = x0 + x1
        eon     x3, x0, x1    // x3 = ~(x0^x1)
        eor     x4, x2, x1    // x4 = x2^x1
        tst     x4, x3        // x4 & x3
        bmi     .L6           // branch if negative
        tbz     x2, 1, .L6    // test bit 1 == 0
        eor     x0, x3, 3     // x0 = x3^3
        ret
.L6:
        b       kk_add_slow(long, long)

and in the portable way:

        add     x3, x0, x1   // x3 = x0 + x1
        orr     w2, w3, 2    // w2 = w3|2
        sxtw    x2, w2       // sign extend w2 to x2
        cmp     x2, x3       // x2 == x3?
        bne     .L32         // if not, goto slow
        eor     x0, x2, 3    // x0 = x2^3
        ret
.L32:
        b       kk_add_slow(long, long)
        
So, overall, the portable way seems to always be better with a single test
but we can only use a half-word for small integers. We make it a define so we can 
measure the impact on specific platforms.

Some quick timings on a Xeon x86-64 show though that the builtin performs (a bit) better
on that platform:
  small add, 100000000x
         portable  builtin
  msvc:  0.102s    N/A
  gcc :  0.068s    0.054s
  clang: 0.067s    0.053s

  small add + subtract, 100000000x
         portable  builtin
  msvc:  0.185s    N/A
  gcc :  0.147s    0.105s
  clang: 0.171s    0.158s

but, when we do the same  "small add + subtract, 100000000x", but surrounded by lots
of other code, we measure for gcc:
  gcc :  0.137s    0.158s
  clang: 0.171s    0.171s

so more experimentation is needed.
--------------------------------------------------------------------------------------------------*/

#if defined(__GNUC__) && !defined(__INTEL_COMPILER)
#define KK_USE_BUILTIN_OVF (1)  
#endif

#ifndef KK_USE_BUILTIN_OVF
#define KK_USE_BUILTIN_OVF (0)       // default to portable overflow detection
#endif

#if KK_USE_BUILTIN_OVF
typedef intptr_t kk_smallint_t;
#define KK_SMALLINT_BITS  (KK_INTPTR_BITS)
#elif KK_INTPTR_SIZE==8
typedef int32_t kk_smallint_t;
#define KK_SMALLINT_BITS  (32)
#elif KK_INTPTR_SIZE==4
typedef int16_t kk_smallint_t;
#define KK_SMALLINT_BITS  (16)
#else
# error "platform must be 32 or 64 bits."
#endif

#define KK_SMALLINT_MAX  ((intptr_t)(((uintptr_t)INTPTR_MAX >> (KK_INTPTR_BITS - KK_SMALLINT_BITS)) >> 2))  // use unsigned shift to avoid UB
#define KK_SMALLINT_MIN  (-KK_SMALLINT_MAX - 1)

static inline bool kk_is_smallint(kk_integer_t i) {
  return ((i.value&1) != 0);
}
static inline bool kk_is_bigint(kk_integer_t i) {
  return ((i.value&1) == 0);
}
static inline kk_ptr_t _kk_as_bigint(kk_integer_t i) {
  kk_assert_internal(kk_is_bigint(i));
  return (kk_ptr_t)(i.value);
}
static inline kk_integer_t _kk_new_integer(intptr_t i) {
  kk_integer_t z = { i };
  return z;
}

static inline kk_intx_t kk_smallint_from_integer(kk_integer_t i) {  // use for known small ints
  kk_assert_internal(kk_is_smallint(i) && (i.value&3)==1);
  return kk_sar(i.value,2);
}

static inline kk_integer_t kk_integer_from_small(intptr_t i) {   // use for known small int constants (at most 14 bits)
  kk_assert_internal(i >= KK_SMALLINT_MIN && i <= KK_SMALLINT_MAX);
  return _kk_new_integer((i<<2)|1);
}

static inline bool kk_is_integer(kk_integer_t i) {
  return ((kk_is_smallint(i) && kk_smallint_from_integer(i) >= KK_SMALLINT_MIN && kk_smallint_from_integer(i) <= KK_SMALLINT_MAX) 
         || (kk_is_bigint(i) && kk_block_tag(_kk_as_bigint(i)) == KK_TAG_BIGINT));
}

static inline bool kk_are_smallints(kk_integer_t i, kk_integer_t j) {
  kk_assert_internal(kk_is_integer(i) && kk_is_integer(j));
  return ((i.value&j.value)&1)!=0;
}

static inline bool kk_integer_small_eq(kk_integer_t x, kk_integer_t y) {
  kk_assert_internal(kk_are_smallints(x, y));
  return (x.value == y.value);
}


#define kk_integer_zero     (kk_integer_from_small(0))
#define kk_integer_one      (kk_integer_from_small(1))
#define kk_integer_min_one  (kk_integer_from_small(-1))

/*---------------------------------------------------------------------------------
  Generic operations on integers
-----------------------------------------------------------------------------------*/

static inline kk_box_t kk_integer_box(kk_integer_t i) { 
  return _kk_box_new((uintptr_t)i.value);
}
static inline kk_integer_t kk_integer_unbox(kk_box_t b) { 
  return _kk_new_integer((intptr_t)b.box);
}

static inline kk_integer_t kk_integer_dup(kk_integer_t i) {
  if (kk_unlikely(kk_is_bigint(i))) { kk_block_dup(_kk_as_bigint(i)); }
  return i;
}

static inline void kk_integer_drop(kk_integer_t i, kk_context_t* ctx) { 
  if (kk_unlikely(kk_is_bigint(i))) { kk_block_drop(_kk_as_bigint(i), ctx); }
}

kk_decl_export bool          kk_integer_parse(const char* num, kk_integer_t* result, kk_context_t* ctx);
kk_decl_export kk_integer_t  kk_integer_from_str(const char* num, kk_context_t* ctx); // for known correct string number (returns 0 on wrong string)
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_from_big(kk_intx_t i, kk_context_t* ctx);         // for possibly large i
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_from_big64(int64_t i, kk_context_t* ctx);     // for possibly large i
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_from_bigu64(uint64_t i, kk_context_t* ctx);   // for possibly large i
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_from_double(double d, kk_context_t* ctx);     // round d and convert to integer (0 for NaN/Inf)

kk_decl_export kk_decl_noinline int32_t    kk_integer_clamp32_generic(kk_integer_t i, kk_context_t* ctx);
kk_decl_export kk_decl_noinline int64_t    kk_integer_clamp64_generic(kk_integer_t i, kk_context_t* ctx);
kk_decl_export kk_decl_noinline double     kk_integer_as_double_generic(kk_integer_t i, kk_context_t* ctx);

kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_add_generic(kk_integer_t x, kk_integer_t y, kk_context_t* ctx);
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_sub_generic(kk_integer_t x, kk_integer_t y, kk_context_t* ctx);
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_mul_generic(kk_integer_t x, kk_integer_t y, kk_context_t* ctx);
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_div_generic(kk_integer_t x, kk_integer_t y, kk_context_t* ctx);
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_mod_generic(kk_integer_t x, kk_integer_t y, kk_context_t* ctx);
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_div_mod_generic(kk_integer_t x, kk_integer_t y, kk_integer_t* mod, kk_context_t* ctx);

kk_decl_export kk_decl_noinline int           kk_integer_cmp_generic(kk_integer_t x, kk_integer_t y, kk_context_t* ctx);
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_neg_generic(kk_integer_t x, kk_context_t* ctx);
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_sqr_generic(kk_integer_t x, kk_context_t* ctx);
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_pow(kk_integer_t x, kk_integer_t p, kk_context_t* ctx);

kk_decl_export kk_decl_noinline bool          kk_integer_is_even_generic(kk_integer_t x, kk_context_t* ctx);
kk_decl_export kk_decl_noinline int           kk_integer_signum_generic(kk_integer_t x, kk_context_t* ctx);

kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_ctz(kk_integer_t x, kk_context_t* ctx);           // count trailing zero digits
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_count_digits(kk_integer_t x, kk_context_t* ctx);  // count decimal digits
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_mul_pow10(kk_integer_t x, kk_integer_t p, kk_context_t* ctx);  // x*(10^p)
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_div_pow10(kk_integer_t x, kk_integer_t p, kk_context_t* ctx);  // x/(10^p)

kk_decl_export kk_decl_noinline void          kk_integer_fprint(FILE* f, kk_integer_t x, kk_context_t* ctx);
kk_decl_export kk_decl_noinline void          kk_integer_print(kk_integer_t x, kk_context_t* ctx);


/*---------------------------------------------------------------------------------
  Conversion
-----------------------------------------------------------------------------------*/

static inline kk_integer_t kk_integer_from_int(kk_intx_t i, kk_context_t* ctx) {
  return (kk_likely(i >= KK_SMALLINT_MIN && i <= KK_SMALLINT_MAX) ? kk_integer_from_small(i) : kk_integer_from_big(i,ctx));
}

static inline kk_integer_t kk_integer_from_int32(int32_t i, kk_context_t* ctx) {
#if (KK_SMALLINT_BITS >= 34)
  KK_UNUSED(ctx);
  return kk_integer_from_small(i);
#else
  return (kk_likely(i >= KK_SMALLINT_MIN && i <= KK_SMALLINT_MAX) ? kk_integer_from_small(i) : kk_integer_from_big(i, ctx));
#endif
}

static inline kk_integer_t kk_integer_from_int64(int64_t i, kk_context_t* ctx) {
  return (kk_likely(i >= KK_SMALLINT_MIN && i <= KK_SMALLINT_MAX) ? kk_integer_from_small((intptr_t)i) : kk_integer_from_big64(i, ctx));
}

static inline kk_integer_t kk_integer_from_uint64(uint64_t i, kk_context_t* ctx) {
  return (kk_likely(i <= KK_SMALLINT_MAX) ? kk_integer_from_small((intptr_t)i) : kk_integer_from_bigu64(i, ctx));
}

#if (KK_INTX_SIZE<=4)
static inline kk_integer_t kk_integer_from_uintx_t(kk_uintx_t i, kk_context_t* ctx) {
  return (i <= INT32_MAX ? kk_integer_from_int((kk_intx_t)i,ctx) : kk_integer_from_uint64(i,ctx));
}
#else
static inline kk_integer_t kk_integer_from_uintx_t(kk_uintx_t i, kk_context_t* ctx) {
  return kk_integer_from_uint64(i, ctx);
}
#endif

static inline kk_integer_t kk_integer_from_size_t(size_t i, kk_context_t* ctx) {
  return kk_integer_from_uintx_t(i, ctx);
}

static inline kk_integer_t kk_integer_from_intptr_t(intptr_t i, kk_context_t* ctx) {
  return kk_integer_from_int(i, ctx);
}


/*---------------------------------------------------------------------------------
Addition, Subtraction, and Multiply depend on using __builtin_xxx_overflow or not.

Addition: for small n we have `boxed(n) = n*4 + 1`, and we can add as:
    boxed(n) + boxed(m) - 1
    = (n*4 + 1) + (m*4 + 1) - 1
    = n*4 + m*4 + 2 - 1
    = 4*(n + m) + 1
    = boxed(n+m)
  (to normalize back we use (^ 3) instead of (- 1) to reduce register stalls 
  (since we know the bottom bits of the result are 01)

     x + y = z
    00  00  00    ptr + ptr
    00  01  01    ptr + int
    01  00  01    int + ptr
    01  01  10    int + int
  so we can check if both were small ints by checking if bit 1 is set in the result.

Subtraction: for small `n` we have
      (boxed(n) + 1) - boxed(m) 
      = (n*4 + 2) - (m*4 + 1)
      = n*4 - m*4 + 2 - 1
      = 4*(n - m) + 1
      = boxed(n-m)
  except we use ^3 instead of +1 as we check if both were small ints:

     x - y   x^3  (x^3 - y)
    00  00    11   11       ptr - ptr
    00  01    11   10       ptr - int
    01  00    10   10       int - ptr
    01  01    10   01       int - int
  So we can detect if both were small ints by checking if bit 1 was clear in the result.

Multiply: Since `boxed(n) = n*4 + 1`, we can multiply as:
    (boxed(n)/2) * (boxed(m)/2) + 1
    = (n*4+1)/2 * (m*4+1)/2 + 1
    = (n*2) * (m*2) + 1
    = (n*m*4) + 1
    = boxed(n*m)
    
    we check before multiply for small integers and do not combine with the overflow check.
-----------------------------------------------------------------------------------*/

#if KK_USE_BUILTIN_OVF

static inline kk_integer_t kk_integer_add(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  intptr_t z;
  if (kk_likely(!__builtin_add_overflow(x.value, y.value, &z) && (z&2)!=0)) {
    kk_assert_internal((z&3) == 2);
    return _kk_new_integer(z^3);
  }
  return kk_integer_add_generic(x, y, ctx);
}

static inline kk_integer_t kk_integer_sub(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  intptr_t z;
  if (kk_likely(!__builtin_sub_overflow(x.value^3, y.value, &z) && (z&2)==0)) {
    kk_assert_internal((z&3) == 1);
    return _kk_new_integer(z);
  }
  return kk_integer_sub_generic(x, y, ctx);
}

static inline kk_integer_t kk_integer_mul_small(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  kk_assert_internal(kk_are_smallints(x, y));
  intptr_t i = kk_sar(x.value, 1);
  intptr_t j = kk_sar(y.value, 1);
  intptr_t z;
  if (kk_likely(!__builtin_mul_overflow(i, j, &z))) {
    kk_assert_internal((z&3)==0);
    return _kk_new_integer(z|1);
  }
  return kk_integer_mul_generic(x, y, ctx);
}

#else

static inline kk_integer_t kk_integer_add(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  intptr_t z = x.value + y.value;
  if (kk_likely(z == (kk_smallint_t)(z|2))) {  // set bit 1 and sign extend
    kk_assert_internal((z&3) == 2);
    return _kk_new_integer(z^3);
  }
  return kk_integer_add_generic(x, y, ctx);
}

static inline kk_integer_t kk_integer_sub(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  intptr_t z = (x.value^3) - y.value;
  if (kk_likely(z == (kk_smallint_t)(z&~KIP(2)))) {  // clear bit 1 and sign extend
    kk_assert_internal((z&3) == 1);
    return _kk_new_integer(z);
  }
  return kk_integer_sub_generic(x, y, ctx);
}

static inline kk_integer_t kk_integer_mul_small(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  kk_assert_internal(kk_are_smallints(x, y));
  intptr_t i = kk_sar(x.value, 1);
  intptr_t j = kk_sar(y.value, 1);
  intptr_t z = i*j;
  if (kk_likely(z == (kk_smallint_t)(z))) {
    kk_assert_internal((z&3) == 0);
    return _kk_new_integer(z|1);
  }
  return kk_integer_mul_generic(x, y, ctx);
}

#endif

static inline kk_integer_t kk_integer_mul(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_smallints(x, y))) return kk_integer_mul_small(x, y, ctx);
  return kk_integer_mul_generic(x, y, ctx);
}

/* Fast divide on small integers. Since `boxed(n) = n*4 + 1`, we can divide as:
    4*((boxed(n)/2)/((boxed(m)/2) + 1
    = 4*((n*2)/(m*2)) + 1
    = 4*(n/m) + 1
    = boxed(n/m)
*/
static inline kk_integer_t kk_integer_div_small(kk_integer_t x, kk_integer_t y) {
  kk_assert_internal(kk_are_smallints(x, y));
  intptr_t i = kk_sar(x.value, 1);
  intptr_t j = kk_sar(y.value, 1);
  return _kk_new_integer(((i/j)<<2)|1);
}

/* Fast modulus on small integers. Since `boxed(n) = n*4 + 1`, we can divide as:
    2*((boxed(n)/2)%((boxed(m)/2) + 1
    = 2*((n*2)%(m*2)) + 1
    = 2*2*(n%m) + 1
    = boxed(n%m)
*/
static inline kk_integer_t kk_integer_mod_small(kk_integer_t x, kk_integer_t y) {
  kk_assert_internal(kk_are_smallints(x, y));
  intptr_t i = kk_sar(x.value, 1);
  intptr_t j = kk_sar(y.value, 1);
  return _kk_new_integer(((i%j)<<1)|1);
}

static inline kk_integer_t kk_integer_div_mod_small(kk_integer_t x, kk_integer_t y, kk_integer_t* mod) {
  kk_assert_internal(kk_are_smallints(x, y)); kk_assert_internal(mod!=NULL);
  intptr_t i = kk_sar(x.value, 1);
  intptr_t j = kk_sar(y.value, 1);
  *mod = _kk_new_integer(((i%j)<<1)|1);
  return _kk_new_integer(((i/j)<<2)|1);
}

static inline kk_integer_t kk_integer_div(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_smallints(x, y))) return kk_integer_div_small(x, y);
  return kk_integer_div_generic(x, y, ctx);
}

static inline kk_integer_t kk_integer_mod(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_smallints(x, y))) return kk_integer_mod_small(x, y);
  return kk_integer_mod_generic(x, y, ctx);
}

static inline kk_integer_t kk_integer_div_mod(kk_integer_t x, kk_integer_t y, kk_integer_t* mod, kk_context_t* ctx) {
  kk_assert_internal(mod!=NULL);
  if (kk_likely(kk_are_smallints(x, y))) return kk_integer_div_mod_small(x, y, mod);
  return kk_integer_div_mod_generic(x, y, mod, ctx);
}

static inline int32_t kk_integer_clamp32(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return (int32_t)kk_smallint_from_integer(x);
  return kk_integer_clamp32_generic(x, ctx);
}

static inline int64_t kk_integer_clamp64(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return (int64_t)kk_smallint_from_integer(x);
  return kk_integer_clamp64_generic(x, ctx);
}

static inline kk_intx_t kk_integer_clamp(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return kk_smallint_from_integer(x);
#if KK_INTX_SIZE <= 4
  return kk_integer_clamp32_generic(x, ctx);
#else
  return kk_integer_clamp64_generic(x, ctx);
#endif
}


static inline double kk_integer_as_double(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return (double)(kk_smallint_from_integer(x));
  return kk_integer_as_double_generic(x, ctx);
}

static inline kk_integer_t kk_integer_sqr(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return kk_integer_mul_small(x,x,ctx);
  return kk_integer_sqr_generic(x,ctx);
}

static inline kk_integer_t kk_integer_neg_small(kk_integer_t x, kk_context_t* ctx) {
  kk_assert_internal(kk_is_smallint(x));
  return kk_integer_sub(kk_integer_zero, x, ctx);   // negation can overflow
}

static inline kk_integer_t kk_integer_neg(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return kk_integer_neg_small(x,ctx);
  return kk_integer_neg_generic(x,ctx);
}

static inline kk_integer_t kk_integer_abs(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return (x.value < 0 ? kk_integer_neg_small(x,ctx) : x);
  return (kk_integer_signum_generic(x, ctx) < 0 ? kk_integer_neg_generic(x, ctx) : x);
}

static inline kk_integer_t kk_integer_dec(kk_integer_t x, kk_context_t* ctx) {
  return kk_integer_sub(x,kk_integer_one,ctx);  
}

static inline kk_integer_t kk_integer_inc(kk_integer_t x, kk_context_t* ctx) {
  return kk_integer_add(x, kk_integer_one,ctx);
}

static inline int kk_integer_cmp(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_smallints(x, y))) return (x.value == y.value ? 0 : (x.value > y.value ? 1 : -1));
  return kk_integer_cmp_generic(x, y, ctx);
}

static inline bool kk_integer_lt(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_smallints(x, y))) return (x.value < y.value);
  return (kk_integer_cmp(x, y, ctx) == -1);
}

static inline bool kk_integer_lte(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_smallints(x, y))) return (x.value <= y.value);
  return (kk_integer_cmp(x, y, ctx) <= 0);
}

static inline bool kk_integer_gt(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_smallints(x, y))) return (x.value > y.value);
  return (kk_integer_cmp(x, y, ctx) == 1);
}

static inline bool kk_integer_gte(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_smallints(x, y))) return (x.value >= y.value);
  return (kk_integer_cmp(x, y, ctx) >= 0);
}

static inline bool kk_integer_eq(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_smallints(x, y))) return (x.value == y.value);
  return (kk_integer_cmp(x, y, ctx) == 0);
}

static inline bool kk_integer_neq(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_smallints(x, y))) return (x.value != y.value);
  return (kk_integer_cmp(x, y, ctx) != 0);
}

static inline bool kk_integer_is_zero(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return (x.value == kk_integer_zero.value);
  kk_integer_drop(x,ctx);
  return false;
}

static inline bool kk_integer_is_one(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return (x.value == kk_integer_one.value);
  kk_integer_drop(x,ctx);
  return false;
}

static inline bool kk_integer_is_minus_one(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return (x.value == kk_integer_min_one.value);
  kk_integer_drop(x,ctx);
  return false;
}

static inline bool kk_integer_is_even(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return ((x.value&0x04)==0);
  return kk_integer_is_even_generic(x,ctx);
}

static inline bool kk_integer_is_odd(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return ((x.value&0x04)!=0);
  return !kk_integer_is_even_generic(x,ctx);
}

static inline int kk_integer_signum(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return ((x.value>1)-(x.value<0));
  return kk_integer_signum_generic(x,ctx);
}

static inline bool integer_is_neg(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return (x.value<0);
  return (kk_integer_signum_generic(x,ctx) < 0);
}

static inline bool kk_integer_is_pos(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return (x.value>1);
  return (kk_integer_signum_generic(x,ctx) > 0);
}

static inline kk_integer_t kk_integer_max(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_smallints(x, y))) return (x.value>=y.value ? x : y);
  kk_integer_dup(x); kk_integer_dup(y);
  if (kk_integer_gte(x,y,ctx)) {
    kk_integer_drop(y,ctx); return x;
  }
  else {
    kk_integer_drop(x,ctx); return y;
  }
}

static inline kk_integer_t kk_integer_min(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_smallints(x, y))) return (x.value<=y.value ? x : y);
  kk_integer_dup(x); kk_integer_dup(y);
  if (kk_integer_lte(x, y, ctx)) {
    kk_integer_drop(y, ctx); return x;
  }
  else {
    kk_integer_drop(x, ctx); return y;
  }
}



#endif // include guard
