#pragma once
#ifndef KK_INTEGER_H
#define KK_INTEGER_H
/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
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

so the test `(z&2) == 0` checks if we added 2 integers.
Finally, we subtract 1 (optimized to z^3 in this case) to normalize the integer again. 
With gcc on x86-64 we get:

  kk_integer_add(long x, long y)
          mov     rax, rdi   // move  `x` to rax
          add     rax, rsi   // add `y`
          jo      .L7        // on overflow goto slow
          test    al, 2      // check bit 1 of the result
          je      .L7        // if it is zero, goto slow
          xor     rax, 3     // normalize back to an integer (clear bit 1, set bit 0)
          ret
  .L7:
          jmp     kk_integer_add_generic(long, long)

or on arm64:

  kk_integer_add(long x, long y)
          adds    x2, x0, x1     // add signed x2 = x0 + x1
          bvs     .L9            // branch on overflow
          tbz     x2, 1, .L9     // test if bit 1 equals 0  
          eor     x0, x2, 3      // x0 = x2 ^ 3
          ret
  .L9:        
          jmp     kk_integer_add_generic(long, long)

That is not too bad especially as the jumps are almost never taken and thus 
very predictable.

Unfortunately, not all compilers have a `__builtin_add_overflow`, and it is not always 
well supported by the ISA either. For example on risc-V (with gcc) we get:

  kk_integer_add(long x, long y)
          add     a2, a0, a1
          slt     a3, a2, a0
          slti    a4, a1, 0
          xor     a3, a3, a4
          andi    a4, a2, 2
          seqz    a4, a4
          or      a3, a3, a4
          bnez    a3, .LBB1_2
          xori    a0, a2, 3
          ret
  .LBB1_2:
          tail    kk_integer_add_generic(long, long)
        

about 9 instructions just for one add... :-(

However, we can also test in portable way, and do it with just a single test!
We can do that by limiting kk_smallint_t to a half-word. 
We then use a full-word add and see if the sign-extended lower half-word 
equals the full-word (and thus didn't overflow). 

(I call this the "SOFA" technique: Sign-extended OverFlow Arithmetic :-) )

This also allows us to combine that test with testing
if we added two small integers, (where bit 1 must be set after an addition):

    intptr_t kk_integer_add(intptr_t x, intptr_t y) {
      intptr_t z = x + y;
      if (kk_likely((z|2) == (int32_t)z)) return (z^3);
                                     else return kk_integer_add_generic(x,y);
    }

Now we have just one test that test both for overflow, as well as for the
small integers. Now the code on RISC-V (rv64) with clang 12 looks a bit better:

  kk_integer_add(long, long):
          add     a2, a1, a0         // a2 = a1 + a0
          ori     a3, a2, 2          // a3 = a2 | 2 
          addw    a4, a1, a0         // 32-bit add and sign extend into a4
          bne     a3, a4, .LBB6_2    // if not equal goto slow
          xori    a0, a2, 3          // a0 = a2 ^ 3
          ret          
  .LBB6_2:
          tail    kk_integer_add_generic


and with clang (and gcc/msvc/icc) on x86-64 we get:

  kk_integer_add(long x, long y)
          lea     rax, [rsi + rdi]   // add into rax
          mov     rcx, rax
          or      rcx, 2             // set bit 1 to 1
          movsxd  rdx, eax           // sign extend lower 32-bits to rcx
          cmp     rcx, rdx
          jne     .LBB6_2
          xor     rax, 3
          ret
  .LBB6_2:
          jmp     kk_integer_add_generic

and finally on ARM-v8 with gcc:

  kk_integer_add(long, long):
          add     x8, x1, x0     // x8 = x0 + x1
          orr     x9, x8, #0x2   // x9 = x8|2
          cmp     x9, w8, sxtw   // sign extend w8 and compare to x9
          b.ne    .LBB6_2        // not equal, goto slow
          eor     x0, x8, #0x3   // x0 = x8^3
          ret          
  .L32:
          b       kk_integer_add_generic
          

Initial measurements on x64 (AMD5950x) and on arm64 (M1) seem
to indicate the portable SOFA technique is about 5% (x64) to 10% (M1) faster.
(but more experimentation is needed).

-- Daan Leijen, 2020-2021.
--------------------------------------------------------------------------------------------------*/

#if !defined(KK_USE_BUILTIN_OVF)
#define KK_USE_BUILTIN_OVF (0)       // portable overflow detection seems always faster
#endif

#if KK_USE_BUILTIN_OVF
typedef kk_intf_t kk_smallint_t;
#define KK_SMALLINT_BITS  (KK_INTF_BITS)
#elif KK_INTF_SIZE>=16
typedef int64_t kk_smallint_t;
#define KK_SMALLINT_BITS  (64)
#elif KK_INTF_SIZE==8
typedef int32_t kk_smallint_t;
#define KK_SMALLINT_BITS  (32)
#elif KK_INTF_SIZE==4
typedef int16_t kk_smallint_t;
#define KK_SMALLINT_BITS  (16)
#elif KK_INTF_SIZE==2
typedef int8_t kk_smallint_t;
#define KK_SMALLINT_BITS  (8)
#else
# error "platform must be 16, 32, 64, or 128 bits."
#endif

#define KK_SMALLINT_SIZE (KK_SMALLINT_BITS/8)
#define KK_SMALLINT_MAX  (KK_INTF_MAX >> (KK_INTF_BITS - KK_SMALLINT_BITS + 2))
#define KK_SMALLINT_MIN  (-KK_SMALLINT_MAX - 1)

static inline kk_intf_t _kk_integer_value(kk_integer_t i) {
  return (kk_intf_t)i.ibox;  // potentially cast to smaller kk_intf_t (as on arm CHERI)
}

static inline bool kk_is_smallint(kk_integer_t i) {
  return ((_kk_integer_value(i)&1) != 0);
}

static inline bool kk_is_bigint(kk_integer_t i) {
  return ((_kk_integer_value(i)&1) == 0);
}

static inline kk_ptr_t _kk_integer_ptr(kk_integer_t i) {
  kk_assert_internal(kk_is_bigint(i));
  return (kk_ptr_t)(i.ibox);
}

static inline kk_integer_t _kk_new_integer(kk_intf_t i) {
  kk_integer_t z = { (uintptr_t)i }; // todo: optimize in case sizeof(kk_intf_t) < sizeof(intptr_t) ?
  return z;
}

static inline kk_intf_t kk_smallint_from_integer(kk_integer_t i) {  // use for known small ints
  kk_assert_internal(kk_is_smallint(i) && (_kk_integer_value(i)&3)==1);
  return kk_sarf(_kk_integer_value(i),2);
}

static inline kk_integer_t kk_integer_from_small(kk_intf_t i) {   // use for known small int constants (at most 14 bits)
  kk_assert_internal(i >= KK_SMALLINT_MIN && i <= KK_SMALLINT_MAX);
  return _kk_new_integer(kk_shlf(i,2)|1); 
}

static inline bool kk_is_integer(kk_integer_t i) {
  return ((kk_is_smallint(i) && kk_smallint_from_integer(i) >= KK_SMALLINT_MIN && kk_smallint_from_integer(i) <= KK_SMALLINT_MAX) 
         || (kk_is_bigint(i) && kk_block_tag(_kk_integer_ptr(i)) == KK_TAG_BIGINT));
}

static inline bool kk_are_smallints(kk_integer_t i, kk_integer_t j) {
  kk_assert_internal(kk_is_integer(i) && kk_is_integer(j));
  return (((_kk_integer_value(i)&_kk_integer_value(j))&1) == 1);   
  //return ((_kk_integer_value(i)&1)==1 || (_kk_integer_value(j)&1)==1);
}

static inline bool kk_integer_small_eq(kk_integer_t x, kk_integer_t y) {
  kk_assert_internal(kk_are_smallints(x, y));
  return (_kk_integer_value(x) == _kk_integer_value(y));
}


#define kk_integer_zero     (kk_integer_from_small(0))
#define kk_integer_one      (kk_integer_from_small(1))
#define kk_integer_min_one  (kk_integer_from_small(-1))

static inline bool kk_integer_is_zero_borrow(kk_integer_t x) {
  if (kk_likely(kk_is_smallint(x))) return (_kk_integer_value(x) == _kk_integer_value(kk_integer_zero));
  return false;
}

static inline bool kk_integer_is_one_borrow(kk_integer_t x) {
  if (kk_likely(kk_is_smallint(x))) return (_kk_integer_value(x) == _kk_integer_value(kk_integer_one));
  return false;
}

static inline bool kk_integer_is_minus_one_borrow(kk_integer_t x) {
  if (kk_likely(kk_is_smallint(x))) return (_kk_integer_value(x) == _kk_integer_value(kk_integer_min_one));
  return false;
}

/*---------------------------------------------------------------------------------
  Generic operations on integers
-----------------------------------------------------------------------------------*/

// Isomorphic with boxed values
static inline kk_box_t kk_integer_box(kk_integer_t i) { 
  kk_box_t b = { i.ibox };
  return b;
}
static inline kk_integer_t kk_integer_unbox(kk_box_t b) { 
  kk_integer_t i = { b.box };
  return i;
}

static inline kk_integer_t kk_integer_dup(kk_integer_t i) {
  if (kk_unlikely(kk_is_bigint(i))) { kk_block_dup(_kk_integer_ptr(i)); }
  return i;
}

static inline void kk_integer_drop(kk_integer_t i, kk_context_t* ctx) { 
  if (kk_unlikely(kk_is_bigint(i))) { kk_block_drop(_kk_integer_ptr(i), ctx); }
}

kk_decl_export bool          kk_integer_parse(const char* num, kk_integer_t* result, kk_context_t* ctx);
kk_decl_export bool          kk_integer_hex_parse(const char* s, kk_integer_t* res, kk_context_t* ctx);
kk_decl_export kk_integer_t  kk_integer_from_str(const char* num, kk_context_t* ctx); // for known correct string number (returns 0 on wrong string)

kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_from_big(kk_intx_t i, kk_context_t* ctx);         // for possibly large i
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_from_big64(int64_t i, kk_context_t* ctx);     // for possibly large i
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_from_bigu64(uint64_t i, kk_context_t* ctx);   // for possibly large i
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_from_double(double d, kk_context_t* ctx);     // round d and convert to integer (0 for NaN/Inf)

kk_decl_export kk_decl_noinline int32_t    kk_integer_clamp32_generic(kk_integer_t i, kk_context_t* ctx);
kk_decl_export kk_decl_noinline int64_t    kk_integer_clamp64_generic(kk_integer_t i, kk_context_t* ctx);
kk_decl_export kk_decl_noinline size_t     kk_integer_clamp_size_t_generic(kk_integer_t i, kk_context_t* ctx);
kk_decl_export kk_decl_noinline double     kk_integer_as_double_generic(kk_integer_t i, kk_context_t* ctx);

kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_add_generic(kk_integer_t x, kk_integer_t y, kk_context_t* ctx);
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_sub_generic(kk_integer_t x, kk_integer_t y, kk_context_t* ctx);
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_mul_generic(kk_integer_t x, kk_integer_t y, kk_context_t* ctx);

kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_cdiv_generic(kk_integer_t x, kk_integer_t y, kk_context_t* ctx);
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_cmod_generic(kk_integer_t x, kk_integer_t y, kk_context_t* ctx);
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_cdiv_cmod_generic(kk_integer_t x, kk_integer_t y, kk_integer_t* mod, kk_context_t* ctx);

kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_div_generic(kk_integer_t x, kk_integer_t y, kk_context_t* ctx);
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_mod_generic(kk_integer_t x, kk_integer_t y, kk_context_t* ctx);
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_div_mod_generic(kk_integer_t x, kk_integer_t y, kk_integer_t* mod, kk_context_t* ctx);

kk_decl_export kk_decl_noinline int           kk_integer_cmp_generic(kk_integer_t x, kk_integer_t y, kk_context_t* ctx);
kk_decl_export kk_decl_noinline int           kk_integer_cmp_generic_borrow(kk_integer_t x, kk_integer_t y, kk_context_t* ctx);
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_neg_generic(kk_integer_t x, kk_context_t* ctx);
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_sqr_generic(kk_integer_t x, kk_context_t* ctx);
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_pow(kk_integer_t x, kk_integer_t p, kk_context_t* ctx);

kk_decl_export kk_decl_noinline bool          kk_integer_is_even_generic(kk_integer_t x, kk_context_t* ctx);
kk_decl_export kk_decl_noinline int           kk_integer_signum_generic_bigint(kk_integer_t x);

kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_ctz(kk_integer_t x, kk_context_t* ctx);           // count trailing zero digits
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_count_digits(kk_integer_t x, kk_context_t* ctx);  // count decimal digits
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_mul_pow10(kk_integer_t x, kk_integer_t p, kk_context_t* ctx);  // x*(10^p)
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_cdiv_pow10(kk_integer_t x, kk_integer_t p, kk_context_t* ctx);  // x/(10^p)
kk_decl_export kk_decl_noinline kk_integer_t  kk_integer_div_pow10(kk_integer_t x, kk_integer_t p, kk_context_t* ctx);  // x/(10^p)

kk_decl_export kk_decl_noinline void          kk_integer_fprint(FILE* f, kk_integer_t x, kk_context_t* ctx);
kk_decl_export kk_decl_noinline void          kk_integer_print(kk_integer_t x, kk_context_t* ctx);


/*---------------------------------------------------------------------------------
  Conversion from fixed size integers
-----------------------------------------------------------------------------------*/

static inline kk_integer_t kk_integer_from_int32(int32_t i, kk_context_t* ctx) {
#if (KK_SMALLINT_MAX >= INT32_MAX)
  kk_unused(ctx);
  return kk_integer_from_small(i);
#else
  return (kk_likely(i >= KK_SMALLINT_MIN && i <= KK_SMALLINT_MAX) ? kk_integer_from_small(i) : kk_integer_from_big(i, ctx));
#endif
}

static inline kk_integer_t kk_integer_from_uint32(uint32_t i, kk_context_t* ctx) {
#if (KK_SMALLINT_MAX >= UINT32_MAX)
  return kk_integer_from_small((kk_intf_t)i);
#else  
  return (kk_likely(i <= KK_SMALLINT_MAX) ? kk_integer_from_small((kk_intf_t)i) : kk_integer_from_big(i, ctx));
#endif  
}

static inline kk_integer_t kk_integer_from_int64(int64_t i, kk_context_t* ctx) {
  return (kk_likely(i >= KK_SMALLINT_MIN && i <= KK_SMALLINT_MAX) ? kk_integer_from_small((kk_intf_t)i) : kk_integer_from_big64(i, ctx));
}

static inline kk_integer_t kk_integer_from_uint64(uint64_t i, kk_context_t* ctx) {
  return (kk_likely(i <= KK_SMALLINT_MAX) ? kk_integer_from_small((kk_intf_t)i) : kk_integer_from_bigu64(i, ctx));
}

static inline kk_integer_t kk_integer_from_int(kk_intx_t i, kk_context_t* ctx) {
  return (kk_likely(i >= KK_SMALLINT_MIN && i <= KK_SMALLINT_MAX) ? kk_integer_from_small((kk_intf_t)i) : kk_integer_from_big(i, ctx));
}


#if (KK_INTX_SIZE <= 4)
static inline kk_integer_t kk_integer_from_uintx_t(kk_uintx_t i, kk_context_t* ctx) {
  return kk_integer_from_uint32(i,ctx)
}
#elif (KK_INTX_SIZE <= 8)
static inline kk_integer_t kk_integer_from_uintx_t(kk_uintx_t i, kk_context_t* ctx) {
  return kk_integer_from_uint64(i, ctx);
}
#else
# error "define kk_integer_from_uintx_t for this platform"
#endif

static inline kk_integer_t kk_integer_from_size_t(size_t i, kk_context_t* ctx) {
  return kk_integer_from_uintx_t(i, ctx);
}

static inline kk_integer_t kk_integer_from_ssize_t(kk_ssize_t i, kk_context_t* ctx) {
  return kk_integer_from_int(i, ctx);
}

static inline kk_integer_t kk_integer_from_ptrdiff_t(ptrdiff_t i, kk_context_t* ctx) {
  return kk_integer_from_int(i, ctx);
}

static inline kk_integer_t kk_integer_from_intptr_t(intptr_t i, kk_context_t* ctx) {
  return kk_integer_from_int(i, ctx);
}

static inline kk_integer_t kk_integer_from_byte(uint8_t i, kk_context_t* ctx) {
  kk_unused(ctx);
  return kk_integer_from_small(i);
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
  kk_intf_t z;
  if (kk_unlikely(__builtin_add_overflow(_kk_integer_value(x), _kk_integer_value(y), &z) || (z&2)==0)) {
    return kk_integer_add_generic(x,y,ctx);
  }
  kk_assert_internal((z&3) == 2);
  return _kk_new_integer(z^3);
}

static inline kk_integer_t kk_integer_add_small_const(kk_integer_t x, kk_intf_t i, kk_context_t* ctx) {
  kk_assert_internal(i >= KK_SMALLINT_MIN && i <= KK_SMALLINT_MAX);
  kk_intf_t z;  
  if (kk_unlikely(kk_is_bigint(x) || __builtin_add_overflow(_kk_integer_value(x), kk_shlf(i,2), &z))) {
    return kk_integer_add_generic(x,kk_integer_from_small(i),ctx);
  }
  kk_assert_internal((z&3) == 1);
  return _kk_new_integer(z);
}

static inline kk_integer_t kk_integer_sub(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  kk_intf_t z;
  if (kk_unlikely(__builtin_sub_overflow(_kk_integer_value(x)^3, _kk_integer_value(y), &z) || (z&2)!=0)) {
    return kk_integer_sub_generic(x,y,ctx);
  }
  kk_assert_internal((z&3) == 1);
  return _kk_new_integer(z);
}

static inline kk_integer_t kk_integer_mul_small(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  kk_assert_internal(kk_are_smallints(x, y));
  kk_intf_t i = kk_sar(_kk_integer_value(x), 1);
  kk_intf_t j = kk_sar(_kk_integer_value(y), 1);
  kk_intf_t z;
  if (kk_unlikely(__builtin_mul_overflow(i, j, &z))) {
    return kk_integer_mul_generic(x, y, ctx);
  }
  kk_assert_internal((z&3)==0);
  return _kk_new_integer(z|1);
}

#else // use SOFA

static inline kk_integer_t kk_integer_add(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  kk_intf_t z = _kk_integer_value(x) + _kk_integer_value(y);
  if (kk_likely((z|2) == (kk_smallint_t)z)) {  // set bit 1 and sign extend
    kk_assert_internal((z&3) == 2);
    return _kk_new_integer(z^3);
  }
  return kk_integer_add_generic(x, y, ctx);
}


static inline kk_integer_t kk_integer_add_small_const(kk_integer_t x, kk_intf_t i, kk_context_t* ctx) {
  kk_assert_internal(i >= KK_SMALLINT_MIN && i <= KK_SMALLINT_MAX);
  kk_intf_t z = _kk_integer_value(x) + kk_shlf(i,2);
  if (kk_likely((z|1) == (kk_smallint_t)z)) {  
    kk_assert_internal((z&3) == 1);
    return _kk_new_integer(z);
  }
  return kk_integer_add_generic(x, kk_integer_from_small(i), ctx);
}


static inline kk_integer_t kk_integer_sub(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  kk_intf_t z = (_kk_integer_value(x)^3) - _kk_integer_value(y);
  if (kk_likely((z&~2) == (kk_smallint_t)z)) {  // clear bit 1 and sign extend
    kk_assert_internal((z&3) == 1);
    return _kk_new_integer(z);
  }
  return kk_integer_sub_generic(x, y, ctx);
}


static inline kk_integer_t kk_integer_mul_small(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  kk_assert_internal(kk_are_smallints(x, y));
  kk_intf_t i = kk_sar(_kk_integer_value(x), 1);
  kk_intf_t j = kk_sar(_kk_integer_value(y), 1);
  kk_intf_t z = i*j;
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
  (we could divide by 4 as well but some processors do better on 1-bit shifts)
*/
static inline kk_integer_t kk_integer_cdiv_small(kk_integer_t x, kk_integer_t y) {
  kk_assert_internal(kk_are_smallints(x, y));
  kk_assert_internal(!kk_integer_is_zero_borrow(y));
  kk_intf_t i = kk_sar(_kk_integer_value(x), 1);
  kk_intf_t j = kk_sar(_kk_integer_value(y), 1);
  return _kk_new_integer(kk_shlp(i/j, 2)|1);
}

// Euclidean division: see <https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf>
// Always preferred at it is more regular than C style truncated division. For example:
// - modulus is always positive     
// - x `div` 2^n == sar(x,n)        for any x, n
// - x `mod` 2^n == and(x,2^(n-1))  for any x, n
// - Euclidean division behaves identical to truncated division for positive dividends.
static inline kk_integer_t kk_integer_div_small(kk_integer_t x, kk_integer_t y) {
  kk_assert_internal(kk_are_smallints(x, y));
  kk_assert_internal(!kk_integer_is_zero_borrow(y));
  kk_intf_t i = kk_sar(_kk_integer_value(x), 2);
  kk_intf_t j = kk_sar(_kk_integer_value(y), 2);
  // if (j==0) return kk_integer_zero;
  kk_intf_t d = i/j;
  kk_intf_t m = i%j;
  if (i < 0 && m < 0) { d -= (j < 0 ? -1 : 1); }   // i < 0 is not needed, but see note below
  return _kk_new_integer(kk_shlp(d,2)|1);  // (d<<2)|1
}

/* Fast modulus on small integers. Since `boxed(n) = n*4 + 1`, we can divide as:
    2*((boxed(n)/2)%((boxed(m)/2) + 1
    = 2*((n*2)%(m*2)) + 1
    = 2*2*(n%m) + 1
    = boxed(n%m)
   (we could divide by 4 as well but some processors do better on 1-bit shifts)
*/
static inline kk_integer_t kk_integer_cmod_small(kk_integer_t x, kk_integer_t y) {
  kk_assert_internal(kk_are_smallints(x, y));
  kk_assert_internal(!kk_integer_is_zero_borrow(y));
  kk_intf_t i = kk_sar(_kk_integer_value(x), 1);
  kk_intf_t j = kk_sar(_kk_integer_value(y), 1);
  return _kk_new_integer(kk_shlp(i%j,1)|1);
}

// Euclidean mod on small integers. Since `boxed(n) = n*4 + 1`, we can divide as:
//    4*((boxed(n)/4)%((boxed(m)/4) + 1
//  = 4*(n%m) + 1
//  = boxed(n%m)
static inline kk_integer_t kk_integer_mod_small(kk_integer_t x, kk_integer_t y) {
  kk_assert_internal(kk_are_smallints(x, y));
  kk_assert_internal(!kk_integer_is_zero_borrow(y));
  kk_intf_t i = kk_sar(_kk_integer_value(x), 2);
  kk_intf_t j = kk_sar(_kk_integer_value(y), 2);
  kk_intf_t m = i%j;
  if (i < 0 && m < 0) { m += (j < 0 ? -j : j); }    // i < 0 is not needed, but see note below
  kk_assert_internal(m >= 0);
  return _kk_new_integer(kk_shlp(m,2)|1);
}


static inline kk_integer_t kk_integer_cdiv_cmod_small(kk_integer_t x, kk_integer_t y, kk_integer_t* mod) {
  kk_assert_internal(kk_are_smallints(x, y)); kk_assert_internal(mod!=NULL);
  kk_assert_internal(!kk_integer_is_zero_borrow(y));
  kk_intf_t i = kk_sar(_kk_integer_value(x), 1);
  kk_intf_t j = kk_sar(_kk_integer_value(y), 1);
  *mod = _kk_new_integer(kk_shlp(i%j,2)|1);
  return _kk_new_integer(kk_shlp(i/j,2)|1);
}

static inline kk_integer_t kk_integer_div_mod_small(kk_integer_t x, kk_integer_t y, kk_integer_t* mod) {
  kk_assert_internal(kk_are_smallints(x, y)); kk_assert_internal(mod!=NULL);
  kk_assert_internal(!kk_integer_is_zero_borrow(y));
  kk_intf_t i = kk_sar(_kk_integer_value(x), 2);
  kk_intf_t j = kk_sar(_kk_integer_value(y), 2);
  kk_intf_t d = i/j;
  kk_intf_t m = i%j;
  if (i < 0 && m < 0) {         // note: test i < 0 is not needed but implies m >= 0 and can speed up the test
    if (j < 0) {
      d++; m -= j;
    }
    else {
      d--; m += j;
    }
  }
  kk_assert_internal(m >= 0);
  kk_assert_internal(d*j + m == i);
  *mod = _kk_new_integer(kk_shlp(m,2)|1);
  return _kk_new_integer(kk_shlp(d,2)|1);
}

// small ints and `y != 0` to prevent division by zero.
// (we do not have to check for `MIN_INTF / -1` since a small int can never be `MIN_INTF`).
static inline bool kk_are_small_div_ints(kk_integer_t x, kk_integer_t y) {
  return (kk_are_smallints(x, y) && !kk_integer_is_zero_borrow(y));
}

static inline kk_integer_t kk_integer_cdiv(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_small_div_ints(x, y))) return kk_integer_cdiv_small(x, y);
  return kk_integer_cdiv_generic(x, y, ctx);
}

static inline kk_integer_t kk_integer_cmod(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_small_div_ints(x, y))) return kk_integer_cmod_small(x, y);
  return kk_integer_cmod_generic(x, y, ctx);
}

static inline kk_integer_t kk_integer_cdiv_cmod(kk_integer_t x, kk_integer_t y, kk_integer_t* mod, kk_context_t* ctx) {
  kk_assert_internal(mod!=NULL);
  if (kk_likely(kk_are_small_div_ints(x, y))) return kk_integer_cdiv_cmod_small(x, y, mod);
  return kk_integer_cdiv_cmod_generic(x, y, mod, ctx);
}

static inline kk_integer_t kk_integer_div(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_small_div_ints(x, y))) return kk_integer_div_small(x, y);
  return kk_integer_div_generic(x, y, ctx);
}

static inline kk_integer_t kk_integer_mod(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_small_div_ints(x, y))) return kk_integer_mod_small(x, y);
  return kk_integer_mod_generic(x, y, ctx);
}

static inline kk_integer_t kk_integer_div_mod(kk_integer_t x, kk_integer_t y, kk_integer_t* mod, kk_context_t* ctx) {
  kk_assert_internal(mod!=NULL);
  if (kk_likely(kk_are_small_div_ints(x, y))) return kk_integer_div_mod_small(x, y, mod);
  return kk_integer_div_mod_generic(x, y, mod, ctx);
}

static inline kk_integer_t kk_integer_sqr(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return kk_integer_mul_small(x, x, ctx);
  return kk_integer_sqr_generic(x, ctx);
}

static inline kk_integer_t kk_integer_dec(kk_integer_t x, kk_context_t* ctx) {
  // return kk_integer_sub(x, kk_integer_one, ctx);
  return kk_integer_add_small_const(x, -1, ctx);
}

static inline kk_integer_t kk_integer_inc(kk_integer_t x, kk_context_t* ctx) {
  // return kk_integer_add(x, kk_integer_one, ctx);
  return kk_integer_add_small_const(x, 1, ctx);
}


/*---------------------------------------------------------------------------------
  compare
---------------------------------------------------------------------------------*/

static inline kk_integer_t kk_integer_neg_small(kk_integer_t x, kk_context_t* ctx) {
  kk_assert_internal(kk_is_smallint(x));
  return kk_integer_sub(kk_integer_zero, x, ctx);   // negation can overflow
}

static inline kk_integer_t kk_integer_neg(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return kk_integer_neg_small(x, ctx);
  return kk_integer_neg_generic(x, ctx);
}

static inline kk_integer_t kk_integer_abs(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return (_kk_integer_value(x) < 0 ? kk_integer_neg_small(x, ctx) : x);
  return (kk_integer_signum_generic_bigint(x) < 0 ? kk_integer_neg_generic(x, ctx) : x);
}

static inline int kk_integer_cmp_borrow(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_smallints(x, y))) return (_kk_integer_value(x) == _kk_integer_value(y) ? 0 : (_kk_integer_value(x) > _kk_integer_value(y) ? 1 : -1));
  return kk_integer_cmp_generic_borrow(x, y, ctx);
}

static inline bool kk_integer_lt_borrow(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_smallints(x, y))) return (_kk_integer_value(x) < _kk_integer_value(y));
  return (kk_integer_cmp_generic_borrow(x, y, ctx) == -1);
}

static inline bool kk_integer_lt(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_smallints(x, y))) return (_kk_integer_value(x) < _kk_integer_value(y));
  return (kk_integer_cmp_generic(x, y, ctx) == -1);
}

static inline bool kk_integer_lte_borrow(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_smallints(x, y))) return (_kk_integer_value(x) <= _kk_integer_value(y));
  return (kk_integer_cmp_generic_borrow(x, y, ctx) <= 0);
}

static inline bool kk_integer_gt_borrow(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_smallints(x, y))) return (_kk_integer_value(x) > _kk_integer_value(y));
  return (kk_integer_cmp_generic_borrow(x, y, ctx) == 1);
}

static inline bool kk_integer_gt(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_smallints(x, y))) return (_kk_integer_value(x) > _kk_integer_value(y));
  return (kk_integer_cmp_generic(x, y, ctx) == 1);
}

static inline bool kk_integer_gte_borrow(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_smallints(x, y))) return (_kk_integer_value(x) >= _kk_integer_value(y));
  return (kk_integer_cmp_generic_borrow(x, y, ctx) >= 0);
}

static inline bool kk_integer_eq_borrow(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return (_kk_integer_value(x) == _kk_integer_value(y));  // assume bigint is never small    
  // if (kk_likely(kk_are_smallints(x, y))) return (_kk_integer_value(x) == _kk_integer_value(y));
  return (kk_integer_cmp_generic_borrow(x, y, ctx) == 0);
}

static inline bool kk_integer_eq(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return (_kk_integer_value(x) == _kk_integer_value(y));  // assume bigint is never small  
  // if (kk_likely(kk_are_smallints(x, y))) return (_kk_integer_value(x) == _kk_integer_value(y));
  return (kk_integer_cmp_generic(x, y, ctx) == 0);
}

static inline bool kk_integer_neq_borrow(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return (_kk_integer_value(x) != _kk_integer_value(y));  // assume bigint is never small  
  // if (kk_are_smallints(x,y)) return (_kk_integer_value(x) != _kk_integer_value(y));
  return (kk_integer_cmp_generic_borrow(x, y, ctx) != 0);
}

static inline bool kk_integer_neq(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return (_kk_integer_value(x) != _kk_integer_value(y));  // assume bigint is never small    
  // if (kk_are_smallints(x,y)) return (_kk_integer_value(x) != _kk_integer_value(y)); 
  return (kk_integer_cmp_generic(x, y, ctx) != 0);
}

static inline bool kk_integer_is_even(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return ((_kk_integer_value(x)&0x04)==0);
  return kk_integer_is_even_generic(x, ctx);
}

static inline bool kk_integer_is_odd(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return ((_kk_integer_value(x)&0x04)!=0);
  return !kk_integer_is_even_generic(x, ctx);
}

static inline int kk_integer_signum_borrow(kk_integer_t x) {
  if (kk_likely(kk_is_smallint(x))) return ((_kk_integer_value(x)>1)-(_kk_integer_value(x)<0));
  return kk_integer_signum_generic_bigint(x);
}

static inline bool kk_integer_is_pos_borrow(kk_integer_t x) {
  if (kk_likely(kk_is_smallint(x))) return (_kk_integer_value(x)>1);
  return (kk_integer_signum_generic_bigint(x) > 0);
}

static inline bool kk_integer_is_neg_borrow(kk_integer_t x) {
  if (kk_likely(kk_is_smallint(x))) return (_kk_integer_value(x)<0);
  return (kk_integer_signum_generic_bigint(x) < 0);
}

static inline kk_integer_t kk_integer_max(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_smallints(x, y))) return (_kk_integer_value(x)>=_kk_integer_value(y) ? x : y);
  if (kk_integer_gte_borrow(x, y, ctx)) {
    kk_integer_drop(y, ctx); return x;
  }
  else {
    kk_integer_drop(x, ctx); return y;
  }
}

static inline kk_integer_t kk_integer_min(kk_integer_t x, kk_integer_t y, kk_context_t* ctx) {
  if (kk_likely(kk_are_smallints(x, y))) return (_kk_integer_value(x)<=_kk_integer_value(y) ? x : y);
  if (kk_integer_lte_borrow(x, y, ctx)) {
    kk_integer_drop(y, ctx); return x;
  }
  else {
    kk_integer_drop(x, ctx); return y;
  }
}


/*---------------------------------------------------------------------------------
  clamp int to smaller ints
---------------------------------------------------------------------------------*/

static inline int32_t kk_integer_clamp32(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) {
    kk_intf_t i = kk_smallint_from_integer(x);
#if (KK_SMALLINT_MAX > INT32_MAX)
    return (i < INT32_MIN ? INT32_MIN : (i > INT32_MAX ? INT32_MAX : (int32_t)i));
#else
    return (int32_t)i;
#endif
  }
  else {
    return kk_integer_clamp32_generic(x,ctx);
  }
}

static inline int32_t kk_integer_clamp32_borrow(kk_integer_t x, kk_context_t* ctx) { // used for cfc field of evidence
  if (kk_likely(kk_is_smallint(x))) {
    kk_intf_t i = kk_smallint_from_integer(x);
#if (KK_SMALLINT_MAX > INT32_MAX)
    return (i < INT32_MIN ? INT32_MIN : (i > INT32_MAX ? INT32_MAX : (int32_t)i));
#else
    return (int32_t)i;
#endif
  }
  else {
    return kk_integer_clamp32_generic(kk_integer_dup(x), ctx);
  }
}

static inline int64_t kk_integer_clamp64(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) {
    kk_intf_t i = kk_smallint_from_integer(x);
#if (KK_SMALLINT_MAX > INT64_MAX)
    return (i < INT64_MIN ? INT64_MIN : (i > INT64_MAX ? INT64_MAX : (int64_t)i));
#else
    return (int64_t)i;
#endif
  }
  else {
    return kk_integer_clamp64_generic(x, ctx);
  }
}

static inline int64_t kk_integer_clamp64_borrow(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) {
    kk_intf_t i = kk_smallint_from_integer(x);
#if (KK_SMALLINT_MAX > INT64_MAX)
    return (i < INT64_MIN ? INT64_MIN : (i > INT64_MAX ? INT64_MAX : (int64_t)i));
#else
    return (int64_t)i;
#endif
  }
  else {
    return kk_integer_clamp64_generic(kk_integer_dup(x), ctx);
  }
}

static inline uint8_t kk_integer_clamp_byte(kk_integer_t x, kk_context_t* ctx) {
  int32_t i = kk_integer_clamp32(x,ctx);
  return (i < 0 ? 0 : (i > 255 ? 255 : (uint8_t)(i)));
}

static inline size_t kk_integer_clamp_size_t(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) {
    kk_intf_t i = kk_smallint_from_integer(x);
#if (KK_SMALLINT_MAX > SIZE_MAX)
    return (i < 0 ? 0 : (i > SIZE_MAX ? SIZE_MAX : (size_t)i));
#else
    return (i < 0 ? 0 : (size_t)i);
#endif
  }
  return kk_integer_clamp_size_t_generic(x,ctx);
}

static inline kk_ssize_t kk_integer_clamp_ssize_t(kk_integer_t x, kk_context_t* ctx) {
#if KK_SSIZE_MAX == INT32_MAX
  return kk_integer_clamp32(x,ctx);
#elif KK_SSIZE_MAX == INT64_MAX
  return kk_integer_clamp64(x,ctx);
#else
#error "define integer_clamp_ssize_t on this platform"
#endif
}

static inline kk_ssize_t kk_integer_clamp_ssize_t_borrow(kk_integer_t x, kk_context_t* ctx) { // used for array indexing
#if KK_SSIZE_MAX == INT32_MAX
  return kk_integer_clamp32_borrow(x, ctx);
#elif KK_SSIZE_MAX == INT64_MAX
  return kk_integer_clamp64_borrow(x, ctx);
#else
#error "define integer_clamp_ssize_t_borrow on this platform"
#endif
}

static inline intptr_t kk_integer_clamp_intptr_t(kk_integer_t x, kk_context_t* ctx) {
#if INTPTR_MAX == INT32_MAX
  return kk_integer_clamp32(x,ctx);
#elif INTPTR_MAX == INT64_MAX
  return kk_integer_clamp64(x,ctx);
#else
#error "define integer_clamp_intptr_t on this platform"
#endif
}

static inline kk_intx_t kk_integer_clamp(kk_integer_t x, kk_context_t* ctx) {
#if KK_INTX_MAX == INT32_MAX
  return kk_integer_clamp32(x, ctx);
#elif KK_INTX_MAX == INT64_MAX
  return kk_integer_clamp64(x, ctx);
#else
#error "define integer_clamp on this platform"
#endif
}

static inline kk_intx_t kk_integer_clamp_borrow(kk_integer_t x, kk_context_t* ctx) {
#if KK_INTX_MAX == INT32_MAX
  return kk_integer_clamp32_borrow(x, ctx);
#elif KK_INTX_MAX == INT64_MAX
  return kk_integer_clamp64_borrow(x, ctx);
#else
#error "define integer_clamp_borrow on this platform"
#endif
}

static inline double kk_integer_as_double(kk_integer_t x, kk_context_t* ctx) {
  if (kk_likely(kk_is_smallint(x))) return (double)(kk_smallint_from_integer(x));
  return kk_integer_as_double_generic(x,ctx);
}




#endif // include guard
