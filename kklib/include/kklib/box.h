#pragma once
#ifndef KK_BOX_H
#define KK_BOX_H

/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/*-------------------------------------------------------------------------
Boxing

We assume pointers are always aligned to the machine word size, and we  
use the bottom (least significant) bit to distinguish pointers from values. 
This way, boxing a heap pointer has zero cost and is unchanged. 
For integers, we use a pointer to big integers, or a value for small integers 
(and boxing is zero cost this way as well).

On platforms like arm CHERI, we have 128-bit pointers and a box is always
128-bits in that case, but for values we just use the bottom 64 bits as
the arithmetic registers are still 64-bit (using `kk_intf_t`).

On 64-bit, using `x` for bytes, and `b` for bits, with `z` the least significant byte, we have:

  xxxx xxxz   z = bbbbbbb0  : 64-bit pointer p  (always aligned to (at least) 2 bytes!)
  xxxx xxxz   z = bbbbbbb1  : 63-bit values n as n*2+1

On 64-bit, We can encode half of the doubles as values by saving 1 bit; Possible strategies:
(A1): use value encoding if the 11-bit exponent fits in 10-bits. This uses value encoding
      for numbers whose absolute value is in the range [2^-511,2^512), or if it is
      zero, subnormal, NaN, or infinity. This is the default: it is more computationally
      expensive but it can avoid many allocations as this captures almost
      all doubles that are commonly in use for most workloads.
(A2): heap allocate all negative doubles and use values for positive ones (in 63-bits).
      (for simplicity we always use this for floats on 32-bit platforms)
(A0): We can also box doubles as an int64_t, which means all doubles outside the range
      [0,2.0) and [-inf,-2.0) would be heap allocated. 
----------------------------------------------------------------*/

#if (KK_INTPTR_SIZE == 8)
#define KK_BOX_DOUBLE64    (1)       // box doubles on 64-bit using strategy A1 by default
// #define KK_BOX_DOUBLE64    (2)    // heap allocate negative doubles on 64-bit (strategy A2)
// #define KK_BOX_DOUBLE64    (0)    // heap allocate doubles interpreted as int64_t (strategy A0)
#else
#define KK_BOX_DOUBLE64    (0)
#endif


// Forward declarations
static inline bool         kk_box_is_ptr(kk_box_t b);
static inline kk_block_t*  kk_ptr_unbox(kk_box_t b, kk_context_t* ctx);
static inline kk_box_t     kk_ptr_box(const kk_block_t* p, kk_context_t* ctx);
static inline kk_intf_t    kk_intf_unbox(kk_box_t v);
static inline kk_box_t     kk_intf_box(kk_intf_t i);

// Low level access
static inline kk_box_t kk_box_from_ptr(const kk_block_t* p, kk_context_t* ctx) {
  kk_box_t b = { kk_ptr_encode((kk_ptr_t)p,ctx) };
  return b;
}

static inline kk_box_t kk_box_from_value(kk_intf_t i, int extra_shift ) {
  kk_box_t b = { kk_intf_encode(i,extra_shift) };
  return b;
}

static inline kk_ptr_t kk_box_to_ptr(kk_box_t b, kk_context_t* ctx) {
  return kk_ptr_decode(b.box, ctx);
}

static inline kk_intf_t kk_box_to_value(kk_box_t b, int extra_shift) {
  return kk_intf_decode(b.box, extra_shift);
}


// query
static inline bool kk_box_is_ptr(kk_box_t b) {
  return kk_is_ptr(b.box);
}

static inline bool kk_box_is_value(kk_box_t b) {
  return kk_is_value(b.box);
}


// Are two boxed representations equal?
static inline bool kk_box_eq(kk_box_t b1, kk_box_t b2) {
  return (b1.box == b2.box);
}

// null initializer
#define kk_box_null_init     kk_value_null

// We cannot store NULL as a pointer (`kk_ptr_t`); use `kk_box_null()` instead
static inline kk_box_t kk_box_null(void) {
  kk_box_t b = { kk_box_null_init };
  return b;
}

static inline bool kk_box_is_null(kk_box_t b) {
  return (b.box == kk_box_null_init);
}

static inline bool kk_box_is_non_null_ptr(kk_box_t v) {
  kk_assert_internal(!kk_box_is_ptr(v) || v.box != 0);   // NULL pointers are never allowed as boxed values
  return (kk_box_is_ptr(v));  //  && kk_ptr_unbox(v) != NULL
}

static inline bool kk_box_is_any(kk_box_t b) {
  return (kk_box_is_ptr(b) && kk_block_has_tag(kk_ptr_unbox(b,kk_get_context()), KK_TAG_BOX_ANY));
}

static inline kk_box_t kk_box_from_potential_null_ptr(kk_block_t* p, kk_context_t* ctx) {
  if (p == NULL) return kk_box_null();
            else return kk_box_from_ptr(p,ctx);
}

static inline kk_block_t* kk_box_to_potential_null_ptr(kk_box_t b, kk_context_t* ctx) {
  if (kk_box_is_null(b)) return NULL;
                    else return kk_box_to_ptr(b, ctx);
}

/*----------------------------------------------------------------
  Box pointers and kk_intf_t
----------------------------------------------------------------*/
static inline kk_ptr_t kk_ptr_unbox(kk_box_t v, kk_context_t* ctx) {
  kk_assert_internal(kk_box_is_ptr(v) || kk_box_is_any(v));
  kk_assert_internal(v.box != 0); // no NULL pointers allowed
  return kk_box_to_ptr(v, ctx);
}

static inline kk_box_t kk_ptr_box(const kk_block_t* p, kk_context_t* ctx) {
  kk_assert_internal(((uintptr_t)p & 0x03) == 0); // check alignment
  kk_assert_internal(p != NULL);                  // block should never be NULL
  return kk_box_from_ptr(p,ctx);
}

static inline kk_intf_t kk_intf_unbox(kk_box_t v) {
  kk_assert_internal(kk_box_is_value(v) || kk_box_is_any(v));
  return kk_box_to_value(v, 0);
}

static inline kk_box_t kk_intf_box(kk_intf_t i) {
  return kk_box_from_value(i, 0);
}


static inline kk_uintf_t kk_uintf_unbox(kk_box_t b) {
  kk_assert_internal(kk_box_is_value(b) || kk_box_is_any(b));
  kk_intf_t i = kk_intf_unbox(b);
  return (kk_uintf_t)kk_shrf(kk_shlf(i, KK_TAG_BITS), KK_TAG_BITS);
}

static inline kk_box_t kk_uintf_box(kk_uintf_t u) {
  kk_assert_internal(u <= KK_UINTF_BOX_MAX(0));
  kk_intf_t i = kk_sarf(kk_shlf((kk_intf_t)u, KK_TAG_BITS), KK_TAG_BITS);
  return kk_intf_box(i);
}


static inline kk_box_t kk_box_dup(kk_box_t b, kk_context_t* ctx) {
  if (kk_box_is_ptr(b)) { kk_block_dup(kk_ptr_unbox(b, ctx)); }
  return b;
}

static inline void kk_box_drop(kk_box_t b, kk_context_t* ctx) {
  if (kk_box_is_ptr(b)) { kk_block_drop(kk_ptr_unbox(b, ctx), ctx); }
}

/*----------------------------------------------------------------
  Integers & Floats
----------------------------------------------------------------*/

kk_decl_export intptr_t kk_intptr_unbox(kk_box_t v, kk_context_t* ctx);
kk_decl_export kk_box_t kk_intptr_box(intptr_t i, kk_context_t* ctx);

kk_decl_export kk_box_t   kk_ssize_box(kk_ssize_t i, kk_context_t* ctx);
kk_decl_export kk_ssize_t kk_ssize_unbox(kk_box_t b, kk_context_t* ctx);

#if (KK_INTF_SIZE <= 8)
kk_decl_export int64_t  kk_int64_unbox(kk_box_t v, kk_context_t* ctx);
kk_decl_export kk_box_t kk_int64_box(int64_t i, kk_context_t* ctx);
#else
static inline int64_t kk_int64_unbox(kk_box_t v, kk_context_t* ctx) {
  kk_unused(ctx);
  kk_intf_t i = kk_intf_unbox(v, ctx);
  kk_assert_internal((i >= INT64_MIN && i <= INT64_MAX) || kk_box_is_any(v));
  return (int64_t)i;
}
static inline kk_box_t kk_int64_box(int64_t i, kk_context_t* ctx) {
  kk_unused(ctx);
  return kk_intf_box(i);
}
#endif

#if (KK_INTF_SIZE<=4)
kk_decl_export int32_t  kk_int32_unbox(kk_box_t v, kk_context_t* ctx);
kk_decl_export kk_box_t kk_int32_box(int32_t i, kk_context_t* ctx);
#else
static inline int32_t kk_int32_unbox(kk_box_t v, kk_context_t* ctx) {
  kk_unused(ctx);
  kk_intf_t i = kk_intf_unbox(v);
  kk_assert_internal((i >= INT32_MIN && i <= INT32_MAX) || kk_box_is_any(v));
  return (int32_t)(i);
}

static inline kk_box_t kk_int32_box(int32_t i, kk_context_t* ctx) {
  kk_unused(ctx);
  return kk_intf_box(i);
}
#endif

#if (KK_INTF_SIZE<=2) 
kk_decl_export int16_t  kk_int16_unbox(kk_box_t v, kk_context_t* ctx);
kk_decl_export kk_box_t kk_int16_box(int16_t i, kk_context_t* ctx);
#else
static inline int16_t kk_int16_unbox(kk_box_t v, kk_context_t* ctx) {
  kk_unused(ctx);
  kk_intf_t i = kk_intf_unbox(v);
  kk_assert_internal((i >= INT16_MIN && i <= INT16_MAX) || kk_box_is_any(v));
  return (int16_t)(i);
}
static inline kk_box_t kk_int16_box(int16_t i, kk_context_t* ctx) {
  kk_unused(ctx);
  return kk_intf_box(i);
}
#endif

#if (KK_INTF_SIZE == 8) && KK_BOX_DOUBLE64
kk_decl_export kk_box_t kk_double_box(double d, kk_context_t* ctx);
kk_decl_export double   kk_double_unbox(kk_box_t b, kk_context_t* ctx);
#else
static inline double kk_double_unbox(kk_box_t b, kk_context_t* ctx) {
  int64_t i = kk_int64_unbox(b, ctx);
  return kk_bits_to_double((uint64_t)i);
}
static inline kk_box_t kk_double_box(double d, kk_context_t* ctx) {
  uint64_t u = kk_bits_from_double(d);
  return kk_int64_box((int64_t)u, ctx);
}
#endif

#if (KK_INTF_SIZE == 4)
kk_decl_export float    kk_float_unbox(kk_box_t b, kk_context_t* ctx);
kk_decl_export kk_box_t kk_float_box(float f, kk_context_t* ctx);
#else
static inline float kk_float_unbox(kk_box_t b, kk_context_t* ctx) {
  int32_t i = kk_int32_unbox(b, ctx);
  return kk_bits_to_float((uint32_t)i);  
}
static inline kk_box_t kk_float_box(float f, kk_context_t* ctx) {
  uint32_t u = kk_bits_from_float(f);
  return kk_int32_box((int32_t)u, ctx);
}
#endif

/*----------------------------------------------------------------
  Other primitive types
----------------------------------------------------------------*/

static inline bool kk_bool_unbox(kk_box_t v) {
  return (kk_intf_unbox(v) != 0);
}

static inline kk_box_t kk_bool_box(bool b) {
  return kk_intf_box(b ? 1 : 0);
}

static inline kk_box_t kk_size_box(size_t i, kk_context_t* ctx) {
  return kk_ssize_box((kk_ssize_t)i, ctx);
}

static inline size_t kk_size_unbox(kk_box_t b, kk_context_t* ctx) {
  return (size_t)kk_ssize_unbox(b, ctx);
}

static inline kk_block_t* kk_block_unbox(kk_box_t v, kk_tag_t kk_expected_tag, kk_context_t* ctx ) {
  kk_unused_internal(kk_expected_tag);
  kk_block_t* b = kk_ptr_unbox(v,ctx);
  kk_assert_internal(kk_block_tag(b) == kk_expected_tag);
  return b;
}

#define kk_block_unbox_as(tp,v,tag,ctx)  kk_block_as(tp,kk_block_unbox(v,tag,ctx))

static inline kk_box_t kk_block_box(kk_block_t* b, kk_context_t* ctx) {
  return kk_ptr_box(b, ctx);
}

static inline kk_box_t kk_ptr_box_assert(kk_block_t* b, kk_tag_t tag, kk_context_t* ctx) {
  kk_unused_internal(tag);
  kk_assert_internal(kk_block_tag(b) == tag);
  return kk_ptr_box(b,ctx);
}


static inline kk_uintf_t kk_enum_unbox(kk_box_t b) {
  kk_intf_t i = kk_intf_unbox(b);
  kk_assert_internal(i >= 0);
  return (kk_uintf_t)i;
}

static inline kk_box_t kk_enum_box(kk_uintf_t u) {
  kk_assert_internal(u <= KK_INTF_BOX_MAX(0));
  return kk_intf_box((kk_intf_t)u);
}

static inline kk_box_t kk_box_box(kk_box_t b, kk_context_t* ctx) {
  kk_unused(ctx);
  return b;
}

static inline kk_box_t kk_box_unbox(kk_box_t b, kk_context_t* ctx) {
  kk_unused(ctx);
  return b;
}

// `box_any` is used to return when yielding 
// (and should be accepted by any unbox operation, and also dup/drop operations. That is why we use a ptr)
static inline kk_box_t kk_box_any(kk_context_t* ctx) {
  kk_datatype_ptr_dup_assert(ctx->kk_box_any, KK_TAG_BOX_ANY, ctx);
  return kk_datatype_ptr_box(ctx->kk_box_any);
}

/*----------------------------------------------------------------
  Generic boxing of value types
----------------------------------------------------------------*/

typedef struct kk_boxed_value_s {
  kk_block_t _block;
  intptr_t   data; 
} * kk_boxed_value_t;


kk_decl_export void kk_valuetype_unbox_from_any(kk_box_t* p, size_t size, kk_box_t box, kk_context_t* ctx);

#define kk_valuetype_unbox(tp,x,box,ctx) \
  do { \
    if kk_unlikely(kk_box_is_any(box)) { \
      kk_valuetype_unbox_from_any((kk_box_t*)&x, sizeof(tp), box, ctx); \
    } \
    else { \
      kk_boxed_value_t p = kk_base_type_unbox_as_assert(kk_boxed_value_t, box, KK_TAG_BOX, ctx); \
      memcpy(&x,&p->data,sizeof(tp)); /* avoid aliasing warning,  x = *((tp*)(&p->data)); */ \
      /* if (ctx!=NULL) { */ \
        if (kk_base_type_is_unique(p)) { kk_base_type_free(p,ctx); } \
                                  else { tp##_dup(x,ctx); kk_base_type_decref(p,ctx); } \
      /* } */ \
    }\
  } while(0)
  

#define kk_valuetype_box(tp,x,val,scan_fsize,ctx)  \
  do { \
    kk_boxed_value_t p = kk_block_assert(kk_boxed_value_t, kk_block_alloc(sizeof(kk_block_t) + sizeof(tp), scan_fsize, KK_TAG_BOX, ctx), KK_TAG_BOX); \
    const tp valx = val;               /* ensure we can take the address */ \
    memcpy(&p->data,&valx,sizeof(tp)); /* avoid aliasing warning: *((tp*)(&p->data)) = val; */ \
    x = kk_base_type_box(p,ctx); \
  } while(0)



/*----------------------------------------------------------------
  C pointers and function pointers
----------------------------------------------------------------*/

// A function to free a raw C pointer, raw bytes, or raw string.
typedef void (kk_free_fun_t)(void* p, kk_block_t* block, kk_context_t* ctx);
kk_decl_export void kk_free_fun_null(void* p, kk_block_t* block, kk_context_t* ctx);
kk_decl_export void kk_free_fun(void* p, kk_block_t* block, kk_context_t* ctx);

// "raw" types: first field is pointer to a free function, the next field a pointer to raw C data
typedef struct kk_cptr_raw_s {
  kk_block_t     _block;
  kk_free_fun_t* free;
  void* cptr;
} *kk_cptr_raw_t;

kk_decl_export kk_box_t kk_cptr_raw_box(kk_free_fun_t* freefun, void* p, kk_context_t* ctx);
kk_decl_export void* kk_cptr_raw_unbox(kk_box_t b, kk_context_t* ctx);
kk_decl_export kk_box_t kk_cptr_box(void* p, kk_context_t* ctx);
kk_decl_export void* kk_cptr_unbox(kk_box_t b, kk_context_t* ctx);

// C function pointers
typedef void (*kk_cfun_ptr_t)(void);

typedef struct kk_cfunptr_s {
  kk_block_t     _block;
  kk_cfun_ptr_t  cfunptr;
} *kk_cfunptr_t;


// Koka function pointers.
// We encode these as values for efficiency. It would be best if we can assume functions addresses
// are always aligned but it turns out that this is difficult to ensure with various compilers.
// Instead we assume that the function adresses always fit an `kk_intf_t` and encode as a regular `kk_intf_t`.
// If the heap is compressed, use the offset to the main function.
static inline kk_box_t kk_kkfun_ptr_boxx(kk_cfun_ptr_t fun, kk_context_t* ctx) {  // never drop; only used from function call
  kk_unused(ctx);
  intptr_t f = (intptr_t)fun;
  #if KK_COMPRESS
  f = f - (intptr_t)&kk_main_start;
  #endif
  kk_assert(f >= KK_INTF_BOX_MIN(0) && f <= KK_INTF_BOX_MAX(0));
  return kk_intf_box((kk_intf_t)f);
}

#define kk_kkfun_ptr_box(fun,ctx)  kk_kkfun_ptr_boxx((kk_cfun_ptr_t)fun, ctx)

static inline kk_cfun_ptr_t kk_kkfun_ptr_unbox(kk_box_t b, kk_context_t* ctx) {
  kk_unused(ctx);  
  intptr_t f = kk_intf_unbox(b);
  #if KK_COMPRESS
  f = f + (intptr_t)&kk_main_start;
  #endif
  return (kk_cfun_ptr_t)f;
}


#endif // include guard
