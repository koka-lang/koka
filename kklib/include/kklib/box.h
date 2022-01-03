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
#define KK_BOX_DOUBLE64    (1)    // box doubles on 64-bit using strategy A1 by default
// #define KK_BOX_DOUBLE64    (2)    // heap allocate negative doubles on 64-bit (strategy A2)
// #define KK_BOX_DOUBLE64    (0)    // heap allocate doubles interpreted as int64_t (strategy A0)
#else
#define KK_BOX_DOUBLE64    (0)
#endif

#define KK_BOXED_VALUE_BITS  (KK_INTF_BITS-1)   // note: can be less than intptr_t on CHERI architectures for example

#define KK_MAX_BOXED_INT  ((kk_intf_t)KK_INTF_MAX >> (KK_INTF_BITS - KK_BOXED_VALUE_BITS))
#define KK_MIN_BOXED_INT  (- KK_MAX_BOXED_INT - 1)

#define KK_MAX_BOXED_UINT ((kk_uintf_t)KK_UINTF_MAX >> (KK_INTF_BITS - KK_BOXED_VALUE_BITS))
#define KK_MIN_BOXED_UINT (0)


// Forward declarations
static inline bool         kk_box_is_ptr(kk_box_t b);
static inline kk_block_t*  kk_ptr_unbox(kk_box_t b);
static inline kk_box_t     kk_ptr_box(const kk_block_t* p);
static inline kk_intf_t    kk_intf_unbox(kk_box_t v);
static inline kk_box_t     kk_intf_box(kk_intf_t i);

// Low level access
static inline kk_box_t _kk_box_new_ptr(const kk_block_t* p) {
  kk_box_t b = { (uintptr_t)p };
  return b;
}
static inline kk_box_t _kk_box_new_value(kk_uintf_t u) {
  kk_box_t b = { u };
  return b;
}

static inline kk_uintf_t _kk_box_value(kk_box_t b) {
  return (kk_uintf_t)(b.box);
}
static inline kk_ptr_t _kk_box_ptr(kk_box_t b) {
  return (kk_ptr_t)(b.box);
}

// query
static inline bool kk_box_is_ptr(kk_box_t b) {
  return ((_kk_box_value(b)&1)==0);
}
static inline bool kk_box_is_value(kk_box_t b) {
  return ((_kk_box_value(b)&1)!=0);
}

// Are two boxed representations equal?
static inline bool kk_box_eq(kk_box_t b1, kk_box_t b2) {
  return (b1.box == b2.box);
}

// We cannot store NULL as a pointer (`kk_ptr_t`); use `box_null` instead
#define kk_box_null       (_kk_box_new_ptr((kk_ptr_t)(~KK_UP(0))))  // -1 value

// null initializer
#define kk_box_null_init  {~KK_UP(0)}


static inline bool kk_box_is_null(kk_box_t b) {
  return (b.box == kk_box_null.box);
}

static inline bool kk_box_is_non_null_ptr(kk_box_t v) {
  kk_assert_internal(!kk_box_is_ptr(v) || v.box != 0);   // NULL pointers are never allowed as boxed values
  return (kk_box_is_ptr(v));  //  && kk_ptr_unbox(v) != NULL
}

static inline bool kk_box_is_any(kk_box_t b) {
  return (kk_box_is_ptr(b) && kk_block_has_tag(kk_ptr_unbox(b), KK_TAG_BOX_ANY));
}


/*----------------------------------------------------------------
  Box pointers and kk_intf_t
----------------------------------------------------------------*/
static inline kk_ptr_t kk_ptr_unbox(kk_box_t v) {
  kk_assert_internal(kk_box_is_ptr(v) || kk_box_is_any(v));
  kk_assert_internal(v.box != 0); // no NULL pointers allowed
  return _kk_box_ptr(v);
}

static inline kk_box_t kk_ptr_box(const kk_block_t* p) {
  kk_assert_internal(((uintptr_t)p & 0x03) == 0); // check alignment
  kk_assert_internal(p != NULL);                  // block should never be NULL
  return _kk_box_new_ptr(p);
}

static inline kk_uintf_t kk_uintf_unbox(kk_box_t b) {
  kk_assert_internal(kk_box_is_value(b) || kk_box_is_any(b));
  return kk_shrf(_kk_box_value(b), 1);
}

static inline kk_box_t kk_uintf_box(kk_uintf_t u) {
  kk_assert_internal(u <= KK_MAX_BOXED_UINT);
  return _kk_box_new_value((u << 1)|1);
}

static inline kk_intf_t kk_intf_unbox(kk_box_t v) {
  kk_assert_internal(kk_box_is_value(v) || kk_box_is_any(v));
  return kk_sarf((kk_intf_t)_kk_box_value(v), 1); // preserve sign
}

static inline kk_box_t kk_intf_box(kk_intf_t i) {
  kk_assert_internal(i >= KK_MIN_BOXED_INT && i <= KK_MAX_BOXED_INT);
  return _kk_box_new_value(((kk_uintf_t)i << 1)|1);
}

static inline kk_box_t kk_box_dup(kk_box_t b) {
  if (kk_box_is_ptr(b)) kk_block_dup(kk_ptr_unbox(b));
  return b;
}

static inline void kk_box_drop(kk_box_t b, kk_context_t* ctx) {
  if (kk_box_is_ptr(b)) kk_block_drop(kk_ptr_unbox(b), ctx);
}

/*----------------------------------------------------------------
  Integers & Floats
----------------------------------------------------------------*/

kk_decl_export intptr_t kk_intptr_unbox(kk_box_t v, kk_context_t* ctx);
kk_decl_export kk_box_t kk_intptr_box(intptr_t i, kk_context_t* ctx);

kk_decl_export kk_box_t   kk_ssize_box(kk_ssize_t i, kk_context_t* ctx);
kk_decl_export kk_ssize_t kk_ssize_unbox(kk_box_t b, kk_context_t* ctx);

#if (KK_INTPTR_SIZE <= 8)
kk_decl_export int64_t  kk_int64_unbox(kk_box_t v, kk_context_t* ctx);
kk_decl_export kk_box_t kk_int64_box(int64_t i, kk_context_t* ctx);
#else
static inline int64_t kk_int64_unbox(kk_box_t v, kk_context_t* ctx) {
  kk_unused(ctx);
  intptr_t i = kk_sarp((intptr_t)v.box, 1);
  kk_assert_internal((i >= INT64_MIN && i <= INT64_MAX) || kk_box_is_any(v));
  return (int64_t)i;
}
static inline kk_box_t kk_int64_box(int64_t i, kk_context_t* ctx) {
  kk_unused(ctx);
  kk_box_t b = { ((uintptr_t)i << 1) | 1 };
  return b;
}
#endif

#if (KK_INTPTR_SIZE<=4)
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

#if (KK_INTPTR_SIZE<=2) 
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

#if (KK_INTPTR_SIZE == 8) && KK_BOX_DOUBLE64
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

#if (KK_INTPTR_SIZE == 4)
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

static inline kk_block_t* kk_block_unbox(kk_box_t v, kk_tag_t kk_expected_tag ) {
  kk_unused_internal(kk_expected_tag);
  kk_block_t* b = kk_ptr_unbox(v);
  kk_assert_internal(kk_block_tag(b) == kk_expected_tag);
  return b;
}

static inline kk_box_t kk_block_box(kk_block_t* b) {
  return kk_ptr_box(b);
}

static inline kk_box_t kk_ptr_box_assert(kk_block_t* b, kk_tag_t tag) {
  kk_unused_internal(tag);
  kk_assert_internal(kk_block_tag(b) == tag);
  return kk_ptr_box(b);
}

#define kk_basetype_unbox_as_assert(tp,b,tag)  (kk_block_assert(tp,kk_ptr_unbox(b),tag))
#define kk_basetype_unbox_as(tp,b)             ((tp)kk_ptr_unbox(b))
#define kk_basetype_box(b)                     (kk_ptr_box(&(b)->_block))

#define kk_constructor_unbox_as(tp,b,tag)      (kk_basetype_unbox_as_assert(tp,b,tag))
#define kk_constructor_box(b)                  (kk_basetype_box(&(b)->_base))

static inline kk_datatype_t kk_datatype_unbox(kk_box_t b) {
  kk_datatype_t d = { b.box };
  return d;
}

static inline kk_box_t kk_datatype_box(kk_datatype_t d) {
  kk_box_t b = { d.dbox };
  return b;
}

static inline kk_uintx_t kk_enum_unbox(kk_box_t b) {
  return kk_uintf_unbox(b);
}

static inline kk_box_t kk_enum_box(kk_uintx_t u) {
  return kk_uintf_box(u);
}

static inline kk_box_t kk_box_box(kk_box_t b, kk_context_t* ctx) {
  kk_unused(ctx);
  return b;
}

static inline kk_box_t kk_box_unbox(kk_box_t b, kk_context_t* ctx) {
  kk_unused(ctx);
  return b;
}


/*----------------------------------------------------------------
  Generic boxing of value types
----------------------------------------------------------------*/

typedef struct kk_boxed_value_s {
  kk_block_t _block;
  intptr_t   data; 
} * kk_boxed_value_t;

#define kk_valuetype_unbox_(tp,p,x,box,ctx) \
  do { \
    if (kk_unlikely(kk_box_is_any(box))) { \
      p = NULL; \
      const size_t kk__max_scan_fsize = sizeof(tp)/sizeof(kk_box_t); \
      kk_box_t* _fields = (kk_box_t*)(&x); \
      for (size_t i = 0; i < kk__max_scan_fsize; i++) { _fields[i] = kk_box_any(ctx);  } \
      kk_block_decref(kk_ptr_unbox(box),ctx); \
    } \
    else { \
      p = kk_basetype_unbox_as_assert(kk_boxed_value_t, box, KK_TAG_BOX); \
      memcpy(&x,&p->data,sizeof(tp)); /* avoid aliasing warning,  x = *((tp*)(&p->data)); */ \
    } \
  } while(0)

#define kk_valuetype_box(tp,x,val,scan_fsize,ctx)  \
  do { \
    kk_boxed_value_t p = kk_block_assert(kk_boxed_value_t, kk_block_alloc(sizeof(kk_block_t) + sizeof(tp), scan_fsize, KK_TAG_BOX, ctx), KK_TAG_BOX); \
    const tp valx = val;               /* ensure we can take the address */ \
    memcpy(&p->data,&valx,sizeof(tp)); /* avoid aliasing warning: *((tp*)(&p->data)) = val; */ \
    x = kk_basetype_box(p); \
  } while(0)

// `box_any` is used to return when yielding 
// (and should be accepted by any unbox operation, and also dup/drop operations. That is why we use a ptr)
static inline kk_box_t kk_box_any(kk_context_t* ctx) {
  kk_basetype_dup_assert(kk_box_any_t, ctx->kk_box_any, KK_TAG_BOX_ANY);
  return kk_basetype_box(ctx->kk_box_any);
}



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
kk_decl_export void*    kk_cptr_raw_unbox(kk_box_t b);
kk_decl_export kk_box_t kk_cptr_box(void* p, kk_context_t* ctx);
kk_decl_export void*    kk_cptr_unbox(kk_box_t b);

// C function pointers
typedef void (*kk_cfun_ptr_t)(void);

typedef struct kk_cfunptr_s {
  kk_block_t     _block;
  kk_cfun_ptr_t  cfunptr;
} *kk_cfunptr_t;

#define kk_cfun_ptr_box(f,ctx)  kk_cfun_ptr_boxx((kk_cfun_ptr_t)f, ctx)

kk_decl_export kk_box_t      kk_cfun_ptr_boxx(kk_cfun_ptr_t f, kk_context_t* ctx);
kk_decl_export kk_cfun_ptr_t kk_cfun_ptr_unbox(kk_box_t b);



#endif // include guard
