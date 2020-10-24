#pragma once
#ifndef KK_BOX_H
#define KK_BOX_H

/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/*-------------------------------------------------------------------------
Boxing

We assume pointers are always aligned to the machine word size, and we  
use the bottom (least significant) bit to distinguish pointers from values. 
This way, boxing a heap pointer has zero cost and is unchanged which helps
the processor with prediction. For integers, we use a pointer to big integers,
or a value for small integers (and boxing is zero cost this way as well).

On 32-bit platforms doubles are heap allocated when boxed, but on 64-bit
platforms there are 2 strategies: 
(A) As we lose 1 bit, heap allocate half of the doubles, and use the 
    value encoding for the other half.
(B) limit addresses and values to 52 bits and use the top 12 bits to 
    distinguish pointers, values, or doubles. This effectively encodes 
    pointers and values in the NaN space but encodes it in a way that pointers 
    can be used as is. (This strategy is selected when `USE_NAN_BOX`=1)
    
Option (B) avoids allocating any double for boxing but has a cost in that 
scanning memory for recursive free-ing is more expensive (to distinguish 
pointers from doubles) so we default to option (A).

Using `x` for bytes, and `b` for bits, with `z` the least significant byte, we have:

(A):

    (xxxx xxxx) xxxx xxxz   z = bbbb bbb0  : 64-bit pointer p  (always aligned to (at least) 2 bytes!)
    (xxxx xxxx) xxxx xxxz   z = bbbb bbb1  : 63-bit values n as n*2+1

On 64-bit, We can encode half of the doubles by saving 1 bit; There are two implemented strategies:
(A1): heap allocate negative doubles, where a positive double is encoded as a value 
       `((d<<1) | 1)`. (define BOX_DOUBLE_IF_NEG to use this strategy)
(A2): use value encoding if the 11-bit exponent fits in 10-bits. This uses value encoding
      for numbers whose absolute value is in the range [2^-511,2^512), or if it is
      zero, subnormal, NaN, or infinity. This is the default as this captures almost
      all doubles that are commonly in use for most workloads.

(B), use NaN boxing on 64-bit:
   
For pointers and integers, the top 12-bits are the sign extension of the bottom 52 bits
and thus always 0x000 or 0xFFF (denoted as `sss`).

    000x xxxx xxxx xxxz   z = bbbb bbb0  : 52-bit positive pointer (always aligned to 2 bytes!)
    000x xxxx xxxx xxxz   z = bbbb bbb1  : 51-bit positive value
    001x xxxx xxxx xxxz   z = bbbb bbbb  : positive double: d + (0x001 << 52)
    ...
    800x xxxx xxxx xxxz   z = bbbb bbbb  : negative double: d 
    ... 
    FFFx xxxx xxxx xxxz   z = bbbb bbb0  : 52-bit negative pointer (always aligned to 2 bytes!)
    FFFx xxxx xxxx xxxz   z = bbbb bbb1  : 51-bit negative value

We can encode most doubles such that the top 12-bits are
between 0x001 and 0xFFE. The ranges of IEEE double values are:
    positive doubles        : 0000 0000 0000 0000 - 7FEF FFFF FFFF FFFF
    positive infinity       : 7FF0 0000 0000 0000
    positive NaN            : 7FF0 0000 0000 0001 - 7FFF FFFF FFFF FFFF
    negative doubles        : 8000 0000 0000 0000 - FFEF FFFF FFFF FFFF
    negative infinity       : FFF0 0000 0000 0000
    negative NaN            : FFF0 0000 0000 0001 - FFFF FFFF FFFF FFFF

  Now, if a double is:
  - positive: we add (0x001 << 52), such that the range of positive doubles is boxed between
              0010 0000 0000 0000 and 7FFF FFFF FFFF FFFF
  - negative: leave it as is, so the negative doubles are boxed between
              8000 0000 0000 0000 and FFEF FFFF FFFF FFFF
  - special : either infinity or NaN. We extend the sign over the exponent bits (since these are always 0x7FF),
              and merge the bit 0 with bit 1 to ensure a NaN payload is never unboxed as 0. 
              We set the bottom bit to 1 to encode as a value.
              On unboxing, we extend bit 1 to bit 0, which means we may lose up to 1 bit of the NaN payload.
----------------------------------------------------------------*/

#define KK_USE_NAN_BOX   (0)                  // strategy A(1) by default
//#define KK_USE_NAN_BOX   (KK_INTPTR_SIZE==8)  // strategy B is only possible on 64-bit platforms
//#define KK_BOX_DOUBLE_IF_NEG (1)              // strategy A2

// Forward declarations
static inline bool         kk_box_is_ptr(kk_box_t b);
static inline kk_block_t*  kk_ptr_unbox(kk_box_t b);
static inline kk_box_t     kk_ptr_box(const kk_block_t* p);
static inline kk_intx_t    kk_intx_unbox(kk_box_t v);
static inline kk_box_t     kk_intx_box(kk_intx_t i);

// Use a boxed representation as an intptr
static inline kk_box_t _kk_box_new(uintptr_t u) {
  kk_box_t b; 
  b.box = u;
  return b;
}

// Are two boxed representations equal?
static inline bool kk_box_eq(kk_box_t b1, kk_box_t b2) {
  return (b1.box == b2.box);
}

// We cannot store NULL as a pointer (`kk_ptr_t`); use `box_null` instead
#define kk_box_null       (_kk_box_new(~KUP(0)))  // -1 value

// null initializer
#define kk_box_null_init  {~KUP(0)}

// the _fast versions can apply if you are sure it is not a double
static inline bool _kk_box_is_ptr_fast(kk_box_t b) {
  return ((b.box&1)==0);
}

static inline bool _kk_box_is_value_fast(kk_box_t b) {
  return ((b.box&1)!=0);
}

static inline bool kk_box_is_null(kk_box_t b) {
  return (b.box == kk_box_null.box);
}

static inline bool kk_box_is_any(kk_box_t b);

#define KK_MAX_BOXED_INT  ((intptr_t)INTPTR_MAX >> (KK_INTPTR_BITS - KK_BOXED_VALUE_BITS))
#define KK_MIN_BOXED_INT  (- KK_MAX_BOXED_INT - 1)

#define KK_MAX_BOXED_UINT ((uintptr_t)UINTPTR_MAX >> (KK_INTPTR_BITS - KK_BOXED_VALUE_BITS))
#define KK_MIN_BOXED_UINT (0)


#if !(KK_USE_NAN_BOX)
double   kk_double_unbox_heap(kk_box_t b, kk_context_t* ctx);
kk_box_t kk_double_box_heap(double d, kk_context_t* ctx);
#endif

#if (KK_INTPTR_SIZE==8)
kk_box_t kk_double_box(double d, kk_context_t* ctx);
double   kk_double_unbox(kk_box_t b, kk_context_t* ctx);
#endif

//---------------------------------------------------------------------
// 64-bit
//---------------------------------------------------------------------
#if (KK_INTPTR_SIZE==8) && !KK_USE_NAN_BOX

#define KK_BOXED_VALUE_BITS  (63)

static inline bool kk_box_is_ptr(kk_box_t b) {
  return _kk_box_is_ptr_fast(b);
}

static inline bool kk_box_is_value(kk_box_t b) {
  return _kk_box_is_value_fast(b);
}

static inline int32_t kk_int32_unbox(kk_box_t v, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  kk_intx_t i = kk_intx_unbox(v);
  kk_assert_internal((i >= INT32_MIN && i <= INT32_MAX) || kk_box_is_any(v));
  return (int32_t)(i);
}

static inline kk_box_t kk_int32_box(int32_t i, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  return kk_intx_box(i);
}


//--------------------------------------------------------------
// 64 bit, NaN boxing
//--------------------------------------------------------------
#elif (KK_INTPTR_SIZE==8) && KK_USE_NAN_BOX 

#define KK_BOXED_VALUE_BITS  (51)

static inline bool _kk_double_is_normal(kk_box_t b) {
  // test if top 12 bits are not 0xFFF or 0x000
  intptr_t i = kk_sar((intptr_t)b.box, 21);  // arithmetic shift right until lowest of the 12 bits (at bit 52), is at bit 31.
  return (i != (int32_t)i);               // check if sign extension is not equal
}

static inline bool kk_box_is_ptr(kk_box_t b) {
  return (_kk_box_is_ptr_fast(b) && kk_likely(!_kk_double_is_normal(b)));
}

static inline bool kk_box_is_value(kk_box_t b) {
  return (_kk_box_is_value_fast(b) && kk_likely(!_kk_double_is_normal(b)));
}

static inline bool kk__is_double_special(kk_box_t b) {
  return (_kk_box_is_value_fast(b) && kk_likely(!_kk_double_is_normal(b)));
}

static inline bool _is_double(kk_box_t b) {
  return (_kk_double_is_normal(b) || _kk_box_is_value_fast(b));  // first test for double_normal!
}

static inline int32_t kk_int32_unbox(kk_box_t v, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  kk_intx_t i = kk_intx_unbox(v);
  kk_assert_internal((i >= INT32_MIN && i <= INT32_MAX) || kk_box_is_any(v));
  return (int32_t)(i);
}

static inline kk_box_t kk_int32_box(int32_t i, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  return kk_intx_box(i);
}

//--------------------------------------------------------------
// 32 bit
//--------------------------------------------------------------
#elif KK_INTPTR_SIZE==4

#define KK_BOXED_VALUE_BITS      (31)

static inline bool kk_box_is_ptr(kk_box_t b) {
  return _kk_box_is_ptr_fast(b);
}
static inline bool kk_box_is_value(kk_box_t b) {
  return _kk_box_is_value_fast(b);
}
static inline bool _kk_double_is_normal(kk_box_t v) {
  return (kk_box_is_ptr(v) && kk_block_tag(kk_ptr_unbox(v)) == KK_TAG_DOUBLE);
}
static inline bool _is_double(kk_box_t v) {
  return _kk_double_is_normal(v);
}

static inline double kk_double_unbox(kk_box_t b, kk_context_t* ctx) {
  return kk_double_unbox_heap(b,ctx);
}

static inline kk_box_t kk_double_box(double d, kk_context_t* ctx) {
  return kk_double_box_heap(d, ctx);
}

typedef struct kk_boxed_int32_s {
  kk_block_t  _block;
  int32_t  value;
} *boxed_int32_t;

static inline int32_t kk_int32_unbox(kk_box_t v, kk_context_t* ctx) {
  if (kk_likely(kk_box_is_value(v))) {
    kk_intx_t i = kk_intx_unbox(v);
    kk_assert_internal((i >= INT32_MIN && i <= INT32_MAX) || kk_box_is_any(v));
    return (int32_t)i;
  }
  else {
    kk_assert_internal((kk_box_is_ptr(v) && kk_block_tag(kk_ptr_unbox(v)) == KK_TAG_INT32) || kk_box_is_any(v));
    boxed_int32_t bi = kk_block_assert(boxed_int32_t, kk_ptr_unbox(v), KK_TAG_INT32);
    int32_t i = bi->value;
    if (ctx!=NULL) { kk_block_drop(&bi->_block, ctx); }
    return i;
  }
}

static inline kk_box_t kk_int32_box(int32_t i, kk_context_t* ctx) {
  if (i >= KK_MIN_BOXED_INT && i <= KK_MAX_BOXED_INT) {
    return kk_intx_box(i);
  }
  else {
    boxed_int32_t bi = kk_block_alloc_as(struct kk_boxed_int32_s, 0, KK_TAG_INT32, ctx);
    bi->value = i;
    return kk_ptr_box(&bi->_block);
  }
}

#else
# error "platform must be 32 or 64 bits."
#endif


static inline bool kk_box_is_non_null_ptr(kk_box_t v) {
  kk_assert_internal(!kk_box_is_ptr(v) || v.box != 0);   // NULL pointers are never allowed as boxed values
  return (kk_box_is_ptr(v));  //  && kk_ptr_unbox(v) != NULL
}

static inline kk_ptr_t kk_ptr_unbox(kk_box_t v) {
  kk_assert_internal(kk_box_is_ptr(v) || kk_box_is_any(v));
  kk_assert_internal(v.box != 0); // no NULL pointers allowed
  return (kk_block_t*)(v.box);
}

static inline kk_box_t kk_ptr_box(const kk_block_t* p) {
  kk_assert_internal(((uintptr_t)p & 0x03) == 0); // check alignment
  kk_assert_internal(p != NULL);                  // block should never be NULL
  kk_box_t b = { (uintptr_t)(p) };
  return b;
}

static inline kk_uintx_t kk_enum_unbox(kk_box_t b) {
  kk_assert_internal(kk_box_is_value(b) || kk_box_is_any(b));
  return kk_shr(b.box, 1);
}

static inline kk_box_t kk_enum_box(kk_uintx_t u) {
  kk_assert_internal(u <= KK_MAX_BOXED_UINT);
  kk_box_t b = { ((uintptr_t)u << 1) | 1 };
  kk_assert_internal(kk_box_is_value(b));
  return b;
}

static inline kk_intx_t kk_intx_unbox(kk_box_t v) {
  kk_assert_internal(kk_box_is_value(v) || kk_box_is_any(v));
  return (kk_sar((kk_intx_t)v.box, 1));
}

static inline kk_box_t kk_intx_box(kk_intx_t i) {
  kk_assert_internal(i >= KK_MIN_BOXED_INT && i <= KK_MAX_BOXED_INT);
  kk_box_t v = { (uintptr_t)(i << 1) | 1 };
  kk_assert_internal(kk_box_is_value(v));
  return v;
}

static inline int16_t kk_int16_unbox(kk_box_t v) {
  kk_intx_t i = kk_intx_unbox(v);
  kk_assert_internal(i >= INT16_MIN && i <= INT16_MAX);
  return (int16_t)i;
}

static inline kk_box_t kk_int16_box(int16_t i) {
  return kk_intx_box(i);
}

static inline bool kk_bool_unbox(kk_box_t v) {
  return (kk_enum_unbox(v) != 0);
}

static inline kk_box_t kk_bool_box(bool b) {
  return kk_enum_box(b ? KUX(1) : KUX(0));
}

static inline kk_box_t kk_box_dup(kk_box_t b) {
  if (kk_box_is_ptr(b)) kk_block_dup(kk_ptr_unbox(b));
  return b;
}

static inline void kk_box_drop(kk_box_t b, kk_context_t* ctx) {
  if (kk_box_is_ptr(b)) kk_block_drop(kk_ptr_unbox(b), ctx);
}


static inline kk_block_t* kk_block_unbox(kk_box_t v, kk_tag_t kk_expected_tag ) {
  KK_UNUSED_INTERNAL(kk_expected_tag);
  kk_block_t* b = kk_ptr_unbox(v);
  kk_assert_internal(kk_block_tag(b) == kk_expected_tag);
  return b;
}

static inline kk_box_t kk_block_box(kk_block_t* b) {
  return kk_ptr_box(b);
}

static inline kk_box_t kk_ptr_box_assert(kk_block_t* b, kk_tag_t tag) {
  KK_UNUSED_INTERNAL(tag);
  kk_assert_internal(kk_block_tag(b) == tag);
  return kk_ptr_box(b);
}

#define kk_basetype_unbox_as_assert(tp,b,tag)  (kk_block_assert(tp,kk_ptr_unbox(b),tag))
#define kk_basetype_unbox_as(tp,b)             ((tp)kk_ptr_unbox(b))
#define kk_basetype_box(b)                     (kk_ptr_box(&(b)->_block))

#define kk_constructor_unbox_as(tp,b,tag)      (kk_basetype_unbox_as_assert(tp,b,tag))
#define kk_constructor_box(b)                  (kk_basetype_box(&(b)->_base))

static inline kk_datatype_t kk_datatype_unbox(kk_box_t b) {
  kk_datatype_t d;
  d.singleton = b.box;
  return d;
}

static inline kk_box_t kk_datatype_box(kk_datatype_t d) {
  kk_box_t b;
  b.box = d.singleton;
  return b;
}



/* Generic boxing of value types */

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

static inline bool kk_box_is_any(kk_box_t b) {
  return (kk_box_is_ptr(b) && kk_block_has_tag(kk_ptr_unbox(b), KK_TAG_BOX_ANY));
}


// C pointers

// A function to free a raw C pointer, raw bytes, or raw string.
typedef void (kk_free_fun_t)(void* p, kk_block_t* block);
kk_decl_export void kk_free_fun_null(void* p, kk_block_t* block);
kk_decl_export void kk_free_fun(void* p, kk_block_t* block);

// "raw" types: first field is pointer to a free function, the next field a pointer to raw C data
typedef struct kk_cptr_raw_s {
  kk_block_t     _block;
  kk_free_fun_t* free;
  void* cptr;
} *kk_cptr_raw_t;


static inline kk_box_t kk_cptr_raw_box(kk_free_fun_t* freefun, void* p, kk_context_t* ctx) {
  kk_cptr_raw_t raw = kk_block_alloc_as(struct kk_cptr_raw_s, 0, KK_TAG_CPTR_RAW, ctx);
  raw->free = freefun;
  raw->cptr = p;
  return kk_ptr_box(&raw->_block);
}

static inline void* kk_cptr_raw_unbox(kk_box_t b) {
  kk_cptr_raw_t raw = kk_basetype_unbox_as_assert( kk_cptr_raw_t, b, KK_TAG_CPTR_RAW );
  return raw->cptr;
}

static inline kk_box_t kk_cptr_box(void* p, kk_context_t* ctx) {
  uintptr_t u = (uintptr_t)p;
  if (kk_likely((u&1) == 0)) {  // aligned pointer?
    // box as value
    kk_box_t b = { (u|1) };
    return b;
  }
  else {
    // allocate 
    return kk_cptr_raw_box(&kk_free_fun_null, p, ctx);
  }
}

static inline void* kk_cptr_unbox(kk_box_t b) {
  if (_kk_box_is_value_fast(b)) {
    kk_assert_internal(kk_box_is_value(b));
    return (void*)(b.box ^ 1);  // clear lowest bit
  }
  else {
    return kk_cptr_raw_unbox(b);
  }
}

// C function pointers

typedef void (*kk_cfun_ptr_t)(void);

typedef struct kk_cfunptr_s {
  kk_block_t     _block;
  kk_cfun_ptr_t  cfunptr;
} *kk_cfunptr_t;

#define kk_cfun_ptr_box(f,ctx)  kk_cfun_ptr_boxx((kk_cfun_ptr_t)f, ctx)

static inline kk_box_t kk_cfun_ptr_boxx(kk_cfun_ptr_t f, kk_context_t* ctx) {
  uintptr_t u = (uintptr_t)f;              // assume we can convert a function pointer to uintptr_t...      
  if ((u <= KK_MAX_BOXED_UINT) && sizeof(u)==sizeof(f)) {  // aligned pointer? (and sanity check if function pointer != object pointer)
    return kk_enum_box(u);
  }
  else {
    // otherwise allocate
    kk_cfunptr_t fp = kk_block_alloc_as(struct kk_cfunptr_s, 0, KK_TAG_CFUNPTR, ctx);
    fp->cfunptr = f;
    return kk_ptr_box(&fp->_block);
  }
}

static inline kk_cfun_ptr_t kk_cfun_ptr_unbox(kk_box_t b) {  // never drop; only used from function call
  if (kk_likely(_kk_box_is_value_fast(b))) {
    return (kk_cfun_ptr_t)(kk_enum_unbox(b)); 
  }
  else {
    kk_cfunptr_t fp = kk_basetype_unbox_as_assert(kk_cfunptr_t, b, KK_TAG_CFUNPTR);
    kk_cfun_ptr_t f = fp->cfunptr;
    return f;
  }
}

// size_t
typedef struct kk_box_size_s {
  kk_block_t  _block;
  size_t      value;
} *kk_box_size_t;

static inline kk_box_t kk_size_box(size_t i, kk_context_t* ctx) {
  if (i <= KK_MAX_BOXED_UINT) {
    return kk_enum_box((kk_uintx_t)i);
  }
  else {
    kk_box_size_t b = kk_block_alloc_as(struct kk_box_size_s, 0, KK_TAG_SIZE_T, ctx);
    b->value = i;
    return kk_ptr_box(&b->_block);
  }
}

static inline size_t kk_size_unbox(kk_box_t b, kk_context_t* ctx) {
  if (kk_likely(_kk_box_is_value_fast(b))) {
    return (size_t)kk_enum_unbox(b);
  }
  else {
    kk_box_size_t s = kk_basetype_unbox_as_assert(kk_box_size_t, b, KK_TAG_SIZE_T);
    size_t i = s->value;
    if (ctx != NULL) kk_basetype_free(s);
    return i;
  }
}


#endif // include guard
