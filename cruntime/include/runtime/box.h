#pragma once
#ifndef BOX_H_
#define BOX_H_

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
      numbers whose absolute value is in the range [2^-511,2^512), or if it is
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

#define USE_NAN_BOX   (0)                  // strategy A by default
// #define USE_NAN_BOX   (INTPTR_SIZE==8)  // only possible on 64-bit platforms
// #define BOX_DOUBLE_IF_NEG               // strategy A2

// Forward declarations
static inline bool      is_ptr(box_t b);
static inline block_t*  unbox_ptr(box_t b);
static inline box_t     box_ptr(const block_t* p);
static inline intx_t    unbox_int(box_t v);
static inline box_t     box_int(intx_t i);

// Use a boxed representation as an intptr
static inline box_t _new_box(uintptr_t u) {
  box_t b = { u };
  return b;
}

// Are two boxed representations equal?
static inline bool box_eq(box_t b1, box_t b2) {
  return (b1.box == b2.box);
}

// We cannot store NULL as a pointer (`ptr_t`); use `box_null` instead
#define box_null   (_new_box(~UP(0)))  // -1 value

// `box_any` is used to return when yielding (and should be accepted by any unbox operation)
#define box_any    (_new_box(1))       // 0 value

// the _fast versions can apply if you are sure it is not a double
static inline bool _is_ptr_fast(box_t b) {
  return ((b.box&1)==0);
}

static inline bool _is_value_fast(box_t b) {
  return ((b.box&1)==1);
}

static inline bool is_box_null(box_t b) {
  return (b.box == box_null.box);
}

static inline bool is_box_any(box_t b) {
  return (b.box == box_any.box);
}

#define MAX_BOXED_INT  ((intptr_t)INTPTR_MAX >> (INTPTR_BITS - BOXED_VALUE_BITS))
#define MIN_BOXED_INT  (- MAX_BOXED_INT - 1)

#define MAX_BOXED_UINT ((uintptr_t)UINTPTR_MAX >> (INTPTR_BITS - BOXED_VALUE_BITS))
#define MIN_BOXED_UINT (0)


#if !USE_NAN_BOX
double unbox_double_heap(box_t b, context_t* ctx);
box_t box_double_heap(double d, context_t* ctx);
#endif

#if (INTPTR_SIZE==8)
box_t box_double(double d, context_t* ctx);
double unbox_double(box_t b, context_t* ctx);
#endif

//---------------------------------------------------------------------
// 64-bit
//---------------------------------------------------------------------
#if (INTPTR_SIZE==8) && !USE_NAN_BOX

#define BOXED_VALUE_BITS  (63)

static inline bool is_ptr(box_t b) {
  return _is_ptr_fast(b);
}

static inline bool is_value(box_t b) {
  return _is_value_fast(b);
}

static inline int32_t unbox_int32_t(box_t v, context_t* ctx) {
  UNUSED(ctx);
  intx_t i = unbox_int(v);
  assert_internal(i >= INT32_MIN && i <= INT32_MAX);
  return (int32_t)(i);
}

static inline box_t box_int32_t(int32_t i, context_t* ctx) {
  UNUSED(ctx);
  return box_int(i);
}


//--------------------------------------------------------------
// 64 bit, NaN boxing
//--------------------------------------------------------------
#elif (INTPTR_SIZE==8) && USE_NAN_BOX 

#define BOXED_VALUE_BITS  (51)

static inline bool _is_double_normal(box_t b) {
  // test if top 12 bits are not 0xFFF or 0x000
  intptr_t i = sar((intptr_t)b.box, 21);  // arithmetic shift right until lowest of the 12 bits (at bit 52), is at bit 31.
  return (i != (int32_t)i);               // check if sign extension is not equal
}

static inline bool is_ptr(box_t b) {
  return (_is_ptr_fast(b) && likely(!_is_double_normal(b)));
}

static inline bool is_value(box_t b) {
  return (_is_value_fast(b) && likely(!_is_double_normal(b)));
}

static inline bool _is_double_special(box_t b) {
  return (_is_value_fast(b) && likely(!_is_double_normal(b)));
}

static inline bool _is_double(box_t b) {
  return (_is_double_normal(b) || _is_value_fast(b));  // first test for double_normal!
}

static inline int32_t unbox_int32_t(box_t v, context_t* ctx) {
  UNUSED(ctx);
  intx_t i = unbox_int(v);
  assert_internal(i >= INT32_MIN && i <= INT32_MAX);
  return (int32_t)(i);
}

static inline box_t box_int32_t(int32_t i, context_t* ctx) {
  UNUSED(ctx);
  return box_int(i);
}

//--------------------------------------------------------------
// 32 bit
//--------------------------------------------------------------
#elif INTPTR_SIZE==4

#define BOXED_VALUE_BITS      (31)

static inline bool is_ptr(box_t b) {
  return _is_ptr_fast(b);
}
static inline bool is_value(box_t b) {
  return _is_value_fast(b);
}
static inline bool _is_double_normal(box_t v) {
  return (is_ptr(v) && block_tag(unbox_ptr(v)) == TAG_DOUBLE);
}
static inline bool _is_double(box_t v) {
  return _is_double_normal(v);
}

static inline double unbox_double(box_t b, context_t* ctx) {
  return unbox_double_heap(b,ctx);
}

static inline box_t box_double(double d, context_t* ctx) {
  return box_double_heap(d, ctx);
}

typedef struct boxed_int32_s {
  block_t  _block;
  int32_t  value;
} *boxed_int32_t;

static inline int32_t unbox_int32_t(box_t v, context_t* ctx) {
  if (likely(is_value(v))) {
    intx_t i = unbox_int(v);
    assert_internal(i >= INT32_MIN && i <= INT32_MAX);
    return (int32_t)i;
  }
  else {
    assert_internal(is_ptr(v) && block_tag(unbox_ptr(v)) == TAG_INT32);
    boxed_int32_t bi = block_as_assert(boxed_int32_t, unbox_ptr(v), TAG_INT32);
    int32_t i = bi->value;
    if (ctx!=NULL) { drop_block(&bi->_block, ctx); }
    return i;
  }
}

static inline box_t box_int32_t(int32_t i, context_t* ctx) {
  if (i >= MIN_BOXED_INT && i <= MAX_BOXED_INT) {
    return box_int(i);
  }
  else {
    boxed_int32_t bi = block_alloc_as(struct boxed_int32_s, 0, TAG_INT32, ctx);
    bi->value = i;
    return box_ptr(&bi->_block);
  }
}

#else
# error "platform must be 32 or 64 bits."
#endif


static inline bool is_non_null_ptr(box_t v) {
  assert_internal(!is_ptr(v) || v.box != 0);   // NULL pointers are never allowed as boxed values
  return (is_ptr(v));  //  && unbox_ptr(v) != NULL
}

static inline ptr_t unbox_ptr(box_t v) {
  assert_internal(is_ptr(v) || is_box_any(v));
  assert_internal(v.box != 0); // no NULL pointers allowed
  return (block_t*)(v.box);
}

static inline box_t box_ptr(const block_t* p) {
  assert_internal(((uintptr_t)p & 0x03) == 0); // check alignment
  assert_internal(p != NULL);                  // block should never be NULL
  box_t b = { (uintptr_t)(p) };
  return b;
}

static inline uintx_t unbox_enum(box_t b) {
  assert_internal(is_value(b) || is_box_any(b));
  return shr(b.box, 1);
}

static inline box_t box_enum(uintx_t u) {
  assert_internal(u <= MAX_BOXED_UINT);
  box_t b = { ((uintptr_t)u << 1) | 1 };
  assert_internal(is_value(b));
  return b;
}

static inline intx_t unbox_int(box_t v) {
  assert_internal(is_value(v) || is_box_any(v));
  return (sar(v.box, 1));
}

static inline box_t box_int(intx_t i) {
  assert_internal(i >= MIN_BOXED_INT && i <= MAX_BOXED_INT);
  box_t v = { (uintptr_t)(i << 1) | 1 };
  assert_internal(is_value(v));
  return v;
}

static inline int16_t unbox_int16(box_t v) {
  intx_t i = unbox_int(v);
  assert_internal(i >= INT16_MIN && i <= INT16_MAX);
  return (int16_t)i;
}

static inline box_t box_int16(int16_t i) {
  return box_int(i);
}

static inline bool unbox_bool(box_t v) {
  return (unbox_enum(v) != 0);
}

static inline box_t box_bool(bool b) {
  return box_enum(b ? UX(1) : UX(0));
}

static inline block_t* unbox_block_t(box_t v, tag_t expected_tag ) {
  UNUSED_RELEASE(expected_tag);
  block_t* b = unbox_ptr(v);
  assert_internal(block_tag(b) == expected_tag);
  return b;
}

static inline box_t dup_box_t(box_t b) {
  if (is_ptr(b)) dup_block(unbox_ptr(b));
  return b;
}

static inline void drop_box_t(box_t b, context_t* ctx) {
  if (is_ptr(b)) drop_block(unbox_ptr(b), ctx);
}


static inline box_t box_block_t(block_t* b) {
  return box_ptr(b);
}

static inline box_t box_ptr_assert(block_t* b, tag_t tag) {
  UNUSED_RELEASE(tag);
  assert_internal(block_tag(b) == tag);
  return box_ptr(b);
}

#define unbox_datatype_as_assert(tp,b,tag)  (block_as_assert(tp,unbox_ptr(b),tag))
#define unbox_datatype_as(tp,b)             ((tp)unbox_ptr(b))
#define box_datatype(b)                     (box_ptr(&(b)->_block))

#define unbox_constructor_as(tp,b,tag)   (unbox_datatype_as_assert(tp,b,tag))
#define box_constructor(b)               (box_datatype(&(b)->_type))

/* Generic boxing of value types */

typedef struct boxed_value_s {
  block_t _block;
  char    data[INTPTR_SIZE]; 
} *boxed_value_t;

#define unbox_valuetype(tp,x,box,ctx) \
  do { \
    boxed_value_t p = unbox_datatype_as_assert(boxed_value_t,box,TAG_BOX); \
    x = *((tp*)(&p->data[0])); \
    if (ctx!=NULL) { drop_datatype(p,ctx); } \
  } while(0);

#define box_valuetype(tp,x,val,scan_fsize,ctx)  \
  do { \
    boxed_value_t p = block_as_assert(boxed_value_t, block_alloc(sizeof(block_t) + sizeof(tp), scan_fsize, TAG_BOX, ctx), TAG_BOX); \
    *((tp*)(&p->data[0])) = val;  \
    x = box_datatype(p); \
  } while(0);


// C pointers

// A function to free a raw C pointer, raw bytes, or raw string.
typedef void (free_fun_t)(void*);
decl_export void free_fun_null(void* p);

// "raw" types: first field is pointer to a free function, the next field a pointer to raw C data
typedef struct cptr_raw_s {
  block_t     _block;
  free_fun_t* free;
  void* cptr;
} *cptr_raw_t;


static inline box_t box_cptr_raw(free_fun_t* freefun, void* p, context_t* ctx) {
  cptr_raw_t raw = block_alloc_as(struct cptr_raw_s, 0, TAG_CPTR_RAW, ctx);
  raw->free = freefun;
  raw->cptr = p;
  return box_ptr(&raw->_block);
}

static inline void* unbox_cptr_raw(box_t b) {
  cptr_raw_t raw = unbox_datatype_as_assert( cptr_raw_t, b, TAG_CPTR_RAW );
  return raw->cptr;
}

static inline box_t box_cptr(void* p, context_t* ctx) {
  uintptr_t u = (uintptr_t)p;
  if (likely((u&1) == 0)) {  // aligned pointer?
    // box as value
    box_t b = { (u|1) };
    return b;
  }
  else {
    // allocate 
    return box_cptr_raw(&free_fun_null, p, ctx);
  }
}

static inline void* unbox_cptr(box_t b) {
  if (_is_value_fast(b)) {
    assert_internal(is_value(b));
    return (void*)(b.box ^ 1);  // clear lowest bit
  }
  else {
    return unbox_cptr_raw(b);
  }
}

// C function pointers

typedef void (*cfun_ptr_t)(void);

typedef struct cfunptr_s {
  block_t     _block;
  cfun_ptr_t  cfunptr;
} *cfunptr_t;

typedef void (*cfun_ptr_t)(void);
#define box_cfun_ptr(f,ctx)  _box_cfun_ptr((cfun_ptr_t)f, ctx)

static inline box_t _box_cfun_ptr(cfun_ptr_t f, context_t* ctx) {
  uintptr_t u = (uintptr_t)f;              // assume we can convert a function pointer to uintptr_t...      
  if ((u&1)==0 && sizeof(u)==sizeof(f)) {  // aligned pointer? (and sanity check if function pointer != object pointer)
    box_t b = { (u|1) };
    return b;
  }
  else {
    cfunptr_t fp = block_alloc_as(struct cfunptr_s, 0, TAG_CFUNPTR, ctx);
    fp->cfunptr = f;
    return box_ptr(&fp->_block);
  }
}

static inline cfun_ptr_t unbox_cfun_ptr(box_t b) {
  if (likely(_is_value_fast(b))) {
    return (cfun_ptr_t)(b.box ^ 1); // clear lowest bit
  }
  else {
    cfunptr_t fp = unbox_datatype_as_assert(cfunptr_t, b, TAG_CFUNPTR);
    return fp->cfunptr;
  }
}

#endif // include guard
