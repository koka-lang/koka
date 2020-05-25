#pragma once
#ifndef __RUNTIME_H__
#define __RUNTIME_H__

/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

#include <assert.h>
#include <stdbool.h> // bool
#include <stdint.h>  // int_t
#include <stdio.h>   // FILE*
#include <string.h>  // strlen
#include <stdlib.h>  // malloc, abort, etc.

#define REFCOUNT_LIMIT_TO_32BIT 0
#define MULTI_THREADED          1
#define TLD_IN_REG              1
#define TLD_IN_THREAD           0

/*--------------------------------------------------------------------------------------
  Platform defines
  - we assume C11 or C++11 as C compiler
  - we assume either a 32- or 64-bit platform
--------------------------------------------------------------------------------------*/
#ifdef __cplusplus
#define decl_externc    extern "C"
#else
#define decl_externc    extern
#endif

#if (__cplusplus >= 201103L) || (_MSC_VER > 1900)  // C++11
#define _constexpr      constexpr
#else
#define _constexpr
#endif

#if defined(_MSC_VER) || defined(__MINGW32__)
#if !defined(SHARED_LIB)
#define decl_export     decl_externc
#elif defined(SHARED_LIB_EXPORT)
#define decl_export     __declspec(dllexport)
#else
#define decl_export     __declspec(dllimport)
#endif
#elif defined(__GNUC__) // includes clang and icc
#define decl_export     decl_externc __attribute__((visibility("default"))) 
#else
#define decl_export     decl_externc
#endif

#if defined(__GNUC__)
#define unlikely(h)     __builtin_expect((h),0)
#define likely(h)       __builtin_expect((h),1)
#define decl_const      __attribute__((const))
#define decl_pure       __attribute__((pure))
#define noinline        __attribute__((noinline))
#define decl_thread     __thread
#elif defined(_MSC_VER)
#pragma warning(disable:4214)  // using bit field types other than int
#pragma warning(disable:4101)  // unreferenced local variable
#define unlikely(x)     (x)
#define likely(x)       (x)
#define decl_const
#define decl_pure
#define noinline        __declspec(noinline)
#define decl_thread     __declspec(thread)
#else
#define unlikely(h)     (h)
#define likely(h)       (h)
#define decl_const
#define decl_pure
#define noinline   
#define decl_thread     __thread
#endif

#ifndef UNUSED
#define UNUSED(x)  ((void)(x))
#ifdef NDEBUG
#define UNUSED_RELEASE(x)  UNUSED(x);
#else
#define UNUSED_RELEASE(x)  
#endif
#endif



// Assumptions:
// - a char/byte is 8 bits
// - (>>) on signed integers is an arithmetic right shift (i.e. sign extending)

#if INTPTR_MAX == 9223372036854775807LL
# define INTPTR_SIZE 8
#elif INTPTR_MAX == 2147483647LL
# define INTPTR_SIZE 4
#else
#error platform must be 32 or 64 bits
#endif
#define INTPTR_BITS (8*INTPTR_SIZE)


// abstract over intptr_t so we can in principle target architectures like x32 where 
// the size of a pointer is 32-bit but int's can be 64-bit. 
// An `int_t` should be the size of a boxed value and be able to contain pointers
// to the heap (so pointer could be represented as an offset to a base)
typedef intptr_t  int_t;
typedef uintptr_t uint_t;

#define INT_T_SIZE  INTPTR_SIZE
#define INT_T_BITS  INTPTS_BITS

// Distinguish unsigned shift right and signed arithmetic shift right.
static inline int_t  sar(int_t i, int_t shift)   { return (i >> shift); }
static inline uint_t shr(uint_t i, uint_t shift) { return (i >> shift); }

// Limited reference counts can be more efficient
#if INT_T_SIZE <= 4
#undef  REFCOUNT_LIMIT_TO_32BIT
#define REFCOUNT_LIMIT_TO_32BIT  1
#endif

/*--------------------------------------------------------------------------------------
  Basic datatypes
--------------------------------------------------------------------------------------*/

// A `ptr_t` is a pointer to a `block_t`. We keep it abstract to support tagged pointers or sticky refcounts in the future
typedef uint_t ptr_t;      

// Polymorpic operations work on boxed values. We use unsigned representation to avoid UB on shift operations and overflow.
typedef uint_t box_t;    

// A datatype is either a `ptr_t` or an enumeration as a boxed value. Identity with boxed values.
typedef box_t datatype_t;

// An integer is either a small int or a pointer to a bigint_t. Identity with boxed values.
typedef box_t integer_t;


// Tags for heap blocks
typedef enum tag_e {
  TAG_INVALID = 0,
  TAG_MIN = 1,
  TAG_SMALL_MAX = 15,
  TAG_MAX = 65000,
  TAG_BOX,
  TAG_REF,
  TAG_EVV,
  TAG_FUNCTION,
  TAG_BIGINT,
  TAG_STRING,
  TAG_STRING_SMALL,
  TAG_BYTES,
  TAG_INT64,
  TAG_CPTR,          // full void*
#if INT_T_SIZE < 8
  TAG_DOUBLE,
  TAG_INT32,
#endif
  TAG_LAST
} tag_t;

// Every heap block starts with a header with a reference count and tag.
typedef union header_s {
  uint64_t as_uint64;
#if !defined(ARCH_BIG_ENDIAN)
  struct {
#if INT_T_SIZE==8
    uint64_t refcount : 38;
#else
    uint32_t refcount;
    uint_t _unused_refcount : 6;
#endif    
    uint_t _reserved : 1;
    uint_t thread_shared : 1;      // true if shared among threads (so release/acquire use locked increment/decrement)
    uint_t scan_fsize : 8;         // number of fields that should be scanned when releasing (`scan_fsize <= 0xFF`, if 0xFF, the full scan size is the first field)
    uint_t tag : 16;               // tag
  } h;

  // the following overlays are defined for efficient code generation of reference counting.
  // note: we use a refcount of 0 to denote a single unique reference as that can lead to better
  // code generation. An object is freed when the zero underflows.
  struct {
    uint32_t  lo;                  // used for more efficient decrementing (and detecting a reference count 0)
    uint32_t  hi : 6;
    uint32_t  _unused_hi : 26;
  } rc32;
  struct {
#if (INT_T_SIZE==4)
    uint32_t  extended;            // used for efficient incrementing
    uint32_t  _unused_extended;
#else
    uint64_t  extended;            // used for efficient incrementing
#endif
  } rc;
#else
# error "todo: define big-endian order as well"
#endif
} header_t;

#define SCAN_FSIZE_MAX (255)
#define HEADER(scan_fsize,tag)         {(((uint64_t)tag << 48) | (uint64_t)((uint8_t)scan_fsize) << 40)}           // start with refcount of 0
#define HEADER_STATIC(scan_fsize,tag)  {(((uint64_t)tag << 48) | (uint64_t)((uint8_t)scan_fsize) << 40 | 0xFF00)}  // start with recognisable refcount (anything > 1 is ok)

// A heap block is a header followed by `scan_fsize` boxed fields and further raw bytes
typedef struct block_s {
  header_t header;
  //box_t  large_scan_fsize; // if `scan_fsize == 0xFF` there is a first field with the full scan size
  //box_t  fields[];         // `scan_fsize` boxed fields. Note: flexible array [] is not in C++, and [0] is not in either C or C++ :-(
} block_t;


// boxed forward declarations
static inline bool      is_ptr(box_t b);
static inline bool      is_ptr_fast(box_t b);   // if it is either a pointer, int, or enum, but not a double
static inline bool      is_enum_fast(box_t b);  // if it is either a pointer, int, or enum, but not a double
static inline ptr_t     unbox_ptr(box_t b);
static inline box_t     box_ptr(ptr_t p);
static inline int_t     unbox_int(box_t v);
static inline box_t     box_int(int_t i);
static inline uint_t    unbox_enum(box_t v);
static inline box_t     box_enum(uint_t u);

#define assert_internal assert

/*--------------------------------------------------------------------------------------
  Blocks 
--------------------------------------------------------------------------------------*/

static inline decl_const block_t* ptr_as_block(ptr_t p) {
  return (block_t*)p;
}

static inline decl_const ptr_t block_as_ptr(const block_t* b) {
  return (ptr_t)b;
}

static inline decl_const tag_t block_tag(block_t* b) {
  return (tag_t)(b->header.h.tag);
}

static inline decl_const void* block_data(block_t* b) {
  assert_internal(b->header.h.scan_fsize != SCAN_FSIZE_MAX);
  return ((uint8_t*)b + sizeof(block_t)); // note: assumes the data fields align just after the `block_t` fields.
}

static inline decl_const box_t* block_fields(block_t* b) {
  return (box_t*)block_data(b);
}

static inline decl_pure size_t block_scan_fsize(block_t* b) {
  size_t sfsize = b->header.h.scan_fsize;
  return (likely(sfsize != SCAN_FSIZE_MAX) ? sfsize : (size_t)unbox_int(*((box_t*)((uint8_t*)b + sizeof(header_t)))));
}

static inline decl_const box_t* block_field_at(block_t* b, size_t i) {
  assert_internal(i <= block_scan_fsize(b));  // allowed to point one element beyond
  return &block_fields(b)[i];
}

static inline decl_const box_t block_field(block_t* b, size_t i) {
  assert_internal(i < block_scan_fsize(b));
  return *block_field_at(b, i);
}

static inline decl_pure void* block_data_large(block_t* b) {
  const size_t extra = (likely(b->header.h.scan_fsize != SCAN_FSIZE_MAX) ? 0 : sizeof(box_t));
  return ((uint8_t*)b + sizeof(block_t) + extra);  // note: assumes the data aligns just after the block
}

static inline decl_pure box_t* block_fields_large(block_t* b) {
  return (box_t*)block_data_large(b);
}

static inline decl_pure box_t* block_field_at_large(block_t* b, size_t i) {
  assert_internal(i <= block_scan_fsize(b));  // allowed to point one element beyond
  return &block_fields_large(b)[i];
}

static inline decl_pure box_t block_field_large(block_t* b, size_t i) {
  assert_internal(i < block_scan_fsize(b));  
  return *block_field_at_large(b,i);
}

static inline decl_const block_t* block_from_data(const void* data) {
  block_t* b = (block_t*)((uint8_t*)data - sizeof(block_t));
  assert_internal(b->header.h.scan_fsize != SCAN_FSIZE_MAX); // is a small block?
  return b;
}

static inline block_t* block_from_data_large(const void* data) {
  block_t* b = (block_t*)((uint8_t*)data - sizeof(block_t));
  assert_internal(b->header.h.scan_fsize == SCAN_FSIZE_MAX); // is not a small block?
  return b;
}


/*--------------------------------------------------------------------------------------
  Ptr
--------------------------------------------------------------------------------------*/

extern ptr_t ptr_null;

static inline decl_const tag_t ptr_tag(ptr_t p) {
  return block_tag(ptr_as_block(p));
}

static inline decl_const box_t* ptr_fields(ptr_t p) {
  return block_fields(ptr_as_block(p));
}

static inline decl_const box_t* ptr_field_at(ptr_t p, size_t i) {
  return block_field_at(ptr_as_block(p),i);
}

static inline decl_const box_t ptr_field(ptr_t p, size_t i) {
  return block_field(ptr_as_block(p), i);
}

static inline decl_pure uint_t ptr_refcount(ptr_t p) {
  return ptr_as_block(p)->header.h.refcount;
}

static inline decl_const void* ptr_data(ptr_t p) {
  return block_data(ptr_as_block(p));
}

static inline decl_pure void* ptr_data_assert(ptr_t p, tag_t expected_tag) {
  UNUSED_RELEASE(expected_tag);
  assert_internal(ptr_tag(p) == expected_tag);
  return ptr_data(p);
}

#define ptr_data_as(tp,p)              (tp*)ptr_data(p)
#define ptr_data_as_assert(tp,p,tag)   (tp*)ptr_data_assert(p,tag)

static inline decl_const ptr_t ptr_from_data(const void* data) {
  return block_as_ptr(block_from_data(data));
}

static inline decl_const ptr_t ptr_from_data_large(void* data) {
  return block_as_ptr(block_from_data_large(data));
}


static inline decl_pure bool ptr_is_unique(ptr_t p) {
  block_t* b = ptr_as_block(p);
#if REFCOUNT_LIMIT_TO_32BIT
  return (likely(b->header.rc32.lo == 0));
#else
  return (likely(b->header.rc32.lo == 0) && b->header.rc32.hi == 0);
#endif
}


/*--------------------------------------------------------------------------------------
  The thread local context as `context_t`
  This is passed by the code generator as an argument to every function so it can
  be (usually) accessed efficiently through a register.
--------------------------------------------------------------------------------------*/
typedef void*      heap_t;
typedef datatype_t function_t;

#define YIELD_CONT_MAX (8)

typedef struct yield_s {
  bool       yielding;        // are we yielding to a handler?
  bool       final;           // is this a final yield? (e.g. exception)
  int32_t    marker;          // marker of the handler to yield to
  function_t clause;          // the operation clause to execute when the handler is found
  int_t      conts_count;     // number of continuations in `conts`
  function_t conts[YIELD_CONT_MAX];  // fixed array of continuations. The final continuation `k` is
                              // composed as `fN ○ ... ○ f2 ○ f1` if `conts = { f1, f2, ..., fN }` 
                              // if the array becomes full, a fresh array is allocated and the first
                              // entry points to its composition.
} yield_t;

typedef struct context_s {
  heap_t     heap;             // the (thread-local) heap to allocate in; todo: put in a register?
  datatype_t evv;              // the current evidence vector for effect handling
  yield_t    yield;            // inlined yield structure (for efficiency)
  int32_t    marker_unique;    // unique marker generation
  block_t*   delayed_free;     // list of blocks that still need to be freed
  integer_t  unique;           // thread local unique number generation
  uint_t     thread_id;        // unique thread id
} context_t;

// Get the current (thread local) runtime context (should always equal the `_ctx` parameter)
decl_export context_t* runtime_context(void); 

// The current context is passed as a _ctx parameter in the generated code
#define current_context()   _ctx

// Is the execution yielding?
static inline bool yielding(context_t* ctx) {
  return (ctx->yield.yielding);
};

// Get a thread local marker unique number.
static inline int32_t marker_unique(context_t* ctx) {
  return ctx->marker_unique++;
};



/*--------------------------------------------------------------------------------------
  Allocation
--------------------------------------------------------------------------------------*/

static inline void* runtime_malloc(size_t sz, context_t* ctx) {
  UNUSED(ctx);
  return malloc(sz);
}

static inline void* runtime_zalloc(size_t sz, context_t* ctx) {
  UNUSED(ctx);
  return calloc(1, sz);
}

static inline void runtime_free(void* p) {
  free(p);
}

static inline void* runtime_realloc(void* p, size_t sz, context_t* ctx) {
  UNUSED(ctx);
  return realloc(p, sz);
}

decl_export void block_free(block_t* b, context_t* ctx);

static inline void ptr_free(ptr_t p, context_t* ctx) {
  block_free(ptr_as_block(p),ctx);
}

static inline void block_init(block_t* b, size_t size, size_t scan_fsize, tag_t tag) {
  UNUSED(size);
  header_t header = { 0 };
  header.h.tag = (uint16_t)tag;
  if (likely(scan_fsize < SCAN_FSIZE_MAX)) {
    header.h.scan_fsize = (uint8_t)scan_fsize;
  }
  else {
    header.h.scan_fsize = SCAN_FSIZE_MAX;
    *block_field_at(b,0) = box_enum(scan_fsize);
  }
  b->header = header;
}

static inline block_t* block_alloc(size_t size, size_t scan_fsize, tag_t tag, context_t* ctx) {
  size_t extra = 0;
  if (unlikely(scan_fsize >= SCAN_FSIZE_MAX)) {
    extra = sizeof(box_t);
    scan_fsize++; // scan the scan field itself
  }
  block_t* b = (block_t*)runtime_malloc(size + sizeof(block_t) + extra, ctx);
  block_init(b, size, scan_fsize, tag);
  return b;
}


static inline ptr_t ptr_alloc(size_t size, size_t scan_fsize, tag_t tag, context_t* ctx) {
  block_t* b = block_alloc(size, scan_fsize, tag, ctx);
  return block_as_ptr(b);
}

static inline ptr_t ptr_realloc(ptr_t p, size_t size, context_t* ctx) {
  assert_internal(ptr_is_unique(p));
  block_t* b = ptr_as_block(p);
  const size_t bsize = sizeof(block_t) + (likely(b->header.h.scan_fsize != SCAN_FSIZE_MAX) ? 0 : sizeof(box_t));
  b = (block_t*)runtime_realloc(ptr_as_block(p), size + bsize, ctx);
  return block_as_ptr(b);
}

#define ptr_alloc_data_as(tp,scan_fsize,tag,ctx)  ptr_data_as(tp,ptr_alloc(sizeof(tp),scan_fsize,tag,ctx))


/*--------------------------------------------------------------------------------------
  Reference counting
--------------------------------------------------------------------------------------*/

decl_export void ptr_check_free(ptr_t p, context_t* ctx);


static inline ptr_t ptr_dup(ptr_t p) {
#if REFCOUNT_LIMIT_TO_32BIT
  // with a 32-bit reference count on a 64-bit system, we need a (8*2^32 = 32GiB array to create that many
  // references to a single object. That is often a reasonable restriction and more efficient.
  ptr_as_block(p)->header.rc32.lo++;
#else
  // optimize: increment the full 64-bit hoping to never overflow the 38 refcount bits
  // this is reasonable as it would take a (8*2^38 = 2TiB array to create that many references to a single object)
  ptr_as_block(p)->header.rc.extended++;   
#endif
  return p;
}

static inline void ptr_drop(ptr_t p, context_t* ctx) {
  // optimize: always decrement just the 32 bits; 
  // on larger refcounts check afterwards if the hi-bits were 0. Since we use 0 for a unique reference we can 
  // efficiently check if the block can be freed by comparing to 0.
  uint32_t count = (ptr_as_block(p)->header.rc32.lo)--;
#if REFCOUNT_LIMIT_TO_32BIT
  if (count==0) ptr_free(p,ctx);
#else
  if (count==0) ptr_check_free(p,ctx);
#endif
}

static inline box_t boxed_dup(box_t b) {
  if (is_ptr(b)) ptr_dup(unbox_ptr(b));
  return b;
}

static inline void boxed_drop(box_t b, context_t* ctx) {
  if (is_ptr(b)) ptr_drop(unbox_ptr(b), ctx);
}




/*--------------------------------------------------------------------------------------
  Datatype
--------------------------------------------------------------------------------------*/

#define datatype_null   ((datatype_t)(0x02))   // enum 0

static inline decl_const datatype_t ptr_as_datatype(ptr_t p) {
  return box_ptr(p);
}

static inline decl_const ptr_t datatype_as_ptr(datatype_t d) {
  return unbox_ptr(d);
}

static inline decl_const bool datatype_is_ptr(datatype_t d) {
  return (is_ptr_fast(d));
}

static inline decl_const datatype_t datatype_from_enum(uint_t tag) {
  return box_enum(tag);
}

static inline decl_const uint_t datatype_as_enum(datatype_t d) {
  return unbox_enum(d);
}

static inline decl_const bool datatype_is_enum(datatype_t d) {
  return (is_enum_fast(d));
}

static inline decl_pure tag_t datatype_tag(datatype_t d) {
  return (datatype_is_ptr(d) ? ptr_tag(datatype_as_ptr(d)) : (tag_t)datatype_as_enum(d));
}

static inline decl_pure tag_t datatype_tag_fast(datatype_t d) {
  assert_internal(datatype_is_ptr(d));
  return ptr_tag(datatype_as_ptr(d));
}

#define datatype_alloc_data_as(tp,scan_fsize,tag,ctx)  ((tp*)ptr_alloc_data_as(tp,scan_fsize,tag,ctx))

#define define_static_datatype(decl,name,tag) \
    static block_t _static_##name = { HEADER_STATIC(0,tag) }; \
    decl data_type_t name = (datatype_t)(&_static_name); /* should be `datatype_cptr(&_static_##name*)` but we need a constant initializer */

static inline decl_const datatype_t datatype_from_data(const void* data) {
  return ptr_as_datatype(ptr_from_data(data));
}

static inline decl_const void* datatype_data(datatype_t d) {
  return ptr_data(datatype_as_ptr(d));
}

static inline decl_pure void* datatype_data_assert(datatype_t d, tag_t expected_tag) {
  return ptr_data_assert(datatype_as_ptr(d), expected_tag);
}

#define datatype_data_as(tp,d)             ((tp*)datatype_data(d))
#define datatype_data_as_assert(tp,d,tag)  ((tp*)datatype_data_assert(d,tag))


static inline void datatype_drop(datatype_t d, context_t* ctx) {
  if (datatype_is_ptr(d)) ptr_drop(datatype_as_ptr(d),ctx);
}

static inline datatype_t datatype_dup(datatype_t d) {
  if (datatype_is_ptr(d)) ptr_dup(datatype_as_ptr(d));
  return d;
}


// Tag for value types is always a boxed enum
typedef box_t value_tag_t;

// Use inlined #define to enable constant initializer expression
/*
static inline value_tag_t value_tag(uint_t tag) {
  return box_enum(tag);
}
*/
#define value_tag(tag) (((value_tag_t)tag << 2) | 0x03)



/*--------------------------------------------------------------------------------------
  Reference counting scrutinee of a match where we try to avoid
  reference increments on fields of the match, and reuse memory in-place for
  deallocated scrutinees.
--------------------------------------------------------------------------------------*/
typedef datatype_t orphan_t; 

static inline decl_const orphan_t orphan_null(void) {
  return datatype_from_enum(0);
}

static inline orphan_t orphan_ptr(ptr_t p) {
  ptr_as_block(p)->header.as_uint64 = 0; // set tag to zero, unique, with zero scan size (so decref is valid on it)
  return ptr_as_datatype(p);
}

static inline orphan_t ptr_release0(ptr_t p, context_t* ctx) {
  if (ptr_is_unique(p)) {
    return orphan_ptr(p);
  }
  else {
    ptr_drop(p,ctx);
    return orphan_null();
  }
}

static inline orphan_t ptr_release1(ptr_t p, box_t unused_field1, context_t* ctx ) {
  if (ptr_is_unique(p)) {
    boxed_drop(unused_field1,ctx);
    return orphan_ptr(p);
  }
  else {
    ptr_drop(p,ctx);
    return orphan_null();
  }
}

static inline void ptr_no_reuse(orphan_t o) {
  if (datatype_is_ptr(o)) {
    runtime_free(ptr_as_block(datatype_as_ptr(o)));
  };
}

static inline ptr_t ptr_alloc_reuse(orphan_t o, size_t size, size_t scan_fsize, tag_t tag, context_t* ctx) {
  // TODO: check usable size p >= size
  block_t* b;
  if (datatype_is_ptr(o)) {
    assert_internal(ptr_is_unique(datatype_as_ptr(o)));
    b = ptr_as_block(datatype_as_ptr(o));
  }
  else {
    b = (block_t*)runtime_malloc(size,ctx);
  }
  block_init(b, size, scan_fsize, tag);
  return block_as_ptr(b);
}



/*----------------------------------------------------------------------
  Further includes
----------------------------------------------------------------------*/

#include "runtime/box.h"
#include "runtime/integer.h"
#include "runtime/bitcount.h"


/*----------------------------------------------------------------------
  TLD operations
----------------------------------------------------------------------*/

// Get a thread local unique number.
static inline integer_t gen_unique(context_t* ctx) {
  integer_t u = ctx->unique;
  ctx->unique = integer_inc(integer_dup(u),ctx);
  return u;
};




/*--------------------------------------------------------------------------------------
  Functions
--------------------------------------------------------------------------------------*/

// A function has as its first field a pointer to a (C) function that takes the 
// function itself as a first argument. The following fields are the free variables.
struct function_s {
  box_t   fun;     // boxed cptr
  // followed by free variables
};

#define function_null   (box_null)

static inline function_t function_from_data(const struct function_s* data) {
  return datatype_from_data(data);
}

static inline struct function_s* function_data(function_t f) {
  return datatype_data_as_assert(struct function_s, f, TAG_FUNCTION);
}

static inline ptr_t function_alloc(size_t size, size_t scan_fsize, context_t* ctx) {
  return ptr_alloc(size, scan_fsize, TAG_FUNCTION, ctx);
}

#define function_alloc_as(tp,scan_fsize,ctx)    ptr_data_as(tp,function_alloc(sizeof(tp),scan_fsize,ctx))
#define function_data_as(tp,f)                  ((tp*)function_data(f))

#define function_call(restp,argtps,f,args)  ((restp(*)argtps)(unbox_cptr(function_data(f)->fun)))args

#define define_static_function(name,cfun) \
  static struct { block_t block; box_t fun; } _static_##name = { { HEADER_STATIC(0,TAG_FUNCTION) }, (box_t)(&cfun) }; /* note: should be box_cptr(&cfun) but we need a constant expression */ \
  function_t name = (function_t)(&_static_##name);   // note: should be `block_as_datatype(&_static_##name.block)` but we need as constant expression here


extern function_t function_id;

static inline function_t unbox_function_t(box_t v) {
  return unbox_datatype_assert(v, TAG_FUNCTION);
}

static inline box_t box_function_t(function_t d) {
  return box_datatype(d);
}

static inline bool function_is_unique(function_t f) {
  return ptr_is_unique(datatype_as_ptr(f));
}

static inline void function_drop(function_t f, context_t* ctx) {
  datatype_drop(f, ctx);
}

static inline function_t function_dup(function_t f) {
  return datatype_dup(f);
}


/*--------------------------------------------------------------------------------------
  Strings
--------------------------------------------------------------------------------------*/

// A string is modified UTF-8 (with encoded zeros) ending with a '0' character.
struct string_s {
  size_t   length;
  char     str[1];
};

#define SMALL_STRING_MAX_LEN  (7)

struct small_string_s {
  union {
    uint64_t  value;
    char      str[SMALL_STRING_MAX_LEN+1];
  } u;
};

typedef datatype_t string_t;

#define define_string_literal(decl,name,len,chars) \
  static struct { block_t block; size_t length; char str[len+1]; } _static_##name = { { HEADER_STATIC(0,TAG_STRING) }, len, chars }; \
  decl string_t name = (string_t)(&_static_##name);   // note: should be `block_as_datatype(&_static_##name.block)` but we need as constant expression here


static inline string_t unbox_string_t(box_t v) {
  string_t s = unbox_datatype(v);
  assert_internal(datatype_is_ptr(s) && (datatype_tag(s) == TAG_STRING || datatype_tag(s) == TAG_STRING_SMALL));
  return s;
}

static inline box_t box_string_t(string_t s) {
  return box_datatype(s);
}


/*--------------------------------------------------------------------------------------
  Strings operations
--------------------------------------------------------------------------------------*/

static inline string_t string_alloc_len(size_t len, const char* s, context_t* ctx) {
  if (len <= SMALL_STRING_MAX_LEN) {
    struct small_string_s* str = ptr_alloc_data_as(struct small_string_s, 0, TAG_STRING_SMALL, ctx);
    str->u.value = 0;
    if (s != NULL) {
      for (size_t i = 0; i <= len; i++) {
        str->u.str[i] = s[i];
      }
    }
    return datatype_from_data(str);
  }
  else {
    struct string_s* str = ptr_data_as(struct string_s, ptr_alloc(sizeof(struct string_s) - 1 /* char str[1] */ + len + 1 /* 0 terminator */, 0, TAG_STRING, ctx));
    str->length = len;
    if (s != 0) {
      memcpy(&str->str[0], s, len);
    }
    str->str[len] = 0;
    return datatype_from_data(str);
  }
}

static inline string_t string_alloc_buf(size_t len, context_t* ctx) {
  return string_alloc_len(len, NULL,ctx);
}

static inline string_t string_alloc(const char* s, context_t* ctx) {
  return (s==NULL ? string_alloc_len(0, "", ctx) : string_alloc_len(strlen(s), s, ctx));
}

static inline char* string_buf(string_t str) {
  if (datatype_tag(str) == TAG_STRING_SMALL) {
    return datatype_data_as(struct small_string_s, str)->u.str;
  }
  else {
    return datatype_data_as_assert(struct string_s, str, TAG_STRING)->str;
  }
}

static inline size_t decl_pure small_string_len(const struct small_string_s* s) {
  uint64_t v = s->u.value;  // read string a 64-bit value 
  // use bitcount to find the terminating zero byte
#if defined(ARCH_BIG_ENDIAN)
  return ((size_t)8 - (bits_ctz64(v)/8));
#else
  return ((size_t)8 - (bits_clz64(v)/8));
#endif
}

static inline size_t decl_pure string_len(string_t str) {
  if (datatype_tag(str) == TAG_STRING_SMALL) {
    return small_string_len(datatype_data_as(struct small_string_s, str));
  }
  else {
    return datatype_data_as_assert(struct string_s, str, TAG_STRING)->length;
  }
}

static inline void string_drop(string_t str, context_t* ctx) {
  datatype_drop(str,ctx);
}

static inline string_t string_dup(string_t str) {
  return datatype_dup(str);
}

decl_export int_t string_cmp_borrow(string_t str1, string_t str2);
decl_export int_t string_cmp(string_t str1, string_t str2, context_t* ctx);
decl_export int_t string_icmp_borrow(string_t str1, string_t str2);
decl_export int_t string_icmp(string_t str1, string_t str2, context_t* ctx);

/*--------------------------------------------------------------------------------------
  References
--------------------------------------------------------------------------------------*/
typedef datatype_t ref_t;
struct ref_s {
  box_t value;
};

static inline ref_t ref_alloc(box_t value, context_t* ctx) {
  struct ref_s* r = datatype_alloc_data_as(struct ref_s, 1, TAG_REF, ctx);
  r->value = value;
  return datatype_from_data(r);
}

static inline box_t ref_get(ref_t b) {
  return boxed_dup(datatype_data_as_assert(struct ref_s, b, TAG_REF)->value);
}

static inline void ref_set(ref_t r, box_t value, context_t* ctx) {
  box_t* b = &datatype_data_as_assert(struct ref_s, r, TAG_REF)->value; 
  boxed_drop(*b, ctx);
  *b = value;
}

decl_export void fatal_error(const char* msg);

static inline void unsupported_external(const char* msg) {
  fatal_error(msg);
}

/*--------------------------------------------------------------------------------------
  Unit
--------------------------------------------------------------------------------------*/
typedef enum unit_e {
  unit = 0
} unit_t;

static inline box_t box_unit_t(unit_t u) {
  return box_enum(u);
}

static inline box_t unbox_unit_t(box_t u) {
  return unbox_enum(u);
}

#endif // include guard
