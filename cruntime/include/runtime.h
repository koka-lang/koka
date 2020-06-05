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
#include <math.h>    

#define REFCOUNT_LIMIT_TO_32BIT 0
#define MULTI_THREADED          1
#define ARCH_LITTLE_ENDIAN      1
//#define ARCH_BIG_ENDIAN       1

/*--------------------------------------------------------------------------------------
  Platform:
  - we assume C99 as C compiler (syntax and library), with possible C11 extensions for threads and atomics.
  - we assume either a 32- or 64-bit platform
  - we assume the compiler can do a great job on small static inline definitions (and avoid #define).
  - we assume the compiler will inline small structs (like `struct box_s{ uintptr_t u; }`) without 
    overhead (e.g. pass in a register).
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
//#pragma warning(disable:4214)  // using bit field types other than int
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

#define assert_internal assert

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

// Defining constants of a specific size
#if LONG_MAX == INT64_MAX
# define LONG_SIZE 8
# define I32(i)  (i)
# define I64(i)  (i##L)
# define U32(i)  (i##U)
# define U64(i)  (i##UL)
#elif LONG_MAX == INT32_MAX
# define LONG_SIZE 4
# define I32(i)  (i##L)
# define I64(i)  (i##LL)
# define U32(i)  (i##UL)
# define U64(i)  (i##ULL)
#else
#error size of a `long` must be 32 or 64 bits
#endif

// Define size of intptr_t
#if INTPTR_MAX == INT64_MAX           // 9223372036854775807LL
# define INTPTR_SIZE 8
# define IP(i)  I64(i)
# define UP(i)  U64(i)
#elif INTPTR_MAX == INT32_MAX         // 2147483647LL
# define INTPTR_SIZE 4
# define IP(i)  I32(i)
# define UP(i)  U32(i)
#else
#error platform must be 32 or 64 bits
#endif
#define INTPTR_BITS (8*INTPTR_SIZE)
#define INTPTR_ALIGNUP(x)  ((((x)+INTPTR_SIZE-1)/INTPTR_SIZE)*INTPTR_SIZE)

// Define size of size_t
#if SIZE_MAX == UINT64_MAX           // 18446744073709551615LL
# define SIZE_SIZE 8
# define IZ(i)  I64(i)
# define UZ(i)  U64(i)
#elif SIZE_MAX == UINT32_MAX         // 4294967295LL
# define SIZE_SIZE 4
# define IZ(i)  I32(i)
# define UZ(i)  U32(i)
#else
#error size of a `size_t` must be 32 or 64 bits
#endif
#define SIZE_BITS (8*SIZE_SIZE)


// Abstract over the "natural machine word" as `intx_t`. This is useful when
// targeting architectures where `sizeof(void*) < sizeof(long)` (like x32), or 
// `sizeof(void*) > sizeof(size_t)` (like segmented architectures). 
// We have `sizeof(intx_t) >= sizeof(void*)` and `sizeof(intx_t) >= sizeof(long)`.
#if (LONG_MAX <= INTPTR_MAX)
typedef intptr_t    intx_t;
typedef uintptr_t   uintx_t;
#define UX(i)       (i##ULL)
#define IX(i)       (i##LL)
#define INTX_SIZE   INTPTR_SIZE
#else 
typedef long           intx_t;
typedef unsigned long  uintx_t;
#define UX(i)          (i##UL)
#define IX(i)          (i##L)
#define INTX_SIZE      LONG_SIZE
#endif
#define INTX_BITS  (8*INTX_SIZE)


// Distinguish unsigned shift right and signed arithmetic shift right.
static inline intx_t  sar(intx_t i, intx_t shift)   { return (i >> shift); }
static inline uintx_t shr(uintx_t i, uintx_t shift) { return (i >> shift); }

// Limited reference counts can be more efficient
#if PTRDIFF_MAX <= INT32_MAX
#undef  REFCOUNT_LIMIT_TO_32BIT
#define REFCOUNT_LIMIT_TO_32BIT  1
#endif

/*--------------------------------------------------------------------------------------
  Basic datatypes
--------------------------------------------------------------------------------------*/

// Tags for heap blocks
typedef enum tag_e {
  TAG_INVALID   = 0,
  TAG_MIN       = 1,
  TAG_MAX       = 65000,
  TAG_OPEN,        // open datatype, first field is a string tag
  TAG_BOX,         // boxed value type
  TAG_REF,         // mutable reference
  TAG_FUNCTION,    // function with free its free variables
  TAG_BIGINT,      // big integer (see `integer.c`)
  TAG_STRING_SMALL,// UTF8 encoded string of at most 7 bytes.
  TAG_STRING,      // UTF8 encoded string: valid (modified) UTF8 ending with a zero byte.
  TAG_BYTES,       // a vector of bytes
  TAG_VECTOR,      // a vector of (boxed) values
  TAG_INT64,       // boxed int64_t
#if INTPTR_SIZE < 8
  TAG_INT32,       // boxed int32_t
  TAG_DOUBLE,      // boxed IEEE double (64-bit)
  TAG_FLOAT,       // boxed IEEE float  (32-bit)
#endif
  // raw tags have a free function together with a `void*` to the data
  TAG_CPTR_RAW,    // full void* (must be first, see tag_is_raw())
  TAG_STRING_RAW,  // pointer to a valid UTF8 string
  TAG_BYTES_RAW,   // pointer to bytes
  TAG_LAST         
} tag_t;

static inline bool tag_is_raw(tag_t tag) {
  return (tag >= TAG_CPTR_RAW);
}

// Every heap block starts with a header with a reference count and tag.
typedef union header_s {
  uint64_t as_uint64;
#if defined(ARCH_LITTLE_ENDIAN)
  struct {
#if INTPTR_SIZE==8
    uint64_t  refcount : 38;
#else
    uint32_t  refcount;
    uint32_t  _unused_refcount : 6;
#endif    
    uint32_t  _reserved : 1;
    uint32_t  thread_shared : 1;      // true if shared among threads (so release/acquire use locked increment/decrement)
    uint32_t  scan_fsize : 8;         // number of fields that should be scanned when releasing (`scan_fsize <= 0xFF`, if 0xFF, the full scan size is the first field)
    uint32_t  tag : 16;               // tag
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
#if (INTPTR_SIZE==4)
    uint32_t  extended;            // used for efficient incrementing
    uint32_t  _unused_extended;
#else
    uint64_t  extended;            // used for efficient incrementing
#endif
  } rc;
#else
# error "define non little-endian order as well"
#endif
} header_t;

#define SCAN_FSIZE_MAX (255)
#define HEADER(scan_fsize,tag)         {(((uint64_t)tag << 48) | (uint64_t)((uint8_t)scan_fsize) << 40)}           // start with refcount of 0
#define HEADER_STATIC(scan_fsize,tag)  {(((uint64_t)tag << 48) | (uint64_t)((uint8_t)scan_fsize) << 40 | 0xFF00)}  // start with recognisable refcount (anything > 1 is ok)


// Polymorpic operations work on boxed values. (We use a struct for extra checks on accidental conversion)
// See `box.h` for definitions
typedef struct box_s {
  uintptr_t box;          // We use unsigned representation to avoid UB on shift operations and overflow.
} box_t;

// An integer is either a small int or a pointer to a bigint_t. Identity with boxed values.
typedef box_t integer_t;

// boxed forward declarations
static inline uintx_t   unbox_enum(box_t v);
static inline box_t     box_enum(uintx_t u);


/*--------------------------------------------------------------------------------------
  Blocks 
--------------------------------------------------------------------------------------*/

// A heap block is a header followed by `scan_fsize` boxed fields and further raw bytes
// A `block_t*` is never NULL (to avoid testing for NULL for reference counts).
typedef struct block_s {
  header_t header;
} block_t;

// A large block can has a (boxed) large scan size for vectors.
typedef struct block_large_s {
  header_t header;
  box_t    large_scan_fsize; // if `scan_fsize == 0xFF` there is a first field with the full scan size
} block_large_t;

// A pointer to a block is never NULL.
typedef block_t* ptr_t;


static inline decl_const tag_t block_tag(const block_t* b) {
  return (tag_t)(b->header.h.tag);
}

static inline decl_pure size_t block_scan_fsize(const block_t* b) {
  const size_t sfsize = b->header.h.scan_fsize;
  if (likely(sfsize != SCAN_FSIZE_MAX)) return sfsize;
  const block_large_t* bl = (block_large_t*)b; 
  return unbox_enum(bl->large_scan_fsize);
}

static inline decl_pure uintptr_t block_refcount(const block_t* b) {
  return b->header.h.refcount;
}

static inline decl_pure bool block_is_unique(const block_t* b) {
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
typedef void*  heap_t;

// A function has as its first field a pointer to a C function that takes the 
// `function_t` itself as a first argument. The following fields are the free variables.
typedef struct function_s {
  block_t  _block;
  box_t    fun;     
  // followed by free variables
} *function_t;

// A vector is an array of boxed values.
typedef struct vector_s {
  block_t  _block;
  box_t    length;
  box_t    vec[1];            // vec[length+1] with box_null as sentinel
} *vector_t;

#define YIELD_CONT_MAX (8)

typedef enum yield_kind_e {
  YIELD_NONE,
  YIELD_NORMAL,
  YIELD_FINAL
} yield_kind_t;

typedef struct yield_s {
  uint8_t    yielding;        // are we yielding to a handler? 0:no, 1:yielding, 2:yielding_final (e.g. exception)
  int32_t    marker;          // marker of the handler to yield to
  function_t clause;          // the operation clause to execute when the handler is found
  size_t     conts_count;     // number of continuations in `conts`
  function_t conts[YIELD_CONT_MAX];  // fixed array of continuations. The final continuation `k` is
                              // composed as `fN ○ ... ○ f2 ○ f1` if `conts = { f1, f2, ..., fN }` 
                              // if the array becomes full, a fresh array is allocated and the first
                              // entry points to its composition.
} yield_t;

typedef struct context_s {
  heap_t      heap;             // the (thread-local) heap to allocate in; todo: put in a register?
  vector_t    evv;              // the current evidence vector for effect handling: vector for size 0 and N>1, direct evidence for one element vector
  yield_t     yield;            // inlined yield structure (for efficiency)
  int32_t     marker_unique;    // unique marker generation
  block_t*    delayed_free;     // list of blocks that still need to be freed
  integer_t   unique;           // thread local unique number generation
  uintptr_t   thread_id;        // unique thread id
  function_t* log;              // logging function
  function_t* out;              // std output
} context_t;

// Get the current (thread local) runtime context (should always equal the `_ctx` parameter)
decl_export context_t* runtime_context(void); 

// The current context is passed as a _ctx parameter in the generated code
#define current_context()   _ctx

// Is the execution yielding?
static inline decl_pure bool yielding(const context_t* ctx) {
  return (ctx->yield.yielding != YIELD_NONE);
};

static inline decl_pure bool yielding_non_final(const context_t* ctx) {
  return (ctx->yield.yielding == YIELD_NORMAL);
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

static inline void block_init(block_t* b, size_t size, size_t scan_fsize, tag_t tag) {
  UNUSED(size);
  assert_internal(scan_fsize < SCAN_FSIZE_MAX);
  header_t header = { 0 };
  header.h.tag = (uint16_t)tag;
  header.h.scan_fsize = (uint8_t)scan_fsize;
  b->header = header;
}

static inline void block_large_init(block_large_t* b, size_t size, size_t scan_fsize, tag_t tag) {
  UNUSED(size);
  header_t header = { 0 };
  header.h.tag = (uint16_t)tag;
  header.h.scan_fsize = SCAN_FSIZE_MAX;
  b->header = header;
  b->large_scan_fsize = box_enum(scan_fsize);
}

static inline block_t* block_alloc(size_t size, size_t scan_fsize, tag_t tag, context_t* ctx) {
  assert_internal(scan_fsize < SCAN_FSIZE_MAX);
  block_t* b = (block_t*)runtime_malloc(size, ctx);
  block_init(b, size, scan_fsize, tag);
  return b;
}

static inline block_large_t* block_large_alloc(size_t size, size_t scan_fsize, tag_t tag, context_t* ctx) {
  block_large_t* b = (block_large_t*)runtime_malloc(size + 1 /* the scan_large_fsize field */, ctx);
  block_large_init(b, size, scan_fsize, tag);
  return b;
}

static inline block_t* block_realloc(block_t* b, size_t size, context_t* ctx) {
  assert_internal(block_is_unique(b));
  return (block_t*)runtime_realloc(b, size, ctx);
}

static inline void* _block_as(block_t* b) {
  return (void*)b;
}
static inline void* _block_as_assert(block_t* b, tag_t tag) {
  assert_internal(block_tag(b) == tag);
  return (void*)b;
}

#define block_alloc_as(struct_tp,scan_fsize,tag,ctx)  ((struct_tp*)block_alloc(sizeof(struct_tp),scan_fsize,tag,ctx))
#define block_as(tp,b,tag)                            ((tp)_block_as_assert(b,tag))


/*--------------------------------------------------------------------------------------
  Reference counting
--------------------------------------------------------------------------------------*/

decl_export void block_check_free(block_t* b, context_t* ctx);


static inline block_t* block_dup(block_t* b) {
#if REFCOUNT_LIMIT_TO_32BIT
  // with a 32-bit reference count on a 64-bit system, we need a (8*2^32 = 32GiB array to create that many
  // references to a single object. That is often a reasonable restriction and more efficient.
  b->header.rc32.lo++;
#else
  // optimize: increment the full 64-bit hoping to never overflow the 38 refcount bits
  // this is reasonable as it would take a (8*2^38 = 2TiB array to create that many references to a single object)
  b->header.rc.extended++;   
#endif
  return b;
}

static inline void block_drop(block_t* b, context_t* ctx) {
  // optimize: always decrement just the 32 bits; 
  // on larger refcounts check afterwards if the hi-bits were 0. Since we use 0 for a unique reference we can 
  // efficiently check if the block can be freed by comparing to 0.
  uint32_t count = (b->header.rc32.lo)--;
#if REFCOUNT_LIMIT_TO_32BIT
  if (count==0) block_free(b,ctx);
#else
  if (count==0) block_check_free(b,ctx);
#endif
}

#define datatype_as(tp,v,tag)             (block_as(tp,&(v)->_block,tag))
#define datatype_tag(v)                   (block_tag(&(v)->_block))
#define datatype_drop(v,ctx)              (block_drop(&(v)->_block,ctx))
#define datatype_dup_as(tp,v)             ((tp)block_dup(&(v)->_block))

#define constructor_tag(v)                (datatype_tag(&(v)->_inherit))
#define constructor_drop(v,ctx)           (datatype_drop(&(v)->_inherit,ctx))
#define constructor_dup_as(tp,v)          (datatype_dup_as(tp, &(v)->_inherit))


/*----------------------------------------------------------------------
  Further includes
----------------------------------------------------------------------*/

// The unit type
typedef enum unit_e {
  Unit = 0
} unit_t;

// A function to free a raw C pointer, raw bytes, or raw string.
typedef void (free_fun_t)(void*);
decl_export void free_fun_null(void* p);

// "raw" types: first field is pointer to a free function, the next field a pointer to raw C data
typedef struct cptr_raw_s {
  block_t     _block;
  free_fun_t* free;
  void*       cptr;
} *cptr_raw_t;


#include "runtime/box.h"
#include "runtime/integer.h"
#include "runtime/bitcount.h"
#include "runtime/string.h"

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
  Reference counting scrutinee of a match where we try to avoid
  reference increments on fields of the match, and reuse memory in-place for
  deallocated scrutinees.
--------------------------------------------------------------------------------------*/
typedef struct orphan_s {
  block_t _block;
} *orphan_t;

extern struct orphan_s _orphan_null;

static inline decl_const orphan_t orphan_null(void) {
  return &_orphan_null;
}

static inline orphan_t orphan_block(block_t* b) {
  b->header.as_uint64 = 0; // set tag to zero, unique, with zero scan size (so decref is valid on it)
  return (orphan_t)b;
}

static inline orphan_t block_release0(block_t* b, context_t* ctx) {
  if (block_is_unique(b)) {
    return orphan_block(b);
  }
  else {
    block_drop(b, ctx);
    return orphan_null();
  }
}

static inline orphan_t block_release1(block_t* b, box_t unused_field1, context_t* ctx) {
  if (block_is_unique(b)) {
    boxed_drop(unused_field1, ctx);
    return orphan_block(b);
  }
  else {
    block_drop(b, ctx);
    return orphan_null();
  }
}

static inline void block_no_reuse(orphan_t o) {
  if (o != orphan_null()) {
    runtime_free(o);
  };
}

static inline block_t* block_alloc_reuse(orphan_t o, size_t size, size_t scan_fsize, tag_t tag, context_t* ctx) {
  // TODO: check usable size p >= size
  block_t* b;
  if (o != orphan_null()) {
    assert_internal(block_is_unique(&o->_block));
    b = &o->_block;
  }
  else {
    b = (block_t*)runtime_malloc(size, ctx);
  }
  block_init(b, size, scan_fsize, tag);
  return b;
}

/*--------------------------------------------------------------------------------------
  Value tags
--------------------------------------------------------------------------------------*/


// Tag for value types is always a boxed enum
typedef box_t value_tag_t;

// Use inlined #define to enable constant initializer expression
/*
static inline value_tag_t value_tag(uint_t tag) {
  return box_enum(tag);
}
*/
#define value_tag(tag) (box_from_uintptr(((uintx_t)tag << 2) | 0x03))


/*--------------------------------------------------------------------------------------
  Functions
--------------------------------------------------------------------------------------*/

#define function_alloc_as(tp,scan_fsize,ctx)    block_alloc_as(tp,scan_fsize,TAG_FUNCTION,ctx)
#define function_call(restp,argtps,f,args)      ((restp(*)argtps)(unbox_cptr(f->fun)))args
#define define_static_function(name,cfun) \
  static struct function_s _static_##name = { { HEADER_STATIC(0,TAG_FUNCTION) }, { (uintptr_t)&cfun } }; /* note: should be box_cptr(&cfun) but we need a constant expression */ \
  function_t name = &_static_##name;


extern function_t function_id;

static inline function_t unbox_function_t(box_t v) {
  return unbox_datatype_as(function_t, v, TAG_FUNCTION);
}

static inline box_t box_function_t(function_t d) {
  return box_datatype_as(function_t, d, TAG_FUNCTION);
}

static inline bool function_is_unique(function_t f) {
  return block_is_unique(&f->_block);
}

static inline void function_drop(function_t f, context_t* ctx) {
  block_drop(&f->_block, ctx);
}

static inline function_t function_dup(function_t f) {
  return block_as(function_t,block_dup(&f->_block),TAG_FUNCTION);
}


/*--------------------------------------------------------------------------------------
  References
--------------------------------------------------------------------------------------*/
typedef struct ref_s {
  block_t _block;
  box_t   value;
} *ref_t;

static inline ref_t ref_alloc(box_t value, context_t* ctx) {
  ref_t r = block_alloc_as(struct ref_s, 1, TAG_REF, ctx);
  r->value = value;
  return r;
}

static inline box_t ref_get(ref_t b) {
  return boxed_dup(b->value);
}

static inline unit_t ref_set(ref_t r, box_t value, context_t* ctx) {
  box_t b = r->value; 
  boxed_drop(b, ctx);
  r->value = value;
  return Unit;
}

static inline box_t ref_swap(ref_t r, box_t value) {
  box_t b = r->value;
  r->value = value;
  return b;
}

decl_export void fatal_error(int err, const char* msg, ...);

static inline void unsupported_external(const char* msg) {
  fatal_error(ENOSYS,msg);
}


/*--------------------------------------------------------------------------------------
  Unit
--------------------------------------------------------------------------------------*/

static inline box_t box_unit_t(unit_t u) {
  return box_enum(u);
}

static inline unit_t unbox_unit_t(box_t u) {
  assert_internal( unbox_enum(u) == (uintx_t)Unit);
  return Unit;
}

/*--------------------------------------------------------------------------------------
  Vector
--------------------------------------------------------------------------------------*/

static inline vector_t vector_alloc(size_t length, box_t def, context_t* ctx) {
  vector_t v = block_as(vector_t, block_alloc(sizeof(struct vector_s) + length*sizeof(box_t) /* room for 1 sentinel value */, 1 + length, TAG_VECTOR, ctx), TAG_VECTOR);
  v->length = box_enum(length);
  if (def.box != box_null.box) {
    for (size_t i = 0; i < length; i++) {
      v->vec[i] = def;
    }
    v->vec[length] = box_from_uintptr(0); // ending zero value
  }
  return v;
}

static inline void vector_drop(vector_t v, context_t* ctx) {
  datatype_drop(v, ctx);
}

static inline vector_t vector_dup(vector_t v) {
  return datatype_dup_as(vector_t,v);
}

static inline size_t vector_len(const vector_t v) {
  return unbox_enum(v->length);
}

static inline box_t* vector_buf(vector_t v, size_t* len) {
  if (len != NULL) *len = vector_len(v);
  return &v->vec[0];
}

static inline box_t vector_at(const vector_t v, size_t i) {
  assert(i < unbox_enum(v->length));
  return boxed_dup(v->vec[i]);
}


extern vector_t vector_empty;

static inline box_t box_vector_t(vector_t v, context_t* ctx) {
  UNUSED(ctx);
  return box_datatype_as(vector_t,v,TAG_VECTOR);
}

static inline vector_t unbox_vector_t(box_t b, context_t* ctx) {
  UNUSED(ctx);
  return unbox_datatype_as(vector_t, b, TAG_VECTOR);
}

/*--------------------------------------------------------------------------------------
  Bytes
--------------------------------------------------------------------------------------*/

typedef struct bytes_s {
  block_t  _block;               // TAG_BYTES or TAG_BYTES_RAW
}* bytes_t;

struct bytes_vector_s {          // in-place bytes
  struct bytes_s  _inherit;
  size_t          length;
  char            buf[1];
};

struct bytes_raw_s {             // pointer to bytes with free function
  struct bytes_s _inherit;
  free_fun_t*    free;
  uint8_t*       data;
  size_t         length;
};

#endif // include guard
