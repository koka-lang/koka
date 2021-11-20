#pragma once
#ifndef KKLIB_H
#define KKLIB_H 

#define KKLIB_BUILD        73       // modify on changes to trigger recompilation
#define KK_MULTI_THREADED   1       // set to 0 to be used single threaded only
// #define KK_DEBUG_FULL       1    // set to enable full internal debug checks

/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
#define WIN32_LEAN_AND_MEAN          // reduce windows includes
#define _POSIX_C_SOURCE     200809L  // make posix definitions visible
#define _DARWIN_C_SOURCE    200809L  // make darwin definitions visible
#define _XOPEN_SOURCE       700      // make xopen (posix 2008) definitions visible
#define _FILE_OFFSET_BITS   64       // enable large files
#if defined(__GNUC__) && !(defined(WIN32))
#define _GNU_SOURCE         1        // make gnu definitions visible
#endif

#include <limits.h>           // LONG_MAX, ...
#include <stddef.h>           // size_t
#include <stdint.h>           // int64_t, ...
#include <stdbool.h>          // bool
#include <assert.h>           // assert
#include <errno.h>            // ENOSYS, ...
#include <stdlib.h>           // malloc, abort, ...
#include <string.h>           // strlen, memcpy, ...
#include <math.h>             // isnan, ...
#include <stdio.h>            // FILE*, printf, ...

#include "kklib/platform.h"   // Platform abstractions and portability definitions
#include "kklib/atomic.h"     // Atomic operations
#include "kklib/process.h"    // Process info (memory usage, run time etc.)


/*--------------------------------------------------------------------------------------
  Tags
--------------------------------------------------------------------------------------*/

// Tags for heap blocks
typedef enum kk_tag_e {
  KK_TAG_INVALID   = 0,
  KK_TAG_MIN       = 1,
  KK_TAG_MAX       = 0xFFC0,   // space for 64 special tags
  KK_TAG_OPEN,        // open datatype, first field is a string tag
  KK_TAG_BOX,         // boxed value type
  KK_TAG_BOX_ANY,     // kk_box_any polymorphic value
  KK_TAG_REF,         // mutable reference
  KK_TAG_FUNCTION,    // function closure with the free variables environment
  KK_TAG_BIGINT,      // big integer (see `integer.c`)
  KK_TAG_BYTES_SMALL, // small byte sequence of at most 7 bytes.
  KK_TAG_BYTES,       // byte sequence
  KK_TAG_VECTOR,      // a vector of (boxed) values
  KK_TAG_INT64,       // boxed int64_t               (only on <=64-bit platforms)
  KK_TAG_DOUBLE,      // boxed IEEE double (64-bit)  (only on <=64-bit platforms)
  KK_TAG_INT32,       // boxed int32_t               (only on <=32-bit platforms)
  KK_TAG_FLOAT,       // boxed IEEE float  (32-bit)  (only on <=32-bit platforms)
  KK_TAG_INT16,       // boxed int16_t               (only on <=16-bit platforms)
  KK_TAG_CFUNPTR,     // C function pointer
  KK_TAG_INTPTR,      // boxed intptr_t  
  KK_TAG_EVV_VECTOR,  // evidence vector (used in std/core/hnd)
  KK_TAG_NOTHING,
  KK_TAG_JUST,
  // raw tags have a free function together with a `void*` to the data
  KK_TAG_CPTR_RAW,    // full void* (must be first, see kk_tag_is_raw())
  KK_TAG_BYTES_RAW,   // pointer to byte buffer
  KK_TAG_LAST,
  // strings are represented by bytes but guarantee valid utf-8 encoding
  KK_TAG_STRING_SMALL = KK_TAG_BYTES_SMALL, // utf-8 encoded string of at most 7 bytes.
  KK_TAG_STRING       = KK_TAG_BYTES,       // utf-8 encoded string ending with a zero byte.
  KK_TAG_STRING_RAW   = KK_TAG_BYTES_RAW    // pointer to a valid utf-8 string
} kk_tag_t;

static inline bool kk_tag_is_raw(kk_tag_t tag) {
  return (tag >= KK_TAG_CPTR_RAW);
}

/*--------------------------------------------------------------------------------------
  Headers
--------------------------------------------------------------------------------------*/


// The reference count is 0 for a unique reference (for a faster free test in drop).
// Reference counts larger than 0x8000000 (i.e. < 0) use atomic increment/decrement (for thread shared objects).
// (Reference counts are always 32-bit (even on 64-bit) platforms but get "sticky" if
//  they get too large and in such case we never free the object, see `refcount.c`)
typedef uint32_t kk_refcount_t;

// Are there (possibly) references from other threads? (includes static variables)
static inline bool kk_refcount_is_thread_shared(kk_refcount_t rc) {
  return ((int32_t)rc < 0);
}

// Is the reference unique, or are there (possibly) references from other threads? (includes static variables)
static inline bool kk_refcount_is_unique_or_thread_shared(kk_refcount_t rc) {
  return ((int32_t)rc <= 0);
}



// Every heap block starts with a 64-bit header with a reference count, tag, and scan fields count.
// If the scan_fsize == 0xFF, the full scan count is in the first field as a boxed int (which includes the scan field itself).
typedef struct kk_header_s {
  uint8_t   scan_fsize;  // number of fields that should be scanned when releasing (`scan_fsize <= 0xFF`, if 0xFF, the full scan size is the first field)
  uint8_t   _field_idx;  // private: only used during stack-less freeing and marking (see `refcount.c`)
  uint16_t  tag;         // constructor tag
  _Atomic(kk_refcount_t) refcount; // reference count  (last to reduce code size constants in kk_header_init)
} kk_header_t;

#define KK_SCAN_FSIZE_MAX (0xFF)
#define KK_HEADER(scan_fsize,tag)         { scan_fsize, 0, tag, ATOMIC_VAR_INIT(0) }                // start with refcount of 0
#define KK_HEADER_STATIC(scan_fsize,tag)  { scan_fsize, 0, tag, ATOMIC_VAR_INIT(KU32(0x80000000)) } // start with a stuck refcount (RC_STUCK)

static inline void kk_header_init(kk_header_t* h, kk_ssize_t scan_fsize, kk_tag_t tag) {
  kk_assert_internal(scan_fsize >= 0 && scan_fsize <= KK_SCAN_FSIZE_MAX);
#if (KK_ARCH_LITTLE_ENDIAN && !defined(__aarch64__))
  *((uint64_t*)h) = ((uint64_t)scan_fsize | (uint64_t)tag << 16); // explicit shifts leads to better codegen in general
#else
  kk_header_t header = KK_HEADER((uint8_t)scan_fsize, (uint16_t)tag);
  *h = header;
#endif  
}


/*--------------------------------------------------------------------------------------
  Box, Integer, Datatype
--------------------------------------------------------------------------------------*/

// Polymorphic operations work on boxed values. (We use a struct for extra checks to prevent accidental conversion)
// The least significant bit is clear for `kk_block_t*` pointers, while it is set for values.
// See `box.h` for definitions.
typedef struct kk_box_s {
  uintptr_t box;
} kk_box_t;
 
// An integer is either a small int (as: 4*i + 1) or a `kk_bigint_t*` pointer. Isomorphic with boxed values.
// See `integer.h` for definitions.
typedef struct kk_integer_s {
  uintptr_t ibox;
} kk_integer_t;

// A general datatype with constructors and singletons is either
// an enumeration (with the lowest bit set as: 4*tag + 1) or a `kk_block_t*` pointer.
// Isomorphic with boxed values. 
typedef struct kk_datatype_s {
  uintptr_t dbox;
} kk_datatype_t;


// boxed forward declarations
static inline kk_intf_t kk_intf_unbox(kk_box_t v);
static inline kk_box_t  kk_intf_box(kk_intf_t u);


/*--------------------------------------------------------------------------------------
  Blocks
  A block is an object that starts with a header.

  heap allocated datatypes contain a first `_block` field.
  Their heap allocated constructors in turn contain a first `_base` field that is the datatype.
  This representation ensures correct behaviour under C alias rules and allow good optimization.
  e.g. :

    type tree<a> {
      Leaf( x : a )
      Node( l : tree<a>, r : tree<a> )
    }

    ~>

    typedef struct kk_tree_s {
      kk_block_t _block;
    } * kk_tree_t

    struct Node {
      struct kk_tree_s  _base;
      kk_tree_t        left;
      kk_tree_t        right;
    }

    struct Leaf {
      struct kk_tree_s  _base;
      kk_box_t          value;
    }

  Datatypes that have singletons encode singletons as constants with the lowest bit set to 1.
  We use the type `kk_datatype_t` to represent them:

    struct kk_list_s {
      kk_block_t _block;
    };
    typedef kk_datatype_t kk_list_t;

    struct Cons {
      struct kk_list_s  _base;
      kk_box_t          head;
      kk_list_t         tail;
    }

    // no type for the Nil constructor
--------------------------------------------------------------------------------------*/

// A heap block is a header followed by `scan_fsize` boxed fields and further raw bytes
// A `kk_block_t*` is never NULL (to avoid testing for NULL for reference counts).
typedef struct kk_block_s {
  kk_header_t header;
} kk_block_t;

// A large block has a (boxed) large scan size (currently only used for vectors).
typedef struct kk_block_large_s {
  kk_block_t  _block;
  kk_box_t    large_scan_fsize; // if `scan_fsize == 0xFF` there is a first field with the full scan size
                                // (the full scan size should include the `kk_large_scan_fsize` field itself!)
} kk_block_large_t;

// A pointer to a block. Cannot be NULL.
typedef kk_block_t* kk_ptr_t;


static inline kk_decl_const kk_tag_t kk_block_tag(const kk_block_t* b) {
  return (kk_tag_t)(b->header.tag);
}

static inline kk_decl_const bool kk_block_has_tag(const kk_block_t* b, kk_tag_t t) {
  return (kk_block_tag(b) == t);
}

static inline kk_decl_pure kk_ssize_t kk_block_scan_fsize(const kk_block_t* b) {  // number of scan fields
  const kk_ssize_t sfsize = b->header.scan_fsize;
  if (kk_likely(sfsize != KK_SCAN_FSIZE_MAX)) return sfsize;
  const kk_block_large_t* bl = (const kk_block_large_t*)b;
  return (kk_ssize_t)kk_intf_unbox(bl->large_scan_fsize);
}

static inline kk_decl_pure kk_refcount_t kk_block_refcount(const kk_block_t* b) {
  return kk_atomic_load_relaxed(&b->header.refcount);
}

static inline void kk_block_refcount_set(kk_block_t* b, kk_refcount_t rc) {
  return kk_atomic_store_relaxed(&b->header.refcount, rc);
}

static inline kk_decl_pure bool kk_block_is_unique(const kk_block_t* b) {
  return (kk_likely(kk_block_refcount(b) == 0));
}

static inline kk_decl_pure bool kk_block_is_thread_shared(const kk_block_t* b) {
  return (kk_unlikely(kk_refcount_is_thread_shared(kk_block_refcount(b))));
}

typedef struct kk_block_fields_s {
  kk_block_t _block;
  kk_box_t   fields[1];
} kk_block_fields_t;

static inline kk_decl_pure kk_box_t kk_block_field(kk_block_t* b, kk_ssize_t index) {
  kk_block_fields_t* bf = (kk_block_fields_t*)b;  // must overlap with datatypes with scanned fields.
  return bf->fields[index];
}

static inline void kk_block_field_set(kk_block_t* b, kk_ssize_t index, kk_box_t v) {
  kk_block_fields_t* bf = (kk_block_fields_t*)b;  // must overlap with datatypes with scanned fields.
  bf->fields[index] = v;
}

#if (KK_INTPTR_SIZE==8)
#define KK_BLOCK_INVALID  KUP(0xDFDFDFDFDFDFDFDF)
#else
#define KK_BLOCK_INVALID  KUP(0xDFDFDFDF)
#endif

static inline void kk_block_set_invalid(kk_block_t* b) {
#ifdef KK_DEBUG_FULL
  const kk_ssize_t scan_fsize = kk_block_scan_fsize(b);
  const kk_box_t inv = { KK_BLOCK_INVALID };
  for (kk_ssize_t i = -1; i < scan_fsize; i++) {
    kk_block_field_set(b, i, inv);
  }
#else
  KK_UNUSED(b);
#endif
} 

static inline kk_decl_pure bool kk_block_is_valid(kk_block_t* b) {
  return (b != NULL && ((uintptr_t)b&1)==0 && kk_block_field(b, 0).box != KK_BLOCK_INVALID); // already freed!
}


/*--------------------------------------------------------------------------------------
  The thread local context as `kk_context_t`
  This is passed by the code generator as an argument to every function so it can
  be (usually) accessed efficiently through a register.
--------------------------------------------------------------------------------------*/
#ifdef KK_MIMALLOC
#define MI_MAX_ALIGN_SIZE  KK_INTPTR_SIZE
#ifdef KK_MIMALLOC_INLINE
#include "../mimalloc/include/mimalloc-inline.h"
#else
#include "../mimalloc/include/mimalloc.h"
#endif
typedef mi_heap_t* kk_heap_t;
#else
typedef void*      kk_heap_t;
#endif

// A function has as its first field a pointer to a C function that takes the
// `kk_function_t` itself as a first argument. The following fields are the free variables.
typedef struct kk_function_s {
  kk_block_t  _block;
  kk_box_t    fun;
  // followed by free variables
} *kk_function_t;

// A vector is an array of boxed values, or an empty singleton
typedef kk_datatype_t kk_vector_t;

// Strong random number context (using chacha20)
struct kk_random_ctx_s;

// High precision duration.
typedef struct kk_duration_s {
  double seconds;
  double second_fraction;
} kk_duration_t;


// Box any is used when yielding
typedef struct kk_box_any_s {
  kk_block_t    _block;
  kk_integer_t  _unused;
} *kk_box_any_t;

// Workers run in a task_group
typedef struct kk_task_group_s kk_task_group_t;

//A yield context allows up to 8 continuations to be stored in-place
#define KK_YIELD_CONT_MAX (8)

typedef enum kk_yield_kind_e {
  KK_YIELD_NONE,
  KK_YIELD_NORMAL,
  KK_YIELD_FINAL
} kk_yield_kind_t;

typedef struct kk_yield_s {
  int32_t       marker;          // marker of the handler to yield to
  kk_function_t clause;          // the operation clause to execute when the handler is found
  kk_ssize_t    conts_count;     // number of continuations in `conts`
  kk_function_t conts[KK_YIELD_CONT_MAX]; // fixed array of continuations. The final continuation `k` is
                                          // composed as `fN ○ ... ○ f2 ○ f1` if `conts = { f1, f2, ..., fN }`
                                          // if the array becomes full, a fresh array is allocated and the first
                                          // entry points to its composition.
} kk_yield_t;

extern kk_ptr_t kk_evv_empty_singleton;

     
// The thread local context.
// The fields `yielding`, `heap` and `evv` should come first for efficiency
typedef struct kk_context_s {
  int8_t         yielding;         // are we yielding to a handler? 0:no, 1:yielding, 2:yielding_final (e.g. exception) // put first for efficiency
  kk_heap_t      heap;             // the (thread-local) heap to allocate in; todo: put in a register?
  kk_ptr_t       evv;              // the current evidence vector for effect handling: vector for size 0 and N>1, direct evidence for one element vector
  kk_yield_t     yield;            // inlined yield structure (for efficiency)
  int32_t        marker_unique;    // unique marker generation
  kk_block_t*    delayed_free;     // list of blocks that still need to be freed
  kk_integer_t   unique;           // thread local unique number generation
  uintptr_t      thread_id;        // unique thread id
  kk_box_any_t   kk_box_any;       // used when yielding as a value of any type
  kk_function_t  log;              // logging function
  kk_function_t  out;              // std output
  kk_task_group_t* task_group;     // task group for managing threads. NULL for the main thread.
  
  struct kk_random_ctx_s* srandom_ctx; // strong random using chacha20, initialized on demand
  kk_ssize_t     argc;             // command line argument count 
  const char**   argv;             // command line arguments
  kk_timer_t     process_start;    // time at start of the process
  int64_t        timer_freq;       // high precision timer frequency
  kk_duration_t  timer_prev;       // last requested timer time
  kk_duration_t  timer_delta;      // applied timer delta
  int64_t        time_freq;        // unix time frequency
  kk_duration_t  time_unix_prev;   // last requested unix time
} kk_context_t;

// Get the current (thread local) runtime context (should always equal the `_ctx` parameter)
kk_decl_export kk_context_t* kk_get_context(void);
kk_decl_export void          kk_free_context(void);

kk_decl_export kk_context_t* kk_main_start(int argc, char** argv);
kk_decl_export void          kk_main_end(kk_context_t* ctx);

kk_decl_export void          kk_debugger_break(kk_context_t* ctx);

// The current context is passed as a _ctx parameter in the generated code
#define kk_context()  _ctx

// Is the execution yielding?
static inline kk_decl_pure bool kk_yieldingx(const kk_context_t* ctx) {
  return (ctx->yielding != KK_YIELD_NONE);
}
#define kk_yielding(ctx)   kk_unlikely(kk_yieldingx(ctx))


static inline kk_decl_pure bool kk_yielding_non_final(const kk_context_t* ctx) {
  return (ctx->yielding == KK_YIELD_NORMAL);
}

static inline kk_decl_pure bool kk_yielding_final(const kk_context_t* ctx) {
  return (ctx->yielding == KK_YIELD_FINAL);
}

// Get a thread local marker unique number >= 1.
static inline int32_t kk_marker_unique(kk_context_t* ctx) {
  int32_t m = ++ctx->marker_unique;           // must return a marker >= 1 so increment first;
  if (m == INT32_MAX) ctx->marker_unique = 1; // controlled reset
  return m;
}


kk_decl_export void kk_block_mark_shared( kk_block_t* b, kk_context_t* ctx );
kk_decl_export void kk_box_mark_shared( kk_box_t b, kk_context_t* ctx );
kk_decl_export void kk_box_mark_shared_recx(kk_box_t b, kk_context_t* ctx);

/*--------------------------------------------------------------------------------------
  Allocation
--------------------------------------------------------------------------------------*/

#ifdef KK_MIMALLOC
#ifdef KK_MIMALLOC_INLINE
  static inline void* kk_malloc_small(kk_ssize_t sz, kk_context_t* ctx) {
    return kk_mi_heap_malloc_small_inline(ctx->heap, (size_t)sz);
  }
#else
  static inline void* kk_malloc_small(kk_ssize_t sz, kk_context_t* ctx) {
    return mi_heap_malloc_small(ctx->heap, (size_t)sz);
  } 
#endif

static inline void* kk_malloc(kk_ssize_t sz, kk_context_t* ctx) {
  return mi_heap_malloc(ctx->heap, (size_t)sz);
}

static inline void* kk_zalloc(kk_ssize_t sz, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  return mi_heap_zalloc(ctx->heap, (size_t)sz);
}

static inline void* kk_realloc(void* p, kk_ssize_t sz, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  return mi_heap_realloc(ctx->heap, p, (size_t)sz);
}

static inline void kk_free(const void* p) {
  mi_free((void*)p);
}

static inline void kk_free_local(const void* p) {
  kk_free(p);
}
#else
static inline void* kk_malloc(kk_ssize_t sz, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  return malloc((size_t)sz);
}

static inline void* kk_malloc_small(kk_ssize_t sz, kk_context_t* ctx) {
  return kk_malloc(sz,ctx);
}

static inline void* kk_zalloc(kk_ssize_t sz, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  return calloc(1, (size_t)sz);
}

static inline void* kk_realloc(void* p, kk_ssize_t sz, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  return realloc(p, (size_t)sz);
}

static inline void kk_free(const void* p) {
  KK_UNUSED(p);
  free((void*)p);
}

static inline void kk_free_local(const void* p) {
  kk_free(p);
}
#endif


static inline void kk_block_init(kk_block_t* b, kk_ssize_t size, kk_ssize_t scan_fsize, kk_tag_t tag) {
  KK_UNUSED(size);
  kk_assert_internal(scan_fsize >= 0 && scan_fsize < KK_SCAN_FSIZE_MAX);
  kk_header_init(&b->header, scan_fsize, tag);
}

static inline void kk_block_large_init(kk_block_large_t* b, kk_ssize_t size, kk_ssize_t scan_fsize, kk_tag_t tag) {
  KK_UNUSED(size);
  // to optimize for "small" vectors with less than 255 scanable elements, we still set the small scan_fsize
  // for those in the header. This is still duplicated in the large scan_fsize field as it is used for the vector length for example.
  uint8_t bscan_fsize = (scan_fsize >= KK_SCAN_FSIZE_MAX ? KK_SCAN_FSIZE_MAX : (uint8_t)scan_fsize);
  kk_header_init(&b->_block.header, bscan_fsize, tag);
  kk_assert_internal(scan_fsize > 0);
  b->large_scan_fsize = kk_intf_box(scan_fsize);  
}

typedef kk_block_t* kk_reuse_t;

#define kk_reuse_null  ((kk_reuse_t)NULL)

static inline kk_block_t* kk_block_alloc_at(kk_reuse_t at, kk_ssize_t size, kk_ssize_t scan_fsize, kk_tag_t tag, kk_context_t* ctx) {
  kk_assert_internal(scan_fsize >= 0 && scan_fsize < KK_SCAN_FSIZE_MAX);
  kk_block_t* b;
  if (at==kk_reuse_null) {
    b = (kk_block_t*)kk_malloc_small(size, ctx);
  }
  else {
    kk_assert_internal(kk_block_is_unique(at)); // TODO: check usable size of `at`
    b = at;
  }
  kk_block_init(b, size, scan_fsize, tag);
  return b;
}

static inline kk_block_t* kk_block_alloc(kk_ssize_t size, kk_ssize_t scan_fsize, kk_tag_t tag, kk_context_t* ctx) {
  kk_assert_internal(scan_fsize >= 0 && scan_fsize < KK_SCAN_FSIZE_MAX);
  kk_block_t* b = (kk_block_t*)kk_malloc_small(size, ctx);
  kk_block_init(b, size, scan_fsize, tag);
  return b;
}

static inline kk_block_t* kk_block_alloc_any(kk_ssize_t size, kk_ssize_t scan_fsize, kk_tag_t tag, kk_context_t* ctx) {
  kk_assert_internal(scan_fsize >= 0 && scan_fsize < KK_SCAN_FSIZE_MAX);
  kk_block_t* b = (kk_block_t*)kk_malloc(size, ctx);
  kk_block_init(b, size, scan_fsize, tag);
  return b;
}

static inline kk_block_large_t* kk_block_large_alloc(kk_ssize_t size, kk_ssize_t scan_fsize, kk_tag_t tag, kk_context_t* ctx) {
  kk_block_large_t* b = (kk_block_large_t*)kk_malloc(size, ctx);
  kk_block_large_init(b, size, scan_fsize, tag);
  return b;
}

static inline kk_block_t* kk_block_realloc(kk_block_t* b, kk_ssize_t size, kk_context_t* ctx) {
  kk_assert_internal(kk_block_is_unique(b));
  return (kk_block_t*)kk_realloc(b, size, ctx);
}

static inline kk_block_t* kk_block_assertx(kk_block_t* b, kk_tag_t tag) {
  KK_UNUSED_INTERNAL(tag);
  kk_assert_internal(kk_block_tag(b) == tag || kk_block_tag(b) == KK_TAG_BOX_ANY);
  return b;
}

static inline void kk_block_free(kk_block_t* b) {
  kk_block_set_invalid(b);
  kk_free(b);
}

#define kk_block_alloc_as(struct_tp,scan_fsize,tag,ctx)        ((struct_tp*)kk_block_alloc_at(kk_reuse_null, sizeof(struct_tp),scan_fsize,tag,ctx))
#define kk_block_alloc_at_as(struct_tp,at,scan_fsize,tag,ctx)  ((struct_tp*)kk_block_alloc_at(at, sizeof(struct_tp),scan_fsize,tag,ctx))

#define kk_block_as(tp,b)             ((tp)((void*)(b)))
#define kk_block_assert(tp,b,tag)     ((tp)kk_block_assertx(b,tag))


/*--------------------------------------------------------------------------------------
  Reference counting
  0    : unique reference
  > 0  : non thread-shared reference
  < 0  : thread-shared or sticky

  The main performance trick is to do one single test in a dup/drop for the fast path.
  In drop we can check `rc > 0` to decrement in place, or check further if we need
  atomic decrement or can free (in case rc==0). The kk_block_check_xxx routines are not
  inlined and we get nice inlined assembly for the fast path with the single check.
--------------------------------------------------------------------------------------*/

kk_decl_export void        kk_block_check_drop(kk_block_t* b, kk_refcount_t rc, kk_context_t* ctx);
kk_decl_export void        kk_block_check_decref(kk_block_t* b, kk_refcount_t rc, kk_context_t* ctx);
kk_decl_export kk_block_t* kk_block_check_dup(kk_block_t* b, kk_refcount_t rc);
kk_decl_export kk_reuse_t  kk_block_check_drop_reuse(kk_block_t* b, kk_refcount_t rc0, kk_context_t* ctx);

// Dup a reference.
static inline kk_block_t* kk_block_dup(kk_block_t* b) {
  kk_assert_internal(kk_block_is_valid(b));
  const kk_refcount_t rc = kk_block_refcount(b);
  if (kk_unlikely(kk_refcount_is_thread_shared(rc))) {  // (signed)rc < 0 
    return kk_block_check_dup(b, rc);                   // thread-shared or sticky (overflow) ?
  }
  else {
    kk_block_refcount_set(b, rc+1);
    return b;
  }
}

// Drop a reference: decrement the reference count, and if it was 0 drop the children recursively
static inline void kk_block_drop(kk_block_t* b, kk_context_t* ctx) {
  kk_assert_internal(kk_block_is_valid(b));
  const kk_refcount_t rc = kk_block_refcount(b);
  if (kk_refcount_is_unique_or_thread_shared(rc)) {  // (signed)rc <= 0
    kk_block_check_drop(b, rc, ctx);    // thread-shared, sticky (overflowed), or can be freed?
  }
  else {
    kk_block_refcount_set(b, rc-1);
  }
}


// Decrement a reference count, and if it was 0 free the block (without freeing the children)
static inline void kk_block_decref(kk_block_t* b, kk_context_t* ctx) {
  kk_assert_internal(kk_block_is_valid(b));
  const kk_refcount_t rc = b->header.refcount;  
  if (kk_unlikely(kk_refcount_is_unique_or_thread_shared(rc))) {  // (signed)rc <= 0
    kk_block_check_decref(b, rc, ctx);  // thread-shared, sticky (overflowed), or can be freed? 
  }
  else {
    kk_block_refcount_set(b, rc-1);
  } 
}

// Decrement the reference count, and return the memory for reuse if it drops to zero
static inline kk_reuse_t kk_block_drop_reuse(kk_block_t* b, kk_context_t* ctx) {
  kk_assert_internal(kk_block_is_valid(b));
  const kk_refcount_t rc = kk_block_refcount(b);
  if (kk_refcount_is_unique_or_thread_shared(rc)) {   // (signed)rc <= 0
    return kk_block_check_drop_reuse(b, rc, ctx);     // thread-shared, sticky (overflowed), or can be reused?
  }
  else {
    kk_block_refcount_set(b, rc-1);
    return kk_reuse_null;
  }
}


static inline void kk_box_drop(kk_box_t b, kk_context_t* ctx);

// Drop with inlined dropping of children 
static inline void kk_block_dropi(kk_block_t* b, kk_context_t* ctx) {
  kk_assert_internal(kk_block_is_valid(b));
  const kk_refcount_t rc = kk_block_refcount(b);
  if (rc == 0) {
    const kk_ssize_t scan_fsize = kk_block_scan_fsize(b);
    for (kk_ssize_t i = 0; i < scan_fsize; i++) {
      kk_box_drop(kk_block_field(b, i), ctx);
    }
    kk_block_free(b);
  }
  else if (kk_unlikely(kk_refcount_is_thread_shared(rc))) {  // (signed)rc < 0
    kk_block_check_drop(b, rc, ctx);                         // thread-share or sticky (overflowed) ?    
  }
  else {
    kk_block_refcount_set(b, rc-1);
  }
}

// Decrement the reference count, and return the memory for reuse if it drops to zero (with inlined dropping)
static inline kk_reuse_t kk_block_dropi_reuse(kk_block_t* b, kk_context_t* ctx) {
  kk_assert_internal(kk_block_is_valid(b));
  const kk_refcount_t rc = kk_block_refcount(b);
  if (rc == 0) {
    kk_ssize_t scan_fsize = kk_block_scan_fsize(b);
    for (kk_ssize_t i = 0; i < scan_fsize; i++) {
      kk_box_drop(kk_block_field(b, i), ctx);
    }
    return b;
  }
  else {
    kk_block_drop(b, ctx);
    return kk_reuse_null;
  }
}

// Drop with known scan size 
static inline void kk_block_dropn(kk_block_t* b, kk_ssize_t scan_fsize, kk_context_t* ctx) {
  kk_assert_internal(kk_block_is_valid(b));
  const kk_refcount_t rc = kk_block_refcount(b);
  if (rc == 0) {                 // note: assume two's complement
    kk_assert_internal(scan_fsize == kk_block_scan_fsize(b));
    for (kk_ssize_t i = 0; i < scan_fsize; i++) {
      kk_box_drop(kk_block_field(b, i), ctx);
    }
    kk_block_free(b);
  }
  else if (kk_unlikely(kk_refcount_is_thread_shared(rc))) {  // (signed)rc < 0
    kk_block_check_drop(b, rc, ctx);                         // thread-shared, sticky (overflowed)?
  }
  else {
    kk_block_refcount_set(b, rc-1);
  }
}



// Drop-reuse with known scan size
static inline kk_reuse_t kk_block_dropn_reuse(kk_block_t* b, kk_ssize_t scan_fsize, kk_context_t* ctx) {
  kk_assert_internal(kk_block_is_valid(b));
  const kk_refcount_t rc = kk_block_refcount(b);
  if (rc == 0) {                 
    kk_assert_internal(kk_block_scan_fsize(b) == scan_fsize);
    for (kk_ssize_t i = 0; i < scan_fsize; i++) {
      kk_box_drop(kk_block_field(b, i), ctx);
    }
    return b;
  }
  else if (kk_unlikely(kk_refcount_is_thread_shared(rc))) {  // (signed)rc < 0
    kk_block_check_drop(b, rc, ctx);                         // thread-shared or sticky (overflowed)?
    return kk_reuse_null;
  }
  else {
    kk_block_refcount_set(b, rc-1);
    return kk_reuse_null;
  }
}


static inline void kk_block_drop_assert(kk_block_t* b, kk_tag_t tag, kk_context_t* ctx) {
  KK_UNUSED_INTERNAL(tag);
  kk_assert_internal(kk_block_tag(b) == tag || kk_block_tag(b) == KK_TAG_BOX_ANY);
  kk_block_drop(b,ctx);
}

static inline kk_block_t* kk_block_dup_assert(kk_block_t* b, kk_tag_t tag) {
  KK_UNUSED_INTERNAL(tag);
  kk_assert_internal(kk_block_tag(b) == tag || kk_block_tag(b) == KK_TAG_BOX_ANY);
  return kk_block_dup(b);
}

static inline void kk_reuse_drop(kk_reuse_t r) {
  if (r != NULL) {
    kk_assert_internal(kk_block_is_unique(r));
    kk_free(r);
  }
}


/*--------------------------------------------------------------------------------------
  Datatype and Constructor macros
  We use:
  - basetype      For a pointer to the base type of a heap allocated constructor.
                  Datatypes without singletons are always a basetype.
  - datatype      For a regular datatypes that can have singletons.
  - constructor   For a pointer to a heap allocated constructor (whose first field
                  is `_base` and points to the base type as a `basetype`
--------------------------------------------------------------------------------------*/

//#define kk_basetype_tag(v)                     (kk_block_tag(&((v)->_block)))
#define kk_basetype_has_tag(v,t)               (kk_block_has_tag(&((v)->_block),t))
#define kk_basetype_is_unique(v)               (kk_block_is_unique(&((v)->_block)))
#define kk_basetype_as(tp,v)                   (kk_block_as(tp,&((v)->_block)))
#define kk_basetype_free(v)                    (kk_block_free(&((v)->_block)))
#define kk_basetype_decref(v,ctx)              (kk_block_decref(&((v)->_block),ctx))
#define kk_basetype_dup_as(tp,v)               ((tp)kk_block_dup(&((v)->_block)))
#define kk_basetype_drop(v,ctx)                (kk_block_dropi(&((v)->_block),ctx))
#define kk_basetype_dropn_reuse(v,n,ctx)       (kk_block_dropn_reuse(&((v)->_block),n,ctx))
#define kk_basetype_dropn(v,n,ctx)             (kk_block_dropn(&((v)->_block),n,ctx))
#define kk_basetype_reuse(v)                   (&((v)->_block))

#define kk_basetype_as_assert(tp,v,tag)        (kk_block_assert(tp,&((v)->_block),tag))
#define kk_basetype_drop_assert(v,tag,ctx)     (kk_block_drop_assert(&((v)->_block),tag,ctx))
#define kk_basetype_dup_assert(tp,v,tag)       ((tp)kk_block_dup_assert(&((v)->_block),tag))

#define kk_constructor_tag(v)                  (kk_basetype_tag(&((v)->_base)))
#define kk_constructor_is_unique(v)            (kk_basetype_is_unique(&((v)->_base)))
#define kk_constructor_free(v)                 (kk_basetype_free(&((v)->_base)))
#define kk_constructor_dup_as(tp,v)            (kk_basetype_dup_as(tp, &((v)->_base)))
#define kk_constructor_drop(v,ctx)             (kk_basetype_drop(&((v)->_base),ctx))
#define kk_constructor_dropn_reuse(v,n,ctx)    (kk_basetype_dropn_reuse(&((v)->_base),n,ctx))

#define kk_value_dup(v)                        (v)
#define kk_value_drop(v,ctx)                   (void)
#define kk_value_drop_reuse(v,ctx)             (kk_reuse_null)


/*----------------------------------------------------------------------
  Datatypes
----------------------------------------------------------------------*/

// create a singleton
static inline kk_decl_const kk_datatype_t kk_datatype_from_tag(kk_tag_t t) {
  kk_datatype_t d = { (((kk_uintf_t)t)<<2 | 1) };
  return d;
}

static inline kk_decl_const kk_datatype_t kk_datatype_from_ptr(kk_ptr_t p) {
  kk_datatype_t d = { (uintptr_t)p };
  return d;
}

static inline kk_decl_const bool kk_datatype_eq(kk_datatype_t x, kk_datatype_t y) {
  return (x.dbox == y.dbox);
}

static inline kk_decl_const bool kk_datatype_is_ptr(kk_datatype_t d) {
  return ((((kk_uintf_t)d.dbox)&1) == 0);
}

static inline kk_decl_const  bool kk_datatype_is_singleton(kk_datatype_t d) {
  return ((((kk_uintf_t)d.dbox)&1) == 1);
}

static inline kk_decl_pure kk_tag_t kk_datatype_tag(kk_datatype_t d) {
  if (kk_datatype_is_ptr(d)) {
    return kk_block_tag((kk_ptr_t)d.dbox);
  }
  else {
    return (kk_tag_t)(((kk_uintf_t)d.dbox) >> 2);
  }
}

static inline kk_decl_pure bool kk_datatype_has_tag(kk_datatype_t d, kk_tag_t t) {
  if (kk_datatype_is_ptr(d)) {
    return (kk_block_tag((kk_ptr_t)d.dbox) == t); 
  }
  else {
    return (d.dbox == kk_datatype_from_tag(t).dbox);  // todo: optimize if sizeof(kk_uintf_t) < sizeof(uintptr_t) ?
  }
}

static inline kk_decl_pure bool kk_datatype_has_ptr_tag(kk_datatype_t d, kk_tag_t t) {
  return (kk_datatype_is_ptr(d) && kk_block_tag((kk_ptr_t)d.dbox) == t);
}

static inline kk_decl_pure bool kk_datatype_has_singleton_tag(kk_datatype_t d, kk_tag_t t) {
  return (d.dbox == kk_datatype_from_tag(t).dbox);  // todo: optimize if sizeof(kk_uintf_t) < sizeof(uintptr_t) ?  
}


static inline kk_decl_const kk_block_t* kk_datatype_as_ptr(kk_datatype_t d) {
  kk_assert_internal(kk_datatype_is_ptr(d));
  return (kk_ptr_t)d.dbox;
}


static inline bool kk_decl_pure kk_datatype_is_unique(kk_datatype_t d) {
  kk_assert_internal(kk_datatype_is_ptr(d)); 
  //return (kk_datatype_is_ptr(d) && kk_block_is_unique(kk_datatype_as_ptr(d)));
  return kk_block_is_unique(kk_datatype_as_ptr(d));
}

static inline kk_datatype_t kk_datatype_dup(kk_datatype_t d) {
  if (kk_datatype_is_ptr(d)) { kk_block_dup(kk_datatype_as_ptr(d)); }
  return d;
}

static inline void kk_datatype_drop(kk_datatype_t d, kk_context_t* ctx) {
  if (kk_datatype_is_ptr(d)) { kk_block_drop(kk_datatype_as_ptr(d),ctx); }
}

static inline void kk_datatype_dropn(kk_datatype_t d, kk_ssize_t scan_fsize, kk_context_t* ctx) {
  kk_assert_internal(kk_datatype_is_ptr(d));
  kk_assert_internal(scan_fsize > 0);
  kk_block_dropn(kk_datatype_as_ptr(d), scan_fsize, ctx);
}

static inline kk_datatype_t kk_datatype_dup_assert(kk_datatype_t d, kk_tag_t t) {
  KK_UNUSED_INTERNAL(t);
  kk_assert_internal(kk_datatype_has_tag(d, t));
  return kk_datatype_dup(d);
}

static inline void kk_datatype_drop_assert(kk_datatype_t d, kk_tag_t t, kk_context_t* ctx) {
  KK_UNUSED_INTERNAL(t);
  kk_assert_internal(kk_datatype_has_tag(d, t));
  kk_datatype_drop(d, ctx);
}

static inline kk_reuse_t kk_datatype_dropn_reuse(kk_datatype_t d, kk_ssize_t scan_fsize, kk_context_t* ctx) {
  kk_assert_internal(kk_datatype_is_ptr(d));
  if (kk_unlikely(kk_datatype_is_singleton(d))) {
    return kk_reuse_null;
  }
  else {
    return kk_block_dropn_reuse(kk_datatype_as_ptr(d), scan_fsize, ctx);
  }
}

static inline kk_reuse_t kk_datatype_reuse(kk_datatype_t d) {
  kk_assert_internal(!kk_datatype_is_singleton(d));
  return kk_datatype_as_ptr(d);
  /*
  if (kk_datatype_is_singleton(d)) {
    return kk_reuse_null;
  }
  else {
    return kk_datatype_as_ptr(d);
  }
  */
}

static inline void kk_datatype_free(kk_datatype_t d) {
  kk_assert_internal(kk_datatype_is_ptr(d));
  kk_free(kk_datatype_as_ptr(d));
  /*
  if (kk_datatype_is_ptr(d)) {
    kk_free(kk_datatype_as_ptr(d));
  }
  */
}

static inline void kk_datatype_decref(kk_datatype_t d, kk_context_t* ctx) {
  kk_assert_internal(kk_datatype_is_ptr(d));
  kk_block_decref(kk_datatype_as_ptr(d), ctx);
  /*
  if (kk_datatype_is_ptr(d)) {
    kk_block_decref(kk_datatype_as_ptr(d), ctx);
  }
  */
}

#define kk_datatype_from_base(b)               (kk_datatype_from_ptr(&(b)->_block))
#define kk_datatype_from_constructor(b)        (kk_datatype_from_base(&(b)->_base))
#define kk_datatype_as(tp,v)                   (kk_block_as(tp,kk_datatype_as_ptr(v)))
#define kk_datatype_as_assert(tp,v,tag)        (kk_block_assert(tp,kk_datatype_as_ptr(v),tag))


#define kk_define_static_datatype(decl,kk_struct_tp,name,tag) \
  static kk_struct_tp _static_##name = { { KK_HEADER_STATIC(0,tag) } }; \
  decl kk_struct_tp* name = &_static_##name

#define kk_define_static_open_datatype(decl,kk_struct_tp,name,otag) /* ignore otag as it is initialized dynamically */ \
  static kk_struct_tp _static_##name = { { KK_HEADER_STATIC(0,KK_TAG_OPEN) }, &kk__static_string_empty._base }; \
  decl kk_struct_tp* name = &_static_##name


/*----------------------------------------------------------------------
  Reference counting of pattern matches
----------------------------------------------------------------------*/

// The constructor that is matched on is still used; only duplicate the used fields
#define kk_keep_match(con,dups) \
  do dups while(0)

// The constructor that is matched on is dropped:
// 1. if unique, drop the unused fields and free just the constructor block
// 2. otherwise, duplicate the used fields, and drop the constructor
#define kk_drop_match(con,dups,drops,ctx) \
  if (kk_constructor_is_unique(con)) { \
    do drops while(0); kk_free(con); \
  } else { \
    do dups while(0); kk_constructor_drop(con,ctx); \
  }

// The constructor that is matched on may be reused:
// 1. if unique, drop the unused fields and make the constructor block available for reuse
// 2. otherwise, duplicate the used fields, drop the constructor, and don't reuse
#define kk_reuse_match(reuseid,con,dups,drops,ctx) \
  if (kk_constructor_is_unique(con)) { \
    do drops while(0); reuseid = drop_reuse_constructor(conid,ctx); \
  } else { \
    do dups while(0); kk_constructor_drop(con,ctx); reuseid = NULL; \
  }


/*----------------------------------------------------------------------
  Further includes
----------------------------------------------------------------------*/

// The unit type
typedef enum kk_unit_e {
  kk_Unit = 0
} kk_unit_t;



#include "kklib/bits.h"
#include "kklib/box.h"
#include "kklib/integer.h"
#include "kklib/bytes.h"
#include "kklib/string.h"
#include "kklib/random.h"
#include "kklib/os.h"
#include "kklib/thread.h"


/*----------------------------------------------------------------------
  TLD operations
----------------------------------------------------------------------*/

// Get a thread local unique number.
static inline kk_integer_t kk_gen_unique(kk_context_t* ctx) {
  kk_integer_t u = ctx->unique;
  ctx->unique = kk_integer_inc(kk_integer_dup(u),ctx);
  return u;
}

kk_decl_export kk_string_t kk_get_host(kk_context_t* ctx); 
kk_decl_export void kk_fatal_error(int err, const char* msg, ...);
kk_decl_export void kk_warning_message(const char* msg, ...);
kk_decl_export void kk_info_message(const char* msg, ...);

static inline void kk_unsupported_external(const char* msg) {
  kk_fatal_error(ENOSYS, "unsupported external: %s", msg);
}




/*--------------------------------------------------------------------------------------
  Value tags
--------------------------------------------------------------------------------------*/

// Tag for value types is always an integer
typedef kk_integer_t kk_value_tag_t;

#define kk_value_tag(tag) (kk_integer_from_small(tag))   

static inline kk_decl_const bool kk_value_tag_eq(kk_value_tag_t x, kk_value_tag_t y) {
  // note: x or y may be box_any so don't assert they are smallints
  return (_kk_integer_value(x) == _kk_integer_value(y));
}

/*--------------------------------------------------------------------------------------
  Optimized support for maybe<a> datatypes
--------------------------------------------------------------------------------------*/
static inline kk_box_t kk_box_Nothing(void) {
  return kk_datatype_box( kk_datatype_from_tag(KK_TAG_NOTHING) );
}

static inline bool kk_box_is_Nothing(kk_box_t b) {
  return (b.box == kk_datatype_from_tag(KK_TAG_NOTHING).dbox);
}

static inline bool kk_box_is_Just(kk_box_t b) {
  return (kk_box_is_ptr(b) && kk_block_has_tag(kk_ptr_unbox(b), KK_TAG_JUST));
}

static inline bool kk_box_is_maybe(kk_box_t b) {
  return (kk_box_is_Just(b) || kk_box_is_Nothing(b));
}

typedef struct kk_just_s {  
  struct kk_block_s _block;
  kk_box_t          value;   
} kk_just_t;

static inline kk_box_t kk_unbox_Just_block( kk_block_t* b, kk_context_t* ctx ) {
  kk_assert_internal(kk_block_has_tag(b,KK_TAG_JUST));
  kk_just_t* just = kk_block_as(kk_just_t*,b);
  kk_box_t res = just->value;        
  if (ctx != NULL) {
    if (kk_basetype_is_unique(just)) {
      kk_basetype_free(just);  
    }
    else {
      kk_box_dup(res);
      kk_basetype_decref(just, ctx);
    }
  }
  return res;
}

static inline kk_box_t kk_unbox_Just( kk_box_t b, kk_context_t* ctx ) {
  if (kk_box_is_ptr(b)) {
    kk_block_t* bl = kk_ptr_unbox(b);
    if (kk_unlikely(kk_block_has_tag(bl, KK_TAG_JUST))) {
      return kk_unbox_Just_block(bl,ctx);
    }
  }
  // if ctx==NULL we should not change refcounts, if ctx!=NULL we consume the b
  return b;
}

static inline kk_box_t kk_box_Just( kk_box_t b, kk_context_t* ctx ) {
  if (kk_likely(!kk_box_is_maybe(b))) {
    return b;
  }
  else {
    kk_just_t* just = kk_block_alloc_as(kk_just_t, 1, KK_TAG_JUST, ctx);
    just->value = b;
    return kk_basetype_box(just);
  }
}



static inline kk_datatype_t kk_datatype_as_Just(kk_box_t b) {
  kk_assert_internal(!kk_box_is_maybe(b));
  return kk_datatype_unbox(b);
}

static inline kk_box_t kk_datatype_unJust(kk_datatype_t d, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  kk_assert_internal(!kk_datatype_has_singleton_tag(d,KK_TAG_NOTHING));
  if (kk_datatype_is_ptr(d)) {
    kk_block_t* b = kk_datatype_as_ptr(d);
    if (kk_block_has_tag(b,KK_TAG_JUST)) {
      return kk_block_field(b,0);
    }
  }
  return kk_datatype_box(d);
}

/*--------------------------------------------------------------------------------------
  Functions
--------------------------------------------------------------------------------------*/

#define kk_function_as(tp,fun)                     kk_basetype_as_assert(tp,fun,KK_TAG_FUNCTION)
#define kk_function_alloc_as(tp,scan_fsize,ctx)    kk_block_alloc_as(tp,scan_fsize,KK_TAG_FUNCTION,ctx)
#define kk_function_call(restp,argtps,f,args)      ((restp(*)argtps)(kk_cfun_ptr_unbox(f->fun)))args
#define kk_define_static_function(name,cfun,ctx) \
  static struct kk_function_s _static_##name = { { KK_HEADER_STATIC(0,KK_TAG_FUNCTION) }, { ~KUP(0) } }; /* must be box_null */ \
  kk_function_t name = &_static_##name; \
  if (kk_box_eq(name->fun,kk_box_null)) { name->fun = kk_cfun_ptr_box((kk_cfun_ptr_t)&cfun,ctx); }  // initialize on demand so it can be boxed properly



kk_function_t kk_function_id(kk_context_t* ctx);
kk_function_t kk_function_null(kk_context_t* ctx);

static inline kk_decl_pure kk_function_t kk_function_unbox(kk_box_t v) {
  return kk_basetype_unbox_as_assert(kk_function_t, v, KK_TAG_FUNCTION);
}

static inline kk_decl_pure kk_box_t kk_function_box(kk_function_t d) {
  return kk_basetype_box(d);
}

static inline kk_decl_pure bool kk_function_is_unique(kk_function_t f) {
  return kk_block_is_unique(&f->_block);
}

static inline void kk_function_drop(kk_function_t f, kk_context_t* ctx) {
  kk_basetype_drop_assert(f, KK_TAG_FUNCTION, ctx);
}

static inline kk_function_t kk_function_dup(kk_function_t f) {
  return kk_basetype_dup_assert(kk_function_t, f, KK_TAG_FUNCTION);
}



/*--------------------------------------------------------------------------------------
  Vector
--------------------------------------------------------------------------------------*/

typedef struct kk_vector_large_s {  // always use a large block for a vector so the offset to the elements is fixed
  struct kk_block_large_s _base;
  kk_box_t                vec[1];               // vec[(large_)scan_fsize - 1]
} *kk_vector_large_t;


static inline kk_decl_const kk_vector_t kk_vector_empty(void) {
  return kk_datatype_from_tag((kk_tag_t)1);
}

static inline kk_decl_pure kk_vector_large_t kk_vector_as_large_borrow(kk_vector_t v) {
  if (kk_datatype_is_singleton(v)) {
    return NULL;
  }
  else {
    return kk_datatype_as_assert(kk_vector_large_t, v, KK_TAG_VECTOR);
  }
}

static inline void kk_vector_drop(kk_vector_t v, kk_context_t* ctx) {
  kk_datatype_drop(v, ctx);
}

static inline kk_vector_t kk_vector_dup(kk_vector_t v) {
  return kk_datatype_dup(v);
}

static inline kk_vector_t kk_vector_alloc_uninit(kk_ssize_t length, kk_box_t** buf, kk_context_t* ctx) {
  if (kk_unlikely(length<=0)) {
    if (buf != NULL) *buf = NULL;
    return kk_vector_empty();
  }
  else {
    kk_vector_large_t v = (kk_vector_large_t)kk_block_large_alloc(
        kk_ssizeof(struct kk_vector_large_s) + (length-1)*kk_ssizeof(kk_box_t),  // length-1 as the vector_large_s already includes one element 
        length + 1, // +1 to include the kk_large_scan_fsize field itself 
        KK_TAG_VECTOR, ctx);
    if (buf != NULL) *buf = &v->vec[0];
    return kk_datatype_from_base(&v->_base);
  }
}

kk_decl_export void        kk_vector_init_borrow(kk_vector_t _v, kk_ssize_t start, kk_box_t def, kk_context_t* ctx);
kk_decl_export kk_vector_t kk_vector_realloc(kk_vector_t vec, kk_ssize_t newlen, kk_box_t def, kk_context_t* ctx);
kk_decl_export kk_vector_t kk_vector_copy(kk_vector_t vec, kk_context_t* ctx);

static inline kk_vector_t kk_vector_alloc(kk_ssize_t length, kk_box_t def, kk_context_t* ctx) {
  kk_vector_t v = kk_vector_alloc_uninit(length, NULL, ctx);
  kk_vector_init_borrow(v, 0, def, ctx);
  return v;
}

static inline kk_box_t* kk_vector_buf_borrow(kk_vector_t vd, kk_ssize_t* len) {
  kk_vector_large_t v = kk_vector_as_large_borrow(vd);
  if (kk_unlikely(v==NULL)) {
    if (len != NULL) *len = 0;
    return NULL;
  }
  else {
    if (len != NULL) {
      *len = (kk_ssize_t)kk_intf_unbox(v->_base.large_scan_fsize) - 1;  // exclude the large scan_fsize field itself
      kk_assert_internal(*len + 1 == kk_block_scan_fsize(&v->_base._block));
      kk_assert_internal(*len > 0);
    }
    return &(v->vec[0]);
  }
}

static inline kk_decl_pure kk_ssize_t kk_vector_len_borrow(const kk_vector_t v) {
  kk_ssize_t len;
  kk_vector_buf_borrow(v, &len);
  return len;
}

static inline kk_ssize_t kk_vector_len(const kk_vector_t v, kk_context_t* ctx) {
  kk_ssize_t len = kk_vector_len_borrow(v);
  kk_vector_drop(v, ctx);
  return len;
}

static inline kk_box_t kk_vector_at_borrow(const kk_vector_t v, kk_ssize_t i) {
  kk_assert(i < kk_vector_len_borrow(v));
  kk_box_t res = kk_box_dup(kk_vector_buf_borrow(v, NULL)[i]);
  return res;
}

static inline kk_decl_const kk_box_t kk_vector_box(kk_vector_t v, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  return kk_datatype_box(v);
}

static inline kk_decl_const kk_vector_t kk_vector_unbox(kk_box_t v, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  return kk_datatype_unbox(v);
}


 
/*--------------------------------------------------------------------------------------
  References
--------------------------------------------------------------------------------------*/
typedef struct kk_ref_s {
  kk_block_t         _block;
  _Atomic(uintptr_t) value;   // kk_box_t
} *kk_ref_t;

kk_decl_export kk_box_t  kk_ref_get_thread_shared(kk_ref_t r, kk_context_t* ctx);
kk_decl_export kk_box_t  kk_ref_swap_thread_shared_borrow(kk_ref_t r, kk_box_t value);
kk_decl_export kk_unit_t kk_ref_vector_assign_borrow(kk_ref_t r, kk_integer_t idx, kk_box_t value, kk_context_t* ctx);

static inline kk_decl_const kk_box_t kk_ref_box(kk_ref_t r, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  return kk_basetype_box(r);
}

static inline kk_decl_const kk_ref_t kk_ref_unbox(kk_box_t b, kk_context_t* ctx) {
  KK_UNUSED(ctx);
  return kk_basetype_unbox_as_assert(kk_ref_t, b, KK_TAG_REF);
}

static inline void kk_ref_drop(kk_ref_t r, kk_context_t* ctx) {
  kk_basetype_drop_assert(r, KK_TAG_REF, ctx);
}

static inline kk_ref_t kk_ref_dup(kk_ref_t r) {
  return kk_basetype_dup_assert(kk_ref_t, r, KK_TAG_REF);
}

static inline kk_ref_t kk_ref_alloc(kk_box_t value, kk_context_t* ctx) {
  kk_ref_t r = kk_block_alloc_as(struct kk_ref_s, 1, KK_TAG_REF, ctx);
  kk_atomic_store_relaxed(&r->value,value.box);
  return r;
}

static inline kk_box_t kk_ref_get(kk_ref_t r, kk_context_t* ctx) {
  if (kk_likely(!kk_block_is_thread_shared(&r->_block))) {
    // fast path
    kk_box_t b; b.box = kk_atomic_load_relaxed(&r->value);
    kk_box_dup(b);
    kk_ref_drop(r,ctx);    // TODO: make references borrowed (only get left)
    return b;
  }
  else {
    // thread shared
    return kk_ref_get_thread_shared(r,ctx);
  }  
}

static inline kk_box_t kk_ref_swap_borrow(kk_ref_t r, kk_box_t value) {
  if (kk_likely(!kk_block_is_thread_shared(&r->_block))) {
    // fast path
    kk_box_t b; b.box = kk_atomic_load_relaxed(&r->value);
    kk_atomic_store_relaxed(&r->value, value.box);
    return b;
  }
  else {
    // thread shared
    return kk_ref_swap_thread_shared_borrow(r, value);
  }
}


static inline kk_unit_t kk_ref_set_borrow(kk_ref_t r, kk_box_t value, kk_context_t* ctx) {
  kk_box_t b = kk_ref_swap_borrow(r, value);
  kk_box_drop(b, ctx);
  return kk_Unit;
}

// In Koka we can constrain the argument of f to be a local-scope reference.
static inline kk_box_t kk_ref_modify(kk_ref_t r, kk_function_t f, kk_context_t* ctx) {
  return kk_function_call(kk_box_t,(kk_function_t,kk_ref_t,kk_context_t*),f,(f,r,ctx));
}

/*--------------------------------------------------------------------------------------
  kk_Unit
--------------------------------------------------------------------------------------*/

static inline kk_decl_const kk_box_t kk_unit_box(kk_unit_t u) {
  return kk_intf_box((kk_intf_t)u);
}

static inline kk_decl_const kk_unit_t kk_unit_unbox(kk_box_t u) {
  KK_UNUSED_INTERNAL(u);
  kk_assert_internal( kk_intf_unbox(u) == (kk_intf_t)kk_Unit || kk_box_is_any(u));
  return kk_Unit; // (kk_unit_t)kk_enum_unbox(u);
}



#endif // include guard
