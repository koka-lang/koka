#pragma once
#ifndef KKLIB_H
#define KKLIB_H 
/*---------------------------------------------------------------------------
  Copyright 2020-2022, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

#define KKLIB_BUILD         101     // modify on changes to trigger recompilation  
// #define KK_DEBUG_FULL       1    // set to enable full internal debug checks

// Includes
#define WIN32_LEAN_AND_MEAN          // reduce windows includes
#define _POSIX_C_SOURCE     200809L  // make posix definitions visible
#define _DARWIN_C_SOURCE    200809L  // make darwin definitions visible
#define _XOPEN_SOURCE       700      // make xopen (posix 2008) definitions visible
#define _FILE_OFFSET_BITS   64
#define _TIME_BITS          64
#if !defined(_WIN32)
#ifndef _BSD_SOURCE
#define _BSD_SOURCE 
#endif
#ifndef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE   
#endif
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#endif

#include <limits.h>           // LONG_MAX, ...
#include <stddef.h>           // size_t
#include <stdbool.h>          // bool
#include <stdint.h>           // int64_t, ...
#include <inttypes.h>         // PRIx64, ...
#include <assert.h>           // assert
#include <errno.h>            // ENOSYS, ...
#include <stdlib.h>           // malloc, abort, ...
#include <string.h>           // strlen, memcpy, ...
#include <math.h>             // isnan, isfinite, ...
#include <stdio.h>            // FILE*, printf, ...

#include "kklib/platform.h"   // Platform abstractions and portability definitions
#include "kklib/atomic.h"     // Atomic operations


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
  KK_TAG_NOTHING,     // used to avoid allocation for unnested maybe-like types
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
// Negative reference counts use atomic increment/decrement (for thread shared objects).
// (Reference counts are always 32-bit (even on 64-bit) platforms but get "sticky" if
//  they overflow into the negative range and in such case we never free the object, see `refcount.c`)
typedef int32_t kk_refcount_t;

// Are there (possibly) references from other threads? (includes static variables)
static inline bool kk_refcount_is_thread_shared(kk_refcount_t rc) {
  return (rc < 0);
}

// Is the reference unique, or are there (possibly) references from other threads? (includes static variables)
static inline bool kk_refcount_is_unique_or_thread_shared(kk_refcount_t rc) {
  return (rc <= 0);
}

// Increment a positive reference count. To avoid UB on overflow, use unsigned addition.
static inline kk_refcount_t kk_refcount_inc(kk_refcount_t rc) {
  kk_assert_internal(rc >= 0);
  return (kk_refcount_t)((uint32_t)rc + 1);
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
#define KK_HEADER(scan_fsize,tag)         { scan_fsize, 0, tag, KK_ATOMIC_VAR_INIT(0) }         // start with unique refcount 
#define KK_HEADER_STATIC(scan_fsize,tag)  { scan_fsize, 0, tag, KK_ATOMIC_VAR_INIT(INT32_MIN) } // start with a stuck refcount (RC_STUCK)

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
// The least-significant bit is clear for heap pointers (`kk_ptr_t == kk_block_t*`), while the bit is set for values.
// See `box.h` for definitions.
typedef struct kk_box_s {
  kk_intb_t box;
} kk_box_t;
 
// An integer is either a small int (as: 4*i + 1) or a `kk_bigint_t*` pointer. Isomorphic with boxed values.
// See `integer.h` for definitions.
typedef struct kk_integer_s {
  kk_intb_t ibox;
} kk_integer_t;

// A general datatype with constructors and singletons is either
// an enumeration (with the lowest bit set as: 4*tag + 1) or a `kk_block_t*` pointer.
// Isomorphic with boxed values. 
typedef struct kk_datatype_s {
  kk_intb_t dbox;
} kk_datatype_t;

// Typedef to signify datatypes that have no singletons (and are always a pointer)
typedef kk_datatype_t kk_datatype_ptr_t;

// boxed forward declarations
static inline kk_intf_t kk_intf_unbox(kk_box_t b);
static inline kk_box_t  kk_intf_box(kk_intf_t i);
static inline bool      kk_box_is_any(kk_box_t b);

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
  if kk_likely(sfsize != KK_SCAN_FSIZE_MAX) return sfsize;
  const kk_block_large_t* bl = (const kk_block_large_t*)b;
  return (kk_ssize_t)kk_intf_unbox(bl->large_scan_fsize);
}

static inline void kk_block_set_invalid(kk_block_t* b) {
#ifdef KK_DEBUG_FULL
  const kk_ssize_t scan_fsize = kk_block_scan_fsize(b);
  const kk_ssize_t bsize = (sizeof(kk_box_t) * scan_fsize) + (b->header.scan_fsize == KK_SCAN_FSIZE_MAX ? sizeof(kk_block_large_t) : sizeof(kk_block_t));
  uint8_t* p = (uint8_t*)b;
  for (kk_ssize_t i = 0; i < bsize; i++) {
    p[i] = 0xDF;
  }
#else
  kk_unused(b);
#endif
}

static inline kk_decl_pure bool kk_block_is_valid(kk_block_t* b) {
  return (b != NULL && ((uintptr_t)b & 1) == 0 && *((uint64_t*)b) != KK_U64(0xDFDFDFDFDFDFDFDF) // already freed!
    && (b->header.tag > KK_TAG_MAX || b->header.tag < 0xFF)
    && (b->header._field_idx <= b->header.scan_fsize)
    );
}


static inline kk_decl_pure kk_refcount_t kk_block_refcount(const kk_block_t* b) {
  return kk_atomic_load_relaxed(&b->header.refcount);
}

static inline void kk_block_refcount_set(kk_block_t* b, kk_refcount_t rc) {
  return kk_atomic_store_relaxed(&b->header.refcount, rc);
}

static inline kk_decl_pure bool kk_block_is_unique(const kk_block_t* b) {
  return kk_likely(kk_block_refcount(b) == 0);
}

static inline kk_decl_pure bool kk_block_is_thread_shared(const kk_block_t* b) {
  return kk_unlikely(kk_refcount_is_thread_shared(kk_block_refcount(b)));
}

// Used to generically inspect the scannable fields of an object as used 
// to recursively free data, or mark as shared. This must overlay with
// any heap block and if pointer compression is used we need to use packed
// structures to avoid any potential padding in a struct (at least up to
// the first `scan_fsize` fields)
typedef struct kk_block_fields_s {
  kk_block_t _block;
  kk_box_t   fields[1];
} kk_block_fields_t;

static inline kk_decl_pure kk_box_t kk_block_field(kk_block_t* b, kk_ssize_t index) {
  kk_assert_internal(kk_block_is_valid(b));
  kk_block_fields_t* bf = (kk_block_fields_t*)b;  // must overlap with datatypes with scanned fields.
  return bf->fields[index];
}

static inline void kk_block_field_set(kk_block_t* b, kk_ssize_t index, kk_box_t v) {
  kk_block_fields_t* bf = (kk_block_fields_t*)b;  // must overlap with datatypes with scanned fields.
  bf->fields[index] = v;
}

static inline kk_decl_pure kk_box_t* kk_block_field_address(kk_block_t* b, kk_ssize_t index) {
  kk_block_fields_t* bf = (kk_block_fields_t*)b;  // must overlap with datatypes with scanned fields.
  return &bf->fields[index];
}

static inline kk_decl_pure uint8_t kk_block_field_idx(const kk_block_t* b) {
  return b->header._field_idx;
}

static inline void kk_block_field_idx_set(kk_block_t* b, uint8_t idx ) {
  kk_assert_internal(idx <= b->header.scan_fsize); // allow +1 for trmc context paths
  b->header._field_idx = idx;
}


/*--------------------------------------------------------------------------------------
  The thread local context as `kk_context_t`
  This is passed by the code generator as an argument to every function so it can
  be (usually) accessed efficiently through a register.
--------------------------------------------------------------------------------------*/
#ifdef KK_MIMALLOC
  #if !defined(MI_MAX_ALIGN_SIZE)
    #define MI_MAX_ALIGN_SIZE  KK_INTPTR_SIZE
  #endif
  #if !defined(MI_DEBUG) && defined(KK_DEBUG_FULL)
    #define MI_DEBUG  3
  #endif
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
struct kk_function_s {
  kk_block_t  _block;
  kk_box_t    fun;        // kk_kkfun_ptr_t
  // followed by free variables
}; 
typedef kk_datatype_ptr_t kk_function_t;

// A vector is an array of boxed values, or an empty singleton
typedef kk_datatype_t kk_vector_t;

// Strong random number context (using chacha20)
struct kk_random_ctx_s;


// High precision duration as `seconds + (attoseconds * 1e-18)`. 
// (attosecond precision with a range of about 300 billion years)
typedef int64_t kk_secs_t;
typedef int64_t kk_asecs_t;
typedef struct kk_duration_s {
  kk_secs_t  seconds;
  kk_asecs_t attoseconds;  // always >= 0, use `kk_duration_norm` to normalize
} kk_duration_t;  


// Box any is used when yielding
struct kk_box_any_s {
  kk_block_t    _block;
  kk_integer_t  _unused;
};
typedef kk_datatype_t kk_box_any_t;

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
  kk_intf_t     conts_count;     // number of continuations in `conts`
  kk_function_t conts[KK_YIELD_CONT_MAX]; // fixed array of continuations. The final continuation `k` is
                                          // composed as `fN ○ ... ○ f2 ○ f1` if `conts = { f1, f2, ..., fN }`
                                          // if the array becomes full, a fresh array is allocated and the first
                                          // entry points to its composition.
} kk_yield_t;
     
// The thread local context.
// The fields `yielding`, `heap` and `evv` should come first for efficiency
typedef struct kk_context_s {
  int8_t            yielding;         // are we yielding to a handler? 0:no, 1:yielding, 2:yielding_final (e.g. exception) // put first for efficiency
  const kk_heap_t   heap;             // the (thread-local) heap to allocate in; todo: put in a register?
  const kk_addr_t   heap_mid;         // mid point of the reserved heap address space (or 0 if the heap is not compressed)
  const void*       heap_start;       // start of the heap space (or NULL if the heap is not compressed)
  kk_datatype_ptr_t evv;              // the current evidence vector for effect handling: vector for size 0 and N>1, direct evidence for one element vector
  kk_yield_t        yield;            // inlined yield structure (for efficiency)
  int32_t           marker_unique;    // unique marker generation
  kk_block_t*       delayed_free;     // list of blocks that still need to be freed
  kk_integer_t      unique;           // thread local unique number generation
  size_t            thread_id;        // unique thread id
  kk_box_any_t      kk_box_any;       // used when yielding as a value of any type
  kk_function_t     log;              // logging function
  kk_function_t     out;              // std output
  kk_task_group_t*  task_group;       // task group for managing threads. NULL for the main thread.
  
  struct kk_random_ctx_s* srandom_ctx;// strong random using chacha20, initialized on demand
  kk_ssize_t        argc;             // command line argument count 
  const char**      argv;             // command line arguments
  kk_duration_t     process_start;    // time at start of the process
  int64_t           timer_freq;       // high precision timer frequency
  kk_duration_t     timer_prev;       // last requested timer time
  kk_duration_t     timer_delta;      // applied timer delta (to ensure monotonicity)
  int64_t           time_freq;        // unix time frequency
  kk_duration_t     time_unix_prev;   // last requested unix time
} kk_context_t;

// Get the current (thread local) runtime context (should always equal the `_ctx` parameter)
kk_decl_export kk_context_t* kk_get_context(void);
kk_decl_export void          kk_free_context(void);

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
  int32_t m = ++ctx->marker_unique;               // must return a marker >= 1 so increment first;
  if (m == INT32_MAX) { ctx->marker_unique = 1; } // controlled reset
  return m;
}

kk_decl_export kk_context_t* kk_main_start(int argc, char** argv);
kk_decl_export void          kk_main_end(kk_context_t* ctx);

kk_decl_export void kk_debugger_break(kk_context_t* ctx);
kk_decl_export void kk_fatal_error(int err, const char* msg, ...);
kk_decl_export void kk_warning_message(const char* msg, ...);
kk_decl_export void kk_info_message(const char* msg, ...);
kk_decl_export void kk_unsupported_external(const char* msg);

kk_decl_export kk_datatype_ptr_t kk_evv_empty_singleton(kk_context_t* ctx);


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
  kk_unused(ctx);
  return mi_heap_zalloc(ctx->heap, (size_t)sz);
}

static inline void* kk_realloc(void* p, kk_ssize_t sz, kk_context_t* ctx) {
  kk_unused(ctx);
  return mi_heap_realloc(ctx->heap, p, (size_t)sz);
}

static inline void kk_free(const void* p, kk_context_t* ctx) {
  // mi_unsafe_free_with_threadid((void*)p, ctx->thread_id);
  kk_unused(ctx);
  mi_free((void*)p);
}

static inline void kk_free_local(const void* p, kk_context_t* ctx) {
  kk_free(p,ctx);
}

#define kk_malloc_usable_size(p)  mi_usable_size(p)

#else
static inline void* kk_malloc(kk_ssize_t sz, kk_context_t* ctx) {
  kk_unused(ctx);
  return malloc((size_t)sz);
}

static inline void* kk_malloc_small(kk_ssize_t sz, kk_context_t* ctx) {
  return kk_malloc(sz,ctx);
}

static inline void* kk_zalloc(kk_ssize_t sz, kk_context_t* ctx) {
  kk_unused(ctx);
  return calloc(1, (size_t)sz);
}

static inline void* kk_realloc(void* p, kk_ssize_t sz, kk_context_t* ctx) {
  kk_unused(ctx);
  return realloc(p, (size_t)sz);
}

static inline void kk_free(const void* p, kk_context_t* ctx) {
  kk_unused(ctx);
  free((void*)p);
}

static inline void kk_free_local(const void* p, kk_context_t* ctx) {
  kk_free(p,ctx);
}

#if defined(__linux__) || defined(__GLIBC__)
#include <malloc.h>
#define kk_malloc_usable_size(p)  malloc_usable_size(p)
#elif defined(__APPLE__)
#include <malloc/malloc.h>
#define kk_malloc_usable_size(p)  malloc_size(p)
#elif defined(_MSC_VER)
#include <malloc.h>
#define kk_malloc_usable_size(p)  _msize(p)
#endif

#endif

#if defined(kk_malloc_usable_size)
#define KK_HAS_MALLOC_COPY
static inline void* kk_malloc_copy(const void* p, kk_context_t* ctx) {
  const size_t size = kk_malloc_usable_size(p);
  void* q = kk_malloc(kk_to_ssize_t(size), ctx);
  memcpy(q,p,size);
  return q;
}
#endif

static inline void kk_block_init(kk_block_t* b, kk_ssize_t size, kk_ssize_t scan_fsize, kk_tag_t tag) {
  kk_unused(size);
  kk_assert_internal(scan_fsize >= 0 && scan_fsize < KK_SCAN_FSIZE_MAX);
  kk_header_init(&b->header, scan_fsize, tag);
}

static inline void kk_block_large_init(kk_block_large_t* b, kk_ssize_t size, kk_ssize_t scan_fsize, kk_tag_t tag) {
  kk_unused(size);
  // to optimize for "small" vectors with less than 255 scanable elements, we still set the small scan_fsize
  // for those in the header. This is still duplicated in the large scan_fsize field as it is used for the vector length for example.
  uint8_t bscan_fsize = (scan_fsize >= KK_SCAN_FSIZE_MAX ? KK_SCAN_FSIZE_MAX : (uint8_t)scan_fsize);
  kk_header_init(&b->_block.header, bscan_fsize, tag);
  kk_assert_internal(scan_fsize > 0);
  kk_assert_internal(scan_fsize <= KK_INTF_MAX);
  b->large_scan_fsize = kk_intf_box((kk_intf_t)scan_fsize);  
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
  kk_unused_internal(tag);
  kk_assert_internal(kk_block_tag(b) == tag || kk_block_tag(b) == KK_TAG_BOX_ANY);
  return b;
}

static inline void kk_block_free(kk_block_t* b, kk_context_t* ctx) {
  kk_block_set_invalid(b);
  kk_free(b, ctx);
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
  if kk_unlikely(kk_refcount_is_thread_shared(rc)) {  // (signed)rc < 0 
    return kk_block_check_dup(b, rc);                 // thread-shared or sticky (overflow) ?
  }
  else {
    kk_block_refcount_set(b, kk_refcount_inc(rc));
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
// Note: the way the compiler generates decref instructions, the only time it could become zero
// is if it happened to be thread shared and another thread decremented the count as well.
static inline void kk_block_decref(kk_block_t* b, kk_context_t* ctx) {
  kk_assert_internal(kk_block_is_valid(b));
  const kk_refcount_t rc = b->header.refcount;  
  if kk_unlikely(kk_refcount_is_unique_or_thread_shared(rc)) {  // (signed)rc <= 0
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
    kk_block_free(b,ctx);
  }
  else if kk_unlikely(kk_refcount_is_thread_shared(rc)) {    // (signed)rc < 0
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
    kk_block_free(b,ctx);
  }
  else if kk_unlikely(kk_refcount_is_thread_shared(rc)) {  // (signed)rc < 0
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
  else if kk_unlikely(kk_refcount_is_thread_shared(rc)) {    // (signed)rc < 0
    kk_block_check_drop(b, rc, ctx);                         // thread-shared or sticky (overflowed)?
    return kk_reuse_null;
  }
  else {
    kk_block_refcount_set(b, rc-1);
    return kk_reuse_null;
  }
}


static inline void kk_block_drop_assert(kk_block_t* b, kk_tag_t tag, kk_context_t* ctx) {
  kk_unused_internal(tag);
  kk_assert_internal(kk_block_tag(b) == tag || kk_block_tag(b) == KK_TAG_BOX_ANY);
  kk_block_drop(b,ctx);
}

static inline kk_block_t* kk_block_dup_assert(kk_block_t* b, kk_tag_t tag) {
  kk_unused_internal(tag);
  kk_assert_internal(kk_block_tag(b) == tag || kk_block_tag(b) == KK_TAG_BOX_ANY);
  return kk_block_dup(b);
}

static inline void kk_reuse_drop(kk_reuse_t r, kk_context_t* ctx) {
  if (r != NULL) {
    kk_assert_internal(kk_block_is_unique(r));
    kk_free(r,ctx);
  }
}


/*--------------------------------------------------------------------------------------
  Thread-shared marking (see `refcount.c`)
--------------------------------------------------------------------------------------*/

kk_decl_export void        kk_block_mark_shared(kk_block_t* b, kk_context_t* ctx);
kk_decl_export void        kk_box_mark_shared(kk_box_t b, kk_context_t* ctx);
kk_decl_export void        kk_box_mark_shared_recx(kk_box_t b, kk_context_t* ctx);


/*--------------------------------------------------------------------------------------
  Base type and Constructor macros
  - base_type     For a pointer to the base type of a heap allocated constructor.
  - constructor   For a pointer to a heap allocated constructor (whose first field
                  is `_base` and points to the base type as a `basetype`
--------------------------------------------------------------------------------------*/

#define kk_base_type_has_tag(v,t)               (kk_block_has_tag(&((v)->_block),t))
#define kk_base_type_is_unique(v)               (kk_block_is_unique(&((v)->_block)))
#define kk_base_type_as(tp,v)                   (kk_block_as(tp,&((v)->_block)))
#define kk_base_type_free(v,ctx)                (kk_block_free(&((v)->_block),ctx))
#define kk_base_type_decref(v,ctx)              (kk_block_decref(&((v)->_block),ctx))
#define kk_base_type_dup_as(tp,v)               ((tp)kk_block_dup(&((v)->_block)))
#define kk_base_type_drop(v,ctx)                (kk_block_dropi(&((v)->_block),ctx))
#define kk_base_type_dropn_reuse(v,n,ctx)       (kk_block_dropn_reuse(&((v)->_block),n,ctx))
#define kk_base_type_dropn(v,n,ctx)             (kk_block_dropn(&((v)->_block),n,ctx))
#define kk_base_type_reuse(v)                   (&((v)->_block))
#define kk_base_type_field_idx_set(v,x)         (kk_block_field_idx_set(&((v)->_block),x))

#define kk_base_type_as_assert(tp,v,tag)        (kk_block_assert(tp,&((v)->_block),tag))
#define kk_base_type_drop_assert(v,tag,ctx)     (kk_block_drop_assert(&((v)->_block),tag,ctx))
#define kk_base_type_dup_assert(tp,v,tag)       ((tp)kk_block_dup_assert(&((v)->_block),tag))

#define kk_base_type_unbox_as_assert(tp,b,tag,ctx)  (kk_block_as(tp,kk_block_unbox(b,tag,ctx)))
#define kk_base_type_unbox_as(tp,b,ctx)             ((tp)kk_base_type_as(tp,kk_ptr_unbox(b,ctx),ctx))
#define kk_base_type_box(b,ctx)                     (kk_block_box(&(b)->_block,ctx))    

#define kk_constructor_is_unique(v)             (kk_base_type_is_unique(&((v)->_base)))
#define kk_constructor_free(v,ctx)              (kk_base_type_free(&((v)->_base),ctx))
#define kk_constructor_dup_as(tp,v)             (kk_base_type_dup_as(tp, &((v)->_base)))
#define kk_constructor_drop(v,ctx)              (kk_base_type_drop(&((v)->_base),ctx))
#define kk_constructor_dropn_reuse(v,n,ctx)     (kk_base_type_dropn_reuse(&((v)->_base),n,ctx))
#define kk_constructor_field_idx_set(v,x)       (kk_base_type_field_idx_set(&((v)->_base),x))
#define kk_constructor_unbox_as(tp,b,tag,ctx)   (kk_base_type_unbox_as_assert(tp,b,tag,ctx))
#define kk_constructor_box(b,ctx)               (kk_base_type_box(&(b)->_base),ctx)


/*----------------------------------------------------------------------
  Low-level encoding of small integers (`kk_intf_t`) and pointers
  into a boxed integer `kk_intb_t`.  
----------------------------------------------------------------------*/
// We generally tag boxed values; the least-significant bit is clear for heap pointers (`kk_ptr_t == kk_block_t*`),
// while the bit is set for values.
#define KK_TAG_BITS             (1)
#define KK_TAG_MASK             ((1<<KK_TAG_BITS)-1)
#define KK_TAG_PTR              (0)
#define KK_TAG_VALUE            (1)

static inline bool kk_is_ptr(kk_intb_t i) {
  return ((i & KK_TAG_MASK) == KK_TAG_PTR);
}
static inline bool kk_is_value(kk_intb_t i) {
  return !kk_is_ptr(i);
}

// null values; can be an arbitrary value.
#define kk_value_null      ((~KK_IB(0)&~KK_TAG_MASK)|KK_TAG_VALUE)


// If we assume `intptr_t` aligned pointers in the heap, we can use a larger heap when 
// using pointer compression (by shifting them by `KK_BOX_PTR_SHIFT`).
#if !defined(KK_BOX_PTR_SHIFT)
  #if (KK_INTB_SIZE <= 4)
    // shift by pointer alignment if we have at most 32-bit boxed ints
    #define KK_BOX_PTR_SHIFT   (KK_INTPTR_SHIFT - KK_TAG_BITS)
  #else
    // don't bother with shifting if we have more than 32 bits available
    #define KK_BOX_PTR_SHIFT   (0)
  #endif
#endif

// Without compression, pointer encode/decode is an identity operation.
static inline kk_intb_t kk_ptr_encode(kk_ptr_t p, kk_context_t* ctx) {
  kk_assert_internal(((intptr_t)p & KK_TAG_MASK) == 0);  
  kk_addr_t a;
#if KK_COMPRESS
  #if KK_CHERI
  a = (kk_addr_t)__builtin_cheri_address_get(p);
  #else
  a = (kk_addr_t)p;
  #endif
  #if (KK_INTB_SIZE==4)
  // compress to 32-bit offsets, ctx->heap_mid contains the mid-point in the heap so we can do signed extension
  a = a - ctx->heap_mid;
  #else
  // for 64- or 128-bit we use the address as is (and for 128 bit we assume we locate our heap in the lower 2^63-1 address space)
  kk_unused(ctx);
  #endif  
  #if KK_BOX_PTR_SHIFT > 0
  kk_assert_internal((a & ((1 << KK_BOX_PTR_SHIFT) - 1)) == 0);
  a = kk_sara(a, KK_BOX_PTR_SHIFT);
  #endif 
#else // no compression: |kk_intptr_t| == |kk_addr_t| == |kk_intb_t|
  kk_unused(ctx);
  a = (kk_addr_t)p;
#endif
  kk_assert_internal(a >= KK_INTB_MIN && a <= KK_INTB_MAX);  
  kk_assert_internal((a & KK_TAG_MASK) == 0);
  return ((kk_intb_t)a | KK_TAG_PTR);
}

static inline kk_ptr_t kk_ptr_decode(kk_intb_t b, kk_context_t* ctx) {
  kk_assert_internal(kk_is_ptr(b));
  kk_addr_t a = b;        // may sign-extend  
#if (KK_TAG_PTR != 0)
  a = (a & ~KK_TAG_MASK);
#endif
#if KK_COMPRESS
  #if (KK_BOX_PTR_SHIFT > 0)
  a = kk_shla(a, KK_BOX_PTR_SHIFT);
  #endif
  #if (KK_INTB_SIZE == 4)
  a = a + ctx->heap_mid;
  #else
  kk_unused(ctx);
  #endif  
  #if KK_CHERI
  return (kk_ptr_t)__builtin_cheri_address_set(ctx->heap_start, (vaddr_t)a);
  #else
  return (kk_ptr_t)a;
  #endif
#else // no compression: |kk_intb_t| == |kk_addr_t| == |intptr_t|
  kk_unused(ctx);
  return (kk_ptr_t)a;
#endif
}

// Integer value encoding/decoding. May use smaller integers (`kk_intf_t`)
// then boxed integers if `kk_intb_t` is larger than the natural register size.
#define KK_INTF_BOX_BITS(extra)  (KK_INTF_BITS - KK_TAG_BITS + (extra))
#define KK_INTF_BOX_MAX(extra)   (KK_INTF_MAX >> (KK_TAG_BITS + (extra)))
#define KK_INTF_BOX_MIN(extra)   (-KK_INTF_BOX_MAX(extra) - 1)
#define KK_UINTF_BOX_MAX(extra)  (KK_UINTF_MAX >> (KK_TAG_BITS + (extra)))

static inline kk_intb_t kk_intf_encode(kk_intf_t i, int extra_shift) {
  kk_assert_internal(extra_shift >= 0);
  kk_intb_t b = i; // may sign-extend
  kk_assert_internal(b >= KK_INTF_BOX_MIN(extra_shift) && i <= KK_INTF_BOX_MAX(extra_shift));
  return (kk_shlb(b,KK_TAG_BITS + extra_shift) | KK_TAG_VALUE);
}

static inline kk_intf_t kk_intf_decode(kk_intb_t b, int extra_shift) {
  kk_assert_internal(extra_shift >= 0);
  kk_assert_internal(kk_is_value(b) || b == kk_get_context()->kk_box_any.dbox);
  kk_intb_t i = kk_sarb( b & ~KK_TAG_VALUE, KK_TAG_BITS + extra_shift);  
  kk_assert_internal(i >= KK_INTF_MIN && i <= KK_INTF_MAX);
  return (kk_intf_t)i;
}



/*----------------------------------------------------------------------
  Datatypes
  We use the `_ptr` suffix if it is guaranteed that the datatype
  is a pointer and not a value (singleton).
----------------------------------------------------------------------*/

// create a singleton
static inline kk_decl_const kk_datatype_t kk_datatype_from_tag(kk_tag_t t) {
  kk_datatype_t d = { kk_intf_encode((kk_intf_t)t,1) };
  return d;
}

// create a pointer into the heap
static inline kk_decl_const kk_datatype_t kk_datatype_from_ptr(kk_ptr_t p, kk_context_t* ctx) {
  kk_datatype_t d = { kk_ptr_encode(p, ctx) };
  return d;
}

static inline kk_decl_const bool kk_datatype_eq(kk_datatype_t x, kk_datatype_t y) {
  return (x.dbox == y.dbox);
}

static inline kk_decl_const bool kk_datatype_is_ptr(kk_datatype_t d) {
  return kk_is_ptr(d.dbox);
}

static inline kk_decl_const  bool kk_datatype_is_singleton(kk_datatype_t d) {
  return kk_is_value(d.dbox);
}

static inline kk_decl_const kk_block_t* kk_datatype_as_ptr(kk_datatype_t d, kk_context_t* ctx) {
  kk_assert_internal(kk_datatype_is_ptr(d));
  return kk_ptr_decode(d.dbox,ctx);
}

static inline kk_decl_pure kk_tag_t kk_datatype_ptr_tag(kk_datatype_t d, kk_context_t* ctx) {
  kk_assert_internal(kk_datatype_is_ptr(d));
  return kk_block_tag(kk_datatype_as_ptr(d, ctx));
}

static inline kk_decl_pure kk_tag_t kk_datatype_tag(kk_datatype_t d, kk_context_t* ctx) {
  if (kk_datatype_is_ptr(d)) {
    return kk_datatype_ptr_tag(d, ctx);
  }
  else {
    return (kk_tag_t)kk_intf_decode(d.dbox,1);
  }
}

static inline kk_decl_pure bool kk_datatype_ptr_has_tag(kk_datatype_t d, kk_tag_t t, kk_context_t* ctx) {
  kk_assert_internal(kk_datatype_is_ptr(d));
  return (kk_block_tag(kk_datatype_as_ptr(d, ctx)) == t);
}


static inline kk_decl_pure bool kk_datatype_has_tag(kk_datatype_t d, kk_tag_t t, kk_context_t* ctx) {
  if (kk_datatype_is_ptr(d)) {
    return kk_datatype_ptr_has_tag(d, t, ctx);
  }
  else {
    return (d.dbox == kk_datatype_from_tag(t).dbox); 
  }
}

static inline kk_decl_pure bool kk_datatype_has_ptr_tag(kk_datatype_t d, kk_tag_t t, kk_context_t* ctx) {
  return (kk_datatype_is_ptr(d) && kk_block_tag(kk_datatype_as_ptr(d,ctx)) == t);
}

static inline kk_decl_pure bool kk_datatype_has_singleton_tag(kk_datatype_t d, kk_tag_t t) {
  return (d.dbox == kk_datatype_from_tag(t).dbox);  
}

static inline bool kk_decl_pure kk_datatype_ptr_is_unique(kk_datatype_t d, kk_context_t* ctx) {
  kk_assert_internal(kk_datatype_is_ptr(d));
  //return (kk_datatype_is_ptr(d) && kk_block_is_unique(kk_datatype_as_ptr(d)));
  return kk_block_is_unique(kk_datatype_as_ptr(d,ctx));
}

static inline bool kk_decl_pure kk_datatype_is_unique(kk_datatype_t d, kk_context_t* ctx) {
  return (kk_datatype_is_ptr(d) && kk_block_is_unique(kk_datatype_as_ptr(d,ctx)));
}

static inline kk_datatype_t kk_datatype_ptr_dup(kk_datatype_t d, kk_context_t* ctx) {
  kk_assert_internal(kk_datatype_is_ptr(d));
  kk_block_dup(kk_datatype_as_ptr(d, ctx));
  return d;
}


static inline kk_datatype_t kk_datatype_dup(kk_datatype_t d, kk_context_t* ctx) {
  if (kk_datatype_is_ptr(d)) { 
    kk_datatype_ptr_dup(d,ctx); 
  }
  return d;
}

static inline void kk_datatype_ptr_drop(kk_datatype_t d, kk_context_t* ctx) {
  kk_assert_internal(kk_datatype_is_ptr(d));
  kk_block_drop(kk_datatype_as_ptr(d, ctx), ctx);
}

static inline void kk_datatype_drop(kk_datatype_t d, kk_context_t* ctx) {
  if (kk_datatype_is_ptr(d)) {
    kk_datatype_ptr_drop(d, ctx);
  }
}

static inline void kk_datatype_ptr_dropn(kk_datatype_t d, kk_ssize_t scan_fsize, kk_context_t* ctx) {
  kk_assert_internal(kk_datatype_is_ptr(d));
  kk_assert_internal(scan_fsize > 0);
  kk_block_dropn(kk_datatype_as_ptr(d,ctx), scan_fsize, ctx);
}

static inline kk_datatype_t kk_datatype_ptr_dup_assert(kk_datatype_t d, kk_tag_t t, kk_context_t* ctx) {
  kk_unused_internal(t);
  kk_assert_internal(kk_datatype_ptr_has_tag(d, t, ctx));
  return kk_datatype_ptr_dup(d, ctx);
}

static inline kk_datatype_t kk_datatype_dup_assert(kk_datatype_t d, kk_tag_t t, kk_context_t* ctx) {
  kk_unused_internal(t);
  kk_assert_internal(kk_datatype_has_tag(d, t, ctx));
  return kk_datatype_dup(d, ctx);
}

static inline void kk_datatype_ptr_drop_assert(kk_datatype_t d, kk_tag_t t, kk_context_t* ctx) {
  kk_unused(t);
  kk_assert_internal(kk_datatype_ptr_has_tag(d, t, ctx) || kk_datatype_ptr_has_tag(d, KK_TAG_BOX_ANY, ctx));
  kk_datatype_ptr_drop(d, ctx);
}

static inline void kk_datatype_drop_assert(kk_datatype_t d, kk_tag_t t, kk_context_t* ctx) {
  kk_unused_internal(t);
  kk_assert_internal(kk_datatype_has_tag(d, t, ctx));
  kk_datatype_drop(d, ctx);
}

static inline kk_reuse_t kk_datatype_ptr_dropn_reuse(kk_datatype_t d, kk_ssize_t scan_fsize, kk_context_t* ctx) {
  kk_assert_internal(kk_datatype_is_ptr(d));
  if kk_unlikely(kk_datatype_is_singleton(d)) { // todo: why is this test here?
    return kk_reuse_null;
  }
  else {
    return kk_block_dropn_reuse(kk_datatype_as_ptr(d,ctx), scan_fsize, ctx);
  }
}

static inline kk_reuse_t kk_datatype_ptr_reuse(kk_datatype_t d, kk_context_t* ctx) {
  return kk_datatype_as_ptr(d,ctx);
}

static inline void kk_datatype_ptr_free(kk_datatype_t d, kk_context_t* ctx) {
  kk_free(kk_datatype_as_ptr(d,ctx), ctx);
}

static inline void kk_datatype_ptr_decref(kk_datatype_t d, kk_context_t* ctx) {
  kk_block_decref(kk_datatype_as_ptr(d,ctx), ctx);
}

#define kk_datatype_from_base(b,ctx)               (kk_datatype_from_ptr(&(b)->_block,ctx))
#define kk_datatype_from_constructor(b,ctx)        (kk_datatype_from_base(&(b)->_base,ctx))
#define kk_datatype_as(tp,v,ctx)                   (kk_block_as(tp,kk_datatype_as_ptr(v,ctx)))
#define kk_datatype_as_assert(tp,v,tag,ctx)        (kk_block_assert(tp,kk_datatype_as_ptr(v,ctx),tag))


#define kk_datatype_null_init  kk_value_null

static inline kk_datatype_t kk_datatype_null(void) {
  kk_datatype_t d = { kk_datatype_null_init };
  return d;
}

static inline bool kk_datatype_is_null(kk_datatype_t d) {
  return kk_datatype_eq(d, kk_datatype_null());
}

static inline kk_datatype_t kk_datatype_unbox(kk_box_t b) {
  kk_datatype_t d = { b.box };
  return d;
}

static inline kk_datatype_t kk_datatype_ptr_unbox(kk_box_t b) {
  kk_datatype_t d = { b.box };
  kk_assert_internal(kk_datatype_is_ptr(d));
  return d;
}

static inline kk_box_t kk_datatype_box(kk_datatype_t d) {
  kk_box_t b = { d.dbox };
  return b;
}

static inline kk_box_t kk_datatype_ptr_box(kk_datatype_t d) {
  kk_assert_internal(kk_datatype_is_ptr(d));
  kk_box_t b = { d.dbox };
  return b;
}

static inline kk_datatype_t kk_datatype_unbox_assert(kk_box_t b, kk_tag_t t, kk_context_t* ctx) {
  kk_unused_internal(ctx);
  kk_unused_internal(t);
  kk_datatype_t d = kk_datatype_unbox(b);
  kk_assert_internal(kk_datatype_has_tag(d, t, ctx));
  return d;
}

static inline kk_datatype_t kk_datatype_ptr_unbox_assert(kk_box_t b, kk_tag_t t, kk_context_t* ctx) {
  kk_unused_internal(ctx);
  kk_unused_internal(t);
  kk_datatype_t d = kk_datatype_ptr_unbox(b);
  kk_assert_internal(kk_datatype_has_tag(d, t, ctx));
  return d;
}


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
    do drops while(0); kk_free(con,ctx); \
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


/*--------------------------------------------------------------------------------------
  kk_Unit
--------------------------------------------------------------------------------------*/

// The unit type
typedef enum kk_unit_e {
  kk_Unit = 0
} kk_unit_t;

static inline kk_decl_const kk_box_t kk_unit_box(kk_unit_t u) {
  return kk_intf_box((kk_intf_t)u);
}

static inline kk_decl_const kk_unit_t kk_unit_unbox(kk_box_t u) {
  kk_unused_internal(u);
  kk_assert_internal(kk_intf_unbox(u) == (kk_intf_t)kk_Unit || kk_box_is_any(u));
  return kk_Unit; // (kk_unit_t)kk_enum_unbox(u);
}

/*--------------------------------------------------------------------------------------
  Functions
--------------------------------------------------------------------------------------*/

#define kk_function_as(tp,fun,ctx)                 kk_datatype_as_assert(tp,fun,KK_TAG_FUNCTION,ctx)
#define kk_function_alloc_as(tp,scan_fsize,ctx)    kk_block_alloc_as(tp,scan_fsize,KK_TAG_FUNCTION,ctx)
#define kk_function_call(restp,argtps,f,args,ctx)  ((restp(*)argtps)(kk_kkfun_ptr_unbox(kk_datatype_as_assert(struct kk_function_s*,f,KK_TAG_FUNCTION,ctx)->fun,ctx)))args

#if (KK_COMPRESS==0)
#define kk_define_static_function(name,cfun,ctx) \
  static struct kk_function_s _static_##name = { { KK_HEADER_STATIC(0,KK_TAG_FUNCTION) }, { kk_box_null_init } }; /* must be box_null */ \
  struct kk_function_s* const _##name = &_static_##name; \
  kk_function_t name = { (kk_intb_t)_##name }; \
  if (kk_box_eq(_##name->fun,kk_box_null())) { _##name->fun = kk_kkfun_ptr_box(&cfun,ctx); }  // initialize on demand we can encode the field */
#else
// for a compressed heap, allocate static functions once in the heap on demand; these are never deallocated  
#define kk_define_static_function(name,cfun,ctx) \
  static kk_function_t name = { kk_datatype_null_init }; \
  if (kk_datatype_is_null(name)) { \
    struct kk_function_s* _fun = kk_block_alloc_as(struct kk_function_s, 1, KK_TAG_FUNCTION, ctx); \
    _fun->fun = kk_kkfun_ptr_box(&cfun, ctx); \
    name = kk_datatype_from_base(_fun,ctx); \
  }
#endif


kk_function_t kk_function_id(kk_context_t* ctx);
kk_function_t kk_function_null(kk_context_t* ctx);
bool          kk_function_is_null(kk_function_t f, kk_context_t* ctx);

static inline kk_decl_pure kk_function_t kk_function_unbox(kk_box_t v, kk_context_t* ctx) {
  kk_unused(ctx);
  return kk_datatype_ptr_unbox(v);
}

static inline kk_decl_pure kk_box_t kk_function_box(kk_function_t d, kk_context_t* ctx) {
  kk_unused(ctx);
  return kk_datatype_ptr_box(d);
}

static inline kk_decl_pure bool kk_function_is_unique(kk_function_t f, kk_context_t* ctx) {
  return kk_datatype_ptr_is_unique(f,ctx);
}

static inline void kk_function_drop(kk_function_t f, kk_context_t* ctx) {
  kk_datatype_ptr_drop_assert(f, KK_TAG_FUNCTION, ctx);
}

static inline kk_function_t kk_function_dup(kk_function_t f, kk_context_t* ctx) {
  return kk_datatype_ptr_dup_assert(f, KK_TAG_FUNCTION, ctx);
}


/*--------------------------------------------------------------------------------------
  TRMC (Further primitives are defined in `lib/std/core/types-ctail-inline.h`)
--------------------------------------------------------------------------------------*/

#if !defined(KK_HAS_MALLOC_COPY)
#define KK_CTAIL_NO_CONTEXT_PATH
#else

// functional context composition by copying along the context path and attaching `child` at the hole.
kk_decl_export kk_box_t kk_ctail_context_copy_compose( kk_box_t res, kk_box_t child, kk_context_t* ctx);

// update the field_idx with the field index + 1 that is along the context path, and return `d` as is.
static inline kk_datatype_t kk_ctail_set_context_path(kk_datatype_t d, size_t field_offset, kk_context_t* ctx) {
  kk_assert_internal((field_offset % sizeof(kk_box_t)) == 0);
  kk_assert_internal(kk_datatype_is_ptr(d));
  const size_t field_index = (field_offset - sizeof(kk_header_t)) / sizeof(kk_box_t);
  kk_assert_internal(field_index <= KK_SCAN_FSIZE_MAX - 2);
  kk_block_field_idx_set( kk_datatype_as_ptr(d,ctx), 1 + (uint8_t)field_index);
  return d;
}

#endif


/*----------------------------------------------------------------------
  Further primitive datatypes and api's
----------------------------------------------------------------------*/

#include "kklib/bits.h"
#include "kklib/box.h"

#include "kklib/maybe.h"
#include "kklib/integer.h"
#include "kklib/bytes.h"
#include "kklib/string.h"
#include "kklib/ref.h"
#include "kklib/vector.h"

#include "kklib/random.h"
#include "kklib/os.h"
#include "kklib/thread.h"
#include "kklib/process.h"    // Process info (memory usage, run time etc.)



/*----------------------------------------------------------------------
  Thread local context operations
----------------------------------------------------------------------*/

// Get a thread local unique number.
static inline kk_integer_t kk_gen_unique(kk_context_t* ctx) {
  kk_integer_t u = ctx->unique;
  ctx->unique = kk_integer_inc(kk_integer_dup(u,ctx),ctx);
  return u;
}

kk_decl_export kk_string_t kk_get_host(kk_context_t* ctx);


/*--------------------------------------------------------------------------------------
  Value tags (used for tags in structs)
--------------------------------------------------------------------------------------*/

// Tag for value types is always an integer
typedef kk_integer_t kk_value_tag_t;

#define kk_value_tag(tag) (kk_integer_from_small(tag))   

static inline kk_decl_const bool kk_value_tag_eq(kk_value_tag_t x, kk_value_tag_t y) {
  // note: x or y may be box_any so don't assert they are smallints
  return (_kk_integer_value(x) == _kk_integer_value(y));
}


#endif // include guard
