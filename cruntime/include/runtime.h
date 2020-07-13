#pragma once
#ifndef __RUNTIME_H__
#define __RUNTIME_H__

/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
#include <assert.h>  // assert
#include <errno.h>   // ENOSYS, ...
#include <limits.h>  // LONG_MAX, ...
#include <stddef.h>  // ptrdiff_t
#include <stdint.h>  // int_t, ...
#include <stdbool.h> // bool
#include <stdio.h>   // FILE*, printf, ...
#include <string.h>  // strlen, memcpy, ...
#include <stdlib.h>  // malloc, abort, ...
#include <math.h>    // isnan, ...

#define MULTI_THREADED  1      // set to 0 to be used single threaded only

#include "runtime/platform.h"  // Platform abstractions and portability definitions
#include "runtime/atomic.h"    // Atomic operations



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
  TAG_DOUBLE,      // boxed IEEE double (64-bit)
  TAG_INT32,       // boxed int32_t               (on 32-bit platforms)
  TAG_FLOAT,       // boxed IEEE float  (32-bit)  (on 32-bit platforms)
  TAG_CFUNPTR,     // C function pointer
  // raw tags have a free function together with a `void*` to the data
  TAG_CPTR_RAW,    // full void* (must be first, see tag_is_raw())
  TAG_STRING_RAW,  // pointer to a valid UTF8 string
  TAG_BYTES_RAW,   // pointer to bytes
  TAG_LAST         
} tag_t;

static inline bool tag_is_raw(tag_t tag) {
  return (tag >= TAG_CPTR_RAW);
}

// Every heap block starts with a 64-bit header with a reference count, tag, and scan fields count.
// The reference count is 0 for a unique reference (for a faster free test in drop).
// Reference counts larger than 0x8000000 use atomic increment/decrement (for thread shared objects).
// (Reference counts are always 32-bit (even on 64-bit) platforms but get "sticky" if 
//  they get too large (>0xC0000000) and in such case we never free the object, see `refcount.c`)
typedef struct header_s {
  uint32_t  refcount;         // reference count
  uint16_t  tag;              // header tag
  uint8_t   scan_fsize;       // number of fields that should be scanned when releasing (`scan_fsize <= 0xFF`, if 0xFF, the full scan size is the first field)  
  uint8_t   thread_shared : 1;
} header_t;

#define SCAN_FSIZE_MAX (0xFF)
#define HEADER(scan_fsize,tag)         { 0, tag, scan_fsize, 0 }            // start with refcount of 0
#define HEADER_STATIC(scan_fsize,tag)  { U32(0xFF00), tag, scan_fsize, 0 }  // start with recognisable refcount (anything > 1 is ok)


// Polymorphic operations work on boxed values. (We use a struct for extra checks on accidental conversion)
// See `box.h` for definitions.
typedef struct box_s {
  uintptr_t box;          // We use unsigned representation to avoid UB on shift operations and overflow.
} box_t;

// An integer is either a small int or a pointer to a bigint_t. Identity with boxed values.
// See `integer.h` for definitions.
typedef struct integer_s {
  intptr_t value;
} integer_t;

// boxed forward declarations
static inline uintx_t   unbox_enum(box_t v);
static inline box_t     box_enum(uintx_t u);


/*--------------------------------------------------------------------------------------
  Blocks 
  A block is an object that starts with a header.

  (non-value) datatypes contain a first `_block` field.
  Their constructors in turn contain a first `_type` field that is the datatype.
  This representation ensures correct behaviour under C alias rules and allow good optimization.
  e.g. :

  typedef struct list_s {  
    block_t _block; 
  } *list_t;

  struct Cons { 
    struct list_s  _type;
    box_t          head;
    list_t         tail;
  }

  struct Nil {
    struct list_s  _type;
  }
--------------------------------------------------------------------------------------*/

// A heap block is a header followed by `scan_fsize` boxed fields and further raw bytes
// A `block_t*` is never NULL (to avoid testing for NULL for reference counts).
typedef struct block_s {
  header_t header;
} block_t;

// A large block has a (boxed) large scan size for vectors.
typedef struct block_large_s {
  block_t  _block;
  box_t    large_scan_fsize; // if `scan_fsize == 0xFF` there is a first field with the full scan size
                             // (the full scan size should include the `large_scan_fsize` field itself!)
} block_large_t;

// A pointer to a block. Cannot be NULL.
typedef block_t* ptr_t;


static inline decl_const tag_t block_tag(const block_t* b) {
  return (tag_t)(b->header.tag);
}

static inline decl_pure size_t block_scan_fsize(const block_t* b) {
  const size_t sfsize = b->header.scan_fsize;
  if (likely(sfsize != SCAN_FSIZE_MAX)) return sfsize;
  const block_large_t* bl = (const block_large_t*)b; 
  return unbox_enum(bl->large_scan_fsize);
}

static inline decl_pure uintptr_t block_refcount(const block_t* b) {
  return b->header.refcount;
}

static inline decl_pure bool block_is_unique(const block_t* b) {
  return (likely(b->header.refcount == 0));
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
  block_t _block;
} *vector_t;

// Strong random number context (using chacha20)
struct random_ctx_s;

//A yield context allows up to 8 continuations to be stored in-place
#define YIELD_CONT_MAX (8)

typedef enum yield_kind_e {
  YIELD_NONE,
  YIELD_NORMAL,
  YIELD_FINAL
} yield_kind_t;

typedef struct yield_s {
  int32_t    marker;          // marker of the handler to yield to
  function_t clause;          // the operation clause to execute when the handler is found
  size_t     conts_count;     // number of continuations in `conts`
  function_t conts[YIELD_CONT_MAX];  // fixed array of continuations. The final continuation `k` is
                              // composed as `fN ○ ... ○ f2 ○ f1` if `conts = { f1, f2, ..., fN }` 
                              // if the array becomes full, a fresh array is allocated and the first
                              // entry points to its composition.
} yield_t;

// The thread local context.
// The fields `yielding`, `heap` and `evv` should come first for efficiency
typedef struct context_s {
  uint8_t     yielding;         // are we yielding to a handler? 0:no, 1:yielding, 2:yielding_final (e.g. exception) // put first for efficiency
  heap_t      heap;             // the (thread-local) heap to allocate in; todo: put in a register?
  vector_t    evv;              // the current evidence vector for effect handling: vector for size 0 and N>1, direct evidence for one element vector
  yield_t     yield;            // inlined yield structure (for efficiency)
  int32_t     marker_unique;    // unique marker generation
  block_t*    delayed_free;     // list of blocks that still need to be freed
  integer_t   unique;           // thread local unique number generation
  uintptr_t   thread_id;        // unique thread id
  function_t  log;              // logging function
  function_t  out;              // std output
  struct random_ctx_s* srandom_ctx;    // secure random using chacha20, initialized on demand
} context_t;

// Get the current (thread local) runtime context (should always equal the `_ctx` parameter)
decl_export context_t* runtime_context(void); 

// The current context is passed as a _ctx parameter in the generated code
#define current_context()   _ctx

// Is the execution yielding?
static inline decl_pure bool _yielding(const context_t* ctx) {
  return (ctx->yielding != YIELD_NONE);
}
#define yielding(ctx)   unlikely(_yielding(ctx))


static inline decl_pure bool yielding_non_final(const context_t* ctx) {
  return (ctx->yielding == YIELD_NORMAL);
}

static inline decl_pure bool yielding_final(const context_t* ctx) {
  return (ctx->yielding == YIELD_FINAL);
}

// Get a thread local marker unique number >= 1.
static inline int32_t marker_unique(context_t* ctx) {
  int32_t m = ++ctx->marker_unique;           // must return a marker >= 1 so increment first;
  if (m == INT32_MAX) ctx->marker_unique = 1; // controlled reset
  return m;
}



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
  UNUSED(p);
  // free(p);
}

static inline void* runtime_realloc(void* p, size_t sz, context_t* ctx) {
  UNUSED(ctx);
  return realloc(p, sz);
}

decl_export void block_free(block_t* b, context_t* ctx);

static inline void block_init(block_t* b, size_t size, size_t scan_fsize, tag_t tag) {
  UNUSED(size);
  assert_internal(scan_fsize < SCAN_FSIZE_MAX);
  header_t header = { 0, (uint16_t)tag, (uint8_t)scan_fsize, 0 };  
  b->header = header;
}

static inline void block_large_init(block_large_t* b, size_t size, size_t scan_fsize, tag_t tag) {
  UNUSED(size);
  header_t header = { 0, (uint16_t)tag, SCAN_FSIZE_MAX, 0 };
  b->_block.header = header;
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

static inline char* _block_as_assert(block_t* b, tag_t tag) {
  UNUSED_RELEASE(tag);
  assert_internal(block_tag(b) == tag);
  return (char*)b;
}

#define block_alloc_as(struct_tp,scan_fsize,tag,ctx)  ((struct_tp*)block_alloc(sizeof(struct_tp),scan_fsize,tag,ctx))
#define block_as(tp,b)                                ((tp)((char*)(b)))
#define block_as_assert(tp,b,tag)                     ((tp)_block_as_assert(b,tag))


/*--------------------------------------------------------------------------------------
  Reference counting
--------------------------------------------------------------------------------------*/

decl_export void     block_check_free(block_t* b, context_t* ctx);
decl_export block_t* dup_block_check(block_t* b);

static inline block_t* dup_block(block_t* b) {
  uint32_t rc = b->header.refcount;
  if (unlikely((int32_t)rc < 0)) {  // note: assume two's complement  (we can skip this check if we never overflow a reference count or use thread-shared objects.)
    return dup_block_check(b);      // thread-shared or sticky (overflow) ?
  }
  else {
    b->header.refcount = rc+1;
    return b;
  }
}

static inline void drop_block(block_t* b, context_t* ctx) {
  uint32_t rc = b->header.refcount;
  if ((int32_t)rc <= 0) {         // note: assume two's complement
    block_check_free(b, ctx);     // thread-shared, sticky (overflowed), or can be freed? 
  }
  else {
    b->header.refcount = rc-1;
  }
}

static inline void drop_block_assert(block_t* b, tag_t tag, context_t* ctx) {
  UNUSED_RELEASE(tag);
  assert_internal(block_tag(b) == tag);
  drop_block(b,ctx);
}

static inline block_t* dup_block_assert(block_t* b, tag_t tag) {
  UNUSED_RELEASE(tag);
  assert_internal(block_tag(b) == tag);
  return dup_block(b);
}


/*--------------------------------------------------------------------------------------
  Datatype and Constructor macros
--------------------------------------------------------------------------------------*/

#define datatype_tag(v)                     (block_tag(&((v)->_block)))
#define datatype_is_unique(v)               (block_is_unique(&((v)->_block)))
#define datatype_as(tp,v)                   (block_as(tp,&((v)->_block)))
#define drop_datatype(v,ctx)                (drop_block(&((v)->_block),ctx))
#define dup_datatype_as(tp,v)               ((tp)dup_block(&((v)->_block)))

#define datatype_as_assert(tp,v,tag)        (block_as_assert(tp,&((v)->_block),tag))
#define drop_datatype_assert(v,tag,ctx)     (drop_block_assert(&((v)->_block),tag,ctx))
#define dup_datatype_as_assert(tp,v,tag)    ((tp)dup_block_assert(&((v)->_block),tag))

#define constructor_tag(v)                  (datatype_tag(&((v)->_type)))
#define constructor_is_unique(v)            (datatype_is_unique(&((v)->_type)))
#define drop_constructor(v,ctx)             (drop_datatype(&((v)->_type),ctx))
#define dup_constructor_as(tp,v)            (dup_datatype_as(tp, &((v)->_type)))

#define drop_value(v,ctx)                   (void)
#define dup_value(v)                        (v)


#define define_static_datatype(decl,struct_tp,name,tag) \
  static struct_tp _static_##name = { { HEADER_STATIC(0,tag) } }; \
  decl struct_tp* name = &_static_##name; 

#define define_static_open_datatype(decl,struct_tp,name,otag) /* ignore otag as it is initialized dynamically */ \
  static struct_tp _static_##name = { { HEADER_STATIC(0,TAG_OPEN) }, &_static_string_empty._type }; \
  decl struct_tp* name = &_static_##name; 


/*----------------------------------------------------------------------
  Reference counting of pattern matches
----------------------------------------------------------------------*/
typedef block_t* reuse_t;

static inline reuse_t block_reuse(block_t* b) {
  // set tag to zero, unique, with zero scan size (so decref is valid on it)
  memset(&b->header, 0, sizeof(header_t));
  return b;
}

static inline block_t* block_alloc_reuse(reuse_t r, size_t size, size_t scan_fsize, tag_t tag, context_t* ctx) {
  // TODO: check usable size p >= size
  block_t* b;
  if (r != NULL) {
    assert_internal(block_is_unique(r));
    b = r;
  }
  else {
    b = (block_t*)runtime_malloc(size, ctx);
  }
  block_init(b, size, scan_fsize, tag);
  return b;
}

// The constructor that is matched on is still used; only duplicate the used fields
#define keep_match(con,dups) \
  do dups while(0)

// The constructor that is matched on is dropped: 
// 1. if unique, drop the unused fields and free just the constructor block
// 2. otherwise, duplicate the used fields, and drop the constructor
#define drop_match(con,dups,drops,ctx) \
  if (constructor_is_unique(con)) { \
    do drops while(0); runtime_free(con); \
  } else { \
    do dups while(0); drop_constructor(con,ctx); \
  }

// The constructor that is matched on may be reused:
// 1. if unique, drop the unused fields and make the constructor block available for reuse
// 2. otherwise, duplicate the used fields, drop the constructor, and don't reuse
#define reuse_match(reuseid,con,dups,drops,ctx) \
  if (constructor_is_unique(conid)) { \
    do drops while(0); reuseid = constructor_reuse(conid); \
  } else { \
    do dups while(0); drop_constructor(con,ctx); reuseid = NULL; \
  }


/*----------------------------------------------------------------------
  Further includes
----------------------------------------------------------------------*/

// The unit type
typedef enum unit_e {
  Unit = 0
} unit_t;



#include "runtime/bits.h"
#include "runtime/box.h"
#include "runtime/integer.h"
#include "runtime/string.h"
#include "runtime/random.h"

/*----------------------------------------------------------------------
  TLD operations
----------------------------------------------------------------------*/

// Get a thread local unique number.
static inline integer_t gen_unique(context_t* ctx) {
  integer_t u = ctx->unique;
  ctx->unique = integer_inc(dup_integer_t(u),ctx);
  return u;
}



/*--------------------------------------------------------------------------------------
  Value tags
--------------------------------------------------------------------------------------*/

// Tag for value types is always an integer
typedef integer_t value_tag_t;

// Use inlined #define to enable constant initializer expression
/*
static inline value_tag_t value_tag(uintx_t tag) {
  return integer_from_small((intx_t)tag);
}
*/
#define value_tag(tag) (_new_integer(((uintptr_t)tag << 2) | 1))   // as a small int

/*--------------------------------------------------------------------------------------
  Functions
--------------------------------------------------------------------------------------*/

#define function_as(tp,fun)                     datatype_as_assert(tp,fun,TAG_FUNCTION)
#define function_alloc_as(tp,scan_fsize,ctx)    block_alloc_as(tp,scan_fsize,TAG_FUNCTION,ctx)
#define function_call(restp,argtps,f,args)      ((restp(*)argtps)(unbox_fun_ptr(f->fun)))args
#define define_static_function(name,cfun,ctx) \
  static struct function_s _static_##name = { { HEADER_STATIC(0,TAG_FUNCTION) }, { ~UP(0) } }; /* must be box_null */ \
  function_t name = &_static_##name; \
  if (box_eq(name->fun,box_null)) { name->fun = box_fun_ptr((fun_ptr_t)&cfun,ctx); }  // initialize on demand so it can be boxed properly
  


function_t function_id(context_t* ctx);
function_t function_null(context_t* ctx);

static inline function_t unbox_function_t(box_t v) {
  return unbox_datatype_as_assert(function_t, v, TAG_FUNCTION);
}

static inline box_t box_function_t(function_t d) {
  return box_datatype(d);
}

static inline bool function_is_unique(function_t f) {
  return block_is_unique(&f->_block);
}

static inline void drop_function_t(function_t f, context_t* ctx) {
  drop_datatype_assert(f, TAG_FUNCTION, ctx);
}

static inline function_t dup_function_t(function_t f) {
  return dup_datatype_as_assert(function_t, f, TAG_FUNCTION);
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
  return dup_box_t(b->value);
}

static inline unit_t ref_set(ref_t r, box_t value, context_t* ctx) {
  box_t b = r->value; 
  drop_box_t(b, ctx);
  r->value = value;
  return Unit;
}

static inline box_t ref_swap(ref_t r, box_t value) {
  box_t b = r->value;
  r->value = value;
  return b;
}

static inline box_t box_ref_t(ref_t r, context_t* ctx) {
  UNUSED(ctx);
  return box_datatype(r);
}

static inline ref_t unbox_ref_t(box_t b, context_t* ctx) {
  UNUSED(ctx);
  return unbox_datatype_as_assert(ref_t, b, TAG_REF);
}

static inline void drop_ref_t(ref_t r, context_t* ctx) {
  drop_datatype_assert(r, TAG_REF, ctx);
}

static inline ref_t dup_ref_t(ref_t r) {
  return dup_datatype_as_assert(ref_t, r, TAG_REF);
}


decl_export void fatal_error(int err, const char* msg, ...);
decl_export void warning_message(const char* msg, ...);

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
  UNUSED_RELEASE(u);
  assert_internal( unbox_enum(u) == (uintx_t)Unit || is_box_any(u));
  return (unit_t)unbox_enum(u);
}

/*--------------------------------------------------------------------------------------
  Vector
--------------------------------------------------------------------------------------*/
extern vector_t vector_empty;

static inline void drop_vector_t(vector_t v, context_t* ctx) {
  drop_datatype(v, ctx);
}

static inline vector_t dup_vector_t(vector_t v) {
  return dup_datatype_as(vector_t, v);
}

typedef struct vector_large_s {  // always use a large block for a vector so the offset to the elements is fixed
  struct block_large_s _block;
  box_t    vec[1];               // vec[(large_)scan_fsize]
} *vector_large_t;

static inline vector_t vector_alloc(size_t length, box_t def, context_t* ctx) {
  if (length==0) {
    return dup_vector_t(vector_empty);
  }
  else {
    vector_large_t v = (vector_large_t)block_large_alloc(sizeof(struct vector_large_s) + (length-1)*sizeof(box_t), length + 1 /* large_scan_fsize */, TAG_VECTOR, ctx);
    if (def.box != box_null.box) {
      for (size_t i = 0; i < length; i++) {
        v->vec[i] = def;
      }
    }
    return (vector_t)(&v->_block._block);
  }
}

static inline size_t vector_len(const vector_t v) {
  size_t len = unbox_enum( datatype_as_assert(vector_large_t, v, TAG_VECTOR)->_block.large_scan_fsize ) - 1;
  assert_internal(len + 1 == block_scan_fsize(&v->_block));  
  assert_internal(len + 1 != 0);
  return len;
}

static inline box_t* vector_buf(vector_t v, size_t* len) {
  if (len != NULL) *len = vector_len(v);
  return &(datatype_as_assert(vector_large_t, v, TAG_VECTOR)->vec[0]);  
}

static inline box_t vector_at(const vector_t v, size_t i) {
  assert(i < vector_len(v));
  return dup_box_t(vector_buf(v,NULL)[i]);
}

static inline box_t box_vector_t(vector_t v, context_t* ctx) {
  UNUSED(ctx);
  return box_ptr(&v->_block);
}

static inline vector_t unbox_vector_t(box_t v, context_t* ctx) {
  UNUSED(ctx);
  block_t* b = unbox_ptr(v);
  assert_internal(block_tag(b) == TAG_VECTOR);
  return (vector_t)b;
}

/*--------------------------------------------------------------------------------------
  Bytes
--------------------------------------------------------------------------------------*/

typedef struct bytes_s {
  block_t  _block;               // TAG_BYTES or TAG_BYTES_RAW
}* bytes_t;

struct bytes_vector_s {          // in-place bytes
  struct bytes_s  _type;
  size_t          length;
  char            buf[1];
};

struct bytes_raw_s {             // pointer to bytes with free function
  struct bytes_s _type;
  free_fun_t*    free;
  uint8_t*       data;
  size_t         length;
};



decl_export string_t  runtime_host(context_t* ctx);


#endif // include guard
