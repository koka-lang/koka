#pragma once
#include <assert.h>
#include <stdbool.h> // bool
#include <stdint.h>  // intptr_t
#include <stdio.h>   // FILE*
#include <string.h>  // strlen
#include <malloc.h>

#define REFCOUNT_LIMIT_TO_32BIT 0
#define MULTI_THREADED          0
#define TLD_IN_REG              0


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
#define decl_export     __attribute__((visibility("default"))) decl_externc
#else
#define decl_export     decl_externc
#endif

#if defined(__GNUC__)
#define unlikely(h)     __builtin_expect((h),0)
#define likely(h)       __builtin_expect((h),1)
#define decl_const      __attribute__((const))
#define decl_pure       __attribute__((pure))
#define noinline        __attribute__((noinline))
#elif defined(_MSC_VER)
#pragma warning(disable:4214)  // using bit field types other than int
#define unlikely(x)     (x)
#define likely(x)       (x)
#define decl_const
#define decl_pure
#define noinline        __declspec(noinline)
#else
#define unlikely(h)     (h)
#define likely(h)       (h)
#define decl_const
#define decl_pure
#define noinline   
#endif

#ifndef UNUSED
#define UNUSED(x)  ((void)(x))
#endif


// Assumptions:
// - a char/byte is 8 bits
// - (>>) on signed integers is an arithmetic right shift (i.e. sign extending)

static inline intptr_t  sar(intptr_t i, intptr_t shift) { return (i >> shift); }
static inline uintptr_t shr(uintptr_t i, uintptr_t shift) { return (i >> shift); }

#if INTPTR_MAX == 9223372036854775807LL
# define INTPTR_SIZE 8
#elif INTPTR_MAX == 2147483647LL
# define INTPTR_SIZE 4
#else
#error platform must be 32 or 64 bits
#endif
#define INTPTR_BITS (8*INTPTR_SIZE)

#if INTPTR_SIZE <= 4
#undef  REFCOUNT_LIMIT_TO_32BIT
#define REFCOUNT_LIMIT_TO_32BIT  1
#endif

/*--------------------------------------------------------------------------------------
  Basic datatypes
--------------------------------------------------------------------------------------*/

// A `ptr_t` is a pointer to a `block_t`. We keep it abstract to support tagged pointers or sticky refcounts in the future
typedef uintptr_t ptr_t;      

// Polymorpic operations work on boxed values. We use unsigned representation to avoid UB on shift operations and overflow.
typedef uintptr_t box_t;    

// A datatype is either a `ptr_t` or an enumeration as a boxed value. Identity with boxed values.
typedef box_t datatype_t;

// An integer is either a `ptr_t` or a small int. Identity with boxed values.
typedef box_t integer_t;


// Tags for heap blocks
typedef enum tag_e {
  TAG_INVALID = 0,
  TAG_MIN = 1,
  TAG_SMALL_MAX = 15,
  TAG_MAX = 65000,
  TAG_BOX,
  TAG_REF,
  TAG_FUNCTION,
  TAG_BIGINT,
  TAG_STRING,
  TAG_BYTES,
  TAG_INT64,
#if INTPTR_SIZE < 8
  TAG_DOUBLE,
  TAG_INT32,
#endif
  TAG_LAST
} tag_t;

// Every heap block starts with a header with a reference count and tag.
typedef union header_s {
  uint64_t as_uint64;
#if !defined(BIG_ENDIAN)
  struct {
#if INTPTR_SIZE==8
    uint64_t refcount : 38;
#else
    uint32_t refcount;
    size_t _unused_refcount : 6;
#endif    
    size_t _reserved : 1;
    size_t thread_shared : 1;      // true if shared among threads (so release/acquire use locked increment/decrement)
    size_t scan_fsize : 8;         // number of fields that should be scanned when releasing (`scan_fsize <= 0xFF`, if 0xFF, the full scan size is the first field)
    size_t tag : 16;               // tag
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
# error "todo: define big-endian order as well"
#endif
} header_t;

#define SCAN_FSIZE_MAX (255)
#define HEADER(tag)    {(((uint64_t)tag << 48) | 1)}  // start with refcount of 1

#define HEADER_STATIC(scan_fsize,tag)  {(((uint64_t)tag << 48) | (uint64_t)((uint8_t)scan_fsize) << 40 | 0xFF00)}  // start with recognisable refcount (anything > 1 is ok)

// A heap block is a header followed by `scan_fsize` boxed fields and further raw bytes
typedef struct block_s {
  header_t header;
  //box_t    fields[1];  // flexible array [] is not in C++, and [0] is not in either C or C++ :-(
} block_t;


// boxed forward declarations
static inline bool      is_ptr(box_t b);
static inline bool      is_ptr_fast(box_t b);   // if it is either a pointer, int, or enum, but not a double
static inline bool      is_enum_fast(box_t b);  // if it is either a pointer, int, or enum, but not a double
static inline ptr_t     unbox_ptr(box_t b);
static inline box_t     box_ptr(ptr_t p);
static inline intptr_t  unbox_int(box_t v);
static inline box_t     box_int(intptr_t i);
static inline uintptr_t unbox_enum(box_t v);
static inline box_t     box_enum(uintptr_t u);




/*--------------------------------------------------------------------------------------
  Blocks and ptr's
--------------------------------------------------------------------------------------*/

static inline decl_const block_t* ptr_block(ptr_t p) {
  return (block_t*)p;
}

static inline decl_const ptr_t block_ptr(const block_t* b, tag_t tag) {
  UNUSED(tag);
  return (ptr_t)b;
}

static inline decl_const tag_t block_tag(block_t* b) {
  return (tag_t)(b->header.h.tag);
}

static inline decl_const box_t* block_fields(block_t* b) {
  return (box_t*)((uint8_t*)b + sizeof(block_t));
}

// define as macro to allow taking the address of a field.
#define block_field(b,i)  (((box_t*)((uint8_t*)(b) + sizeof(block_t)))[i])


static inline decl_const tag_t ptr_tag(ptr_t p) {
  return block_tag(ptr_block(p));
}

static inline decl_const box_t* ptr_fields(ptr_t p) {
  return block_fields(ptr_block(p));
}

// define as macro to allow taking the address of a field.
#define ptr_field(p,i)  block_field(ptr_block(p),i)

static inline uintptr_t ptr_refcount(ptr_t p) {
  return ptr_block(p)->header.h.refcount;
}

static inline decl_const size_t block_scan_fsize(block_t* b) {
  size_t sfsize = b->header.h.scan_fsize;
  return (likely(sfsize != SCAN_FSIZE_MAX) ? sfsize : (size_t)unbox_int(block_field(b,0)));
}

static inline void* ptr_raw(ptr_t p) {
  return &ptr_field(p, block_scan_fsize(ptr_block(p)));
}

static inline bool ptr_is_unique(ptr_t p) {
  block_t* b = ptr_block(p);
#if REFCOUNT_LIMIT_TO_32BIT
  return (likely(b->header.rc32.lo == 0));
#else
  return (likely(b->header.rc32.lo == 0) && b->header.rc32.hi == 0);
#endif
  // return (b->header.h.refcount == 1);
}

#define ptr_as(tp,p)  (tp*)(&ptr_field(p,0))

static inline ptr_t pointer_ptr(void* p) {
  return (ptr_t)((uint8_t*)p - sizeof(header_t));
}





/*--------------------------------------------------------------------------------------
  Thread local data
  We have a single location that points to the `tld` which is used for efficient
  heap allocation and effect handlers.
--------------------------------------------------------------------------------------*/
typedef struct tld_s {
  void*      yield;            // if not NULL, we are yielding to an effect handler; todo: put in register?
  void*      evv;              // the current evidence vector for effect handling; todo: put in register as well?
  void*      heap;             // the (thread-local) heap to allocate in
  block_t*   delayed_free;     // list of blocks that still need to be freed
  integer_t  unique;           // thread local unique number generation
  int32_t    marker_unique;    // unique marker generation
} tld_t;

#if   (TLD_IN_REG) && defined(__GNUC__) && defined(__x86_64__)
register tld_t* tld asm("%r15");  
#elif (TLD_IN_REG) && defined(__GNUC__) && defined (__arm__) && !defined(__thumb__)
register tld_t* tld asm("r6");
#elif (TLD_IN_REG) && defined(__GNUC__) && defined (__i386__)
register tld_t* tld asm("%esi");
#elif (TLD_IN_REG) && defined(__GNUC__) && (defined(__ppc__) || defined(__ppc64__))
register tld_t* tld asm("26");
#elif !(MULTI_THREADED)
static tld_t* tld;
#elif defined(_MSC_VER) 
_declspec(thread) tld_t* tld;
#else
__thread tld_t* tld;
#endif


static inline bool yielding() {
  return (tld->yield != NULL);
};


/*--------------------------------------------------------------------------------------
  Functions
--------------------------------------------------------------------------------------*/

// A function is a ptr to a function block
struct function_s {
  block_t  block;
  box_t    fun;     // boxed cptr
  // followed by free variables
};
typedef struct function_s* function_t;

#define function_call(restp,argtps,f,args)  ((restp(*)argtps)(unbox_cptr(f->fun)))args

/*--------------------------------------------------------------------------------------
  Strings
--------------------------------------------------------------------------------------*/

// A string is modified UTF-8 (with encoded zeros) ending with a '0' character.
struct string_s {
  block_t  block;
  size_t   length;
  char     str[1];
};
typedef struct string_s* string_t;

struct small_string_s {
  block_t  block;
  char     str[1];
};
typedef struct small_string_s* small_string_t;

#define define_string_literal(decl,name,len,chars) \
  static struct { block_t block; size_t length; char str[len+1]; } _static_##name = { { HEADER_STATIC(0,TAG_STRING) }, len, chars }; \
  decl struct string_s* const name = (struct string_s*)(&_static_##name);



/*--------------------------------------------------------------------------------------
  Allocation
--------------------------------------------------------------------------------------*/

#define runtime_malloc(sz)     malloc(sz)
#define runtime_free(p)        free(p)
#define runtime_realloc(p,sz)  realloc(p,sz)

decl_export void block_free(block_t* b);

static inline void ptr_free(ptr_t p) {
  block_free(ptr_block(p));
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
    block_field(b,0) = box_enum(scan_fsize);
  }
  b->header = header;
}

static inline block_t* block_alloc(size_t size, size_t scan_fsize, tag_t tag) {
  block_t* b = (block_t*)runtime_malloc(size + sizeof(header_t));
  block_init(b, size, scan_fsize, tag);
  return b;
}

#define block_alloc_tp(tp,scan_fsize,tag)  ((tp*)block_alloc(sizeof(tp), scan_fsize, tag))


static inline ptr_t ptr_alloc(size_t size, size_t scan_fsize, tag_t tag) {
  block_t* b = block_alloc(size, scan_fsize, tag);
  return block_ptr(b, tag);
}

static inline ptr_t ptr_realloc(ptr_t p, size_t size) {
  assert(ptr_is_unique(p));
  tag_t tag = ptr_tag(p);
  block_t* b = (block_t*)runtime_realloc(ptr_block(p), size + sizeof(block_t));
  return block_ptr(b, tag);
}

#define ptr_alloc_tp(tp,scan_fsize,tag)  ptr_alloc(sizeof(tp),scan_fsize,tag)

#define ptr_null  ((ptr_t)NULL)



/*--------------------------------------------------------------------------------------
  Reference counting
--------------------------------------------------------------------------------------*/

decl_export void ptr_check_free(ptr_t p);


static inline void ptr_incref(ptr_t p) {
#if REFCOUNT_LIMIT_TO_32BIT
  // with a 32-bit reference count on a 64-bit system, we need a (8*2^32 = 32GiB array to create that many
  // references to a single object. That is often a reasonable restriction and more efficient.
  ptr_block(p)->header.rc32.lo++;
#else
  // optimize: increment the full 64-bit hoping to never overflow the 38 refcount bits
  // this is reasonable as it would take a (8*2^38 = 2TiB array to create that many references to a single object)
  ptr_block(p)->header.rc.extended++;   
#endif
}

static inline void ptr_decref(ptr_t p) {
  // optimize: always decrement just the 32 bits; 
  // on larger refcounts check afterwards if the hi-bits were 0. Since we use 0 for a unique reference we can 
  // efficiently check if the block can be freed by comparing to 0.
  uint32_t count = (ptr_block(p)->header.rc32.lo)--;
#if REFCOUNT_LIMIT_TO_32BIT
  if (count==0) ptr_free(p);
#else
  if (count==0) ptr_check_free(p);
#endif
}
static inline ptr_t ptr_dup(ptr_t p) {
  ptr_incref(p);
  return p;
}

static inline void boxed_incref(box_t b) {
  if (is_ptr(b)) ptr_incref(unbox_ptr(b));
}

static inline void boxed_decref(box_t b) {
  if (is_ptr(b)) ptr_decref(unbox_ptr(b));
}

static inline box_t boxed_dup(box_t b) {
  boxed_incref(b);
  return b;
}


/*--------------------------------------------------------------------------------------
  Reference counting scrutinee of a match where we try to avoid
  reference increments on fields of the match, and reuse memory in-place for
  deallocated scrutinees.
--------------------------------------------------------------------------------------*/
typedef box_t orphan_t;

static inline orphan_t orphan_null() {
  return box_int(0);
}

static inline orphan_t orphan_ptr(ptr_t p) {
  ptr_block(p)->header.as_uint64 = 0; // set tag to zero, unique, with zero scan size (so decref is valid on it)
  return box_ptr(p);
}

static inline orphan_t ptr_release0(ptr_t p) {
  if (ptr_is_unique(p)) {
    return orphan_ptr(p);
  }
  else {
    ptr_decref(p);
    return orphan_null();
  }
}

static inline orphan_t ptr_release1(ptr_t p, box_t unused_field1 ) {
  if (ptr_is_unique(p)) {
    boxed_decref(unused_field1);
    return orphan_ptr(p);
  }
  else {
    ptr_decref(p);
    return orphan_null();
  }
}

static inline void ptr_no_reuse(orphan_t p) {
  if (is_ptr_fast(p)) {
    runtime_free(ptr_block(unbox_ptr(p)));
  };
}

static inline ptr_t ptr_alloc_reuse(orphan_t p, size_t size, size_t scan_fsize, tag_t tag) {
  // TODO: check usable size p >= size
  block_t* b;
  if (is_ptr_fast(p)) {
    assert(ptr_is_unique(unbox_ptr(p)));
    b = ptr_block(unbox_ptr(p));
  }
  else {
    b = (block_t*)runtime_malloc(size);
  }
  block_init(b, size, scan_fsize, tag);
  return block_ptr(b, tag);
}




/*----------------------------------------------------------------------
  Further includes
----------------------------------------------------------------------*/

#include "runtime/box.h"
#include "runtime/integer.h"


/*----------------------------------------------------------------------
  TLD operations
----------------------------------------------------------------------*/

// Get a thread local unique number.
static inline integer_t gen_unique() {
  integer_t u = tld->unique;
  tld->unique = integer_inc(integer_dup(u));
  return u;
};

// Get a thread local marker unique number.
static inline int32_t marker_unique() {
  return tld->marker_unique++;
};

/*--------------------------------------------------------------------------------------
  Datatype
--------------------------------------------------------------------------------------*/

static inline datatype_t ptr_datatype(ptr_t p) {
  return box_ptr(p);
}

static inline ptr_t datatype_ptr(datatype_t d) {
  return unbox_ptr(d);
}

static inline datatype_t block_datatype(block_t* p) {
  return ptr_datatype(block_ptr(p, block_tag(p)));
}

static inline block_t* datatype_block(datatype_t d) {
  return ptr_block(datatype_ptr(d));
}

static inline datatype_t datatype_enum(uintptr_t tag) {
  return box_enum(tag);
}

static inline bool datatype_is_ptr(datatype_t d) {
  return (is_ptr_fast(d));
}

static inline bool datatype_is_enum(datatype_t d) {
  return (is_enum_fast(d));
}


// Tag for value types is always a boxed enum
typedef box_t value_tag_t;

// Use #define to enable constant initializer expression
/*
static inline value_tag_t value_tag(uintptr_t tag) {
  return box_enum(tag);
}
*/
#define value_tag(tag) (((value_tag_t)tag << 2) | 0x03)



/*--------------------------------------------------------------------------------------
  Strings operations
--------------------------------------------------------------------------------------*/

static inline string_t string_alloc_buf(size_t len) {
  string_t str = (string_t)block_alloc(sizeof(struct string_s) - 1 + len + 1, 0, TAG_STRING);
  str->length = len;
  str->str[0] = 0;
  str->str[len] = 0;
  return str;
}


static inline string_t string_alloc_len(size_t len, const char* s) {
  string_t str = string_alloc_buf(len);
  memcpy(&str->str[0], s, len+1);
  return str;
}

static inline string_t string_alloc(const char* s) {
  return (s==NULL ? string_alloc_len(0, "") : string_alloc_len(strlen(s), s));
}

static inline char* string_buf(string_t str) {
  return &str->str[0];
}

static inline void string_decref(string_t str) {
  ptr_decref(block_ptr(&str->block, TAG_STRING));
}

/*--------------------------------------------------------------------------------------
  References
--------------------------------------------------------------------------------------*/
typedef datatype_t ref_t;

struct ref_s {
  block_t  block;
  box_t    value;
};

static inline ref_t ref_new(box_t value) {
  struct ref_s* r = block_alloc_tp(struct ref_s, 1, TAG_REF);
  r->value = value;
  return block_datatype(&r->block);
}

static inline box_t ref_get(ref_t b) {
  struct ref_s* r = (struct ref_s*)datatype_block(b);
  return r->value;
}

static inline void ref_set(ref_t b, box_t value) {
  struct ref_s* r = (struct ref_s*)datatype_block(b);
  r->value = value;
}

static inline void unsupported_external(const char* msg) {
  fprintf(stderr, msg);
}