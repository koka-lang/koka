#pragma once
#ifndef KK_BYTES_H
#define KK_BYTES_H
/*---------------------------------------------------------------------------
  Copyright 2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------------------------------------------
  Bytes.
  There are four possible representations for bytes:
  
  - singleton empty bytes
  - small byte sequence of at most 7 bytes (ending in a zero byte not included in the length)
  - normal sequence of bytes (ending in a zero byte not included in the length)
  - raw bytes, pointing to an (external) sequence of bytes.
  
  These are not necessarily canonical (e.g. a normal or small bytes can have length 0 instead of being an empty singleton)
-------------------------------------------------------------------------------------------------------------*/


// Bytes are represented as a datatype (as we have a singleton)
struct kk_bytes_s {
  kk_block_t _block;
};

typedef kk_datatype_t kk_bytes_t;

static inline kk_bytes_t kk_bytes_empty(void) {
  return kk_datatype_from_tag((kk_tag_t)1);
}

#define KK_BYTES_SMALL_MAX (7)
typedef struct kk_bytes_small_s {
  struct kk_bytes_s _base;
  union {
    uint64_t buf_value;              
    uint8_t  buf[KK_BYTES_SMALL_MAX+1];   // bytes in-place ending in 0 of at most 7 bytes
                                          // (the ending zero is followed by 0xFF bytes to distinguish
                                          //  the final zero from potential internal zero bytes)
  } u;
} *kk_bytes_small_t;

typedef struct kk_bytes_normal_s {
  struct kk_bytes_s _base;
  kk_ssize_t  length;
  uint8_t buf[1];                         // bytes in-place of `length+1` bytes ending in 0
} *kk_bytes_normal_t;

typedef struct kk_bytes_raw_s {
  struct kk_bytes_s _base;
  kk_free_fun_t* free;     
  const uint8_t* cbuf;                    
  kk_ssize_t        clength;
} *kk_bytes_raw_t;

// Define bytes literals
#define kk_define_bytes_literal(decl,name,len,init) \
  static struct { struct kk_bytes_s _base; kk_ssize_t length; uint8_t buf[len+1]; } _static_##name = \
    { { { KK_HEADER_STATIC(0,KK_TAG_BYTES) } }, len, init }; \
  decl kk_bytes_t name = { &_static_##name._base._block }; \
  
#define kk_define_bytes_literal_empty(decl,name) \
  decl kk_bytes_t name = { (kk_block_t*)((uintptr_t)(5)) };

static inline kk_bytes_t kk_bytes_unbox(kk_box_t v) {
  return kk_datatype_unbox(v);  
}

static inline kk_box_t kk_bytes_box(kk_bytes_t s) {
  return kk_datatype_box(s);
}

static inline void kk_bytes_drop(kk_bytes_t b, kk_context_t* ctx) {
  kk_datatype_drop(b, ctx);
}

static inline kk_bytes_t kk_bytes_dup(kk_bytes_t b, kk_context_t* ctx) {
  return kk_datatype_dup(b,ctx);
}


/*--------------------------------------------------------------------------------------
  Bytes operations
--------------------------------------------------------------------------------------*/

// Allocate `len` bytes.
// If (p /= NULL) then initialize with at most `min(len,plen)` bytes from `p`, which must point to at least `plen` valid bytes. 
// Adds a terminating zero at the end. Return the raw buffer pointer in `buf` if non-NULL
kk_decl_export kk_bytes_t kk_bytes_alloc_len(kk_ssize_t len, kk_ssize_t plen, const uint8_t* p, uint8_t** buf, kk_context_t* ctx);
kk_decl_export kk_bytes_t kk_bytes_adjust_length(kk_bytes_t p, kk_ssize_t newlen, kk_context_t* ctx);
kk_decl_export kk_bytes_t kk_bytes_skip_count(kk_bytes_t p, kk_ssize_t count, kk_context_t* ctx);

// allocate uninitialized bytes
static inline kk_bytes_t kk_bytes_alloc_buf(kk_ssize_t len, uint8_t** buf, kk_context_t* ctx) {
  return kk_bytes_alloc_len(len, 0, NULL, buf, ctx);
}

// allocate uninitialized chars
static inline kk_bytes_t kk_bytes_alloc_cbuf(kk_ssize_t len, char** buf, kk_context_t* ctx) {
  return kk_bytes_alloc_len(len, 0, NULL, (uint8_t**)buf, ctx);
}


// Allocate `len` bytes initialized 
static inline kk_bytes_t kk_bytes_alloc_dupn(kk_ssize_t len, const uint8_t* p, kk_context_t* ctx) {
  // kk_assert_internal(kk_utf8_is_valid(s))
  return kk_bytes_alloc_len(len, len, p, NULL, ctx);
}

// Raw bytes that directly points to an external buffer.
static inline kk_bytes_t kk_bytes_alloc_raw_len(kk_ssize_t len, const uint8_t* p, bool free, kk_context_t* ctx) {
  if (len == 0 || p==NULL) return kk_bytes_empty();
  struct kk_bytes_raw_s* br = kk_block_alloc_as(struct kk_bytes_raw_s, 0, KK_TAG_BYTES_RAW, ctx);
  br->free = (free ? &kk_free_fun : NULL);
  br->cbuf = p;
  br->clength = len;
  return kk_datatype_from_base(&br->_base, ctx);
}

// Get access to the bytes via a pointer (and retrieve the length as well)
static inline const uint8_t* kk_bytes_buf_borrow(const kk_bytes_t b, kk_ssize_t* len, kk_context_t* ctx) {
  static const uint8_t empty[16] = { 0 };
  if (kk_datatype_is_singleton(b)) {
    if (len != NULL) *len = 0;
    return empty;
  }
  kk_tag_t tag = kk_datatype_tag(b,ctx);
  if (tag == KK_TAG_BYTES_SMALL) {
    const kk_bytes_small_t bs = kk_datatype_as_assert(kk_bytes_small_t, b, KK_TAG_BYTES_SMALL, ctx);
    if (len != NULL) {
      // a small bytes of length N (<= 7) ends with an ending zero followed by (7 - N) trailing 0xFF bytes.
      #ifdef KK_ARCH_LITTLE_ENDIAN
      const kk_ssize_t trailing = kk_bits_clz64(~(bs->u.buf_value)) / 8;
      #else
      const kk_ssize_t trailing = kk_bits_ctz64(~(bs->u.buf_value)) / 8;
      #endif
      *len = (KK_BYTES_SMALL_MAX - trailing);
    }
    return &bs->u.buf[0];
  }
  else if (tag == KK_TAG_BYTES) {
    kk_bytes_normal_t bn = kk_datatype_as_assert(kk_bytes_normal_t, b, KK_TAG_BYTES, ctx);
    if (len != NULL) *len = bn->length;
    return &bn->buf[0];
  }
  else {
    kk_bytes_raw_t br = kk_datatype_as_assert(kk_bytes_raw_t, b, KK_TAG_BYTES_RAW, ctx);
    if (len != NULL) *len = br->clength;
    return br->cbuf;
  }
}

static inline const char* kk_bytes_cbuf_borrow(const kk_bytes_t b, kk_ssize_t* len, kk_context_t* ctx) {
  return (const char*)kk_bytes_buf_borrow(b, len, ctx);
}

static inline int8_t kk_bytes_at(kk_bytes_t p, uint64_t i, kk_context_t* ctx){
  const uint8_t* buf = kk_bytes_buf_borrow(p, NULL, ctx);
  return (int8_t)buf[i];
}

static inline void kk_bytes_set(kk_bytes_t p, uint64_t i, int8_t b, kk_context_t* ctx){
  uint8_t* buf = (uint8_t*)kk_bytes_buf_borrow(p, NULL, ctx);
  buf[i] = (uint8_t)b;
}

/*--------------------------------------------------------------------------------------------------
  Length, compare
--------------------------------------------------------------------------------------------------*/

static inline kk_ssize_t kk_decl_pure kk_bytes_len_borrow(const kk_bytes_t b, kk_context_t* ctx) {
  kk_ssize_t len;
  kk_bytes_buf_borrow(b, &len, ctx);
  return len;
}

static inline kk_ssize_t kk_decl_pure kk_bytes_len(kk_bytes_t str, kk_context_t* ctx) {    // bytes in UTF8
  kk_ssize_t len = kk_bytes_len_borrow(str,ctx);
  kk_bytes_drop(str,ctx);
  return len;
}

static inline bool kk_bytes_is_empty(kk_bytes_t s, kk_context_t* ctx) {
  return (kk_bytes_len(s, ctx) == 0);
}

static inline kk_bytes_t kk_bytes_copy(kk_bytes_t b, kk_context_t* ctx) {
  if (kk_datatype_is_singleton(b) || kk_datatype_ptr_is_unique(b,ctx)) {
    return b;
  }
  else {
    kk_ssize_t len;
    const uint8_t* buf = kk_bytes_buf_borrow(b, &len, ctx);
    kk_bytes_t bc = kk_bytes_alloc_dupn(len, buf, ctx);
    kk_bytes_drop(b, ctx);
    return bc;
  }
}

static inline bool kk_bytes_ptr_eq_borrow(kk_bytes_t b1, kk_bytes_t b2) {
  return (kk_datatype_eq(b1, b2));
}

static inline bool kk_bytes_is_empty_borrow(kk_bytes_t b, kk_context_t* ctx) {
  return (kk_bytes_len_borrow(b,ctx) == 0);
}

kk_decl_export int kk_bytes_cmp_borrow(kk_bytes_t str1, kk_bytes_t str2, kk_context_t* ctx);
kk_decl_export int kk_bytes_cmp(kk_bytes_t str1, kk_bytes_t str2, kk_context_t* ctx);

static inline bool kk_bytes_is_eq_borrow(kk_bytes_t s1, kk_bytes_t s2, kk_context_t* ctx) {
  return (kk_bytes_cmp_borrow(s1, s2,ctx) == 0);
}
static inline bool kk_bytes_is_neq_borrow(kk_bytes_t s1, kk_bytes_t s2, kk_context_t* ctx) {
  return (kk_bytes_cmp_borrow(s1, s2, ctx) != 0);
}
static inline bool kk_bytes_is_eq(kk_bytes_t s1, kk_bytes_t s2, kk_context_t* ctx) {
  return (kk_bytes_cmp(s1, s2, ctx) == 0);
}
static inline bool kk_bytes_is_neq(kk_bytes_t s1, kk_bytes_t s2, kk_context_t* ctx) {
  return (kk_bytes_cmp(s1, s2, ctx) != 0);
}


/*--------------------------------------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------------------------------------*/

const uint8_t* kk_memmem(const uint8_t* p, kk_ssize_t plen, const uint8_t* pat, kk_ssize_t patlen);

static inline void kk_memcpy(void* dest, const void* src, kk_ssize_t len) {
  memcpy(dest, src, kk_to_size_t(len));
}
static inline void kk_memmove(void* dest, const void* src, kk_ssize_t len) {
  memmove(dest, src, kk_to_size_t(len));
}
static inline void kk_memset(void* dest, int val, kk_ssize_t len) {
  memset(dest, val, kk_to_size_t(len));
}
static inline int kk_memcmp(const void* s, const void* t, kk_ssize_t len) {
  return memcmp((const char*)s, (const char*)t, kk_to_size_t(len));
}


kk_decl_export kk_ssize_t kk_decl_pure kk_bytes_count_pattern_borrow(kk_bytes_t str, kk_bytes_t pattern, kk_context_t* ctx);

kk_decl_export kk_bytes_t kk_bytes_cat(kk_bytes_t s1, kk_bytes_t s2, kk_context_t* ctx);
kk_decl_export kk_bytes_t kk_bytes_cat_from_buf(kk_bytes_t s1, kk_ssize_t len2, const uint8_t* buf2, kk_context_t* ctx);

kk_decl_export kk_vector_t kk_bytes_splitv(kk_bytes_t s, kk_bytes_t sep, kk_context_t* ctx);
kk_decl_export kk_vector_t kk_bytes_splitv_atmost(kk_bytes_t s, kk_bytes_t sep, kk_ssize_t n, kk_context_t* ctx);

kk_decl_export kk_bytes_t kk_bytes_replace_all(kk_bytes_t s, kk_bytes_t pat, kk_bytes_t rep, kk_context_t* ctx);
kk_decl_export kk_bytes_t kk_bytes_replace_atmost(kk_bytes_t s, kk_bytes_t pat, kk_bytes_t rep, kk_ssize_t n, kk_context_t* ctx);

kk_decl_export kk_bytes_t kk_bytes_repeat(kk_bytes_t s, kk_ssize_t n, kk_context_t* ctx);

kk_decl_export kk_ssize_t kk_bytes_index_of1(kk_bytes_t str, kk_bytes_t sub, kk_context_t* ctx);     // returns 0 for not found, or index + 1
kk_decl_export kk_ssize_t kk_bytes_last_index_of1(kk_bytes_t str, kk_bytes_t sub, kk_context_t* ctx);
kk_decl_export bool    kk_bytes_starts_with(kk_bytes_t str, kk_bytes_t pre, kk_context_t* ctx);
kk_decl_export bool    kk_bytes_ends_with(kk_bytes_t str, kk_bytes_t post, kk_context_t* ctx);
kk_decl_export bool    kk_bytes_contains(kk_bytes_t str, kk_bytes_t sub, kk_context_t* ctx);


#endif // KK_BYTES_H
