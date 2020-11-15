#pragma once
#ifndef KK_STRING_H
#define KK_STRING_H
/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------------------
  Char as unicode point
--------------------------------------------------------------------------------------*/
typedef int32_t kk_char_t;

#define kk_char_replacement   KI32(0xFFFD)

static inline kk_char_t kk_char_unbox(kk_box_t b, kk_context_t* ctx) {
  return (kk_char_t)kk_int32_unbox(b,ctx);
}

static inline kk_box_t kk_char_box(kk_char_t c, kk_context_t* ctx) {
  return kk_int32_box(c, ctx);
}

static inline bool kk_ascii_is_digit(char c)    { return (c >= '0' && c <= '9'); }
static inline bool kk_ascii_is_lower(char c)    { return (c >= 'a' && c <= 'z'); }
static inline bool kk_ascii_is_upper(char c)    { return (c >= 'A' && c <= 'Z'); }
static inline bool kk_ascii_is_control(char c)  { return (c < ' '); }
static inline bool kk_ascii_is_white(char c)    { return (c==' ' || c=='\t' || c=='\n' || c=='\r'); }
static inline bool kk_ascii_is_hexdigit(char c) { return (kk_ascii_is_digit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')); }
static inline bool kk_ascii_is_alpha(char c)    { return (kk_ascii_is_lower(c) || kk_ascii_is_upper(c)); }
static inline bool kk_ascii_is_alphanum(char c) { return (kk_ascii_is_alpha(c) || kk_ascii_is_digit(c)); }

/*--------------------------------------------------------------------------------------
  Strings
  Always point to valid modified-UTF8 characters.
  Four kinds:
  - singleton empty string
  - small string of at most 8 utf8 bytes
  - normal string of N utf8 bytes
  - raw string pointing to a C buffer of utf8 bytes
  These are not necessarily canonical (e.g. a normal or small string can have length 0 besides being empty)
--------------------------------------------------------------------------------------*/

// A string is modified UTF8 (with encoded zeros) ending with a '0' character.
struct kk_string_s {
  kk_block_t _block;
};

typedef kk_datatype_t kk_string_t;

static inline kk_string_t kk_string_empty(void) {
  return kk_datatype_from_tag(1);
}

#define KK_STRING_SMALL_MAX (KUZ(8))
typedef struct kk_string_small_s {
  struct kk_string_s _base;
  union {
    uint64_t str_value;              
    uint8_t  str[KK_STRING_SMALL_MAX];  // UTF8 string in-place ending in 0 of at most 8 bytes
  } u;
} *kk_string_small_t;

typedef struct kk_string_normal_s {
  struct kk_string_s _base;
  size_t  length;
  uint8_t str[1];  // UTF8 string in-place of `length+1` bytes ending in 0
} *kk_string_normal_t;

typedef struct kk_string_raw_s {
  struct kk_string_s _base;
  kk_free_fun_t* free;     
  const uint8_t* cstr;   // UTF8 string of `length+1` bytes ending in 0
  size_t         length;
} *kk_string_raw_t;

// Define string literals
#define kk_define_string_literal(decl,name,len,chars) \
  static struct { struct kk_string_s _base; size_t length; char str[len+1]; } _static_##name = \
    { { { KK_HEADER_STATIC(0,KK_TAG_STRING) } }, len, chars }; \
  decl kk_string_t name = { &_static_##name._base._block };  

#define kk_define_string_literal_empty(decl,name) \
  decl kk_string_t name = { (kk_block_t*)((uintptr_t)(5)) };

static inline kk_string_t kk_string_unbox(kk_box_t v) {
  return kk_datatype_unbox(v);  
}

static inline kk_box_t kk_string_box(kk_string_t s) {
  return kk_datatype_box(s);
}

static inline void kk_string_drop(kk_string_t str, kk_context_t* ctx) {
  kk_datatype_drop(str, ctx);
}

static inline kk_string_t kk_string_dup(kk_string_t str) {
  return kk_datatype_dup(str);
}


/*--------------------------------------------------------------------------------------
  Strings operations
--------------------------------------------------------------------------------------*/

// Allocate a string of `len` bytes. `s` must be at least `len` bytes of valid UTF8, or NULL. Adds a terminating zero at the end.
kk_decl_export kk_string_t kk_string_alloc_len_unsafe(size_t len, const char* s, kk_context_t* ctx);
kk_decl_export kk_string_t kk_string_adjust_length(kk_string_t str, size_t newlen, kk_context_t* ctx);

static inline kk_string_t kk_string_alloc_buf(size_t len, kk_context_t* ctx) {
  return kk_string_alloc_len_unsafe(len, NULL, ctx);
}

static inline kk_string_t kk_string_alloc_dup(const char* s, kk_context_t* ctx) {
  if (s == NULL) return kk_string_empty();
  return kk_string_alloc_len_unsafe(strlen(s), s, ctx);
}

static inline kk_string_t kk_string_alloc_dupn(size_t maxlen, const char* s, kk_context_t* ctx) {
  if (s == NULL || maxlen == 0) return kk_string_empty();
  size_t n;
  for(n = 0; n < maxlen && s[n] != 0; n++) { };
  return kk_string_alloc_len_unsafe(n, s, ctx);
}

// Raw string that directly points to an external buffer.
static inline kk_string_t kk_string_alloc_raw_len(size_t len, const char* s, bool free, kk_context_t* ctx) {
  if (len == 0 || s==NULL) return kk_string_empty();
  kk_assert_internal(s[len]==0 && strlen(s)==len);
  struct kk_string_raw_s* str = kk_block_alloc_as(struct kk_string_raw_s, 0, KK_TAG_STRING_RAW, ctx);
  str->free = (free ? &kk_free_fun : NULL);
  str->cstr = (const uint8_t*)s;
  str->length = len;
  // todo: kk_assert valid UTF8 in debug mode
  return kk_datatype_from_base(&str->_base);
}

static inline kk_string_t kk_string_alloc_raw(const char* s, bool free, kk_context_t* ctx) {
  if (s==NULL) return kk_string_empty();
  return kk_string_alloc_raw_len(strlen(s), s, free, ctx);
}

static inline const uint8_t* kk_string_buf_borrow(const kk_string_t str) {
  static const uint8_t empty[1] = { 0 };
  if (kk_datatype_is_singleton(str)) {
    return empty;
  }
  kk_tag_t tag = kk_datatype_tag(str);
  if (tag == KK_TAG_STRING_SMALL) {
    return &(kk_datatype_as_assert(kk_string_small_t, str, KK_TAG_STRING_SMALL)->u.str[0]);
  }
  else if (tag == KK_TAG_STRING) {
    return &(kk_datatype_as_assert(kk_string_normal_t, str, KK_TAG_STRING)->str[0]);
  }
  else {
    return kk_datatype_as_assert(kk_string_raw_t, str, KK_TAG_STRING_RAW)->cstr;
  }
}

static inline const char* kk_string_cbuf_borrow(const kk_string_t str) {
  return (const char*)kk_string_buf_borrow(str);
}

static inline int kk_string_cmp_cstr_borrow(const kk_string_t s, const char* t) {
  return strcmp(kk_string_cbuf_borrow(s), t);
}

static inline size_t kk_decl_pure kk_string_len_borrow(const kk_string_t str) {
  if (kk_datatype_is_singleton(str)) {
    return 0;
  }
  else if (kk_datatype_has_tag(str,KK_TAG_STRING_SMALL)) {
    const kk_string_small_t s = kk_datatype_as_assert(const kk_string_small_t, str, KK_TAG_STRING_SMALL);
#ifdef KK_ARCH_LITTLE_ENDIAN
    return (KK_STRING_SMALL_MAX - (kk_bits_clz64(s->u.str_value)/8));
#else
    return (KK_STRING_SMALL_MAX - (kk_bits_ctz64(s->u.str_value)/8));
#endif
  }
  else if (kk_datatype_has_tag(str,KK_TAG_STRING)) {
    return kk_datatype_as_assert(kk_string_normal_t, str, KK_TAG_STRING)->length;
  }
  else {
    return kk_datatype_as_assert(kk_string_raw_t, str, KK_TAG_STRING_RAW)->length;
  }
}

static inline kk_string_t kk_string_copy(kk_string_t str, kk_context_t* ctx) {
  if (kk_datatype_is_singleton(str) || kk_datatype_is_unique(str)) {
    return str;
  }
  else {
    kk_string_t tstr = kk_string_alloc_dup( kk_string_cbuf_borrow(str), ctx);
    kk_string_drop(str, ctx);
    return tstr;
  }
}

static inline bool kk_string_ptr_eq_borrow(kk_string_t s1, kk_string_t s2) {
  return (kk_datatype_eq(s1, s2));
}

static inline bool kk_string_is_empty_borrow(kk_string_t s) {
  return (kk_string_len_borrow(s) == 0);
}


/*--------------------------------------------------------------------------------------------------
  UTF8 decoding/encoding
--------------------------------------------------------------------------------------------------*/

// Is this a UTF8 continuation byte? (0x80 <= b <= 0xBF, i.e. has the form 0x10xxxxxx)
static inline bool kk_utf8_is_cont(uint8_t c) {
  return (((int8_t)c) <= -65); // we can determine this in a single _signed_ comparison
}

// Advance to the next codepoint. (does not advance past the end)
// This should not validate, but advance to the next non-continuation byte.
static inline const uint8_t* kk_utf8_next(const uint8_t* s) {
  if (*s != 0) s++;                // skip first byte except if 0
  for (; kk_utf8_is_cont(*s); s++) {} // skip continuation bytes
  return s;
}

// Retreat to the previous codepoint. 
// This should not validate, but backup to the previous non-continuation byte.
static inline const uint8_t* kk_utf8_prev(const uint8_t* s) {
  s--;                             // skip back at least 1 byte
  for (; kk_utf8_is_cont(*s); s--) {} // skip while continuation bytes
  return s;
}

// Validating UTF8 decode; careful to only read beyond s[0] if valid.
static inline kk_char_t kk_utf8_read_validate(const uint8_t* s, size_t* count) {
  uint8_t b = s[0];
  if (kk_likely(b <= 0x7F)) {
    *count = 1;
    return b;   // ASCII fast path
  }
  else if (b == 0xC0 && s[1] == 0x80) {
    *count = 2;
    return 0;   // Modified UTF8 encoded zero
  }
  // 2 byte encoding
  else if (b >= 0xC2 && b <= 0xDF && kk_utf8_is_cont(s[1])) {
    *count = 2;
    kk_char_t c = (((b & 0x1F) << 6) | (s[1] & 0x3F));
    kk_assert_internal(c >= 0x80 && c <= 0x7FF);
    return c;
  }
  // 3 byte encoding; reject overlong and UTF16 surrogate halves (0xD800 - 0xDFFF)
  else if ((b == 0xE0 && s[1] >= 0xA0 && s[1] <= 0xBF && kk_utf8_is_cont(s[2]))
    || (b >= 0xE1 && b <= 0xEC && kk_utf8_is_cont(s[1]) && kk_utf8_is_cont(s[2])))
  {
    *count = 3;
    kk_char_t c = (((b & 0x0F) << 12) | ((s[1] & 0x3F) << 6) | (s[2] & 0x3F));
    kk_assert_internal(c >= 0x800 && (c < 0x0D800 || c > 0xDFFF) && c <= 0xFFFF);
    return c;
  }
  // 4 byte encoding; reject overlong and out of bounds (> 0x10FFFF)
  else if ((b == 0xF0 && s[1] >= 0x90 && s[1] <= 0xBF && kk_utf8_is_cont(s[2]) && kk_utf8_is_cont(s[3]))
    || (b >= 0xF1 &&  b <= 0xF3 && kk_utf8_is_cont(s[1]) && kk_utf8_is_cont(s[2]) && kk_utf8_is_cont(s[3]))
    || (b == 0xF4 && s[1] >= 0x80 && s[1] <= 0x8F && kk_utf8_is_cont(s[2]) && kk_utf8_is_cont(s[3])))
  {
    *count = 4;
    kk_char_t c = (((b & 0x07) << 18) | ((s[1] & 0x3F) << 12) | ((s[2] & 0x3F) << 6) | (s[3] & 0x3F));
    kk_assert_internal(c >= 0x10000 && c <= 0x10FFFF);
    return c;
  }
  // invalid, skip continuation bytes
  // note: we treat a full illegal sequence as 1 invalid codepoint (including its continuation bytes)
  // this is important as it allows later to pre-allocate a buffer of the right size even if some
  // sequences are invalid.
  else {
    *count = (size_t)(kk_utf8_next(s) - s);
    return kk_char_replacement;
  }
}

// Non-validating utf8 decoding of a single code point
static inline kk_char_t kk_utf8_read(const uint8_t* s, size_t* count) {
  kk_char_t b = *s;
  kk_char_t c;
  if (kk_likely(b <= 0x7F)) {
    *count = 1;
    c = b; // fast path ASCII
  }
  else if (b <= 0xBF) { // invalid continuation byte (check is strictly not necessary as we don't validate..)
    *count = (size_t)(kk_utf8_next(s) - s);  // skip to next
    c = kk_char_replacement;
  }
  else if (b <= 0xDF) { // b >= 0xC0  // 2 bytes
    *count = 2;
    c = (((b & 0x1F) << 6) | (s[1] & 0x3F));
  }
  else if (b <= 0xEF) { // b >= 0xE0  // 3 bytes 
    *count = 3;
    c = (((b & 0x0F) << 12) | ((s[1] & 0x3F) << 6) | (s[2] & 0x3F));
  }
  else if (b <= 0xF4) { // b >= 0xF0  // 4 bytes 
    *count = 4;
    c = (((b & 0x07) << 18) | ((s[1] & 0x3F) << 12) | ((s[2] & 0x3F) << 6) | (s[3] & 0x3F));
  }
  // invalid, skip continuation bytes
  else {  // b >= 0xF5
    *count = (size_t)(kk_utf8_next(s) - s);  // skip to next
    c = kk_char_replacement;
  }
#if (DEBUG!=0)
  size_t dcount;
  kk_assert_internal(c == kk_utf8_read_validate(s, &dcount));
  kk_assert_internal(*count == dcount);
#endif
  return c;
}

// Number of bytes needed to represent a single code point
static inline size_t kk_utf8_len(kk_char_t c) {
  if (kk_unlikely(c == 0)) {
    return 2;
  }
  else if (kk_likely(c <= 0x7F)) {
    return 1;
  }
  else if (c <= 0x07FF) {
    return 2;
  }
  else if (c < 0xD800 || (c > 0xDFFF && c <= 0xFFFF)) {
    return 3;
  }
  else if (c >= 0x10000 && c <= 0x10FFFF) {
    return 4;
  }
  else {
    return 3; // replacement
  }
}

// UTF8 encode a single codepoint
static inline void kk_utf8_write(kk_char_t c, uint8_t* s, size_t* count) {
  if (kk_unlikely(c == 0)) {
    // modified UTF8 zero
    *count = 2;
    s[0] = 0xC0;
    s[1] = 0x80;
  }
  else if (kk_likely(c <= 0x7F)) {
    *count = 1;
    s[0] = (uint8_t)c;
  }
  else if (c <= 0x07FF) {
    *count = 2;
    s[0] = (0xC0 | ((uint8_t)(c >> 6)));
    s[1] = (0x80 | (((uint8_t)c) & 0x3F));
  }
  else if (c < 0xD800 || (c > 0xDFFF && c <= 0xFFFF)) {
    *count = 3;
    s[0] = (0xE0 |  ((uint8_t)(c >> 12)));
    s[1] = (0x80 | (((uint8_t)(c >>  6)) & 0x3F));
    s[2] = (0x80 | (((uint8_t)c) & 0x3F));
  }
  else if (c >= 0x10000 && c <= 0x10FFFF) {
    *count = 4;
    s[0] = (0xF0 |  ((uint8_t)(c >> 18)));
    s[1] = (0x80 | (((uint8_t)(c >> 12)) & 0x3F));
    s[2] = (0x80 | (((uint8_t)(c >>  6)) & 0x3F));
    s[3] = (0x80 | (((uint8_t)c) & 0x3F));
  }
  else {
    // invalid: encode 0xFFFD
    *count = 3;
    s[0] = 0xEF;
    s[1] = 0xBF;
    s[2] = 0xBD;
  }
}


/*--------------------------------------------------------------------------------------------------
  
--------------------------------------------------------------------------------------------------*/
static inline size_t kk_decl_pure kk_string_len(kk_string_t str, kk_context_t* ctx) {    // bytes in UTF8
  size_t len = kk_string_len_borrow(str);
  kk_string_drop(str,ctx);
  return len;
}

static inline bool kk_string_is_empty(kk_string_t s, kk_context_t* ctx) {
  return (kk_string_len(s, ctx) == 0);
}


kk_decl_export size_t kk_decl_pure kk_string_count(kk_string_t str);  // number of code points
kk_decl_export size_t kk_decl_pure kk_string_count_pattern_borrow(kk_string_t str, kk_string_t pattern);
kk_decl_export int kk_string_cmp_borrow(kk_string_t str1, kk_string_t str2);
kk_decl_export int kk_string_cmp(kk_string_t str1, kk_string_t str2, kk_context_t* ctx);
kk_decl_export int kk_string_icmp_borrow(kk_string_t str1, kk_string_t str2);             // ascii case insensitive
kk_decl_export int kk_string_icmp(kk_string_t str1, kk_string_t str2, kk_context_t* ctx);    // ascii case insensitive

static inline bool kk_string_is_eq_borrow(kk_string_t s1, kk_string_t s2) {
  return (kk_string_cmp_borrow(s1, s2) == 0);
}
static inline bool kk_string_is_neq_borrow(kk_string_t s1, kk_string_t s2) {
  return (kk_string_cmp_borrow(s1, s2) != 0);
}
static inline bool kk_string_is_eq(kk_string_t s1, kk_string_t s2, kk_context_t* ctx) {
  return (kk_string_cmp(s1, s2, ctx) == 0);
}
static inline bool kk_string_is_neq(kk_string_t s1, kk_string_t s2, kk_context_t* ctx) {
  return (kk_string_cmp(s1, s2, ctx) != 0);
}

kk_decl_export kk_string_t kk_string_cat(kk_string_t s1, kk_string_t s2, kk_context_t* ctx);
kk_decl_export kk_string_t kk_string_cat_fromc(kk_string_t s1, const char* s2, kk_context_t* ctx);

kk_decl_export kk_string_t kk_string_from_char(kk_char_t c, kk_context_t* ctx);
kk_decl_export kk_string_t kk_string_from_chars(kk_vector_t v, kk_context_t* ctx);
kk_decl_export kk_vector_t kk_string_to_chars(kk_string_t s, kk_context_t* ctx);

kk_decl_export kk_string_t kk_integer_to_string(kk_integer_t x, kk_context_t* ctx);
kk_decl_export kk_string_t kk_integer_to_hex_string(kk_integer_t x, bool use_capitals, kk_context_t* ctx);

kk_decl_export kk_vector_t kk_string_splitv(kk_string_t s, kk_string_t sep, kk_context_t* ctx);
kk_decl_export kk_vector_t kk_string_splitv_atmost(kk_string_t s, kk_string_t sep, size_t n, kk_context_t* ctx);

kk_decl_export kk_string_t kk_string_replace_all(kk_string_t s, kk_string_t pat, kk_string_t rep, kk_context_t* ctx);
kk_decl_export kk_string_t kk_string_replace_atmost(kk_string_t s, kk_string_t pat, kk_string_t rep, size_t n, kk_context_t* ctx);

kk_decl_export kk_string_t kk_string_repeat(kk_string_t s, size_t n, kk_context_t* ctx);

kk_decl_export size_t kk_string_index_of1(kk_string_t str, kk_string_t sub, kk_context_t* ctx);     // returns 0 for not found, or index + 1
kk_decl_export size_t kk_string_last_index_of1(kk_string_t str, kk_string_t sub, kk_context_t* ctx);
kk_decl_export bool   kk_string_starts_with(kk_string_t str, kk_string_t pre, kk_context_t* ctx);
kk_decl_export bool   kk_string_ends_with(kk_string_t str, kk_string_t post, kk_context_t* ctx);
kk_decl_export bool   kk_string_contains(kk_string_t str, kk_string_t sub, kk_context_t* ctx);

kk_decl_export kk_string_t  kk_string_to_upper(kk_string_t str, kk_context_t* ctx);
kk_decl_export kk_string_t  kk_string_to_lower(kk_string_t strs, kk_context_t* ctx);
kk_decl_export kk_string_t  kk_string_trim_left(kk_string_t strs, kk_context_t* ctx);
kk_decl_export kk_string_t  kk_string_trim_right(kk_string_t strs, kk_context_t* ctx);

kk_decl_export kk_unit_t kk_println(kk_string_t s, kk_context_t* ctx);
kk_decl_export kk_unit_t kk_print(kk_string_t s, kk_context_t* ctx);
kk_decl_export kk_unit_t kk_trace(kk_string_t s, kk_context_t* ctx);
kk_decl_export kk_unit_t kk_trace_any(kk_string_t s, kk_box_t x, kk_context_t* ctx);
kk_decl_export kk_string_t kk_show_any(kk_box_t x, kk_context_t* ctx);

kk_decl_export kk_string_t kk_double_show_fixed(double d, int32_t prec, kk_context_t* ctx);
kk_decl_export kk_string_t kk_double_show_exp(double d, int32_t prec, kk_context_t* ctx);
kk_decl_export kk_string_t kk_double_show(double d, int32_t prec, kk_context_t* ctx);


#endif // include guard
