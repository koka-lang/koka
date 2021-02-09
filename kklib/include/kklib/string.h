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

/*---------------------------------------------------------------------------------------------------------------
  Strings
  Always point to valid utf-8 byte sequences ending with a 0 byte (not included in the length).
  Since we stay strictly in valid utf-8, that means that strings can contain internal 0 characters
  and we cannot generally use C string functions to manipulate our strings.

  There are four possible representations for strings:
  
  - singleton empty string
  - small string of at most 7 utf-8 bytes
  - normal string of utf-8 bytes
  - raw string pointing to a buffer of utf-8 bytes
  
  These are not necessarily canonical (e.g. a normal or small string can have length 0 besides being empty)

  ------
  There few important cases where external text is not quite utf-8 or utf-16.
  We call these "mutf-8" and "mutf-16" for "mostly" (or "mixed") utf-8/16:

  - mutf-16: this is used in Windows file names and JavaScript. These are mostly utf-16 but
    can contain invalid _lone_ parts of surrogate pairs.
  
  - mutf-8: this is mostly utf-8 but contains invalid utf-8 sequences, like overlong
    sequences or lone continuation bytes. This can occur for examply by bad json encoding
    containing binary data, but also as a result of a _locale_ that cannot be decoded properly.
  
  In particular for mutf-16 we would like to guarantee that decoding to utf-8 and encoding 
  again to mutf-16 is an identity transformation; for example, we may list the contents
  of a directory and then try to read each file. This means that we cannot replace invalid
  codes in mutf-16 with a replacement character. One proposed solution for this is 
  to use wtf-8 (used in Rust <https://github.com/rust-lang/rust/issues/12056#issuecomment-55786546>) 
  instead of utf-8 internally. We like to use strict utf-8 internally though so we can always output valid 
  utf-8 without further conversions. (also, new formats like wtf-8 often have tricky edge cases, like 
  naively appending strings may change the interpretation of surrogate pairs in wtf-8)

  Instead, we solve this by staying in strict utf-8 internally, but we reserve a
  particular set of code-points to have a special meaning when converting to mutf-16 (or mutf-8).
  For now, we use an (hopefully forever) unassigned range in the "supplementary special-purpose plane":
  
  - ED800 - EDFFF: corresponds to a lone half of a surrogate pair `x` where `x = code - E0000`.
  - EE000 - EE07F: <unused>
  - EE080 - EE0FF: corresponds to an invalid byte `b` in an invalid utf-8 sequence where `b = code - EE000`.
                   (note: invalid bytes in utf-8 are always >= 0x80 so we need only a limited range).
  
  We call this the "raw range".
  When decoding mutf-8 or mutf-16, we decode invalid sequences to these code points, and only when
  decoding back to mutf-8 or mutf-16, we decode these code points specially again to make this an identity
  transformation. _Otherwise these are just regular code points and valid utf-8 with no special treatment_.
  Also, security wise this is good practice -- for example, we decode the overlong utf-8 sequence `0xC0 0x80` 
  not to a 0 character, but to two raw code points: 0xEE0C0 0xEE080. This way, we maintain an identity 
  transform while still preventing hidden embedded 0 characters.
  
  The advantage over using the replacement character is that we now retain full information what
  the original (invalid) sequences were (and can thus do an identity transform). 
  (Actually, to make it an identity transform, when decoding mutf-16 we need to not just decode lone 
   surrogate halves to our raw range, but also surrogate pairs that happen to decode to our raw range, 
   and similarly for mutf-8; so for both mutf-8 and mutf-16 input we also treat any code points in the 
   raw range as an invalid sequence (which should be fine in practice as these are unassigned anyways). 
------------------------------------------------------------------------------------------------------------*/

#define KK_RAW_PLANE      ((kk_char_t)(0xE0000))
#define KK_RAW_UTF8_OFS   (KK_RAW_PLANE + 0xE000)
#define KK_RAW_UTF16_OFS  (KK_RAW_PLANE)

// A string is valid utf-8 (with potentially internal '0' characters) ending with a '0' character.
struct kk_string_s {
  kk_block_t _block;
};

typedef kk_datatype_t kk_string_t;

static inline kk_string_t kk_string_empty(void) {
  return kk_datatype_from_tag(1);
}

#define KK_STRING_SMALL_MAX (KUZ(7))
typedef struct kk_string_small_s {
  struct kk_string_s _base;
  union {
    uint64_t str_value;              
    uint8_t  str[KK_STRING_SMALL_MAX+1];  // utf-8 string in-place ending in 0 of at most 7 bytes
                                          // (the ending zero is followed by 0xFF bytes to distinguish
                                          //  the final zero from potential internal zero character)
  } u;
} *kk_string_small_t;

typedef struct kk_string_normal_s {
  struct kk_string_s _base;
  size_t  length;
  uint8_t str[1];                       // utf-8 string in-place of `length+1` bytes ending in 0
} *kk_string_normal_t;

typedef struct kk_string_raw_s {
  struct kk_string_s _base;
  kk_free_fun_t* free;     
  const uint8_t* cstr;                  // utf-8 string of `length+1` bytes ending in 0
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

// Allocate a string of `len` bytes. `s` must be at least `len` bytes of valid utf-8, or NULL. Adds a terminating zero at the end.
kk_decl_export kk_string_t kk_string_alloc_len_unsafe(size_t len, const uint8_t* s, kk_context_t* ctx);
kk_decl_export kk_string_t kk_string_adjust_length(kk_string_t str, size_t newlen, kk_context_t* ctx);

static inline kk_string_t kk_string_alloc_buf(size_t len, kk_context_t* ctx) {
  return kk_string_alloc_len_unsafe(len, NULL, ctx);
}

static inline kk_string_t kk_string_alloc_dup_unsafe(const char* s, kk_context_t* ctx) {
  if (s == NULL) return kk_string_empty();
  return kk_string_alloc_len_unsafe(strlen(s), (uint8_t*)s, ctx);
}

static inline kk_string_t kk_string_alloc_dupn_unsafe(size_t maxlen, const char* s, kk_context_t* ctx) {
  if (s == NULL || maxlen == 0) return kk_string_empty();
  size_t n;
  for(n = 0; n < maxlen && s[n] != 0; n++) { }
  return kk_string_alloc_len_unsafe(n, (const uint8_t*)s, ctx);
}

// Raw string that directly points to an external buffer.
static inline kk_string_t kk_string_alloc_raw_len(size_t len, const char* s, bool free, kk_context_t* ctx) {
  if (len == 0 || s==NULL) return kk_string_empty();
  kk_assert_internal(s[len]==0 && strlen(s)==len);
  struct kk_string_raw_s* str = kk_block_alloc_as(struct kk_string_raw_s, 0, KK_TAG_STRING_RAW, ctx);
  str->free = (free ? &kk_free_fun : NULL);
  str->cstr = (const uint8_t*)s;
  str->length = len;
  // todo: kk_assert valid utf-8 in debug mode
  return kk_datatype_from_base(&str->_base);
}

static inline kk_string_t kk_string_alloc_raw(const char* s, bool free, kk_context_t* ctx) {
  if (s==NULL) return kk_string_empty();
  return kk_string_alloc_raw_len(strlen(s), s, free, ctx);
}

static inline const uint8_t* kk_string_buf_borrow(const kk_string_t str, size_t* len) {
  static const uint8_t empty[64] = { 0 };
  if (kk_datatype_is_singleton(str)) {
    if (len != NULL) *len = 0;
    return empty;
  }
  kk_tag_t tag = kk_datatype_tag(str);
  if (tag == KK_TAG_STRING_SMALL) {
    const kk_string_small_t s = kk_datatype_as_assert(const kk_string_small_t, str, KK_TAG_STRING_SMALL);
    if (len != NULL) {
      // a small string of length N (<= 7) ends with an ending zero followed by (7 - N) trailing 0xFF bytes.
      #ifdef KK_ARCH_LITTLE_ENDIAN
      const size_t trailing = kk_bits_clz64(~(s->u.str_value)) / 8;
      #else
      const size_t trailing = kk_bits_ctz64(~(s->u.str_value)) / 8;
      #endif
      *len = (KK_STRING_SMALL_MAX - trailing);
    }
    return &s->u.str[0];
  }
  else if (tag == KK_TAG_STRING) {
    kk_string_normal_t s = kk_datatype_as_assert(kk_string_normal_t, str, KK_TAG_STRING);
    if (len != NULL) *len = s->length;
    return &s->str[0];
  }
  else {
    kk_string_raw_t s = kk_datatype_as_assert(kk_string_raw_t, str, KK_TAG_STRING_RAW);
    if (len != NULL) *len = s->length;
    return s->cstr;
  }
}

static inline const char* kk_string_cbuf_borrow(const kk_string_t str, size_t* len) {
  return (const char*)kk_string_buf_borrow(str, len);
}

static inline int kk_string_cmp_cstr_borrow(const kk_string_t s, const char* t) {
  return strcmp(kk_string_cbuf_borrow(s,NULL), t);
}

static inline size_t kk_decl_pure kk_string_len_borrow(const kk_string_t str) {
  size_t len;
  kk_string_buf_borrow(str, &len);
  return len;
}

static inline kk_string_t kk_string_copy(kk_string_t str, kk_context_t* ctx) {
  if (kk_datatype_is_singleton(str) || kk_datatype_is_unique(str)) {
    return str;
  }
  else {
    size_t len;
    const uint8_t* buf = kk_string_buf_borrow(str, &len);
    kk_string_t tstr = kk_string_alloc_len_unsafe(len, buf, ctx);
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
  UTF-8 decoding/encoding
--------------------------------------------------------------------------------------------------*/

// Is this a utf-8 continuation byte? (0x80 <= b <= 0xBF, i.e. has the form 0x10xxxxxx)
static inline bool kk_utf8_is_cont(uint8_t c) {
  return (((int8_t)c) <= -65); // we can determine this in a single _signed_ comparison
}

// Advance to the next codepoint. (does not advance past the end)
// This should not validate, but advance to the next non-continuation byte.
static inline const uint8_t* kk_utf8_next(const uint8_t* s) {
  s++;                                // always skip first byte 
  for (; kk_utf8_is_cont(*s); s++) {} // skip continuation bytes
  return s;
}

// Retreat to the previous codepoint. 
// This should not validate, but backup to the previous non-continuation byte.
static inline const uint8_t* kk_utf8_prev(const uint8_t* s) {
  s--;                                // skip back at least 1 byte
  for (; kk_utf8_is_cont(*s); s--) {} // skip while continuation bytes
  return s;
}

// Validating mutf-8 decode; careful to only read beyond s[0] if valid.
static inline kk_char_t kk_utf8_read_validate(const uint8_t* s, size_t* count, size_t* vcount) {
  uint8_t b = s[0];
  if (kk_likely(b <= 0x7F)) {
    *count = 1;
    return b;   // ASCII fast path
  }
  // 2 byte encoding
  else if (b >= 0xC2 && b <= 0xDF && kk_utf8_is_cont(s[1])) {
    *count = 2;
    kk_char_t c = (((b & 0x1F) << 6) | (s[1] & 0x3F));
    kk_assert_internal(c >= 0x80 && c <= 0x7FF);
    return c;
  }
  // 3 byte encoding; reject overlong and utf-16 surrogate halves (0xD800 - 0xDFFF)
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
  // invalid: advance just 1 byte and encode it in the "raw" range
  else {
    *count = 1;
    if (vcount != NULL) *vcount = 4;
    kk_assert_internal(b >= 0x80);
    return (KK_RAW_UTF8_OFS + b);
  }
}

// Non-validating utf-8 decoding of a single code point
static inline kk_char_t kk_utf8_read(const uint8_t* s, size_t* count) {
  kk_char_t b = *s;
  kk_char_t c;
  if (kk_likely(b <= 0x7F)) {
    *count = 1;
    c = b; // fast path ASCII
  }
  else if (b <= 0xC1) { // invalid continuation byte or invalid 0xC0, 0xC1 (check is strictly not necessary as we don't validate..)
    *count = (size_t)(kk_utf8_next(s) - s);  // skip to next
    c = kk_char_replacement;
  }
  else if (b <= 0xDF) { // b >= 0xC2  // 2 bytes
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
  // invalid, advance just 1 byte and encode it in the "raw" range
  else {
    *count = 1;
    kk_assert_internal(b >= 0x80);
    c = KK_RAW_UTF8_OFS + b;    
  }  
#if (DEBUG!=0)
  size_t dcount;
  size_t vcount;
  kk_assert_internal(c == kk_utf8_read_validate(s, &dcount, &vcount));
  kk_assert_internal(*count == dcount);
#endif
  return c;
}

// Number of bytes needed to represent a single code point
static inline size_t kk_utf8_len(kk_char_t c) {
  if (kk_likely(c <= 0x7F)) {
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

// utf-8 encode a single codepoint
static inline void kk_utf8_write(kk_char_t c, uint8_t* s, size_t* count) {
  if (kk_likely(c <= 0x7F)) {
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


kk_decl_export uint16_t*    kk_string_to_mutf16(kk_string_t str, kk_context_t* ctx);
kk_decl_export kk_string_t  kk_string_validate_mutf8(kk_string_t str, kk_context_t* ctx);

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


kk_decl_export size_t kk_decl_pure kk_string_count_borrow(kk_string_t str);  // number of code points
kk_decl_export size_t kk_decl_pure kk_string_count(kk_string_t str, kk_context_t* ctx);  // number of code points
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
