#pragma once
#ifndef __STRING_H__
#define __STRING_H__
/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------------------
  Char as unicode point
--------------------------------------------------------------------------------------*/
typedef uint32_t char_t;

#define char_replacement  ((char_t)(0xFFFD))

static inline char_t unbox_char_t(box_t b, context_t* ctx) {
  return (char_t)unbox_int32_t(b,ctx);
}

static inline box_t box_char_t(char_t c, context_t* ctx) {
  return box_int32_t(c, ctx);
}

/*--------------------------------------------------------------------------------------
  Strings
  Always point to valid modified-UTF8 characters.
--------------------------------------------------------------------------------------*/

// A string is modified UTF-8 (with encoded zeros) ending with a '0' character.
struct string_s {
  uint8_t str[1];
};

struct string_raw_s {
  free_fun_t* free;
  const uint8_t* cstr;
};

typedef datatype_t string_t;

// Special definition of empty string so it can be used in static initializers
struct _string_s {
  block_t header;
  struct string_s str;
};
extern struct _string_s _static_string_empty;
extern string_t string_empty;

// Define string literals
#define define_string_literal(decl,name,len,chars) \
  static struct { block_t block; char str[len+1]; } _static_##name = { { HEADER_STATIC(0,TAG_STRING) }, chars }; \
  decl string_t name = { (uintptr_t)&_static_##name };   // note: should be `block_as_datatype(&_static_##name.block)` but we need as constant expression here


static inline string_t unbox_string_t(box_t v) {
  string_t s = unbox_datatype(v);
  assert_internal(datatype_is_ptr(s) && (datatype_tag(s) == TAG_STRING || datatype_tag(s) == TAG_STRING_RAW));
  return s;
}

static inline box_t box_string_t(string_t s) {
  return box_datatype(s);
}

static inline void string_drop(string_t str, context_t* ctx) {
  datatype_drop(str, ctx);
}

static inline string_t string_dup(string_t str) {
  return datatype_dup(str);
}


/*--------------------------------------------------------------------------------------
  Strings operations
--------------------------------------------------------------------------------------*/

// Allocate a string of `len` characters. Adds a terminating zero at the end.
static inline string_t string_alloc_len(size_t len, const char* s, context_t* ctx) {
  struct string_s* str = ptr_data_as(struct string_s, ptr_alloc(sizeof(struct string_s) - 1 /* char str[1] */ + len + 1 /* 0 terminator */, 0, TAG_STRING, ctx));
  if (s != 0) {
    memcpy(&str->str[0], s, len);
  }
  str->str[len] = 0;
  // todo: assert valid UTF8 in debug mode
  return datatype_from_data(str);
}

static inline string_t string_alloc_buf(size_t len, context_t* ctx) {
  return string_alloc_len(len, NULL, ctx);
}

static inline string_t string_alloc_dup(const char* s, context_t* ctx) {
  return (s==NULL ? string_alloc_len(0, "", ctx) : string_alloc_len(strlen(s), s, ctx));
}

static inline string_t string_alloc_raw(const char* s, bool free, context_t* ctx) {
  struct string_raw_s* str = datatype_alloc_data_as(struct string_raw_s, 0, TAG_STRING_RAW, ctx);
  str->free = (free ? &runtime_free : &free_fun_null);
  str->cstr = (const uint8_t*)s;
  // todo: assert valid UTF8 in debug mode
  return datatype_from_data(str);
}

static inline const uint8_t* string_buf_borrow(string_t str) {
  if (datatype_tag(str) == TAG_STRING) {
    return &datatype_data_as_assert(struct string_s, str, TAG_STRING)->str[0];
  }
  else {
    return datatype_data_as_assert(struct string_raw_s, str, TAG_STRING_RAW)->cstr;
  }
}

static inline char* string_cbuf_borrow(string_t str) {
  return (char*)string_buf_borrow(str);
}

static inline int string_cmp_cstr_borrow(string_t s, const char* t) {
  return strcmp(string_cbuf_borrow(s), t);
}


/*--------------------------------------------------------------------------------------------------
  UTF8 decoding/encoding
--------------------------------------------------------------------------------------------------*/

// Is this a UTF8 continuation byte? (0x80 <= b <= 0xBF, i.e. has the form 0x10xxxxxx)
static inline bool utf8_is_cont(uint8_t c) {
  return (((int8_t)c) <= -65); // we can determine this in a single _signed_ comparison
}

// Advance to the next codepoint. (does not advance past the end)
// This should not validate, but advance to the next non-continution byte.
static inline const uint8_t* utf8_next(const uint8_t* s) {
  if (*s != 0) s++;                // skip first byte except if 0
  for (; utf8_is_cont(*s); s++) {} // skip continuation bytes
  return s;
}

// Retreat to the previous codepoint. 
// This should not validate, but backup to the previous non-continution byte.
static inline const uint8_t* utf8_prev(const uint8_t* s) {
  s--;                             // skip back at least 1 byte
  for (; utf8_is_cont(*s); s--) {} // skip while continuation bytes
  return s;
}

// Non-validating utf8 decoding of a single code point
static inline char_t utf8_read(const uint8_t* s, size_t* count) {
  char_t b = *s;
  char_t c;
  if (likely(b <= 0x7F)) {
    *count = 1;
    c = b; // fast path ASCII
  }
  else if (b <= 0xBF) { // invalid continuation byte (check is strictly not necessary as we don't validate..)
    *count = (utf8_next(s) - s);  // skip to next
    c = char_replacement;
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
    *count = (utf8_next(s) - s);  // skip to next
    c = char_replacement;
  }
#if DEBUG
  size_t dcount;
  assert_internal(c == utf8_read_validate(s, &dcount));
  assert_internal(*count = dcount);
#endif
  return c;
}

// Validating UTF8 decode; careful to only read beyond s[0] if valid.
static inline char_t utf8_read_validate(const uint8_t* s, size_t* count) {
  uint8_t b = s[0];
  if (likely(b <= 0x7F)) {
    *count = 1;
    return b;   // ASCII fast path
  }
  else if (b == 0xC0 && s[1] == 0x80) {
    *count = 2;
    return 0;   // Modified UTF8 encoded zero
  }
  // 2 byte encoding
  else if (b >= 0xC2 && b <= 0xDF && utf8_is_cont(s[1])) {
    *count = 2;
    char_t c = (((b & 0x1F) << 6) | (s[1] & 0x3F));
    assert_internal(c >= 0x80 && c <= 0x7FF);
    return c;
  }
  // 3 byte encoding; reject overlong and UTF16 surrogate halves (0xD800 - 0xDFFF)
  else if ((b == 0xE0 && s[1] >= 0xA0 && s[1] <= 0xBF && utf8_is_cont(s[2]))
    || (b >= 0xE1 && b <= 0xEC && utf8_is_cont(s[1]) && utf8_is_cont(s[2])))
  {
    *count = 3;
    char_t c = (((b & 0x0F) << 12) | ((s[1] & 0x3F) << 6) | (s[2] & 0x3F));
    assert_internal(c >= 0x800 && (c < 0x0D800 || c > 0xDFFF) && c <= 0xFFFF);
    return c;
  }
  // 4 byte encoding; reject overlong and out of bounds (> 0x10FFFF)
  else if ((b == 0xF0 && s[1] >= 0x90 && s[1] <= 0xBF && utf8_is_cont(s[2]) && utf8_is_cont(s[3]))
    || (b >= 0xF1 &&  b <= 0xF3 && utf8_is_cont(s[1]) && utf8_is_cont(s[2]) && utf8_is_cont(s[3]))
    || (b == 0xF4 && s[1] >= 0x80 && s[1] <= 0x8F && utf8_is_cont(s[2]) && utf8_is_cont(s[3])))
  {
    *count = 4;
    char_t c = (((b & 0x07) << 18) | ((s[1] & 0x3F) << 12) | ((s[2] & 0x3F) << 6) | (s[3] & 0x3F));
    assert_internal(c >= 0x10000 && c <= 0x10FFFF);
    return c;
  }
  // invalid, skip continuation bytes
  // note: we treat a full illegal sequence as 1 invalid codepoint (including its continuation bytes)
  // this is important as it allows later to pre-allocate a buffer of the right size even if some
  // sequences are invalid.
  else {
    *count = (utf8_next(s) - s);
    return char_replacement;
  }
}

// Number of bytes needed to represent a single code point
static inline size_t utf8_len(char_t c) {
  if (unlikely(c == 0)) {
    return 2;
  }
  else if (likely(c <= 0x7F)) {
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
static inline void utf8_write(char_t c, uint8_t* s, size_t* count) {
  if (unlikely(c == 0)) {
    // modified UTF8 zero
    *count = 2;
    s[0] = 0xC0;
    s[1] = 0x80;
  }
  else if (likely(c <= 0x7F)) {
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
decl_export size_t decl_pure string_len(string_t  str);    // bytes in UTF8
decl_export size_t decl_pure string_count(string_t str);  // number of code points

decl_export intx_t string_cmp_borrow(string_t str1, string_t str2);
decl_export intx_t string_cmp(string_t str1, string_t str2, context_t* ctx);
decl_export intx_t string_icmp_borrow(string_t str1, string_t str2);             // ascii case insensitive
decl_export intx_t string_icmp(string_t str1, string_t str2, context_t* ctx);    // ascii case insensitive

static inline bool string_is_eq(string_t s1, string_t s2, context_t* ctx) {
  return (string_cmp(s1, s2, ctx) == 0);
}
static inline bool string_is_neq(string_t s1, string_t s2, context_t* ctx) {
  return (string_cmp(s1, s2, ctx) != 0);
}

decl_export string_t string_cat(string_t s1, string_t s2, context_t* ctx);

decl_export string_t string_from_char(char_t c, context_t* ctx);
decl_export string_t string_from_chars(vector_t v, context_t* ctx);
decl_export vector_t string_to_chars(string_t s, context_t* ctx);

decl_export string_t integer_to_string(integer_t x, context_t* ctx);
decl_export string_t integer_to_hex_string(integer_t x, bool use_capitals, context_t* ctx);

decl_export vector_t string_splitv(string_t s, string_t sep, context_t* ctx);
decl_export vector_t string_splitv_atmost(string_t s, string_t sep, uintx_t n, context_t* ctx);

decl_export string_t string_repeat(string_t s, uintx_t n, context_t* ctx);

decl_export unit_t println(string_t s, context_t* ctx);
decl_export unit_t print(string_t s, context_t* ctx);
decl_export unit_t trace(string_t s, context_t* ctx);
decl_export unit_t trace_any(string_t s, box_t x, context_t* ctx);
decl_export string_t show_any(box_t x, context_t* ctx);

decl_export string_t double_show_fixed(double d, int32_t prec, context_t* ctx);
decl_export string_t double_show_exp(double d, int32_t prec, context_t* ctx);


#endif // include guard
