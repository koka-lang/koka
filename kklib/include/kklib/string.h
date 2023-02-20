#pragma once
#ifndef KK_STRING_H
#define KK_STRING_H
/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------------------------------------------
  Strings.

  Always point to valid utf-8 byte sequences ending with a 0 byte (not included in the length).
  Since we stay strictly in valid utf-8, that means that strings can contain internal 0 characters
  and we cannot generally use C string functions to manipulate our strings.

  There are four possible representations for strings:
  
  - singleton empty string
  - small string of at most 7 utf-8 bytes
  - normal string of utf-8 bytes
  - raw string pointing to a buffer of utf-8 bytes
  
  These are not necessarily canonical (e.g. a normal or small string can have length 0 besides being empty)

  Strings use kk_bytes_t directly: they are just bytes except always contain valid utf-8.
  This allows for better code sharing, and efficient conversion from bytes to strings in-place when
  possible. It does mean we need to be very careful to never accidentally pass a kk_bytes_t to a string
  function that may rely on the bytes being valid utf-8!
-------------------------------------------------------------------------------------------------------------*/

/*-------------------------------------------------------------------------------------------------------------
  qutf-8 and qutf-16
  
  There few important cases where *external* text is not quite utf-8 or utf-16.
  We call these "qutf-8" and "qutf-16" for "quite like" utf-8/16:

  - qutf-8: this is mostly utf-8, but allows invalid utf-8 like overlong sequences or lone
    continuation bytes -- as such, any byte sequence is valid qutf-8. This occurs a lot in
    practice, for example by bad json encoding containing binary data, but also as a result 
    of a _locale_ that cannot be decoded properly, or generally just random byte input.

  - qutf-16: this is mostly utf-16 but allows again any invalid utf-16 which consists
    of lone halves of surrogate pairs -- and again, any sequence of uint16_t is valid qutf-16. 
    This is actually what is used in Windows file names and JavaScript. 
  
  In particular for qutf-16 we would like to guarantee that decoding to utf-8 and encoding 
  again to qutf-16 is an identity transformation -- for example, we may list the contents
  of a directory and then try to read each file. As a consequence we cannot replace invalid
  codes in qutf-16 with a generic replacement character. One proposed solution for this is 
  to use wtf-8 (used in Rust <https://github.com/rust-lang/rust/issues/12056#issuecomment-55786546>) 
  instead of utf-8 internally. 
  
  We like to use strict utf-8 internally though, so we can always output valid utf-8 directly
  without further conversions. (also, new formats like wtf-8 often have tricky edge cases, like   
  naively appending strings may change the interpretation of surrogate pairs in wtf-8)

  Instead, we solve this by staying in strict utf-8 internally, but we reserve a
  particular set of code-points to have a special meaning when converting to/from qutf-8 and qutf-16.
  For now, we use an (hopefully forever) unassigned range in the "supplementary special-purpose plane" (14)
  
  - ED800 - EDFFF: corresponds to a lone half `h` of a surrogate pair where `h = code - E0000`.
  - EE000 - EE07F: <unused>
  - EE080 - EE0FF: corresponds to an invalid byte `b` in an invalid utf-8 sequence where `b = code - EE000`.
                   (note: invalid bytes in utf-8 are always >= 0x80 so we need only a limited range).
  
  We call this the "raw range". The advantage over using the replacement character is that we 
  now retain full information what the original (invalid) sequences were (and can thus do an 
  identity transform) -- and we stay with valid utf-8. Moreover, we can handle both invalid
  utf-8 and invalid utf-16 with this.
  
  When decoding qutf-8/16 to utf-8, we decode invalid sequences to these code points; and only when
  encoding back to qutf-8/16, we encode these code points specially again to make this an identity
  transformation. 
  
  _Otherwise these are just regular code points and valid utf-8 with no special treatment_.

  Security wise this is also good practice -- for example, we decode the overlong qutf-8 
  sequence `0xC0 0x80` not to a 0 character, but to two raw code points: 0xEE0C0 0xEE080. This 
  way, we maintain an identity transform while still preventing hidden embedded 0 characters.
  
  (Actually, to make it a true identity transform, when decoding qutf-8/16 we also need to treat
   bytes/surrogate pairs that happen be code points in our raw range as an invalid sequence.
   This should be fine in practice as these are unassigned anyways). 
------------------------------------------------------------------------------------------------------------*/

#define KK_RAW_PLANE      ((kk_char_t)(0xE0000))
#define KK_RAW_UTF8_OFS   (KK_RAW_PLANE + 0xE000)
#define KK_RAW_UTF16_OFS  (KK_RAW_PLANE)

// A string is valid utf-8 (with potentially internal '0' characters) ending with a '0' character.
// Reuse the `kk_bytes_t` datatype.
typedef struct kk_string_s {
  kk_bytes_t bytes;
} kk_string_t;


kk_decl_export bool kk_utf8_is_validn(kk_ssize_t len, const uint8_t* s);
kk_decl_export bool kk_utf8_is_valid(const char* s);

// Cast bytes to a string; only use when the bytes are for sure valid utf-8!
static inline kk_string_t kk_unsafe_bytes_as_string_unchecked(kk_bytes_t b) {
  kk_string_t s = { b };
  return s;
}

static inline kk_string_t kk_unsafe_bytes_as_string(kk_bytes_t b) {
  kk_assert_internal(kk_datatype_tag(b,kk_get_context()) == KK_TAG_BOX_ANY || kk_utf8_is_valid(kk_bytes_cbuf_borrow(b, NULL, kk_get_context())));
  return kk_unsafe_bytes_as_string_unchecked(b);
}

static inline kk_string_t kk_string_empty() {
  return kk_unsafe_bytes_as_string( kk_bytes_empty() );
}

// Define string literals
#if 0
#define kk_define_string_literal(decl,name,len,chars) \
  static struct { struct kk_bytes_s _base; size_t length; char str[len+1]; } _static_##name = \
    { { { KK_HEADER_STATIC(0,KK_TAG_STRING) } }, len, chars }; \
  decl kk_string_t name = { { (intptr_t)&_static_##name._base._block } };  
#else
#define kk_declare_string_literal(decl,name,len,chars) \
  static kk_ssize_t  _static_len_##name = len; \
  static const char* _static_##name = chars; \
  decl kk_string_t name = { { kk_datatype_null_init } };

#define kk_init_string_literal(name,ctx) \
  if (kk_datatype_is_null(name.bytes)) { name = kk_string_alloc_from_utf8n(_static_len_##name, _static_##name, ctx); }  

#define kk_define_string_literal(decl,name,len,chars,ctx) \
  kk_declare_string_literal(decl,name,len,chars) \
  kk_init_string_literal(name,ctx)

#endif

static inline kk_string_t kk_string_unbox(kk_box_t v) {
  return kk_unsafe_bytes_as_string( kk_bytes_unbox(v) );
}

static inline kk_box_t kk_string_box(kk_string_t s) {
  return kk_bytes_box(s.bytes);
}

static inline void kk_string_drop(kk_string_t str, kk_context_t* ctx) {
  kk_bytes_drop(str.bytes, ctx);
}

static inline kk_string_t kk_string_dup(kk_string_t str, kk_context_t* ctx) {
  return kk_unsafe_bytes_as_string(kk_bytes_dup(str.bytes,ctx));
}


/*--------------------------------------------------------------------------------------
  Char as unicode point
--------------------------------------------------------------------------------------*/
typedef int32_t kk_char_t;

#define kk_char_replacement   KK_I32(0xFFFD)

static inline kk_char_t kk_char_unbox(kk_box_t b, kk_borrow_t borrow, kk_context_t* ctx) {
  return (kk_char_t)kk_int32_unbox(b, borrow, ctx);
}

static inline kk_box_t kk_char_box(kk_char_t c, kk_context_t* ctx) {
  return kk_int32_box(c, ctx);
}

static inline bool kk_ascii_is_digit(char c) { return (c >= '0' && c <= '9'); }
static inline bool kk_ascii_is_lower(char c) { return (c >= 'a' && c <= 'z'); }
static inline bool kk_ascii_is_upper(char c) { return (c >= 'A' && c <= 'Z'); }
static inline bool kk_ascii_is_control(char c) { return (c < ' '); }
static inline bool kk_ascii_is_white(char c) { return (c == ' ' || c == '\t' || c == '\n' || c == '\r'); }
static inline bool kk_ascii_is_hexdigit(char c) { return (kk_ascii_is_digit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')); }
static inline bool kk_ascii_is_alpha(char c) { return (kk_ascii_is_lower(c) || kk_ascii_is_upper(c)); }
static inline bool kk_ascii_is_alphanum(char c) { return (kk_ascii_is_alpha(c) || kk_ascii_is_digit(c)); }



/*--------------------------------------------------------------------------------------
  Strings operations
--------------------------------------------------------------------------------------*/

static inline kk_ssize_t kk_sstrlen(const char* s) {
  return kk_to_ssize_t(strlen(s));
}

static inline kk_string_t kk_string_adjust_length(kk_string_t str, kk_ssize_t newlen, kk_context_t* ctx) {
  return kk_unsafe_bytes_as_string(kk_bytes_adjust_length(str.bytes, newlen, ctx));
}

// allocate an uninitialized string buffer; ensure to initialize to valid utf-8
static inline kk_string_t kk_unsafe_string_alloc_buf(kk_ssize_t len, uint8_t** buf, kk_context_t* ctx) {
  return kk_unsafe_bytes_as_string_unchecked(kk_bytes_alloc_buf(len, buf, ctx));
}

// allocate an uninitialized string buffer; ensure to initialize to valid utf-8
static inline kk_string_t kk_unsafe_string_alloc_cbuf(kk_ssize_t len, char** buf, kk_context_t* ctx) {
  return kk_unsafe_string_alloc_buf(len, (uint8_t**)buf, ctx);
}


// must be guaranteed valid utf8
static inline kk_string_t kk_string_alloc_dupn_valid_utf8(kk_ssize_t len, const uint8_t* s, kk_context_t* ctx) {
  kk_assert_internal(kk_utf8_is_validn(len, s));
  if (s == NULL || len == 0) return kk_string_empty();
  return kk_unsafe_bytes_as_string(kk_bytes_alloc_dupn(len, s, ctx));
}

// must be guaranteed valid utf8
static inline kk_string_t kk_string_alloc_dup_valid_utf8(const char* s, kk_context_t* ctx) { 
  kk_assert_internal(kk_utf8_is_valid(s));
  if (s == NULL) return kk_string_empty();
  return kk_string_alloc_dupn_valid_utf8( kk_sstrlen(s), (const uint8_t*)s, ctx);
}

/*
// must be guaranteed valid utf8
static inline kk_string_t kk_string_alloc_dup_valid_utf8_atmost(size_t maxlen, const char* s, kk_context_t* ctx) {
  kk_assert_internal(kk_utf8_is_validn(maxlen,(const uint8_t*)s));
  if (s == NULL || maxlen == 0) return kk_string_empty();
  size_t n;
  for(n = 0; n < maxlen && s[n] != 0; n++) { }
  return kk_string_alloc_dupn_valid_utf8(n, (const uint8_t*)s, ctx);
}
*/

// Raw string that directly points to an external buffer.
static inline kk_string_t kk_string_alloc_raw_len(kk_ssize_t len, const char* s, bool free, kk_context_t* ctx) {
  if (len == 0 || s==NULL) return kk_string_empty();
  kk_assert_internal(s[len]==0 && kk_sstrlen(s)==len);
  kk_assert_internal(kk_utf8_is_valid(s));
  return kk_unsafe_bytes_as_string(kk_bytes_alloc_raw_len(len, (const uint8_t*)s, free, ctx));
}

static inline kk_string_t kk_string_alloc_raw(const char* s, bool free, kk_context_t* ctx) {
  if (s==NULL) return kk_string_empty();
  return kk_string_alloc_raw_len(kk_sstrlen(s), s, free, ctx);
}

static inline const uint8_t* kk_string_buf_borrow(const kk_string_t str, kk_ssize_t* len, kk_context_t* ctx) {
  return kk_bytes_buf_borrow(str.bytes, len, ctx);  
}

static inline const char* kk_string_cbuf_borrow(const kk_string_t str, kk_ssize_t* len, kk_context_t* ctx) {
  return (const char*)kk_string_buf_borrow(str, len, ctx);
}

static inline int kk_string_cmp_cstr_borrow(const kk_string_t s, const char* t, kk_context_t* ctx) {
  return strcmp(kk_string_cbuf_borrow(s,NULL,ctx), t);
}

static inline kk_ssize_t kk_decl_pure kk_string_len_borrow(const kk_string_t str, kk_context_t* ctx) {
  return kk_bytes_len_borrow(str.bytes,ctx);
}

static inline kk_ssize_t kk_decl_pure kk_string_len(kk_string_t str, kk_context_t* ctx) {    // bytes in UTF8
  kk_ssize_t len = kk_string_len_borrow(str,ctx);
  kk_string_drop(str, ctx);
  return len;
}

static inline kk_integer_t kk_decl_pure kk_string_len_int(kk_string_t str, kk_context_t* ctx) {    // bytes in UTF8
  return kk_integer_from_ssize_t(kk_string_len(str,ctx),ctx);
}

static inline kk_string_t kk_string_copy(kk_string_t str, kk_context_t* ctx) {
  return kk_unsafe_bytes_as_string(kk_bytes_copy(str.bytes, ctx));
}

static inline bool kk_string_ptr_eq_borrow(kk_string_t s1, kk_string_t s2) {
  return kk_bytes_ptr_eq_borrow(s1.bytes, s2.bytes);
}

static inline bool kk_string_is_empty_borrow(kk_string_t s, kk_context_t* ctx) {
  return (kk_string_len_borrow(s,ctx) == 0);
}

static inline bool kk_string_is_empty(kk_string_t s, kk_context_t* ctx) {
  return (kk_string_len(s, ctx) == 0);
}

/*--------------------------------------------------------------------------------------------------
  UTF-8 decoding/encoding
--------------------------------------------------------------------------------------------------*/

// Is this a utf-8 continuation byte? (0x80 <= b <= 0xBF, i.e. has the form 0x10xxxxxx)
static inline bool kk_utf8_is_cont(uint8_t c) {
  // return (((int8_t)c) <= -65); // we can determine this in a single _signed_ comparison
  return ((c & 0xC0) == 0x80);
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

// utf-8 valitating read.
kk_decl_export kk_char_t kk_utf8_read_validate(const uint8_t* s, kk_ssize_t* count, kk_ssize_t* vcount, bool qutf8_identity );

// Non-validating utf-8 decoding of a single code point
kk_decl_export kk_char_t kk_utf8_readx(const uint8_t* s, kk_ssize_t* count);
static inline kk_char_t  kk_utf8_read(const uint8_t* s, kk_ssize_t* count) {
  kk_char_t c = *s;
  if kk_likely(c <= 0x7F) {
    *count = 1;
    return c;
  }
  else {
    return kk_utf8_readx(s, count);
  }
}

// Number of bytes needed to represent a single code point
kk_decl_export kk_ssize_t kk_utf8_lenx(kk_char_t c);
static inline  kk_ssize_t kk_utf8_len(kk_char_t c) {
  return kk_likely(c <= 0x7F) ? 1 : kk_utf8_lenx(c);
}

// utf-8 encode a single codepoint
kk_decl_export void kk_utf8_writex(kk_char_t c, uint8_t* s, kk_ssize_t* count);
static inline  void kk_utf8_write(kk_char_t c, uint8_t* s, kk_ssize_t* count) {
  if kk_likely(c <= 0x7F) {
    *count = 1;
    s[0] = (uint8_t)c;
  }
  else kk_utf8_writex(c, s, count);
}



/*--------------------------------------------------------------------------------------------------
  utf-8 string conversion to qutf8 and qutf16
--------------------------------------------------------------------------------------------------*/

kk_decl_export kk_string_t    kk_string_alloc_from_qutf8(const char* str, kk_context_t* ctx);
kk_decl_export kk_string_t    kk_string_alloc_from_qutf8n(kk_ssize_t len, const char* str, kk_context_t* ctx);

kk_decl_export kk_string_t    kk_string_alloc_from_utf8(const char* str, kk_context_t* ctx);
kk_decl_export kk_string_t    kk_string_alloc_from_utf8n(kk_ssize_t len, const char* str, kk_context_t* ctx);

kk_decl_export kk_string_t    kk_string_alloc_from_qutf16(const uint16_t* wstr, kk_context_t* ctx);
kk_decl_export kk_string_t    kk_string_alloc_from_qutf16n(kk_ssize_t len, const uint16_t* wstr, kk_context_t* ctx);

kk_decl_export kk_string_t    kk_string_alloc_from_utf16(const uint16_t* wstr, kk_context_t* ctx);
kk_decl_export kk_string_t    kk_string_alloc_from_utf16n(kk_ssize_t len, const uint16_t* wstr, kk_context_t* ctx);

kk_decl_export kk_string_t    kk_string_alloc_from_codepage(const uint8_t* bstr, const uint16_t* codepage /*NULL == windows-1252*/, kk_context_t* ctx);

kk_decl_export kk_string_t    kk_string_convert_from_qutf8(kk_bytes_t b, kk_context_t* ctx);

kk_decl_export uint16_t*      kk_string_to_qutf16_borrow(kk_string_t str, kk_context_t* ctx);
kk_decl_export const char*    kk_string_to_qutf8_borrow(kk_string_t str, bool* should_free, kk_context_t* ctx);


#define kk_with_string_as_qutf8_borrow(str,ustr,ctx) /* { action } */ \
  bool should_free_##ustr; \
  for( const char* ustr = kk_string_to_qutf8_borrow(str,&should_free_##ustr,ctx); ustr != nullptr; \
      ustr = (should_free_##ustr ? (kk_free(ustr,ctx), nullptr) : nullptr) )

#define kk_with_string_as_qutf16_borrow(str,wstr,ctx) /* { action } */ \
  for( const uint16_t* wstr = kk_string_to_qutf16_borrow(str,ctx); wstr != nullptr; kk_free(wstr,ctx), wstr = nullptr )

#define kk_with_string_as_qutf16w_borrow(str,wstr,ctx) /* { action } */ \
  for( const wchar_t* wstr = (const wchar_t*)kk_string_to_qutf16_borrow(str,ctx); wstr != nullptr; kk_free(wstr,ctx), wstr = nullptr )

static inline kk_string_t    kk_string_alloc_from_qutf16w(const wchar_t* wstr, kk_context_t* ctx) {
  return kk_string_alloc_from_qutf16((const uint16_t*)wstr, ctx);
}

/*--------------------------------------------------------------------------------------------------
  Utilities that can use the bytes functions
--------------------------------------------------------------------------------------------------*/

static inline int kk_string_cmp_borrow(kk_string_t str1, kk_string_t str2, kk_context_t* ctx) {
  return kk_bytes_cmp_borrow(str1.bytes, str2.bytes,ctx);
}

static inline int kk_string_cmp(kk_string_t str1, kk_string_t str2, kk_context_t* ctx) {
  return kk_bytes_cmp(str1.bytes, str2.bytes, ctx);
}

static inline bool kk_string_is_eq_borrow(kk_string_t s1, kk_string_t s2, kk_context_t* ctx) {
  return (kk_string_cmp_borrow(s1, s2, ctx) == 0);
}

static inline bool kk_string_is_neq_borrow(kk_string_t s1, kk_string_t s2, kk_context_t* ctx) {
  return (kk_string_cmp_borrow(s1, s2, ctx) != 0);
}

static inline bool kk_string_is_eq(kk_string_t s1, kk_string_t s2, kk_context_t* ctx) {
  return (kk_string_cmp(s1, s2, ctx) == 0);
}

static inline bool kk_string_is_neq(kk_string_t s1, kk_string_t s2, kk_context_t* ctx) {
  return (kk_string_cmp(s1, s2, ctx) != 0);
}

static inline kk_string_t kk_string_cat(kk_string_t s1, kk_string_t s2, kk_context_t* ctx) {
  return kk_unsafe_bytes_as_string(kk_bytes_cat(s1.bytes, s2.bytes, ctx));
}
static inline kk_string_t kk_string_cat_from_valid_utf8(kk_string_t s1, const char* s2, kk_context_t* ctx) {
  kk_assert_internal(kk_utf8_is_valid(s2));
  return kk_unsafe_bytes_as_string(kk_bytes_cat_from_buf(s1.bytes, kk_sstrlen(s2), (const uint8_t*)s2, ctx));
}

static inline kk_string_t kk_string_replace_all(kk_string_t s, kk_string_t pat, kk_string_t rep, kk_context_t* ctx) {
  return kk_unsafe_bytes_as_string(kk_bytes_replace_all(s.bytes, pat.bytes, rep.bytes, ctx));
}

static inline kk_string_t kk_string_replace_atmost(kk_string_t s, kk_string_t pat, kk_string_t rep, kk_ssize_t n, kk_context_t* ctx) {
  return kk_unsafe_bytes_as_string(kk_bytes_replace_atmost(s.bytes, pat.bytes, rep.bytes, n, ctx));
}

static inline kk_string_t kk_string_repeat(kk_string_t s, kk_ssize_t n, kk_context_t* ctx) {
  return kk_unsafe_bytes_as_string(kk_bytes_repeat(s.bytes, n, ctx));
}

static inline kk_ssize_t kk_string_index_of1(kk_string_t str, kk_string_t sub, kk_context_t* ctx) {     // returns 0 for not found, or index + 1
  return kk_bytes_index_of1(str.bytes, sub.bytes, ctx);
}

static inline kk_ssize_t kk_string_last_index_of1(kk_string_t str, kk_string_t sub, kk_context_t* ctx) {
  return kk_bytes_last_index_of1(str.bytes, sub.bytes, ctx);
}

static inline bool   kk_string_starts_with(kk_string_t str, kk_string_t pre, kk_context_t* ctx) {
  return kk_bytes_starts_with(str.bytes, pre.bytes, ctx);
}

static inline bool   kk_string_ends_with(kk_string_t str, kk_string_t post, kk_context_t* ctx) {
  return kk_bytes_ends_with(str.bytes, post.bytes, ctx);
}

static inline bool   kk_string_contains(kk_string_t str, kk_string_t sub, kk_context_t* ctx) {
  return kk_bytes_contains(str.bytes, sub.bytes, ctx);
}


/*--------------------------------------------------------------------------------------------------
  Utilities that are string specific
--------------------------------------------------------------------------------------------------*/

kk_decl_export kk_ssize_t kk_decl_pure kk_string_count_borrow(kk_string_t str, kk_context_t* ctx);  // number of code points
kk_decl_export kk_ssize_t kk_decl_pure kk_string_count(kk_string_t str, kk_context_t* ctx);  // number of code points
kk_decl_export kk_ssize_t kk_decl_pure kk_string_count_pattern_borrow(kk_string_t str, kk_string_t pattern, kk_context_t* ctx);

kk_decl_export int kk_string_icmp_borrow(kk_string_t str1, kk_string_t str2, kk_context_t* ctx);             // ascii case insensitive
kk_decl_export int kk_string_icmp(kk_string_t str1, kk_string_t str2, kk_context_t* ctx);    // ascii case insensitive


kk_decl_export kk_string_t kk_string_from_char(kk_char_t c, kk_context_t* ctx);
kk_decl_export kk_string_t kk_string_from_chars(kk_vector_t v, kk_context_t* ctx);
kk_decl_export kk_vector_t kk_string_to_chars(kk_string_t s, kk_context_t* ctx);

kk_decl_export kk_string_t kk_integer_to_string(kk_integer_t x, kk_context_t* ctx);
kk_decl_export kk_string_t kk_integer_to_hex_string(kk_integer_t x, bool use_capitals, kk_context_t* ctx);

kk_decl_export kk_vector_t kk_string_splitv(kk_string_t s, kk_string_t sep, kk_context_t* ctx);
kk_decl_export kk_vector_t kk_string_splitv_atmost(kk_string_t s, kk_string_t sep, kk_ssize_t n, kk_context_t* ctx);

kk_decl_export kk_string_t  kk_string_to_upper(kk_string_t str, kk_context_t* ctx);
kk_decl_export kk_string_t  kk_string_to_lower(kk_string_t strs, kk_context_t* ctx);
kk_decl_export kk_string_t  kk_string_trim_left(kk_string_t strs, kk_context_t* ctx);
kk_decl_export kk_string_t  kk_string_trim_right(kk_string_t strs, kk_context_t* ctx);

kk_decl_export kk_unit_t   kk_println(kk_string_t s, kk_context_t* ctx);
kk_decl_export kk_unit_t   kk_print(kk_string_t s, kk_context_t* ctx);
kk_decl_export kk_unit_t   kk_trace(kk_string_t s, kk_context_t* ctx);
kk_decl_export kk_unit_t   kk_trace_any(kk_string_t s, kk_box_t x, kk_context_t* ctx);
kk_decl_export kk_string_t kk_show_any(kk_box_t x, kk_context_t* ctx);

kk_decl_export kk_string_t kk_double_show_fixed(double d, int32_t prec, kk_context_t* ctx);
kk_decl_export kk_string_t kk_double_show_exp(double d, int32_t prec, kk_context_t* ctx);
kk_decl_export kk_string_t kk_double_show(double d, int32_t prec, kk_context_t* ctx);


#endif // include guard
