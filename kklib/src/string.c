/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
#ifndef  _CRT_SECURE_NO_WARNINGS
#define  _CRT_SECURE_NO_WARNINGS
#endif
#ifndef  __USE_MINGW_ANSI_STDIO
#define  __USE_MINGW_ANSI_STDIO   // so %z is valid on mingw
#endif

#include "kklib.h"
#include <string.h>
#include <stdio.h>


// Allow reading aligned words as long as some bytes in it are part of a valid C object
#define KK_ARCH_ALLOW_WORD_READS  (1)  

static uint8_t kk_ascii_toupper(uint8_t c) {
  return (c >= 'a' && c <= 'z' ? c - 'a' + 'A' : c);
}
static uint8_t kk_ascii_tolower(uint8_t c) {
  return (c >= 'A' && c <= 'Z' ? c - 'A' + 'a' : c);
}
static uint8_t kk_ascii_iswhite(uint8_t c) {
  return (c == ' ' || c == '\t' || c == '\n' || c == '\r');
}

static int kk_memicmp(const uint8_t* s, const uint8_t* t, kk_ssize_t len) {
  if (s==t) return 0;
  for (kk_ssize_t i = 0; i < len; i++) {
    uint8_t c = kk_ascii_tolower(*s); s++;
    uint8_t d = kk_ascii_tolower(*t); t++;
    if (c != d) return (c < d ? -1 : 1);
  }
  return 0;
}

static kk_ssize_t kk_wcslen(const uint16_t* wstr) {
  if (wstr == NULL) return 0;
  const uint16_t* p;
  for (p = wstr; *p != 0; p++) {}
  return (p - wstr);
}


int kk_string_icmp_borrow(kk_string_t str1, kk_string_t str2, kk_context_t* ctx) {
  kk_ssize_t len1;
  const uint8_t* s1 = kk_string_buf_borrow(str1, &len1, ctx);
  kk_ssize_t len2;
  const uint8_t* s2 = kk_string_buf_borrow(str2, &len2, ctx);
  kk_ssize_t minlen = (len1 <= len2 ? len1 : len2);
  int ord = kk_memicmp(s1, s2, minlen);
  if (ord == 0) {
    if (len1 > len2) return 1;
    else if (len1 < len2) return -1;
  }
  return ord;
}

int kk_string_icmp(kk_string_t str1, kk_string_t str2, kk_context_t* ctx) {
  int ord = kk_string_icmp_borrow(str1, str2, ctx);
  kk_string_drop(str1, ctx);
  kk_string_drop(str2, ctx);
  return ord;
}


// Count code points in a valid utf-8 string.
kk_ssize_t kk_decl_pure kk_string_count_borrow(kk_string_t str, kk_context_t* ctx) {
  kk_ssize_t len;
  const uint8_t* s = kk_string_buf_borrow(str, &len, ctx);
  kk_ssize_t cont = 0;      // continuation character counts
  const uint8_t* t = s; // current position 
  const uint8_t* end = t + len;
  kk_assert_internal(*end == 0);

  // advance per byte until aligned
  for (; ((((uintptr_t)t) % sizeof(kk_uintx_t)) != 0) && (t < end); t++) {
    // count continuation bytes
    if (kk_utf8_is_cont(*t)) cont++;
  }
  // advance per sizeof(kk_uintx_t). 
  if (t < end) {
    kk_assert_internal(((uintptr_t)t) % sizeof(kk_uintx_t) == 0);
    const kk_uintx_t* p = (const kk_uintx_t*)t;
    const kk_uintx_t* pend = (const kk_uintx_t*)(((uintptr_t)end / sizeof(kk_uintx_t)) * sizeof(kk_uintx_t)); // align down
    for (; p < pend; p++) {
      // count continuation bytes (0b10xxxxxx bytes) in parallel
      const kk_uintx_t u = *p;
      const kk_uintx_t m = ((u & kk_mask_bytes_hi_bit64) >> 7) & ((~u) >> 6); // each byte in `m` is 0x01 iff it was a continuation byte
      cont += kk_bits_byte_sum(m);
    }
    t = (const uint8_t*)p; // restore t
  }

  // advance per byte until reaching the end
  for (; t < end; t++) {
    // count continuation characters
    if (kk_utf8_is_cont(*t)) cont++;
  }
  kk_assert_internal(t == end);
  kk_assert_internal(len == (t - s));
  kk_assert_internal(len == 0 || len > cont);
  return (len - cont);
}

kk_ssize_t kk_decl_pure kk_string_count(kk_string_t str, kk_context_t* ctx) {
  kk_ssize_t count = kk_string_count_borrow(str,ctx);
  kk_string_drop(str, ctx);
  return count;
}


/*--------------------------------------------------------------------------------------------------
 Utf-8 read/write
--------------------------------------------------------------------------------------------------*/

kk_ssize_t kk_utf8_lenx(kk_char_t c) {
  if kk_likely(c <= 0x7F) {
    return 1;
  }
  else if (c <= 0x07FF) {
    return 2;
  }

  if (c >= 0xD800 && c <= 0xDFFF) {
    c += KK_RAW_UTF16_OFS;   // encode in raw range to maintain valid utf-8
  }
  else if (c > 0x10FFFF) {
    c = kk_char_replacement; // out-of-range to replacement char
  }

  if (c <= 0xFFFF) {
    return 3;
  }
  else {
    return 4;
  }
}

void kk_utf8_writex(kk_char_t c, uint8_t* s, kk_ssize_t* count) {
  if kk_likely(c <= 0x7F) {
    *count = 1;
    s[0] = (uint8_t)c;
    return;
  }
  if (c <= 0x07FF) {
    *count = 2;
    s[0] = (0xC0 | ((uint8_t)(c >> 6)));
    s[1] = (0x80 | (((uint8_t)c) & 0x3F));
    return;
  }

  if (c >= 0xD800 && c <= 0xDFFF) {
    c += KK_RAW_UTF16_OFS;   // encode in raw range to maintain valid utf-8
  }
  else if (c > 0x10FFFF) {
    c = kk_char_replacement; // out-of-range to replacement char
  }

  if (c <= 0xFFFF) {
    *count = 3;
    s[0] = (0xE0 |  ((uint8_t)(c >> 12)));
    s[1] = (0x80 | (((uint8_t)(c >>  6)) & 0x3F));
    s[2] = (0x80 | (((uint8_t)c) & 0x3F));
  }
  else {
    *count = 4;
    s[0] = (0xF0 |  ((uint8_t)(c >> 18)));
    s[1] = (0x80 | (((uint8_t)(c >> 12)) & 0x3F));
    s[2] = (0x80 | (((uint8_t)(c >>  6)) & 0x3F));
    s[3] = (0x80 | (((uint8_t)c) & 0x3F));
  }
}

kk_char_t kk_utf8_readx(const uint8_t* s, kk_ssize_t* count) {
  kk_char_t b = *s;  kk_assert_internal(b >= 0); // shift left is not UB on b
  kk_char_t c;
  if kk_likely(b <= 0x7F) {
    *count = 1;
    c = b; // fast path ASCII
  }
  else if (b <= 0xC1) { // invalid continuation byte or invalid 0xC0, 0xC1 (check is strictly not necessary as we don't validate..)
    goto fail;
  }
  else if (b <= 0xDF) { // b >= 0xC2  // 2 bytes
    *count = 2;
    c = (((b & 0x1F) << 6) | (s[1] & 0x3F));
    kk_assert_internal(c < 0xD800 || c > 0xDFFF);
  }
  else if (b <= 0xEF) { // b >= 0xE0  // 3 bytes 
    *count = 3;
    c = (((b & 0x0F) << 12) | ((s[1] & 0x3F) << 6) | (s[2] & 0x3F));
  }
  else if (b <= 0xF4) { // b >= 0xF0  // 4 bytes 
    *count = 4;
    c = (((b & 0x07) << 18) | ((s[1] & 0x3F) << 12) | ((s[2] & 0x3F) << 6) | (s[3] & 0x3F));
  }
  else {
  fail:
    // invalid, advance just 1 byte and encode it in the "raw" range
    kk_assert_internal(false);
    *count = 1;
    kk_assert_internal(b >= 0x80);
    c = KK_RAW_UTF8_OFS + b;
  }
#if (DEBUG!=0)
  kk_ssize_t dcount;
  kk_ssize_t vcount;
  kk_assert_internal(c == kk_utf8_read_validate(s, &dcount, &vcount, false));
  kk_assert_internal(*count == dcount);
#endif
  return c;
}



/*--------------------------------------------------------------------------------------------------
 String conversion to/from qutf8
--------------------------------------------------------------------------------------------------*/

static inline bool kk_char_is_raw(kk_char_t c) {
  return (c >= (KK_RAW_PLANE + 0xD800) && c <= (KK_RAW_PLANE + 0xE0FF));
}


// Validating qutf-8 decode; careful to only read beyond s[0] if valid.
// `count` returns the number of bytes read. 
// `vcount` is only set on an invalid sequence and return the number of bytes
// needed for a replacement character -- this is always 4 since we use the raw range.
// (if qutf8_identity is `true`, characters in the raw range are also treated as 
//  invalid so they can be decoded back into the raw sequence)
kk_char_t kk_utf8_read_validate(const uint8_t* s, kk_ssize_t* count, kk_ssize_t* vcount, bool qutf8_identity) {
  uint8_t b = s[0];
  if kk_likely(b <= 0x7F) {
    *count = 1;
    return b;   // ASCII fast path
  }
  // 2 byte encoding
  if (b >= 0xC2 && b <= 0xDF && kk_utf8_is_cont(s[1])) {
    *count = 2;
    kk_char_t c = (((b & 0x1F) << 6) | (s[1] & 0x3F));
    kk_assert_internal(c >= 0x80 && c <= 0x7FF);
    return c;
  }
  // 3 byte encoding; reject overlong and utf-16 surrogate halves (0xD800 - 0xDFFF)
  if ((b == 0xE0 && s[1] >= 0xA0 && s[1] <= 0xBF && kk_utf8_is_cont(s[2]))
    || ((b >= 0xE1 && b <= 0xEF && b != 0xED) && kk_utf8_is_cont(s[1]) && kk_utf8_is_cont(s[2]))
    || (b == 0xED && s[1] >= 0x80 && s[1] <= 0x9F && kk_utf8_is_cont(s[2])))
  {
    *count = 3;
    kk_char_t c = (((b & 0x0F) << 12) | ((s[1] & 0x3F) << 6) | (s[2] & 0x3F));
    kk_assert_internal(c >= 0x800 && (c < 0x0D800 || c > 0xDFFF) && c <= 0xFFFF);
    return c;
  }
  // 4 byte encoding; reject overlong and out of bounds (> 0x10FFFF)
  if ((b == 0xF0 && s[1] >= 0x90 && s[1] <= 0xBF && kk_utf8_is_cont(s[2]) && kk_utf8_is_cont(s[3]))
    || (b >= 0xF1 && b <= 0xF3 && kk_utf8_is_cont(s[1]) && kk_utf8_is_cont(s[2]) && kk_utf8_is_cont(s[3]))
    || (b == 0xF4 && s[1] >= 0x80 && s[1] <= 0x8F && kk_utf8_is_cont(s[2]) && kk_utf8_is_cont(s[3])))
  {
    kk_char_t c = (((b & 0x07) << 18) | ((s[1] & 0x3F) << 12) | ((s[2] & 0x3F) << 6) | (s[3] & 0x3F));
    kk_assert_internal(c >= 0x10000 && c <= 0x10FFFF);
    if (!qutf8_identity || !kk_char_is_raw(c)) {
      *count = 4;
      return c;
    }
    // fall through
  }
  // invalid: advance just 1 byte and encode it in the "raw" range  
  *count = 1;
  if (vcount != NULL) *vcount = 4;
  kk_assert_internal(b >= 0x80);
  return (KK_RAW_UTF8_OFS + b);
}

// validate a qutf8 sequence; return in `pvlen` the bytes needed to convert to a valid utf8 sequence.
static bool kk_qutf8_validate(kk_ssize_t len, const uint8_t* s, bool qutf8_identity, kk_ssize_t* pvlen) {
  const uint8_t* const end = s + len;
  kk_ssize_t vlen = 0;
  const uint8_t* p = s;
  while (p < end) {
    // optimize for ascii
    // todo: optimize further with word reads?
    if kk_likely(*p < 0x80) {
      p++;
      vlen++;
    }
    else {
      kk_ssize_t count;
      kk_ssize_t vcount = 0;
      kk_utf8_read_validate(p, &count, &vcount, qutf8_identity);
      p += count;
      if (vcount == 0) {
        vlen += count;
      }
      else {
        kk_assert_internal(count != vcount);
        vlen += vcount;
      }
    }
  }
  kk_assert_internal(p == end);
  // already valid; return as-is
  if (pvlen != NULL) { *pvlen = vlen; }
  return (vlen == len);
}

bool kk_utf8_is_validn(kk_ssize_t len, const uint8_t* s) {
  if (s == NULL) return true;
  bool valid = kk_qutf8_validate(len, s, true, NULL);
  kk_assert_internal(valid);
  return valid;
}

bool kk_utf8_is_valid(const char* s) {
  return kk_utf8_is_validn(kk_sstrlen(s), (const uint8_t*)s);
}


// allocate and translate to valid utf-8; `vlen` is the required length for the valid utf8 translation
static kk_string_t kk_qutf8_convert_from_invalid(kk_ssize_t len, const uint8_t* s, kk_ssize_t vlen, bool qutf8_identity, kk_context_t* ctx) {
  kk_assert_internal(vlen >= len);
  uint8_t* t;
  kk_string_t tstr = kk_unsafe_string_alloc_buf(vlen, &t, ctx);
  const uint8_t* p = s;
  const uint8_t* end = s + len;
  while (p < end) {
    if kk_likely(*p < 0x80) {
      *t++ = *p++;
    }
    else {
      // copy sequence    
      // todo: this can be optimized a lot more..
      kk_ssize_t count;
      kk_char_t c = kk_utf8_read_validate(p, &count, NULL, qutf8_identity);
      p += count;
      kk_ssize_t tcount;
      kk_utf8_write(c, t, &tcount);
      t += tcount;
    }
  }
  kk_assert_internal((t - kk_string_buf_borrow(tstr, NULL, ctx)) == vlen);
  return tstr;
}

static kk_string_t kk_qutf8_convert(kk_ssize_t len, const char* cstr, bool qutf8_identity, kk_context_t* ctx) {
  const uint8_t* s = (const uint8_t*)cstr;
  kk_ssize_t vlen;
  bool valid = kk_qutf8_validate(len, s, qutf8_identity, &vlen);
  if (valid) {
    // if already valid; copy directly
    return kk_string_alloc_dupn_valid_utf8(len, s, ctx);
  }
  else {
    // invalid sequences found: translate to valid utf-8
    return kk_qutf8_convert_from_invalid(len, s, vlen, qutf8_identity, ctx);
  }
}


kk_string_t kk_string_alloc_from_qutf8n(kk_ssize_t len, const char* cstr, kk_context_t* ctx) {
  return kk_qutf8_convert(len, cstr, true, ctx);
}

kk_string_t kk_string_alloc_from_qutf8(const char* str, kk_context_t* ctx) {
  return kk_string_alloc_from_qutf8n(kk_sstrlen(str), str, ctx);
}

kk_string_t kk_string_alloc_from_utf8n(kk_ssize_t len, const char* cstr, kk_context_t* ctx) {
  // for safety, we still always validate.
  return kk_qutf8_convert(len, cstr, false, ctx);
}

kk_string_t kk_string_alloc_from_utf8(const char* str, kk_context_t* ctx) {
  return kk_string_alloc_from_utf8n(kk_sstrlen(str), str, ctx);
}

kk_string_t kk_string_convert_from_qutf8(kk_bytes_t str, kk_context_t* ctx) {
  // to avoid reallocation (to accommodate invalid sequences), we first check if
  // it is already valid utf-8 which should be very common; in that case we return the bytes/string as-is.
  kk_ssize_t len;
  const uint8_t* const s = kk_bytes_buf_borrow(str, &len, ctx);
  kk_ssize_t vlen;
  bool valid = kk_qutf8_validate(len, s, true, &vlen);
  if (valid) {
    // if already valid; return as-is
    return kk_unsafe_bytes_as_string(str);
  }
  else {
    // invalid sequences found: copy and translate to valid utf-8
    kk_string_t tstr = kk_qutf8_convert_from_invalid(len, s, vlen, true, ctx);
    kk_bytes_drop(str, ctx);
    return tstr;
  }
}


const char* kk_string_to_qutf8_borrow(kk_string_t str, bool* should_free, kk_context_t* ctx) {
  // to avoid allocation, we first check if none of the characters are in the raw range.
  kk_ssize_t len;
  const uint8_t* const s = kk_string_buf_borrow(str, &len, ctx);
  const uint8_t* const end = s + len;
  kk_ssize_t extra_count = 0;
  const uint8_t* p = s;
  while (p < end) {
    // optimize for ascii
    // todo: optimize further with word reads?
    if kk_likely(*p < 0x80) {
      p++;
    }
    else {
      kk_ssize_t count;
      kk_char_t c = kk_utf8_read(p, &count);
      p += count;
      if (c >= KK_RAW_UTF8_OFS && c <= KK_RAW_UTF8_OFS + 0x7F) {
        extra_count += 3;  // encoded as 4 utf bytes but just 1 output byte needed
      }
    }
  }
  kk_assert_internal(p == end);
  if (extra_count == 0) {
    *should_free = false;
    return (const char*)s;
  }

  // contains raw bytes, allocate a buffer;
  kk_assert_internal(extra_count < len);
  const kk_ssize_t blen = len - extra_count;
  uint8_t* bstr = (uint8_t*)kk_malloc(blen + 1, ctx);
  bstr[blen] = 0;
  uint8_t* q = bstr;
  p = s;
  while (p < end) {
    // optimize for ascii
    // todo: optimize further with word reads?
    if kk_likely(*p < 0x80) {
      *q++ = *p++;
    }
    else {
      kk_ssize_t count;
      kk_char_t c = kk_utf8_read(p, &count);
      p += count;
      if (c >= KK_RAW_UTF8_OFS && c <= KK_RAW_UTF8_OFS + 0x7F) {
        *q++ = (uint8_t)(c - KK_RAW_UTF8_OFS);
      }
      else {
        kk_utf8_write(c, q, &count);
        q += count;
      }
    }
  }
  kk_assert_internal(p == end);
  kk_assert_internal(q == bstr + blen && *q == 0);
  *should_free = true;
  return (const char*)bstr;
}


/*--------------------------------------------------------------------------------------------------
  qutf-16 encoding/decoding
--------------------------------------------------------------------------------------------------*/

uint16_t* kk_string_to_qutf16_borrow(kk_string_t str, kk_context_t* ctx) {
  kk_ssize_t len;
  const uint8_t* const s = kk_string_buf_borrow(str, &len, ctx);
  const uint8_t* const end = s + len;

  // count utf-16 length (in 16-bit units)
  kk_ssize_t wlen = 0;
  for (const uint8_t* p = s; p < end; ) {
    kk_ssize_t count;
    kk_char_t c = kk_utf8_read(p, &count);
    p += count;
    wlen++;
    if (c > 0xFFFF && (c < KK_RAW_UTF16_OFS + 0xD800 || c > KK_RAW_UTF16_OFS + 0xDFFF)) {
      // surrogate pair
      wlen++;
    }
  }

  // encode to utf-16
  uint16_t* wstr = (uint16_t*)kk_malloc((wlen + 1) * kk_ssizeof(uint16_t), ctx);
  uint16_t* q = wstr;
  for (const uint8_t* p = s; p < end; ) {
    kk_ssize_t count;
    kk_char_t c = kk_utf8_read(p, &count);
    p += count;
    if (c <= 0xFFFF) {
      kk_assert_internal(c < 0xD800 || c > 0xDFFF);
      *q++ = (uint16_t)c;
    }
    else if (c < KK_RAW_UTF16_OFS + 0xD800 || c > KK_RAW_UTF16_OFS + 0xDFFF) {
      // surrogate pair
      kk_assert_internal(c <= 0x10FFFF);
      c -= 0x10000;
      *q++ = 0xD800 + (uint16_t)(c >> 10);
      *q++ = 0xDC00 + ((uint16_t)c & 0x3FF);
    }
    else {
      // raw range (for lone half of a surrogate)
      *q++ = (uint16_t)(c - KK_RAW_UTF16_OFS);
    }
  }
  *q = 0;
  kk_assert_internal(q == wstr + wlen && *q == 0);
  return wstr;
}

static kk_string_t kk_string_alloc_from_qutf16n_prim(kk_ssize_t wlen, const uint16_t* wstr, bool qutf16_identity, kk_context_t* ctx) {
  // count utf-8 length
  kk_ssize_t len = 0;
  const uint16_t* const end = wstr + wlen;
  for (const uint16_t* p = wstr; p < end; p++) {
    if (*p <= 0x7F) {
      len++;
    }
    else if (*p <= 0x7FF) {
      len += 2;
    }
    else if (*p <= 0xD800 || *p > 0xDFFF) {
      len += 3;
    }
    else if (*p <= 0xDBFF && p+1 < end && (p[1] >= 0xDC00 && p[1] <= 0xDFFF)) {
      if (qutf16_identity) {
        kk_char_t c = 0x10000 + ((kk_char_t)((p[0] - 0xD800) << 10)) + ((kk_char_t)(p[1]) - 0xDC00);
        if (kk_char_is_raw(c)) {
          // invalid codepoint in raw range; decode as two lone surrogates
          len += 4;
          continue;
        }
        // fallthrough
      }
      // valid surrogate
      len += 4;
      p++;  // skip the other half of the surrogate
    }
    else {
      // lone half of a surrogate: encoded in the raw range
      len += 4;
    }
  }

  // allocate and encode to utf-8
  uint8_t* s;
  kk_string_t str = kk_unsafe_string_alloc_buf(len, &s, ctx);
  uint8_t* q = s;
  for (const uint16_t* p = wstr; p < end; p++) {
    // optimize for ascii
    if (*p <= 0x7F) {
      *q++ = (uint8_t)*p;
    }
    else {
      kk_char_t c;
      if (*p <= 0xD800) {
        c = *p;
      }
      else if (*p <= 0xDBFF && p+1 < end && (p[1] >= 0xDC00 && p[1] <= 0xDFFF)) {
        c = 0x10000 + (((kk_char_t)(p[0]) - 0xD800) << 10) + ((kk_char_t)(p[1]) - 0xDC00);
        if (qutf16_identity && kk_char_is_raw(c)) {
          // codepoint in raw range: encode as two lone surrogate halves
          c = KK_RAW_UTF16_OFS + (kk_char_t)(*p);
        }
        else {
          p++; // skip the second surrogate half
        }
      }
      else {
        // lone half of a surrogate: encoded in the raw range
        c = KK_RAW_UTF16_OFS + (kk_char_t)(*p);
      }
      kk_ssize_t count;
      kk_utf8_write(c, q, &count);
      q += count;
    }
  }
  *q = 0;
  kk_assert_internal(q == s + len && *q == 0);
  return str;
}


kk_string_t kk_string_alloc_from_qutf16n(kk_ssize_t wlen, const uint16_t* wstr, kk_context_t* ctx) {
  return kk_string_alloc_from_qutf16n_prim(wlen, wstr, true, ctx);
}

kk_string_t kk_string_alloc_from_utf16n(kk_ssize_t wlen, const uint16_t* wstr, kk_context_t* ctx) {
  return kk_string_alloc_from_qutf16n_prim(wlen, wstr, false /* leave raw code points _as is_ */, ctx);
}

kk_string_t kk_string_alloc_from_qutf16(const uint16_t* wstr, kk_context_t* ctx) {
  return kk_string_alloc_from_qutf16n(kk_wcslen(wstr), wstr, ctx);
}

kk_string_t kk_string_alloc_from_utf16(const uint16_t* wstr, kk_context_t* ctx) {
  return kk_string_alloc_from_qutf16n(kk_wcslen(wstr), wstr, ctx);
}

/*--------------------------------------------------------------------------------------------------
   Convert using a codepage
--------------------------------------------------------------------------------------------------*/

static const uint16_t kk_codepage_latin[256] = {     // windows-1252, latin1
  0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
  0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F,
  0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F,
  0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F,
  0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
  0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F,
  0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F,
  0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F,

  0x20AC, 0xFFFD, 0x201A, 0x0192, 0x201E, 0x2026, 0x2020, 0x2021, 0x02C6, 0x2030, 0x0160, 0x2039, 0x0152, 0xFFFD, 0x017D, 0xFFFD,
  0xFFFD, 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014, 0x20DC, 0x2122, 0x0161, 0x203A, 0x0153, 0xFFFD, 0x017E, 0x0178,
  0xA0, 0xA1, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7, 0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF,
  0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF,
  0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF,
  0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF,
  0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF,
  0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF
};

kk_string_t kk_string_alloc_from_codepage(const uint8_t* bstr, const uint16_t* codepage /*NULL==kk_codepage_latin*/, kk_context_t* ctx) {
  if (codepage == NULL) codepage = kk_codepage_latin;
  // determine utf-8 length
  kk_ssize_t len = 0;
  const uint8_t* p = bstr;
  while (*p != 0) {
    const uint16_t c = codepage[*p++];
    if (c <= 0x7F) {
      len++;
    }
    else if (c <= 0x7FF) {
      len += 2;
    }
    else {
      kk_assert_internal(c < 0xD800 && c > 0xDFFF);
      len += 3;
    }
  }
  // and decode
  uint8_t* s;
  kk_string_t str = kk_unsafe_string_alloc_buf(len, &s, ctx);
  p = bstr;
  while (*p != 0) {
    const uint16_t c = codepage[*p++];
    kk_ssize_t count;
    kk_utf8_write(c, s, &count);
    s += count;
  }
  kk_assert_internal(s == (kk_string_buf_borrow(str, NULL, ctx) + len) && *s == 0);
  return str;
}


/*--------------------------------------------------------------------------------------------------
 String utilities
--------------------------------------------------------------------------------------------------*/

kk_ssize_t kk_decl_pure kk_string_count_pattern_borrow(kk_string_t str, kk_string_t pattern, kk_context_t* ctx) {
  kk_ssize_t patlen;
  const uint8_t* pat = kk_string_buf_borrow(pattern, &patlen, ctx);
  kk_ssize_t len;
  const uint8_t* s = kk_string_buf_borrow(str, &len, ctx);
  if (patlen <= 0)  return kk_string_count_borrow(str,ctx);
  if (patlen > len) return 0;

  //todo: optimize by doing backward Boyer-Moore? or use forward Knuth-Morris-Pratt?
  kk_ssize_t count = 0;
  const uint8_t* end = s + (len - (patlen - 1));
  for (const uint8_t* p = s; p < end; p++) {
    if (kk_memcmp(p, pat, patlen) == 0) {
      count++;
      p += (patlen - 1);
    }
  }
  return count;
}


kk_string_t kk_string_from_char(kk_char_t c, kk_context_t* ctx) {
  uint8_t buf[16];
  kk_ssize_t count;
  kk_utf8_write(c, buf, &count);
  buf[count] = 0;
  return kk_unsafe_bytes_as_string(kk_bytes_alloc_dupn(count, buf, ctx));
}

kk_string_t kk_string_from_chars(kk_vector_t v, kk_context_t* ctx) {
  kk_ssize_t n;
  kk_box_t* cs = kk_vector_buf_borrow(v, &n, ctx);
  kk_ssize_t len = 0;
  for (kk_ssize_t i = 0; i < n; i++) {
    len += kk_utf8_len(kk_char_unbox(cs[i], KK_BORROWED, ctx));
  }
  uint8_t* p;
  kk_string_t s = kk_unsafe_string_alloc_buf(len + 1, &p, ctx);
  for (kk_ssize_t i = 0; i < n; i++) {
    kk_ssize_t count;
    kk_utf8_write(kk_char_unbox(cs[i], KK_BORROWED, ctx), p, &count);
    p += count;
  }
  kk_assert_internal(kk_string_buf_borrow(s, NULL, ctx) + n == p);
  kk_vector_drop(v, ctx);
  return s;
}

kk_vector_t kk_string_to_chars(kk_string_t s, kk_context_t* ctx) {
  kk_ssize_t n = kk_string_count_borrow(s,ctx);
  kk_box_t* cs;
  kk_vector_t v = kk_vector_alloc_uninit(n, &cs, ctx);
  kk_ssize_t len;
  const uint8_t* p = kk_string_buf_borrow(s, &len, ctx);
  for (kk_ssize_t i = 0; i < n; i++) {
    kk_ssize_t count;
    cs[i] = kk_char_box(kk_utf8_read(p, &count), ctx);
    p += count;
  }
  kk_assert_internal(p == kk_string_buf_borrow(s, NULL, ctx) + len);
  kk_string_drop(s, ctx);
  return v;
}

kk_vector_t kk_string_splitv(kk_string_t s, kk_string_t sep, kk_context_t* ctx) {
  return kk_string_splitv_atmost(s, sep, KK_SSIZE_MAX, ctx);
}

kk_vector_t kk_string_splitv_atmost(kk_string_t str, kk_string_t sepstr, kk_ssize_t n, kk_context_t* ctx)
{
  if (n < 1) n = 1;
  kk_ssize_t len;
  const uint8_t* s = kk_string_buf_borrow(str, &len, ctx);
  const uint8_t* const end = s + len;
  kk_ssize_t seplen;
  const uint8_t* sep = kk_string_buf_borrow(sepstr, &seplen, ctx);

  // count parts
  kk_ssize_t count = 1;
  if (seplen > 0) {
    const uint8_t* p = s;
    while (count < n && (p = kk_memmem(p, end - p, sep, seplen)) != NULL) {
      p += seplen;
      count++;
    }
  }
  else if (n > 1) {
    count = kk_string_count_borrow(str,ctx); // todo: or special count upto n?
    if (count > n) count = n;
  }
  kk_assert_internal(count >= 1 && count <= n);

  // copy to vector
  kk_box_t* v;
  kk_vector_t vec = kk_vector_alloc_uninit(count, &v, ctx);
  const uint8_t* p = s;
  for (kk_ssize_t i = 0; i < (count-1) && p < end; i++) {
    const uint8_t* r;
    if (seplen > 0) {
      r = kk_memmem(p, end - p, sep, seplen);
    }
    else {
      r = kk_utf8_next(p);
    }
    kk_assert_internal(r != NULL && r >= p && r < end);
    const kk_ssize_t partlen = (r - p);
    v[i] = kk_string_box(kk_string_alloc_dupn_valid_utf8(partlen, p, ctx));
    p = r + seplen;  // advance
  }
  kk_assert_internal(p <= end);
  v[count-1] = kk_string_box(kk_string_alloc_dupn_valid_utf8(end - p, p, ctx));  // todo: share string if p == s ?
  kk_string_drop(str, ctx);
  kk_string_drop(sepstr, ctx);
  return vec;
}



/*--------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------*/

kk_string_t kk_string_to_upper(kk_string_t str, kk_context_t* ctx) {
  kk_ssize_t len;
  const uint8_t* s = kk_string_buf_borrow(str, &len, ctx);
  kk_string_t tstr;
  if (kk_datatype_is_unique(str.bytes, ctx)) {
    tstr = str;  // update in-place
  }
  else {
    kk_string_dup(str, ctx);  // multi-thread safe as we still reference str with s
    tstr = kk_string_copy(str, ctx);
    kk_assert_internal(!kk_datatype_eq(str.bytes, tstr.bytes));
  }
  uint8_t* t = (uint8_t*)kk_string_buf_borrow(tstr, NULL, ctx);   // t & s may alias!
  for (kk_ssize_t i = 0; i < len; i++) {
    t[i] = kk_ascii_toupper(s[i]);
  }
  if (!kk_datatype_eq(str.bytes, tstr.bytes)) kk_string_drop(str, ctx);  // drop if not reused in-place
  return tstr;
}

kk_string_t  kk_string_to_lower(kk_string_t str, kk_context_t* ctx) {
  kk_ssize_t len;
  const uint8_t* s = kk_string_buf_borrow(str, &len, ctx);
  kk_string_t tstr;
  if (kk_datatype_is_unique(str.bytes, ctx)) {
    tstr = str;  // update in-place
  }
  else {
    kk_string_dup(str, ctx);  // multi-thread safe as we still reference str with s
    tstr = kk_string_copy(str, ctx);
    kk_assert_internal(!kk_datatype_eq(str.bytes, tstr.bytes));
  }
  uint8_t* t = (uint8_t*)kk_string_buf_borrow(tstr, NULL, ctx);   // t & s may alias!
  for (kk_ssize_t i = 0; i < len; i++) {
    t[i] = kk_ascii_tolower(s[i]);
  }
  if (!kk_datatype_eq(str.bytes, tstr.bytes)) kk_string_drop(str, ctx);  // drop if not reused in-place
  return tstr;
}

kk_string_t  kk_string_trim_left(kk_string_t str, kk_context_t* ctx) {
  kk_ssize_t len;
  const uint8_t* s = kk_string_buf_borrow(str, &len, ctx);
  const uint8_t* p = s;
  for (; *p != 0 && kk_ascii_iswhite(*p); p++) {}
  if (p == s) return str;           // no trim needed
  const kk_ssize_t tlen = len - (p - s);      // todo: if s is unique and tlen close to slen, move inplace?
  kk_string_t tstr = kk_string_alloc_dupn_valid_utf8(tlen, p, ctx);
  kk_string_drop(str, ctx);
  return tstr;
}

kk_string_t  kk_string_trim_right(kk_string_t str, kk_context_t* ctx) {
  kk_ssize_t len;
  const uint8_t* s = kk_string_buf_borrow(str, &len, ctx);
  const uint8_t* p = s + len - 1;
  for (; p >= s && kk_ascii_iswhite(*p); p--) {}
  const kk_ssize_t tlen = (p - s) + 1;
  if (len == tlen) return str;  // no trim needed
  kk_string_t tstr = kk_string_alloc_dupn_valid_utf8(tlen, s, ctx);
  kk_string_drop(str, ctx);
  return tstr;
}

/*--------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------*/

kk_unit_t kk_println(kk_string_t s, kk_context_t* ctx) {
  // TODO: set locale to utf-8?
  puts(kk_string_cbuf_borrow(s, NULL, ctx));  // todo: allow printing embedded 0 characters?
  fflush(stdout);
  kk_string_drop(s, ctx);
  return kk_Unit;
}

kk_unit_t kk_print(kk_string_t s, kk_context_t* ctx) {
  // TODO: set locale to utf-8?
  fputs(kk_string_cbuf_borrow(s, NULL, ctx), stdout); // todo: allow printing embedded 0 characters?
  fflush(stdout);
  kk_string_drop(s, ctx);
  return kk_Unit;
}

kk_unit_t kk_trace(kk_string_t s, kk_context_t* ctx) {
  fputs(kk_string_cbuf_borrow(s, NULL, ctx), stderr); // todo: allow printing embedded 0 characters?
  fputs("\n", stderr);
  fflush(stdout);
  kk_string_drop(s, ctx);
  return kk_Unit;
}

kk_unit_t kk_trace_any(kk_string_t s, kk_box_t x, kk_context_t* ctx) {
  fprintf(stderr, "%s: ", kk_string_cbuf_borrow(s, NULL, ctx));
  kk_string_drop(s, ctx);
  kk_trace(kk_show_any(x, ctx), ctx);
  return kk_Unit;
}


static kk_string_t kk_double_show_special(double d, kk_context_t* ctx) {
  if (d == HUGE_VAL) {
    return kk_string_alloc_dup_valid_utf8("inf", ctx);
  }
  else if (d == -HUGE_VAL) {
    return kk_string_alloc_dup_valid_utf8("-inf", ctx);
  }
  else {
    kk_assert(isnan(d));
    return kk_string_alloc_dup_valid_utf8("nan", ctx);
  }
}

static kk_string_t kk_double_show_spec(double d, int32_t prec, char spec, kk_context_t* ctx) {
  if (!isfinite(d)) return kk_double_show_special(d, ctx);
  char buf[64];
  char fmt[16];
  if (prec < 0)  prec = -prec;
  if (prec > 48) prec = 48;
  snprintf(fmt, 16, "%%.%i%c", (int)prec, spec);
  snprintf(buf, 64, fmt, d);
  // check if it ends with a 0 exponent and remove if so. 
  char* p = buf;
  while (*p != 'e' && *p != 0) { p++; }
  if (p[0] == 'e'
    && (p[1] == '+' || p[1] == '-')
    && (p[2] == '0')
    && (p[3] == 0 || (p[3] == '0' && (p[4] == 0 || (p[4] == '0' && p[5] == 0)))))
  {
    *p = 0; // remove exponent
  }
  return kk_string_alloc_dup_valid_utf8(buf, ctx);
}

kk_string_t kk_double_show_fixed(double d, int32_t prec, kk_context_t* ctx) {
  return kk_double_show_spec(d, prec, prec < 0 ? 'g' : 'f', ctx);
}

kk_string_t kk_double_show_exp(double d, int32_t prec, kk_context_t* ctx) {
  return kk_double_show_spec(d, prec, prec < 0 ? 'g' : 'e', ctx);
}

kk_string_t kk_double_show(double d, int32_t prec, kk_context_t* ctx) {
  return kk_double_show_spec(d, prec, 'g', ctx);
}



kk_string_t kk_show_any(kk_box_t b, kk_context_t* ctx) {
  char buf[128];
#if KK_USE_NAN_BOX
  if (_is_double(b)) {
    return kk_double_show(kk_double_unbox(b, ctx), 0, ctx);
  }
  else
#endif
    if (kk_box_is_value(b)) {
      snprintf(buf, 128, "value(%li)", (long)kk_intf_unbox(b));
      return kk_string_alloc_dup_valid_utf8(buf, ctx);
    }
    else if (b.box == kk_box_null().box) {
      return kk_string_alloc_dup_valid_utf8("null", ctx);
    }
    else if (b.box == 0) {
      return kk_string_alloc_dup_valid_utf8("ptr(NULL)", ctx);
    }
    else {
      kk_block_t* p = kk_ptr_unbox(b, ctx);
      kk_tag_t tag = kk_block_tag(p);
      if (tag == KK_TAG_BIGINT) {
        // todo: add tag
        return kk_integer_to_string(kk_integer_unbox(b, ctx), ctx);
      }
      else if (tag == KK_TAG_STRING_SMALL || tag == KK_TAG_STRING || tag == KK_TAG_STRING_RAW) {
        // todo: add tag
        return kk_string_unbox(b);
      }
      else if (tag == KK_TAG_FUNCTION) {
        struct kk_function_s* fun = kk_block_assert(struct kk_function_s*, p, KK_TAG_FUNCTION);
        snprintf(buf, 128, "function(0x%zx)", (uintptr_t)(kk_cptr_unbox_borrowed(fun->fun,ctx)));
        kk_box_drop(b, ctx);
        return kk_string_alloc_dup_valid_utf8(buf, ctx);
      }
      else {
        // TODO: handle all builtin tags 
        snprintf(buf, 128, "ptr(0x%zx, tag: %i, rc: 0x%x, scan: %zu)", (uintptr_t)p, tag, kk_block_refcount(p), (size_t)kk_block_scan_fsize(p));
        kk_box_drop(b, ctx);
        return kk_string_alloc_dup_valid_utf8(buf, ctx);
      }
    }
}
