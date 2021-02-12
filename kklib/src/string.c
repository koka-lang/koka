/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
#ifndef  _CRT_SECURE_NO_WARNINGS
#define  _CRT_SECURE_NO_WARNINGS
#endif
#define  __USE_MINGW_ANSI_STDIO 1  // so %z is valid on mingw
#include "kklib.h"


// Allow reading aligned words as long as some bytes in it are part of a valid C object
#define ARCH_ALLOW_WORD_READS  (1)  

static char kk_ascii_toupper(char c) {
  return (c >= 'a' && c <= 'z' ? c - 'a' + 'A' : c);
}
static char kk_ascii_tolower(char c) {
  return (c >= 'A' && c <= 'Z' ? c - 'A' + 'a' : c);
}
static char ascii_iswhite(char c) {
  return (c == ' ' || c == '\t' || c == '\n' || c == '\r');
}

static int kk_memicmp(const uint8_t* s, const uint8_t* t, size_t len) {
  if (s==t) return 0;
  for(size_t i = 0; i < len; i++) {
    char c = kk_ascii_tolower(*s); s++;
    char d = kk_ascii_tolower(*t); t++;
    if (c != d) return (c < d ? -1 : 1);
  }
  return 0;
}

static const uint8_t* kk_memmem(const uint8_t* s, size_t slen, const uint8_t* pat, size_t patlen) {
  kk_assert(s != NULL && pat != NULL);
  if (patlen == 0 || patlen > slen) return NULL;
  const uint8_t* end = s + (slen - (patlen-1));
  for (; s < end; s++) {
    if (memcmp(s, pat, patlen) == 0) return s;
  }
  return NULL;
}


int kk_string_cmp_borrow(kk_string_t str1, kk_string_t str2) {
  if (kk_string_ptr_eq_borrow(str1, str2)) return 0;
  size_t len1;
  const uint8_t* s1 = kk_string_buf_borrow(str1,&len1);
  size_t len2;
  const uint8_t* s2 = kk_string_buf_borrow(str2,&len2);
  size_t minlen = (len1 <= len2 ? len1 : len2);
  int ord = memcmp(s1, s2, minlen);
  if (ord == 0) {
    if (len1 > len2) return 1;
    else if (len1 < len2) return -1;
  }
  return ord;
}

int kk_string_cmp(kk_string_t str1, kk_string_t str2, kk_context_t* ctx) {
  int ord = kk_string_cmp_borrow(str1,str2);
  kk_string_drop(str1,ctx);
  kk_string_drop(str2,ctx);
  return ord;
}

int kk_string_icmp_borrow(kk_string_t str1, kk_string_t str2) {
  size_t len1;
  const uint8_t* s1 = kk_string_buf_borrow(str1, &len1);
  size_t len2;
  const uint8_t* s2 = kk_string_buf_borrow(str2, &len2);
  size_t minlen = (len1 <= len2 ? len1 : len2);
  int ord = kk_memicmp(s1, s2, minlen);
  if (ord == 0) {
    if (len1 > len2) return 1;
    else if (len1 < len2) return -1;
  }
  return ord;
}

int kk_string_icmp(kk_string_t str1, kk_string_t str2, kk_context_t* ctx) {
  int ord = kk_string_icmp_borrow(str1, str2);
  kk_string_drop(str1, ctx);
  kk_string_drop(str2, ctx);
  return ord;
}

// Allocate a string of `len` bytes. 
// `s` must be at least `len` bytes of valid utf-8, or NULL. Adds a terminating zero at the end.
kk_decl_export kk_decl_noinline kk_string_t kk_string_alloc_len_unsafe(size_t len, const uint8_t* s, kk_context_t* ctx) {
  // kk_assert_internal(s == NULL || strlen(s) >= len);  // s may contain embedded 0 characters
  if (len == 0) {
    return kk_string_empty();
  }
  else if (len <= KK_STRING_SMALL_MAX) {
    kk_string_small_t str = kk_block_alloc_as(struct kk_string_small_s, 0, KK_TAG_STRING_SMALL, ctx);
    str->u.str_value = ~KUZ(0);
    if (s != NULL && len > 0) {
      memcpy(&str->u.str[0], s, len);
    }
    str->u.str[len] = 0;
    return kk_datatype_from_base(&str->_base);
  }
  else {
    kk_string_normal_t str = kk_block_assert(kk_string_normal_t, kk_block_alloc_any(sizeof(struct kk_string_normal_s) - 1 /* char str[1] */ + len + 1 /* 0 terminator */, 0, KK_TAG_STRING, ctx), KK_TAG_STRING);
    if (s != NULL && len > 0) {
      memcpy(&str->str[0], s, len);
    }
    str->length = len;
    str->str[len] = 0;
    // todo: kk_assert valid utf-8 in debug mode
    return kk_datatype_from_base(&str->_base);
  }
}


// Count code points in a valid utf-8 string.
size_t kk_decl_pure kk_string_count_borrow(kk_string_t str) {
  size_t len;
  const uint8_t* s = kk_string_buf_borrow(str,&len);
  size_t cont = 0;      // continuation character counts
  const uint8_t* t = s; // current position 
  const uint8_t* end = t + len;
  kk_assert_internal(*end == 0);

  // advance per byte until aligned
  for ( ; ((((uintptr_t)t) % sizeof(kk_uintx_t)) != 0) && (t < end); t++) {
    // count continuation bytes
    if (kk_utf8_is_cont(*t)) cont++;
  }  
  // advance per sizeof(kk_uintx_t). 
  if (t < end) {
    kk_assert_internal(((uintptr_t)t) % sizeof(kk_uintx_t) == 0);
    const kk_uintx_t* p    = (const kk_uintx_t*)t;
    const kk_uintx_t* pend = (const kk_uintx_t*)(((uintptr_t)end / sizeof(kk_uintx_t)) * sizeof(kk_uintx_t)); // align down
    for ( ; p < pend; p++) {
      // count continuation bytes (0b10xxxxxx bytes) in parallel
      const kk_uintx_t u = *p;
      const kk_uintx_t m = ((u & kk_bits_high_mask) >> 7) & ((~u) >> 6); // each byte in `m` is 0x01 iff it was a continuation byte
      cont += kk_bits_byte_sum(m);
    }
    t = (const uint8_t*)p; // restore t
  }

  // advance per byte until 0
  for (; t < end; t++) {
    // count continuation characters
    if (kk_utf8_is_cont(*t)) cont++;
  }
  kk_assert_internal(t == end);
  kk_assert_internal(len == (size_t)(t - s));
  kk_assert_internal(len > cont);
  return (len - cont);
}

size_t kk_decl_pure kk_string_count(kk_string_t str, kk_context_t* ctx) {
  size_t count = kk_string_count_borrow(str);
  kk_string_drop(str, ctx);
  return count;
}



/*--------------------------------------------------------------------------------------------------
 String conversion to/from mutf8
--------------------------------------------------------------------------------------------------*/

kk_string_t  kk_string_from_mutf8(kk_string_t str, kk_context_t* ctx) {
  // to avoid reallocation (to accommodate invalid sequences), we first check if
  // it is already valid utf-8 which should be very common; in that case we resurn the string as-is.
  size_t len;
  const uint8_t* const s = kk_string_buf_borrow(str, &len);
  const uint8_t* const end = s + len;
  bool valid = true;
  size_t vlen = 0;
  const uint8_t* p = s;
  while (p < end) {
    // optimize for ascii
    // todo: optimize further with word reads?
    if (kk_likely(*p < 0x80)) {
      p++;
      vlen++;
    }
    else {
      size_t count;
      size_t vcount = 0;
      kk_utf8_read_validate(p, &count, &vcount);
      p += count;
      if (vcount == 0) {
        vlen += count;
      }
      else {
        valid = false;
        vlen += vcount;
      }
    }
  }
  kk_assert_internal(p == end);
  // already valid; return as-is
  if (vlen == len) return str;

  // invalid sequences found: copy and translate to valid utf-8
  kk_string_t tstr = kk_string_alloc_buf(vlen, ctx);
  uint8_t* t = (uint8_t*)kk_string_buf_borrow(tstr, NULL);
  p = s;
  while (p < end) {    
    if (kk_likely(*p < 0x80)) {
      *t++ = *p++;
    }
    else {
      // copy sequence    
      // todo: this can be optimized a lot more..
      size_t count;
      kk_char_t c = kk_utf8_read_validate(p, &count, NULL);
      p += count;
      size_t tcount;
      kk_utf8_write(c, t, &tcount);
      t += tcount;
    }
  }
  kk_assert_internal( (t - kk_string_buf_borrow(tstr, NULL)) == (ptrdiff_t)vlen);
  kk_string_drop(str, ctx);
  return tstr;
}

const uint8_t* kk_string_to_mutf8_borrow(kk_string_t str, bool* should_free, kk_context_t* ctx) {
  // to avoid allocation, we first check if none of the characters are in the raw range.

}

/*--------------------------------------------------------------------------------------------------
  mutf-16 encoding/decoding
--------------------------------------------------------------------------------------------------*/

uint16_t* kk_string_to_mutf16_borrow(kk_string_t str, kk_context_t* ctx) {
  size_t total = kk_string_count_borrow(str);
  size_t len;
  const uint8_t* const s = kk_string_buf_borrow(str, &len);
  const uint8_t* const end = s + len;

  // count utf-16 length (in 16-bit units)
  size_t wlen = 0;
  for (const uint8_t* p = s; p < end; ) {
    size_t count;
    kk_char_t c = kk_utf8_read(p, &count);
    p += count;
    wlen++;
    if (c > 0xFFFF && (c < KK_RAW_UTF16_OFS + 0xD800 || c > KK_RAW_UTF16_OFS + 0xDFFF)) {
      // surrogate pair
      wlen++;
    }
  }

  // encode to utf-16
  uint16_t* wstr = kk_malloc((wlen + 1) * sizeof(uint16_t), ctx);
  uint16_t* q = wstr;
  for (const uint8_t* p = s; p < end; ) {
    size_t count;
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

kk_string_t kk_string_from_mutf16(const uint16_t* wstr, kk_context_t* ctx) {
  // count utf-8 length
  size_t len = 0;
  for (const uint16_t* p = wstr; *p != 0; p++) {
    if (*p <= 0x7F) {
      len++;
    }
    else if (*p <= 0x7FF) {
      len += 2;
    }
    else if (*p <= 0xD800 || *p > 0xDFFF) {
      len += 3;
    }
    else if (*p <= 0xDBFF && (p[1] >= 0xDC00 && p[1] <= 0xDFFF)) {
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
  kk_string_t str = kk_string_alloc_buf(len, ctx);
  uint8_t* s = kk_string_buf_borrow(str, NULL);
  uint8_t* q = s;
  for (const uint16_t* p = wstr; *p != 0; p++) {
    // optimize for ascii
    if (*p <= 0x7F) {
      *q++ = *p;
    }
    else {
      kk_char_t c;
      if (*p <= 0xD800) {
        c = *p;
      }
      else if (*p <= 0xDBFF && (p[1] >= 0xDC00 && p[1] <= 0xDFFF)) {
        c = 0x10000 + (((kk_char_t)(p[0]) - 0xD800) << 10) + ((kk_char_t)(p[1]) - 0xDC00);
        p++;
      }
      else {
        // lone half of a surrogate: encoded in the raw range
        c = KK_RAW_UTF16_OFS + (kk_char_t)(*p);
      }
      size_t count;
      kk_utf8_write(c, q, &count);
      q += count;
    }
  }
  *q = 0;
  kk_assert_internal(q == s + len && *q == 0);
  return str;
}



/*--------------------------------------------------------------------------------------------------
 String utilities
--------------------------------------------------------------------------------------------------*/

size_t kk_decl_pure kk_string_count_pattern_borrow(kk_string_t str, kk_string_t pattern) {
  size_t patlen;
  const uint8_t* pat = kk_string_buf_borrow(pattern,&patlen);  
  size_t len;
  const uint8_t* s   = kk_string_buf_borrow(str,&len);
  if (patlen == 0)  return kk_string_count_borrow(str);
  if (patlen > len) return 0;
  
  //todo: optimize by doing backward Boyer-Moore? or use forward Knuth-Morris-Pratt?
  size_t count = 0;
  const uint8_t* end = s + (len - (patlen - 1));
  for (const uint8_t* p = s; p < end; p++) {
    if (memcmp(p, pat, patlen) == 0) {
      count++;
      p += (patlen - 1);
    }    
  }
  return count;
}


kk_string_t kk_string_from_char(kk_char_t c, kk_context_t* ctx) {
  uint8_t buf[16];
  size_t count;
  kk_utf8_write(c, buf, &count);
  buf[count] = 0;
  return kk_string_alloc_len_unsafe(count, buf, ctx);
}

kk_string_t kk_string_from_chars(kk_vector_t v, kk_context_t* ctx) {
  size_t n;
  kk_box_t* cs = kk_vector_buf(v, &n);
  size_t len = 0;
  for (size_t i = 0; i < n; i++) {
    len += kk_utf8_len(kk_char_unbox(cs[i], ctx));
  }
  kk_string_t s = kk_string_alloc_buf(len + 1, ctx);
  uint8_t* p = (uint8_t*)kk_string_buf_borrow(s,NULL);
  for (size_t i = 0; i < n; i++) {
    size_t count;
    kk_utf8_write(kk_char_unbox(cs[i], ctx), p, &count);
    p += count;
  }
  kk_assert_internal(kk_string_buf_borrow(s,NULL) + n == p);
  kk_vector_drop(v,ctx);
  return s;
}

kk_vector_t kk_string_to_chars(kk_string_t s, kk_context_t* ctx) {
  size_t n = kk_string_count_borrow(s);
  kk_vector_t v = kk_vector_alloc(n, kk_box_null, ctx);
  kk_box_t* cs = kk_vector_buf(v, NULL);
  size_t len;
  const uint8_t* p = kk_string_buf_borrow(s, &len);
  for (size_t i = 0; i < n; i++) {
    size_t count;
    cs[i] = kk_char_box(kk_utf8_read(p, &count),ctx);
    p += count;
  }
  kk_assert_internal(p == kk_string_buf_borrow(s,NULL) + len);
  kk_string_drop(s,ctx);
  return v;
}

kk_string_t kk_string_cat(kk_string_t str1, kk_string_t str2, kk_context_t* ctx) {
  size_t len1;
  const uint8_t* s1 = kk_string_buf_borrow(str1, &len1);
  size_t len2;
  const uint8_t* s2 = kk_string_buf_borrow(str2, &len2);
  kk_string_t t = kk_string_alloc_buf(len1 + len2, ctx );
  uint8_t* p = (uint8_t*)kk_string_buf_borrow(t,NULL);
  memcpy(p, s1, len1);
  memcpy(p+len1, s2, len2);
  kk_assert_internal(p[len1+len2] == 0);
  kk_string_drop(str1, ctx);
  kk_string_drop(str2, ctx);
  return t;
}

kk_string_t kk_string_cat_fromc_unsafe(kk_string_t str1, const char* s2, kk_context_t* ctx) {
  if (s2 == NULL || *s2 == 0) return str1;
  size_t len1;
  const uint8_t* s1 = kk_string_buf_borrow(str1,&len1);
  const size_t len2 = strlen(s2);
  kk_string_t t = kk_string_alloc_buf(len1 + len2, ctx);
  uint8_t* p = (uint8_t*)kk_string_buf_borrow(t,NULL);
  memcpy(p, s1, len1);
  memcpy(p+len1, s2, len2);
  kk_assert_internal(p[len1+len2] == 0);
  kk_string_drop(str1, ctx);
  return t;
}

kk_vector_t kk_string_splitv(kk_string_t s, kk_string_t sep, kk_context_t* ctx) {
  return kk_string_splitv_atmost(s, sep, UINT32_MAX, ctx);
}

kk_vector_t kk_string_splitv_atmost(kk_string_t str, kk_string_t sepstr, size_t n, kk_context_t* ctx) 
{
  if (n < 1) n = 1;
  size_t len;
  const uint8_t* s = kk_string_buf_borrow(str, &len);
  const uint8_t* const end = s + len;
  size_t seplen;
  const uint8_t* sep = kk_string_buf_borrow(sepstr, &seplen);

  // count parts
  size_t count = 1;
  if (seplen > 0) {    
    const uint8_t* p = s;
    while (count < n && (p = kk_memmem(p, end - p, sep, seplen)) != NULL) {
      p += seplen;
      count++;
    }
  }
  else if (n > 1) {
    count = kk_string_count_borrow(str); // todo: or special count upto n?
    if (count > n) count = n;
  }
  kk_assert_internal(count >= 1 && count <= n);
  
  // copy to vector
  kk_vector_t vec = kk_vector_alloc(count, kk_box_null, ctx);
  kk_box_t* v  = kk_vector_buf(vec, NULL);
  const uint8_t* p = s;
  for (size_t i = 0; i < (count-1) && p < end; i++) {
    const uint8_t* r;
    if (seplen > 0) {
      r = kk_memmem(p, end - p,  sep, seplen);
    }
    else {
      r = kk_utf8_next(p);
    }
    kk_assert_internal(r != NULL && r >= p && r < end);    
    const size_t partlen = (size_t)(r - p);
    v[i] = kk_string_box(kk_string_alloc_len_unsafe(partlen, p, ctx));
    p = r + seplen;  // advance
  }
  kk_assert_internal(p <= end);
  v[count-1] = kk_string_box(kk_string_alloc_len_unsafe(end - p, p, ctx));  // todo: share string if p == s ?
  kk_string_drop(str,ctx);
  kk_string_drop(sepstr, ctx);
  return vec;
}

kk_string_t kk_string_replace_all(kk_string_t s, kk_string_t pat, kk_string_t rep, kk_context_t* ctx) {
  return kk_string_replace_atmost(s, pat, rep, UINT32_MAX, ctx);
}

kk_string_t kk_string_replace_atmost(kk_string_t s, kk_string_t pat, kk_string_t rep, size_t n, kk_context_t* ctx) {
  kk_string_t t = s;
  if (n==0 || kk_string_is_empty_borrow(s) || kk_string_is_empty_borrow(pat)) goto done;

  size_t plen;
  const uint8_t* p = kk_string_buf_borrow(s,&plen);
  size_t ppat_len;
  const uint8_t* ppat = kk_string_buf_borrow(pat,&ppat_len);
  size_t prep_len; 
  const uint8_t* prep = kk_string_buf_borrow(rep, &prep_len);
  
  const uint8_t* const pend = p + plen;
  // if unique s && |rep| == |pat|, update in-place
  // TODO: if unique s & |rep| <= |pat|, maybe update in-place if not too much waste?
  if (kk_datatype_is_unique(s) && ppat_len == prep_len) {
    size_t count = 0;
    while (count < n && p < pend) {
      const uint8_t* r = kk_memmem(p, pend - p, ppat, ppat_len);
      if (r == NULL) break;
      memcpy((uint8_t*)r, prep, prep_len);
      count++;
      p = r + prep_len;
    }
  }
  else {
    // count pat occurrences so we can pre-allocate the result buffer
    size_t count = 0;
    const uint8_t* r = p;
    while (count < n && ((r = kk_memmem(r, pend - r, ppat, ppat_len)) != NULL)) {
      count++;
      r += ppat_len;
    }
    if (count == 0) goto done; // no pattern found
    
    // allocate
    size_t newlen = plen - (count * ppat_len) + (count * prep_len);
    t = kk_string_alloc_buf(newlen, ctx);
    uint8_t* q = (uint8_t*)kk_string_buf_borrow(t,NULL);
    while (count > 0) {
      count--;
      r = kk_memmem(p, pend - p, ppat, ppat_len);
      kk_assert_internal(r != NULL);
      size_t ofs = (size_t)(r - p);
      memcpy(q, p, ofs);
      memcpy(q + ofs, prep, prep_len);
      q += ofs + prep_len;
      p += ofs + ppat_len;
    }
    size_t rest = (size_t)(pend - p);
    memcpy(q, p, rest);
    kk_assert_internal(q + rest == kk_string_buf_borrow(t,NULL) + newlen);
  }

done:
  kk_string_drop(pat, ctx);
  kk_string_drop(rep, ctx);
  if (!kk_datatype_eq(t, s)) kk_datatype_drop(s, ctx);
  return t;
}


kk_string_t kk_string_repeat(kk_string_t str, size_t n, kk_context_t* ctx) {
  size_t len;
  const uint8_t* s = kk_string_buf_borrow(str,&len);  
  if (len == 0 || n==0) return kk_string_empty();  
  kk_string_t tstr = kk_string_alloc_buf(len*n, ctx); // TODO: check overflow
  uint8_t* t = (uint8_t*)kk_string_buf_borrow(tstr,NULL);
  if (len == 1) {
    memset(t, *s, n);
    t += n;
  }
  else {
    for (size_t i = 0; i < n; i++) {
      memcpy(t, s, len);
      t += len;
    }
  }
  kk_assert_internal(*t == 0);
  kk_string_drop(str,ctx);
  return tstr;
}

// to avoid casting to signed, return 0 for not found, or the index+1
size_t kk_string_index_of1(kk_string_t str, kk_string_t sub, kk_context_t* ctx) {
  size_t slen;
  const uint8_t* s = kk_string_buf_borrow(str, &slen);
  size_t tlen;
  const uint8_t* t = kk_string_buf_borrow(sub, &tlen);  
  size_t idx;
  if (tlen == 0) {
    idx = (slen == 0 ? 0 : 1);
  }
  else if (tlen > slen) {
    idx = 0;
  }
  else {
    const uint8_t* p = kk_memmem(s, slen, t, tlen);
    idx = (p == NULL ? 0 : (size_t)(p - s) + 1);
  }
  kk_string_drop(str, ctx);
  kk_string_drop(sub, ctx);
  return idx;
}

size_t kk_string_last_index_of1(kk_string_t str, kk_string_t sub, kk_context_t* ctx) {
  size_t slen;
  const uint8_t* s = kk_string_buf_borrow(str, &slen);
  size_t tlen;
  const uint8_t* t = kk_string_buf_borrow(sub, &tlen);
  size_t idx;
  if (tlen == 0) {
    idx = slen;
  }
  else if (tlen > slen) {
    idx = 0;
  }
  else if (tlen == slen) {
    idx = (kk_string_cmp_borrow(str, sub) == 0 ? 1 : 0);
  }
  else {
    const uint8_t* p;
    for (p = s + slen - tlen; p >= s; p--) {  // todo: use reverse Boyer-Moore instead of one character at a time
      if (memcmp(p, t, tlen) == 0) break;
    }
    idx = (p >= s ? (size_t)(p - s) + 1 : 0);
  }
  kk_string_drop(str, ctx);
  kk_string_drop(sub, ctx);
  return idx;
}

bool kk_string_starts_with(kk_string_t str, kk_string_t pre, kk_context_t* ctx) {
  size_t slen;
  const uint8_t* s = kk_string_buf_borrow(str, &slen);
  size_t tlen;
  const uint8_t* t = kk_string_buf_borrow(pre, &tlen);
  bool starts;
  if (tlen == 0) {
    starts = (slen > 0);
  }
  else if (tlen > slen) {
    starts = false;
  }
  else {
    starts = (memcmp(s, t, tlen) == 0);
  }
  kk_string_drop(str, ctx);
  kk_string_drop(pre, ctx);
  return starts;
}

bool kk_string_ends_with(kk_string_t str, kk_string_t post, kk_context_t* ctx) {
  size_t slen;
  const uint8_t* s = kk_string_buf_borrow(str, &slen);
  size_t tlen;
  const uint8_t* t = kk_string_buf_borrow(post, &tlen);
  bool ends;
  if (tlen == 0) {
    ends = (slen > 0);
  }
  else if (tlen > slen) {
    ends = false;
  }
  else {
    ends = (memcmp(s + slen - tlen, t, tlen) == 0);
  }
  kk_string_drop(str, ctx);
  kk_string_drop(post, ctx);
  return ends;
}

bool kk_string_contains(kk_string_t str, kk_string_t sub, kk_context_t* ctx) {
  return (kk_string_index_of1(str, sub, ctx) > 0);
}


kk_string_t kk_string_to_upper(kk_string_t str, kk_context_t* ctx) {
  size_t len;
  const uint8_t* s = kk_string_buf_borrow(str,&len);
  kk_string_t tstr; 
  if (kk_datatype_is_unique(str)) {
    tstr = str;  // update in-place
  }
  else {
    kk_string_dup(str);  // multi-thread safe as we still reference str with s
    tstr = kk_string_copy(str, ctx);
    kk_assert_internal(!kk_datatype_eq(str, tstr));
  }
  uint8_t* t = (uint8_t*)kk_string_buf_borrow(tstr,NULL);   // t & s may alias!
  for (size_t i = 0; i < len; i++) {
    t[i] = kk_ascii_toupper(s[i]);
  }
  if (!kk_datatype_eq(str,tstr)) kk_string_drop(str, ctx);  // drop if not reused in-place
  return tstr;
}

kk_string_t  kk_string_to_lower(kk_string_t str, kk_context_t* ctx) {
  size_t len;
  const uint8_t* s = kk_string_buf_borrow(str, &len);
  kk_string_t tstr;
  if (kk_datatype_is_unique(str)) {
    tstr = str;  // update in-place
  }
  else {
    kk_string_dup(str);  // multi-thread safe as we still reference str with s
    tstr = kk_string_copy(str, ctx);
    kk_assert_internal(!kk_datatype_eq(str, tstr));
  }
  uint8_t* t = (uint8_t*)kk_string_buf_borrow(tstr, NULL);   // t & s may alias!
  for (size_t i = 0; i < len; i++) {
    t[i] = kk_ascii_tolower(s[i]);
  }
  if (!kk_datatype_eq(str, tstr)) kk_string_drop(str, ctx);  // drop if not reused in-place
  return tstr;
}

kk_string_t  kk_string_trim_left(kk_string_t str, kk_context_t* ctx) {
  size_t len;
  const uint8_t* s = kk_string_buf_borrow(str, &len);
  const uint8_t* p = s;
  for ( ; *p != 0 && ascii_iswhite(*p); p++) { }
  if (p == s) return str;           // no trim needed
  const size_t tlen = len - (size_t)(p - s);      // todo: if s is unique and tlen close to slen, move inplace?
  kk_string_t tstr = kk_string_alloc_len_unsafe(tlen, p, ctx);
  kk_string_drop(str, ctx);
  return tstr;
}

kk_string_t  kk_string_trim_right(kk_string_t str, kk_context_t* ctx) {
  size_t len;
  const uint8_t* s = kk_string_buf_borrow(str, &len);
  const uint8_t* p = s + len - 1;
  for (; p >= s && ascii_iswhite(*p); p--) {}
  const size_t tlen = (size_t)(p - s) + 1;
  if (len == tlen) return str;  // no trim needed
  kk_string_t tstr = kk_string_alloc_len_unsafe(tlen, s, ctx);
  kk_string_drop(str, ctx);
  return tstr;
}


kk_string_t kk_string_adjust_length(kk_string_t str, size_t newlen, kk_context_t* ctx) {
  if (newlen==0) {
    kk_string_drop(str, ctx);
    return kk_string_empty();
  }
  size_t len;
  const uint8_t* s = kk_string_buf_borrow(str,&len);
  if (len == newlen) {
    return str;
  }
  else if (len > newlen && (3*(len/4)) < newlen &&  // 0.75*len < newlen < len: update length in place if we can
           kk_datatype_is_unique(str) && kk_datatype_has_tag(str, KK_TAG_STRING)) {
    // length in place
    kk_assert_internal(kk_datatype_has_tag(str, KK_TAG_STRING) && kk_datatype_is_unique(str));
    kk_string_normal_t nstr = kk_datatype_as_assert(kk_string_normal_t, str, KK_TAG_STRING);
    nstr->length = newlen;
    nstr->str[newlen] = 0;
    // kk_assert_internal(kk_string_is_valid(kk_string_dup(s),ctx));
    return str;
  }
  else if (newlen < len) {
    // full copy
    kk_string_t tstr = kk_string_alloc_len_unsafe(newlen, s, ctx);
    kk_string_drop(str, ctx);
    return tstr;
  }
  else {
    // full copy
    kk_assert_internal(newlen > len);
    kk_string_t tstr = kk_string_alloc_buf(newlen,ctx);
    uint8_t* t = (uint8_t*)kk_string_buf_borrow(tstr,NULL);
    memcpy( t, s, len );
    memset(t + len, 0, newlen - len);
    kk_string_drop(str, ctx);
    return tstr;
  }
}

/*--------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------*/

kk_unit_t kk_println(kk_string_t s, kk_context_t* ctx) {
  // TODO: set locale to utf-8?
  puts(kk_string_cbuf_borrow(s,NULL));  // todo: allow printing embedded 0 characters?
  kk_string_drop(s,ctx);
  return kk_Unit;
}

kk_unit_t kk_print(kk_string_t s, kk_context_t* ctx) {
  // TODO: set locale to utf-8?
  fputs(kk_string_cbuf_borrow(s,NULL), stdout); // todo: allow printing embedded 0 characters?
  kk_string_drop(s,ctx);
  return kk_Unit;
}

kk_unit_t kk_trace(kk_string_t s, kk_context_t* ctx) {
  fputs(kk_string_cbuf_borrow(s,NULL), stderr); // todo: allow printing embedded 0 characters?
  fputs("\n", stderr);
  kk_string_drop(s, ctx);
  return kk_Unit;
}

kk_unit_t kk_trace_any(kk_string_t s, kk_box_t x, kk_context_t* ctx) {
  fprintf(stderr, "%s: ", kk_string_cbuf_borrow(s,NULL));
  kk_string_drop(s, ctx);
  kk_trace(kk_show_any(x,ctx),ctx);
  return kk_Unit;
}


static kk_string_t kk_double_show_spec(double d, int32_t prec, char spec, kk_context_t* ctx) {
  char buf[64];
  char fmt[16];
  if (prec < 0)  prec = -prec;
  if (prec > 48) prec = 48;
  snprintf(fmt, 16, "%%.%i%c", (int)prec, spec);
  snprintf(buf, 64, fmt, d);
  return kk_string_alloc_dup_unsafe(buf, ctx);
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
    snprintf(buf, 128, "value(%zi)", kk_intx_unbox(b));
    return kk_string_alloc_dup_unsafe(buf, ctx);
  }
  else if (b.box == kk_box_null.box) {
    return kk_string_alloc_dup_unsafe("null", ctx);
  }
  else if (b.box == 0) {
    return kk_string_alloc_dup_unsafe("ptr(NULL)", ctx);
  }
  else {
    kk_block_t* p = kk_ptr_unbox(b);
    kk_tag_t tag = kk_block_tag(p);
    if (tag == KK_TAG_BIGINT) {
      // todo: add tag
      return kk_integer_to_string(kk_integer_unbox(b), ctx);
    }
    else if (tag == KK_TAG_STRING_SMALL || tag == KK_TAG_STRING || tag == KK_TAG_STRING_RAW) {
      // todo: add tag
      return kk_string_unbox(b);
    }
    else if (tag == KK_TAG_FUNCTION) {
      kk_function_t fun = kk_block_assert(kk_function_t, p, KK_TAG_FUNCTION);
      snprintf(buf, 128, "function(0x%zx)", (uintptr_t)(kk_cptr_unbox(fun->fun)));
      kk_box_drop(b,ctx);
      return kk_string_alloc_dup_unsafe(buf, ctx);
    }
    else {
      // TODO: handle all builtin tags 
      snprintf(buf, 128, "ptr(0x%zx, tag: %i, rc: 0x%zx, scan: %zu)", (uintptr_t)p, tag, kk_block_refcount(p), kk_block_scan_fsize(p));
      kk_box_drop(b, ctx);
      return kk_string_alloc_dup_unsafe(buf, ctx);
    }
  }
}
