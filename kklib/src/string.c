/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
#define  _CRT_SECURE_NO_WARNINGS
#define  __USE_MINGW_ANSI_STDIO 1  // so %z is valid on mingw
#include "kklib.h"


// Allow reading aligned words as long as some bytes in it are part of a valid C object
#define ARCH_ALLOW_WORD_READS  (1)  

struct kk_string_normal_s kk__static_string_empty = { { { KK_HEADER_STATIC(0,KK_TAG_STRING) } }, 0, {0} };

static char kk_ascii_toupper(char c) {
  return (c >= 'a' && c <= 'z' ? c - 'a' + 'A' : c);
}
static char kk_ascii_tolower(char c) {
  return (c >= 'A' && c <= 'Z' ? c - 'A' + 'a' : c);
}
static char ascii_iswhite(char c) {
  return (c == ' ' || c == '\t' || c == '\n' || c == '\r');
}

static int stricmpx(const char* s, const char* t) {
  if (s==t) return 0;
  char c,d;
  do {
    c = *s++;
    d = *t++;
    c = kk_ascii_tolower(c);
    d = kk_ascii_tolower(d);
  } while (c == d && c != 0);
  return (c - d);
}


int kk_string_cmp_borrow(kk_string_t str1, kk_string_t str2) {
  const char* s1 = kk_string_cbuf_borrow(str1);
  const char* s2 = kk_string_cbuf_borrow(str2);
  if (s1==s2) return 0;
  return strcmp(s1, s2);
}

int kk_string_cmp(kk_string_t str1, kk_string_t str2, kk_context_t* ctx) {
  int ord = kk_string_cmp_borrow(str1,str2);
  kk_string_drop(str1,ctx);
  kk_string_drop(str2,ctx);
  return ord;
}

int kk_string_icmp_borrow(kk_string_t str1, kk_string_t str2) {
  const char* s1 = kk_string_cbuf_borrow(str1);
  const char* s2 = kk_string_cbuf_borrow(str2);
  if (s1==s2) return 0;
  return stricmpx(s1, s2);
}

int kk_string_icmp(kk_string_t str1, kk_string_t str2, kk_context_t* ctx) {
  int ord = kk_string_icmp_borrow(str1, str2);
  kk_string_drop(str1, ctx);
  kk_string_drop(str2, ctx);
  return ord;
}


// Count code points in a UTF8 string.
size_t kk_decl_pure kk_string_count(kk_string_t str) {
  const uint8_t* s = kk_string_buf_borrow(str);
  size_t cont = 0;      // continuation character counts
  const uint8_t* t = s; // current position 

#ifdef ARCH_ALLOW_WORD_READS
  // advance per byte until aligned
  for ( ; ((((uintptr_t)t) % sizeof(kk_uintx_t)) != 0) && (*t != 0); t++) {
    // count continuation bytes
    if (kk_utf8_is_cont(*t)) cont++;
  }  
  // advance per sizeof(kk_uintx_t). This may read (sizeof(kk_uintx_t)-1) bytes past the end
  // but that should always be ok (as protection is word size aligned on all architectures (?))
  if (*t != 0) {
    kk_assert_internal(((uintptr_t)t) % sizeof(kk_uintx_t) == 0);
    const kk_uintx_t* p;
    for (p = (const kk_uintx_t*)t; !kk_bits_has_zero_byte(*p); p++) {
      // count continuation bytes (0b10xxxxxx bytes) in parallel
      const kk_uintx_t u = *p;
      const kk_uintx_t m = ((u & kk_bits_high_mask) >> 7) & ((~u) >> 6); // each byte in `m` is 0x01 iff it was a continuation byte
      cont += kk_bits_byte_sum(m);
    }
    t = (const uint8_t*)p; // restore t
  }
#endif

  // advance per byte until 0
  for (; *t != 0; t++) {
    // count continuation characters
    if (kk_utf8_is_cont(*t)) cont++;
  }
  kk_assert_internal(t >= s);
  size_t count = t - s;
  kk_assert_internal(count >= cont);
  return (count - cont);
}


kk_string_t kk_string_from_char(kk_char_t c, kk_context_t* ctx) {
  uint8_t buf[16];
  size_t count;
  kk_utf8_write(c, buf, &count);
  if (count < 16) buf[count] = 0; else buf[0] = 0;
  return kk_string_alloc_dup((const char*)buf, ctx);
}

kk_string_t kk_string_from_chars(kk_vector_t v, kk_context_t* ctx) {
  size_t n;
  kk_box_t* cs = kk_vector_buf(v, &n);
  size_t len = 0;
  for (size_t i = 0; i < n; i++) {
    len += kk_utf8_len(kk_char_unbox(cs[i], ctx));
  }
  kk_string_t s = kk_string_alloc_buf(len + 1, ctx);
  uint8_t* p = (uint8_t*)kk_string_cbuf_borrow(s);
  for (size_t i = 0; i < n; i++) {
    size_t count;
    kk_utf8_write(kk_char_unbox(cs[i], ctx), p, &count);
    p += count;
  }
  kk_assert_internal(kk_string_buf_borrow(s) + n == p);
  kk_vector_drop(v,ctx);
  return s;
}

kk_vector_t kk_string_to_chars(kk_string_t s, kk_context_t* ctx) {
  size_t n = kk_string_count(kk_string_dup(s));
  kk_vector_t v = kk_vector_alloc(n, kk_box_null, ctx);
  kk_box_t* cs = kk_vector_buf(v, NULL);
  const uint8_t* p = kk_string_buf_borrow(s);
  for (size_t i = 0; i < n; i++) {
    size_t count;
    cs[i] = kk_char_box(kk_utf8_read(p, &count),ctx);
    p += count;
  }
  kk_assert_internal(p == kk_string_buf_borrow(s) + kk_string_len_borrow(s));
  kk_string_drop(s,ctx);
  return v;
}

kk_string_t kk_string_cat(kk_string_t s1, kk_string_t s2, kk_context_t* ctx) {
  const size_t len1 = kk_string_len_borrow(s1);
  const size_t len2 = kk_string_len_borrow(s2);
  kk_string_t t = kk_string_alloc_buf(len1 + len2, ctx );
  uint8_t* p = (uint8_t*)kk_string_buf_borrow(t);
  memcpy(p, kk_string_buf_borrow(s1), len1);
  memcpy(p+len1, kk_string_buf_borrow(s2), len2);
  kk_assert_internal(p[len1+len2] == 0);
  kk_string_drop(s1, ctx);
  kk_string_drop(s2, ctx);
  return t;
}

kk_vector_t kk_string_splitv(kk_string_t s, kk_string_t sep, kk_context_t* ctx) {
  return kk_string_splitv_atmost(s, sep, UINT32_MAX, ctx);
}

kk_vector_t kk_string_splitv_atmost(kk_string_t s, kk_string_t sep, size_t n, kk_context_t* ctx) {
  const char* p = kk_string_cbuf_borrow(s);
  const char* q = kk_string_cbuf_borrow(sep);
  if (n<1) n = 1;
  size_t seplen = strlen(q);
  size_t count;
  if (seplen > 0) {
    // count separators
    count = 1;
    const char* r = p;
    while (count < n && ((r = strstr(r, q)) != NULL)) {  
      count++;
      r += seplen;
    }
  }
  else {
    // split into characters
    count = kk_string_count(kk_string_dup(s)); 
    if (count > n) count = n;
  }
  kk_assert_internal(n > 0);
  // copy to vector
  kk_vector_t v = kk_vector_alloc(n, kk_box_null, ctx);
  kk_box_t* ss = kk_vector_buf(v, NULL);
  for (size_t i = 0; i < (n-1); i++) {
    const char* r;
    if (seplen > 0) {
      r = strstr(p, q);
    }
    else {
      r = (const char*)kk_utf8_next((const uint8_t*)p);
    }
    kk_assert_internal(r != NULL && r > p);
    size_t len = (r - p);
    ss[i] = kk_string_box(kk_string_alloc_len(len, p, ctx));
    p = r;  // advance
  }
  ss[n-1] = kk_string_box(kk_string_alloc_dup(p, ctx));  // todo: share string if p == s ?
  kk_string_drop(s,ctx);
  kk_string_drop(sep, ctx);
  return v;
}

kk_string_t kk_string_repeat(kk_string_t str, size_t n, kk_context_t* ctx) {
  const char* s = kk_string_cbuf_borrow(str);
  size_t len = strlen(s);
  if (len == 0) return kk_string_dup(kk_string_empty);
  kk_string_t tstr = kk_string_alloc_len(len*n, NULL, ctx); // TODO: check overflow
  char* t = (char*)kk_string_cbuf_borrow(tstr);
  for (size_t i = 0; i < n; i++) {
    strcpy(t, s);
    t += len;
  }
  kk_assert_internal(*t == 0);
  kk_string_drop(str,ctx);
  return tstr;
}

ptrdiff_t kk_string_index_of(kk_string_t str, kk_string_t sub, kk_context_t* ctx) {
  size_t slen = kk_string_len_borrow(str);
  size_t tlen = kk_string_len_borrow(sub);
  ptrdiff_t idx;
  if (tlen == 0) {
    idx = (slen == 0 ? -1 : 0);
  }
  else if (tlen > slen) {
    idx = -1;
  }
  else {
    const char* s = kk_string_cbuf_borrow(str);
    const char* t = kk_string_cbuf_borrow(sub);
    const char* p = strstr(s, t);
    idx = (p == NULL ? -1 : (p - s));
  }
  kk_string_drop(str, ctx);
  kk_string_drop(sub, ctx);
  return idx;
}

ptrdiff_t kk_string_last_index_of(kk_string_t str, kk_string_t sub, kk_context_t* ctx) {
  size_t slen = kk_string_len_borrow(str);
  size_t tlen = kk_string_len_borrow(sub);
  ptrdiff_t idx;
  if (tlen == 0) {
    idx = (slen - 1);
  }
  else if (tlen > slen) {
    idx = -1;
  }
  else if (tlen == slen) {
    idx = (kk_string_cmp_borrow(str, sub) == 0 ? 0 : -1);
  }
  else {
    const char* s = kk_string_cbuf_borrow(str);
    const char* t = kk_string_cbuf_borrow(sub);
    const char* p;
    for (p = s + slen - tlen; p >= s; p--) {  // todo: use reverse Boyer-Moore instead of one character at a time
      if (strncmp(p, t, tlen) == 0) break;
    }
    idx = (p >= s ? p - s : -1);
  }
  kk_string_drop(str, ctx);
  kk_string_drop(sub, ctx);
  return idx;
}

bool kk_string_starts_with(kk_string_t str, kk_string_t pre, kk_context_t* ctx) {
  size_t slen = kk_string_len_borrow(str);
  size_t tlen = kk_string_len_borrow(pre);
  bool starts;
  if (tlen == 0) {
    starts = (slen > 0);
  }
  else if (tlen > slen) {
    starts = false;
  }
  else {
    const char* s = kk_string_cbuf_borrow(str);
    const char* t = kk_string_cbuf_borrow(pre);
    starts = (strncmp(s, t, tlen) == 0);
  }
  kk_string_drop(str, ctx);
  kk_string_drop(pre, ctx);
  return starts;
}

bool kk_string_ends_with(kk_string_t str, kk_string_t post, kk_context_t* ctx) {
  size_t slen = kk_string_len_borrow(str);
  size_t tlen = kk_string_len_borrow(post);
  bool ends;
  if (tlen == 0) {
    ends = (slen > 0);
  }
  else if (tlen > slen) {
    ends = false;
  }
  else {
    const char* s = kk_string_cbuf_borrow(str);
    const char* t = kk_string_cbuf_borrow(post);
    ends = (strncmp(s + slen - tlen, t, tlen) == 0);
  }
  kk_string_drop(str, ctx);
  kk_string_drop(post, ctx);
  return ends;
}

bool kk_string_contains(kk_string_t str, kk_string_t sub, kk_context_t* ctx) {
  return (kk_string_index_of(str, sub, ctx) >= 0);
}


kk_string_t kk_string_to_upper(kk_string_t str, kk_context_t* ctx) {
  const size_t len = kk_string_len_borrow(str);
  const char* s = kk_string_cbuf_borrow(str);
  kk_string_t tstr = kk_string_copy(str, ctx);
  char* t = (char*)kk_string_cbuf_borrow(tstr);   // t & s may align!
  for (size_t i = 0; i < len; i++) {
    t[i] = kk_ascii_toupper(s[i]);
  }
  return tstr;
}

kk_string_t  kk_string_to_lower(kk_string_t str, kk_context_t* ctx) {
  const size_t len = kk_string_len_borrow(str);
  const char* s = kk_string_cbuf_borrow(str);
  kk_string_t tstr = kk_string_copy(str, ctx);
  char* t = (char*)kk_string_cbuf_borrow(tstr);   // t & s may align!
  for (size_t i = 0; i < len; i++) {
    t[i] = kk_ascii_tolower(s[i]);
  }
  return tstr;
}

kk_string_t  kk_string_trim_left(kk_string_t str, kk_context_t* ctx) {
  const size_t len = kk_string_len_borrow(str);
  const char* s = kk_string_cbuf_borrow(str);
  const char* p = s;
  for ( ; *p != 0 && ascii_iswhite(*p); p++) { }
  if (p == s) return str;           // no trim needed
  const size_t tlen = len - (p - s);      // todo: if s is unique and tlen close to slen, move inplace?
  kk_string_t tstr = kk_string_alloc_len(tlen, p, ctx);
  kk_string_drop(str, ctx);
  return tstr;
}

kk_string_t  kk_string_trim_right(kk_string_t str, kk_context_t* ctx) {
  const size_t len = kk_string_len_borrow(str);
  const char* s = kk_string_cbuf_borrow(str);
  const char* p = s + len - 1;
  for (; p >= s && ascii_iswhite(*p); p--) {}
  const size_t tlen = (p - s) + 1;
  if (len == tlen) return str;  // no trim needed
  kk_string_t tstr = kk_string_alloc_len(tlen, s, ctx);
  kk_string_drop(str, ctx);
  return tstr;
}

/*--------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------*/

kk_unit_t kk_println(kk_string_t s, kk_context_t* ctx) {
  // TODO: set locale to UTF8?
  puts(kk_string_cbuf_borrow(s));
  kk_string_drop(s,ctx);
  return kk_Unit;
}

kk_unit_t kk_print(kk_string_t s, kk_context_t* ctx) {
  // TODO: set locale to UTF8?
  fputs(kk_string_cbuf_borrow(s), stdout);
  kk_string_drop(s,ctx);
  return kk_Unit;
}

kk_unit_t kk_trace(kk_string_t s, kk_context_t* ctx) {
  fputs(kk_string_cbuf_borrow(s), stderr);
  fputs("\n", stderr);
  kk_string_drop(s, ctx);
  return kk_Unit;
}

kk_unit_t kk_trace_any(kk_string_t s, kk_box_t x, kk_context_t* ctx) {
  fprintf(stderr, "%s: ", kk_string_cbuf_borrow(s));
  kk_string_drop(s, ctx);
  kk_trace(kk_show_any(x,ctx),ctx);
  return kk_Unit;
}


kk_string_t kk_double_show_fixed(double d, int32_t prec, kk_context_t* ctx) {
  // TODO: respect prec
  KK_UNUSED(prec);
  char buf[32];
  snprintf(buf, 32, "%f", d);
  return kk_string_alloc_dup(buf, ctx);
}

kk_string_t kk_double_show_exp(double d, int32_t prec, kk_context_t* ctx) {
  // TODO: respect prec
  KK_UNUSED(prec);
  char buf[32];
  snprintf(buf, 32, "%e", d);
  return kk_string_alloc_dup(buf, ctx);
}

kk_string_t kk_double_show(double d, int32_t prec, kk_context_t* ctx) {
  // TODO: respect prec
  KK_UNUSED(prec);
  char buf[32];
  snprintf(buf, 32, "%g", d);
  return kk_string_alloc_dup(buf, ctx);
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
    return kk_string_alloc_dup(buf, ctx);
  }
  else if (b.box == kk_box_null.box) {
    return kk_string_alloc_dup("null", ctx);
  }
  else if (b.box == 0) {
    return kk_string_alloc_dup("ptr(NULL)", ctx);
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
      return (kk_string_t)p;
    }
    else if (tag == KK_TAG_FUNCTION) {
      kk_function_t fun = kk_block_assert(kk_function_t, p, KK_TAG_FUNCTION);
      snprintf(buf, 128, "function(0x%zx)", (uintptr_t)kk_cptr_unbox(fun->fun));
      kk_box_drop(b,ctx);
      return kk_string_alloc_dup(buf, ctx);
    }
    else {
      // TODO: handle all builtin tags 
      snprintf(buf, 128, "ptr(0x%zx, tag: %i, rc: 0x%zx, scan: %zu)", (uintptr_t)p, tag, kk_block_refcount(p), kk_block_scan_fsize(p));
      kk_box_drop(b, ctx);
      return kk_string_alloc_dup(buf, ctx);
    }
  }
}
