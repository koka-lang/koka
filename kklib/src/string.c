/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
#define  _CRT_SECURE_NO_WARNINGS
#define  __USE_MINGW_ANSI_STDIO 1
#include "kklib.h"


// Allow reading aligned words as long as some bytes in it are part of a valid C object
#define ARCH_ALLOW_WORD_READS  (1)  

struct string_normal_s _static_string_empty = { { { HEADER_STATIC(0,TAG_STRING) } }, 0, {0} };

static char ascii_toupper(char c) {
  return (c >= 'a' && c <= 'z' ? c - 'a' + 'A' : c);
}
static char ascii_tolower(char c) {
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
    c = ascii_tolower(c);
    d = ascii_tolower(d);
  } while (c == d && c != 0);
  return (c - d);
}


int string_cmp_borrow(string_t str1, string_t str2) {
  const char* s1 = string_cbuf_borrow(str1);
  const char* s2 = string_cbuf_borrow(str2);
  if (s1==s2) return 0;
  return strcmp(s1, s2);
}

int string_cmp(string_t str1, string_t str2, context_t* ctx) {
  int ord = string_cmp_borrow(str1,str2);
  drop_string_t(str1,ctx);
  drop_string_t(str2,ctx);
  return ord;
}

int string_icmp_borrow(string_t str1, string_t str2) {
  const char* s1 = string_cbuf_borrow(str1);
  const char* s2 = string_cbuf_borrow(str2);
  if (s1==s2) return 0;
  return stricmpx(s1, s2);
}

int string_icmp(string_t str1, string_t str2, context_t* ctx) {
  int ord = string_icmp_borrow(str1, str2);
  drop_string_t(str1, ctx);
  drop_string_t(str2, ctx);
  return ord;
}


// Count code points in a UTF8 string.
size_t decl_pure string_count(string_t str) {
  const uint8_t* s = string_buf_borrow(str);
  size_t cont = 0;      // continuation character counts
  const uint8_t* t = s; // current position 

#ifdef ARCH_ALLOW_WORD_READS
  // advance per byte until aligned
  for ( ; ((((uintptr_t)t) % sizeof(uintx_t)) != 0) && (*t != 0); t++) {
    // count continuation bytes
    if (utf8_is_cont(*t)) cont++;
  }  
  // advance per sizeof(uintx_t). This may read (sizeof(uintx_t)-1) bytes past the end
  // but that should always be ok (as protection is word size aligned on all architectures (?))
  if (*t != 0) {
    assert_internal(((uintptr_t)t) % sizeof(uintx_t) == 0);
    const uintx_t* p;
    for (p = (const uintx_t*)t; !bits_has_zero_byte(*p); p++) {
      // count continuation bytes (0b10xxxxxx bytes) in parallel
      const uintx_t u = *p;
      const uintx_t m = ((u & bits_high_mask) >> 7) & ((~u) >> 6); // each byte in `m` is 0x01 iff it was a continuation byte
      cont += bits_byte_sum(m);
    }
    t = (const uint8_t*)p; // restore t
  }
#endif

  // advance per byte until 0
  for (; *t != 0; t++) {
    // count continuation characters
    if (utf8_is_cont(*t)) cont++;
  }
  assert_internal(t >= s);
  size_t count = t - s;
  assert_internal(count >= cont);
  return (count - cont);
}


string_t string_from_char(char_t c, context_t* ctx) {
  uint8_t buf[16];
  size_t count;
  utf8_write(c, buf, &count);
  if (count < 16) buf[count] = 0; else buf[0] = 0;
  return string_alloc_dup((const char*)buf, ctx);
}

string_t string_from_chars(vector_t v, context_t* ctx) {
  size_t n;
  box_t* cs = vector_buf(v, &n);
  size_t len = 0;
  for (size_t i = 0; i < n; i++) {
    len += utf8_len(unbox_char_t(cs[i], ctx));
  }
  string_t s = string_alloc_buf(len + 1, ctx);
  uint8_t* p = (uint8_t*)string_cbuf_borrow(s);
  for (size_t i = 0; i < n; i++) {
    size_t count;
    utf8_write(unbox_char_t(cs[i], ctx), p, &count);
    p += count;
  }
  assert_internal(string_buf_borrow(s) + n == p);
  drop_vector_t(v,ctx);
  return s;
}

vector_t string_to_chars(string_t s, context_t* ctx) {
  size_t n = string_count(dup_string_t(s));
  vector_t v = vector_alloc(n, box_null, ctx);
  box_t* cs = vector_buf(v, NULL);
  const uint8_t* p = string_buf_borrow(s);
  for (size_t i = 0; i < n; i++) {
    size_t count;
    cs[i] = box_char_t(utf8_read(p, &count),ctx);
    p += count;
  }
  assert_internal(p == string_buf_borrow(s) + string_len_borrow(s));
  drop_string_t(s,ctx);
  return v;
}

string_t string_cat(string_t s1, string_t s2, context_t* ctx) {
  const size_t len1 = string_len_borrow(s1);
  const size_t len2 = string_len_borrow(s2);
  string_t t = string_alloc_buf(len1 + len2, ctx );
  uint8_t* p = (uint8_t*)string_buf_borrow(t);
  memcpy(p, string_buf_borrow(s1), len1);
  memcpy(p+len1, string_buf_borrow(s2), len2);
  assert_internal(p[len1+len2] == 0);
  drop_string_t(s1, ctx);
  drop_string_t(s2, ctx);
  return t;
}

vector_t string_splitv(string_t s, string_t sep, context_t* ctx) {
  return string_splitv_atmost(s, sep, UINT32_MAX, ctx);
}

vector_t string_splitv_atmost(string_t s, string_t sep, size_t n, context_t* ctx) {
  const char* p = string_cbuf_borrow(s);
  const char* q = string_cbuf_borrow(sep);
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
    count = string_count(dup_string_t(s)); 
    if (count > n) count = n;
  }
  assert_internal(n > 0);
  // copy to vector
  vector_t v = vector_alloc(n, box_null, ctx);
  box_t* ss = vector_buf(v, NULL);
  for (size_t i = 0; i < (n-1); i++) {
    const char* r;
    if (seplen > 0) {
      r = strstr(p, q);
    }
    else {
      r = (const char*)utf8_next((const uint8_t*)p);
    }
    assert_internal(r != NULL && r > p);
    size_t len = (r - p);
    ss[i] = box_string_t(string_alloc_len(len, p, ctx));
    p = r;  // advance
  }
  ss[n-1] = box_string_t(string_alloc_dup(p, ctx));  // todo: share string if p == s ?
  drop_string_t(s,ctx);
  drop_string_t(sep, ctx);
  return v;
}

string_t string_repeat(string_t str, size_t n, context_t* ctx) {
  const char* s = string_cbuf_borrow(str);
  size_t len = strlen(s);
  if (len == 0) return dup_string_t(string_empty);
  string_t tstr = string_alloc_len(len*n, NULL, ctx); // TODO: check overflow
  char* t = (char*)string_cbuf_borrow(tstr);
  for (size_t i = 0; i < n; i++) {
    strcpy(t, s);
    t += len;
  }
  assert_internal(*t == 0);
  drop_string_t(str,ctx);
  return tstr;
}

ptrdiff_t string_index_of(string_t str, string_t sub, context_t* ctx) {
  size_t slen = string_len_borrow(str);
  size_t tlen = string_len_borrow(sub);
  ptrdiff_t idx;
  if (tlen == 0) {
    idx = (slen == 0 ? -1 : 0);
  }
  else if (tlen > slen) {
    idx = -1;
  }
  else {
    const char* s = string_cbuf_borrow(str);
    const char* t = string_cbuf_borrow(sub);
    const char* p = strstr(s, t);
    idx = (p == NULL ? -1 : (p - s));
  }
  drop_string_t(str, ctx);
  drop_string_t(sub, ctx);
  return idx;
}

ptrdiff_t string_last_index_of(string_t str, string_t sub, context_t* ctx) {
  size_t slen = string_len_borrow(str);
  size_t tlen = string_len_borrow(sub);
  ptrdiff_t idx;
  if (tlen == 0) {
    idx = (slen - 1);
  }
  else if (tlen > slen) {
    idx = -1;
  }
  else if (tlen == slen) {
    idx = (string_cmp_borrow(str, sub) == 0 ? 0 : -1);
  }
  else {
    const char* s = string_cbuf_borrow(str);
    const char* t = string_cbuf_borrow(sub);
    const char* p;
    for (p = s + slen - tlen; p >= s; p--) {  // todo: use reverse Boyer-Moore instead of one character at a time
      if (strncmp(p, t, tlen) == 0) break;
    }
    idx = (p >= s ? p - s : -1);
  }
  drop_string_t(str, ctx);
  drop_string_t(sub, ctx);
  return idx;
}

bool string_starts_with(string_t str, string_t pre, context_t* ctx) {
  size_t slen = string_len_borrow(str);
  size_t tlen = string_len_borrow(pre);
  bool starts;
  if (tlen == 0) {
    starts = (slen > 0);
  }
  else if (tlen > slen) {
    starts = false;
  }
  else {
    const char* s = string_cbuf_borrow(str);
    const char* t = string_cbuf_borrow(pre);
    starts = (strncmp(s, t, tlen) == 0);
  }
  drop_string_t(str, ctx);
  drop_string_t(pre, ctx);
  return starts;
}

bool string_ends_with(string_t str, string_t post, context_t* ctx) {
  size_t slen = string_len_borrow(str);
  size_t tlen = string_len_borrow(post);
  bool ends;
  if (tlen == 0) {
    ends = (slen > 0);
  }
  else if (tlen > slen) {
    ends = false;
  }
  else {
    const char* s = string_cbuf_borrow(str);
    const char* t = string_cbuf_borrow(post);
    ends = (strncmp(s + slen - tlen, t, tlen) == 0);
  }
  drop_string_t(str, ctx);
  drop_string_t(post, ctx);
  return ends;
}

bool string_contains(string_t str, string_t sub, context_t* ctx) {
  return (string_index_of(str, sub, ctx) >= 0);
}


string_t string_to_upper(string_t str, context_t* ctx) {
  const size_t len = string_len_borrow(str);
  const char* s = string_cbuf_borrow(str);
  string_t tstr = string_copy(str, ctx);
  char* t = (char*)string_cbuf_borrow(tstr);   // t & s may align!
  for (size_t i = 0; i < len; i++) {
    t[i] = ascii_toupper(s[i]);
  }
  return tstr;
}

string_t  string_to_lower(string_t str, context_t* ctx) {
  const size_t len = string_len_borrow(str);
  const char* s = string_cbuf_borrow(str);
  string_t tstr = string_copy(str, ctx);
  char* t = (char*)string_cbuf_borrow(tstr);   // t & s may align!
  for (size_t i = 0; i < len; i++) {
    t[i] = ascii_tolower(s[i]);
  }
  return tstr;
}

string_t  string_trim_left(string_t str, context_t* ctx) {
  const size_t len = string_len_borrow(str);
  const char* s = string_cbuf_borrow(str);
  const char* p = s;
  for ( ; *p != 0 && ascii_iswhite(*p); p++) { }
  if (p == s) return str;           // no trim needed
  const size_t tlen = len - (p - s);      // todo: if s is unique and tlen close to slen, move inplace?
  string_t tstr = string_alloc_len(tlen, p, ctx);
  drop_string_t(str, ctx);
  return tstr;
}

string_t  string_trim_right(string_t str, context_t* ctx) {
  const size_t len = string_len_borrow(str);
  const char* s = string_cbuf_borrow(str);
  const char* p = s + len - 1;
  for (; p >= s && ascii_iswhite(*p); p--) {}
  const size_t tlen = (p - s) + 1;
  if (len == tlen) return str;  // no trim needed
  string_t tstr = string_alloc_len(tlen, s, ctx);
  drop_string_t(str, ctx);
  return tstr;
}

/*--------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------*/

unit_t println(string_t s, context_t* ctx) {
  // TODO: set locale to UTF8?
  puts(string_cbuf_borrow(s));
  drop_string_t(s,ctx);
  return Unit;
}

unit_t print(string_t s, context_t* ctx) {
  // TODO: set locale to UTF8?
  fputs(string_cbuf_borrow(s), stdout);
  drop_string_t(s,ctx);
  return Unit;
}

unit_t trace(string_t s, context_t* ctx) {
  fputs(string_cbuf_borrow(s), stderr);
  fputs("\n", stderr);
  drop_string_t(s, ctx);
  return Unit;
}

unit_t trace_any(string_t s, box_t x, context_t* ctx) {
  fprintf(stderr, "%s: ", string_cbuf_borrow(s));
  drop_string_t(s, ctx);
  trace(show_any(x,ctx),ctx);
  return Unit;
}


string_t double_show_fixed(double d, int32_t prec, context_t* ctx) {
  // TODO: respect prec
  UNUSED(prec);
  char buf[32];
  snprintf(buf, 32, "%f", d);
  return string_alloc_dup(buf, ctx);
}

string_t double_show_exp(double d, int32_t prec, context_t* ctx) {
  // TODO: respect prec
  UNUSED(prec);
  char buf[32];
  snprintf(buf, 32, "%e", d);
  return string_alloc_dup(buf, ctx);
}

string_t double_show(double d, int32_t prec, context_t* ctx) {
  // TODO: respect prec
  UNUSED(prec);
  char buf[32];
  snprintf(buf, 32, "%g", d);
  return string_alloc_dup(buf, ctx);
}



string_t show_any(box_t b, context_t* ctx) {
  char buf[128];
#if USE_NAN_BOX
  if (_is_double(b)) {
    return double_show(unbox_double(b, ctx), 0, ctx);
  }
  else
#endif
  if (is_value(b)) {
    snprintf(buf, 128, "value(%zi)", unbox_int(b));
    return string_alloc_dup(buf, ctx);
  }
  else if (b.box == box_null.box) {
    return string_alloc_dup("null", ctx);
  }
  else if (b.box == 0) {
    return string_alloc_dup("ptr(NULL)", ctx);
  }
  else {
    block_t* p = unbox_ptr(b);
    tag_t tag = block_tag(p);
    if (tag == TAG_BIGINT) {
      // todo: add tag
      return integer_to_string(unbox_integer_t(b), ctx);
    }
    else if (tag == TAG_STRING_SMALL || tag == TAG_STRING || tag == TAG_STRING_RAW) {
      // todo: add tag
      return (string_t)p;
    }
    else if (tag == TAG_FUNCTION) {
      function_t fun = block_as_assert(function_t, p, TAG_FUNCTION);
      snprintf(buf, 128, "function(0x%zx)", (uintptr_t)unbox_cptr(fun->fun));
      drop_box_t(b,ctx);
      return string_alloc_dup(buf, ctx);
    }
    else {
      // TODO: handle all builtin tags 
      snprintf(buf, 128, "ptr(0x%zx, tag: %i, rc: 0x%zx, scan: %zu)", (uintptr_t)p, tag, block_refcount(p), block_scan_fsize(p));
      drop_box_t(b, ctx);
      return string_alloc_dup(buf, ctx);
    }
  }
}
