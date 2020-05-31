/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
#define  _CRT_SECURE_NO_WARNINGS
#include "runtime.h"

struct _string_s _static_string_empty = { HEADER_STATIC(0,TAG_STRING), {0} };

// Allow reading aligned words as long as some bytes in it are part of a valid C object
#define ARCH_ALLOW_WORD_READS  (1)  


int_t string_cmp_borrow(string_t str1, string_t str2) {
  const char* s1 = string_cbuf_borrow(str1);
  const char* s2 = string_cbuf_borrow(str2);
  return strcmp(s1, s2);
}

int_t string_cmp(string_t str1, string_t str2, context_t* ctx) {
  int_t ord = string_cmp_borrow(str1,str2);
  string_drop(str1,ctx);
  string_drop(str2,ctx);
  return ord;
}

int_t string_icmp_borrow(string_t str1, string_t str2) {
  const char* s1 = string_cbuf_borrow(str1);
  const char* s2 = string_cbuf_borrow(str2);
  return _stricmp(s1, s2);
}

int_t string_icmp(string_t str1, string_t str2, context_t* ctx) {
  int_t ord = string_icmp_borrow(str1, str2);
  string_drop(str1, ctx);
  string_drop(str2, ctx);
  return ord;
}

uint_t decl_pure string_len_borrow(string_t str) {
  return strlen(string_cbuf_borrow(str));
}


// Count code points in a UTF8 string.
uint_t decl_pure string_count(string_t str) {
  const uint8_t* s = string_buf_borrow(str);
  uint_t cont = 0;      // continuation character counts
  const uint8_t* t = s; // current position 

#ifdef ARCH_ALLOW_WORD_READS
  // advance per byte until aligned
  for ( ; ((((uintptr_t)t) % sizeof(uint_t)) != 0) && (*t != 0); t++) {
    // count continuation bytes
    if (utf8_is_cont(*t)) cont++;
  }  
  // advance per sizeof(uint_t). This may read (sizeof(uint_t)-1) bytes past the end
  // but that should always be ok (as protection is word size aligned on all architectures (?))
  if (*t != 0) {
    assert_internal(((uintptr_t)t) % sizeof(uint_t) == 0);
    const uint_t* p;
    for (p = (const uint_t*)t; !bits_has_zero_byte(*p); p++) {
      // count continuation bytes (0b10xxxxxx bytes) in parallel
      // see <https://graphics.stanford.edu/~seander/bithacks.html#HasLessInWord>
      const uint_t u = *p;
      const uint_t m = ((u & bits_high_mask) >> 7) & ((~u) >> 6); // each byte in `m` is 0x01 iff it was a continuation byte
      cont += (m * bits_one_mask) >> ((sizeof(uint_t) - 1) * 8);  // multiply by one_mask leaves count of 0x01 bytes in the msb
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
  uint_t count = t - s;
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
  vector_drop(v,ctx);
  return s;
}

vector_t string_to_chars(string_t s, context_t* ctx) {
  size_t n = string_count(string_dup(s));
  vector_t v = vector_alloc(n, 0, ctx);
  box_t* cs = vector_buf(v, NULL);
  const uint8_t* p = string_buf_borrow(s);
  for (size_t i = 0; i < n; i++) {
    size_t count;
    cs[i] = box_char_t(utf8_read(p, &count),ctx);
    p += count;
  }
  assert_internal(p == string_buf_borrow(s) + string_len(s));
  string_drop(s,ctx);
  return v;
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
    count = string_count(string_dup(s)); 
    if (count > n) count = n;
  }
  assert_internal(n > 0);
  // copy to vector
  vector_t v = vector_alloc(n, 0, ctx);
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
  string_drop(s,ctx);
  string_drop(sep, ctx);
  return v;
}

string_t string_repeat(string_t str, uint_t n, context_t* ctx) {
  const char* s = string_cbuf_borrow(str);
  size_t len = strlen(s);
  if (len == 0) return string_dup(string_empty);
  string_t tstr = string_alloc_len(len*n, NULL, ctx); // TODO: check overflow
  char* t = (char*)string_cbuf_borrow(tstr);
  for (size_t i = 0; i < n; i++) {
    strcpy(t, s);
    t += len;
  }
  assert_internal(*t == 0);
  string_drop(str,ctx);
  return tstr;
}

/*--------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------*/

unit_t println(string_t s, context_t* ctx) {
  // TODO: set locale to UTF8?
  puts(string_cbuf_borrow(s));
  string_drop(s,ctx);
  return Unit;
}

unit_t print(string_t s, context_t* ctx) {
  // TODO: set locale to UTF8?
  fputs(string_cbuf_borrow(s), stdout);
  string_drop(s,ctx);
  return Unit;
}

unit_t trace(string_t s, context_t* ctx) {
  fputs(string_cbuf_borrow(s), stderr);
  fputs("\n", stderr);
  string_drop(s, ctx);
  return Unit;
}

unit_t trace_any(string_t s, box_t x, context_t* ctx) {
  fprintf(stderr, "%s: ", string_cbuf_borrow(s));
  string_drop(s, ctx);
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
  if (is_double(b)) {
    return double_show(unbox_double(b, ctx), 0, ctx);
  }
  else if (is_enum(b)) {
    snprintf(buf, 128, "enum(%zu)", unbox_enum(b));
    return string_alloc_dup(buf, ctx);
  }
  else if (is_int(b)) {
    snprintf(buf, 128, "int(%zi)", unbox_int(b));
    return string_alloc_dup(buf, ctx);
  }
  else if (b == box_null) {
    return string_alloc_dup("null", ctx);
  }
  else if (b == 0) {
    return string_alloc_dup("cptr(NULL)", ctx);
  }
  else {
    ptr_t p = unbox_ptr(b);
    tag_t tag = ptr_tag(p);
    if (tag == TAG_BIGINT) {
      // todo: add tag
      return integer_to_string(b, ctx);
    }
    else if (tag == TAG_STRING || tag == TAG_STRING_RAW) {
      // todo: add tag
      return b;
    }
    else if (tag == TAG_FUNCTION) {
      struct function_s* fun = ptr_data_as(struct function_s, p);
      snprintf(buf, 128, "function(0x%zx)", (uintptr_t)unbox_cptr(fun->fun));
      boxed_drop(b,ctx);
      return string_alloc_dup(buf, ctx);
    }
    else {
      // TODO: handle all builtin tags 
      snprintf(buf, 128, "ptr(0x%zx, tag: %i, rc: 0x%zx, scan: %zu)", p, tag, (uintptr_t)ptr_refcount(p), block_scan_fsize(ptr_as_block(p)));
      boxed_drop(b, ctx);
      return string_alloc_dup(buf, ctx);
    }
  }
}
