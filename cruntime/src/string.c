/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
#include "runtime.h"

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


static inline bool utf8_is_cont(uint8_t c) {
  return (((int8_t)c) <= -65); // c is between 0x80 and 0xBF (i.e. has the form 0x10xxxxxx)
}

uint_t decl_pure string_count(string_t str) {
  const uint8_t* s0 = string_buf_borrow(str);
  assert_internal(((uintptr_t)s0 % sizeof(uint_t)) == 0); // always aligned start

  // count per sizeof(uint_t) bytes; this may read (sizeof(uint_t)-1) bytes past the end
  // but that should always be ok (as protection is word size aligned on all architectures (?))
  const uint_t* p = (const uint_t*)s0;
  uint_t cont = 0;  // continuation character counts
  for(; !bits_has_zero_byte(*p); p++) {
    // count continuation bytes (0b10xxxxxx bytes) in parallel
    // see <https://graphics.stanford.edu/~seander/bithacks.html#HasLessInWord>
    const uint_t u = *p;
    const uint_t m = ((u & bits_high_mask) >> 7) & ((~u) >> 6);
    cont += (m * bits_one_mask) >> ((sizeof(uint_t)-1)*8);  // multiply by one_mask leaves count in the msb
  }
  // count left over
  const uint8_t* s1 = (const uint8_t*)p;
  for (; *s1 != 0; s1++) {
    // count continuation characters
    if (utf8_is_cont(*s1)) cont++;
  }
  assert_internal(s1 >= s0);
  uint_t chars = s1 - s0;
  assert_internal(chars > cont);
  return (chars - cont);
}
