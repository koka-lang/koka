/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
#include "runtime.h"

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

// Is this a UTF8 continuation byte?
static inline bool utf8_is_cont(uint8_t c) {
  return (((int8_t)c) <= -65); // c is between 0x80 and 0xBF (i.e. has the form 0x10xxxxxx)
}

// Advance to the next codepoint. (does not advance past the end)
static inline const uint8_t* utf8_next(const uint8_t* s) {
  if (*s != 0) s++;                 // skip first byte
  for( ; utf8_is_cont(*s); s++) { } // skip continuation bytes
  return s;
}

// Retreat to the previous codepoint. 
static inline const uint8_t* utf8_prev(const uint8_t* s) {
  s--;                             // skip back at least 1 byte
  for (; utf8_is_cont(*s); s--) {} // skip while continuation bytes
  return s;
}


// Read code point
static inline char_t utf8_read(const uint8_t* s) {
  char_t b = *s;
  if (b <= 0x7F) {
    return b; // ASCII
  }
  if (b == 0xC0 && s[1] == 0x80) {
    return 0; // modified UTF8 zero 
  }
  if (b >= 0xC2 && b <= 0xDF && utf8_is_cont(s[1])) {
    return (((b & 0x1F) << 6) | (b & 0x3F));
  }
  if (  (b == 0xE0 && s[1] >= 0xA0 && s[1] <= 0xBF && utf8_is_cont(s[2]))
     || (b >= 0xE1 && b <= 0xEC && utf8_is_cont(s[1]) && utf8_is_cont(s[2]))
     || (b == 0xF0 && s[1] >= 0x90 && s[1] <= 0xBF && utf8_is_cont(s[2])) )
  {
    return (((b & 0x0F) << 12) | ((s[1] & 0x3F) << 6) | (s[2] & 0x3F));
  }
  if (  (b >= 0xF1 &&  b <= 0xF3 && utf8_is_cont(s[1]) && utf8_is_cont(s[2]) && utf8_is_cont(s[3]))
     || (b == 0xF4 && s[1] >= 0x80 && s[1] <= 0x8F && utf8_is_cont(s[2]) && utf8_is_cont(s[3])) )
  {
    return (((b & 0x07) << 18) | ((s[1] & 0x3F) << 12) | ((s[2] & 0x3F) << 6) | (s[3] & 0x3F));
  }
  return 0xFFFD;
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
