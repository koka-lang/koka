#pragma once
#ifndef __BITCOUNT_H__
#define __BITCOUNT_H__

/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/


/* -----------------------------------------------------------
  `clz` count leading zero bits
  `ctz` count trailing zero bits  
----------------------------------------------------------- */

static inline uint8_t bits_clz32(uint32_t x);
static inline uint8_t bits_ctz32(uint32_t x);
static inline uint8_t bits_ctz64(uint64_t x);
static inline uint8_t bits_ctz64(uint64_t x);

#if defined(_MSC_VER)
#include <intrin.h>
static inline uint8_t bits_clz32(uint32_t x) {
  unsigned long idx;
  return (_BitScanReverse(&idx, x) ? 31 - (uint8_t)idx : 32);
}
static inline uint8_t bits_ctz32(uint32_t x) {
  unsigned long idx;
  return (_BitScanForward(&idx, x) ? (uint8_t)idx : 32);
}
#if (INTPTR_SIZE >= 8)
#define HAVE_BIT_COUNT64
static inline uint8_t bits_clz64(uint64_t x) {
  unsigned long idx;
  return (_BitScanReverse64(&idx, x) ? 63 - (uint8_t)idx : 64);
}
static inline uint8_t bits_ctz64(uint64_t x) {
  unsigned long idx;
  return (_BitScanForward64(&idx, x) ? (uint8_t)idx : 64);
}
#endif

#elif defined(__GNUC__)
static inline uint8_t bits_clz32(uint32_t x) {
  return (x==0 ? 32 : __builtin_clz(x));
}
static inline uint8_t bits_ctz32(uint32_t x) {
  return (x==0 ? 32 : __builtin_ctz(x));
}
#if (INTPTR_SIZE >= 8)
#define HAVE_BIT_COUNT64
static inline uint8_t bits_clz64(uint64_t x) {
  return (x==0 ? 64 : __builtin_clzll(x));
}
static inline uint8_t bits_ctz64(uint64_t x) {
  return (x==0 ? 64 : __builtin_ctzll(x));
}
#endif
#else
static inline uint8_t bits_ctz32(uint32_t x) {
  // de Bruijn multiplication, see <http://supertech.csail.mit.edu/papers/debruijn.pdf>
  static const unsigned char debruijn[32] = {
    0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
    31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
  };
  return debruijn[((x & -(int32_t)x) * U32(0x077CB531)) >> 27];
}

static inline uint8_t bits_clz32(uint32_t x) {
  // de Bruijn multiplication, see <http://supertech.csail.mit.edu/papers/debruijn.pdf>
  static const uint8_t debruijn[32] = {
    31, 22, 30, 21, 18, 10, 29, 2, 20, 17, 15, 13, 9, 6, 28, 1, 
    23, 19, 11, 3, 16, 14, 7, 24, 12, 4, 8, 25, 5, 26, 27, 0
  };
  if (x==0) return 32;
  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  return debruijn[(uint32_t)(x * U32(0x07C4ACDD)) >> 27];
}
#endif

#ifndef HAVE_BIT_COUNT64
#define HAVE_BIT_COUNT64
static inline uint8_t bits_clz64(uint64_t x) {
  uint8_t cnt = bits_clz32((uint32_t)(x >> 32));
  if (cnt < 32) return cnt;
  return (32 + bits_clz32((uint32_t)x));
}
static inline uint8_t bits_ctz64(uint64_t x) {
  uint8_t cnt = bits_ctz32((uint32_t)x);
  if (cnt < 32) return cnt;
  return (32 + bits_ctz32((uint32_t)(x >> 32)));
}
#endif

#if (INTPTR_SIZE == 8) 
static inline uint8_t bits_clz(uintx_t x) {
  return bits_clz64(x);
}
static inline uint8_t bits_ctz(uintx_t x) {
  return bits_ctz64(x);
}
#else
static inline uint8_t bits_clz(uintx_t x) {
  return bits_clz32(x);
}
static inline uint8_t bits_ctz(uintx_t x) {
  return bits_ctz32(x);
}
#endif


/* -----------------------------------------------------------
  Is there a zero byte in a word? see: <https://graphics.stanford.edu/~seander/bithacks.html#ZeroInWord>
----------------------------------------------------------- */

#define bits_one_mask32     U32(0x01010101)
#define bits_one_mask64     U64(0x0101010101010101)
#define bits_one_mask       ((~(UX(0)))/0xFF)         // 0x01010101 ...

#define bits_high_mask32    (bits_one_mask32<<7)      // 0x80808080
#define bits_high_mask64    (bits_one_mask64<<7)      // 0x8080808080808080
#define bits_high_mask      (bits_one_mask<<7)        // 0x80808080 ...

static inline bool bits_has_zero_byte32(uint32_t x) {
  return ((x - bits_one_mask32) & (~x & bits_high_mask32));
}

static inline bool bits_has_zero_byte64(uint64_t x) {
  return ((x - bits_one_mask64) & (~x & bits_high_mask64));
}

static inline bool bits_has_zero_byte(uintx_t x) {
  return ((x - bits_one_mask) & (~x & bits_high_mask));
}


/* -----------------------------------------------------------
  Is a word a power of two? see: <https://graphics.stanford.edu/~seander/bithacks.html#DetermineIfPowerOf2>
  (0 is not a power of two)
----------------------------------------------------------- */
#define _bits_is_power_of2(x)  ((x!=0) && ((x&(x-1)) == 0))

static inline bool bits_is_power_of2_32(uint32_t x) {
  return _bits_is_power_of2(x);
}

static inline bool bits_is_power_of2_64(uint64_t x) {
  return _bits_is_power_of2(x);
}

static inline bool bits_is_power_of2(uintx_t x) {
  return _bits_is_power_of2(x);
}

/* -----------------------------------------------------------
  Rotations
----------------------------------------------------------- */

static inline uint32_t rotl32(uint32_t x, uint32_t shift) {
  return (x << shift) | (x >> (32 - shift));
}

static inline uint32_t rotr32(uint32_t x, uint32_t shift) {
  return (x >> shift) | (x << (32 - shift));
}

static inline uint64_t rotl64(uint64_t x, uint64_t shift) {
  return (x << shift) | (x >> (64 - shift));
}

static inline uint64_t rotr64(uint64_t x, uint64_t shift) {
  return (x >> shift) | (x << (64 - shift));
}

static inline uintx_t rotl(uintx_t x, uintx_t shift) {
  return (x << shift) | (x >> (INTX_BITS - shift));
}

static inline uintx_t rotr(uintx_t x, uintx_t shift) {
  return (x >> shift) | (x << (INTX_BITS - shift));
}


#endif // include guard