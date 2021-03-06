#pragma once
#ifndef KK_BITS_H
#define KK_BITS_H

/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Define __builtin suffixes for gcc/clang
#if defined(__GNUC__)
#if (LONG_MAX == INT32_MAX) 
#define __builtin32(name)  __builtin_##name##l
#else
#define __builtin32(name)  __builtin_##name
#endif
#if (LONG_MAX == INT64_MAX) 
#define __builtin64(name)  __builtin_##name##l
#else
#define __builtin64(name)  __builtin_##name##ll
#endif
#endif


#if (KK_INTX_SIZE==4)
#define kk_bitsx(name)  kk_bits_##name##32
#else
#define kk_bitsx(name)  kk_bits_##name##64
#endif

/* -----------------------------------------------------------
  Is a word a power of two? see: <https://graphics.stanford.edu/~seander/bithacks.html#DetermineIfPowerOf2>
  (0 is not a power of two)
----------------------------------------------------------- */
#define _kk_bits_is_power_of2(x)  ((x!=0) && ((x&(x-1)) == 0))

static inline bool kk_bits_is_power_of2_32(uint32_t x) {
  return _kk_bits_is_power_of2(x);
}

static inline bool kk_bits_is_power_of2_64(uint64_t x) {
  return _kk_bits_is_power_of2(x);
}

static inline bool kk_bits_is_power_of2(kk_uintx_t x) {
  return _kk_bits_is_power_of2(x);
}

/* -----------------------------------------------------------
  Rotations
----------------------------------------------------------- */
#ifdef _MSC_VER
#include <intrin.h>
static inline uint16_t kk_bits_rotl16(uint16_t x, uint16_t shift) {
  return _rotl16(x, (uint8_t)shift);  // in <intrin.h>
}
static inline uint16_t kk_bits_rotr16(uint16_t x, uint16_t shift) {
  return _rotr16(x, (uint8_t)shift);
}
static inline uint32_t kk_bits_rotl32(uint32_t x, uint32_t shift) {
  return _lrotl(x, (int)shift);
}
static inline uint32_t kk_bits_rotr32(uint32_t x, uint32_t shift) {
  return _lrotr(x, (int)shift);
}
static inline uint64_t kk_bits_rotl64(uint64_t x, uint64_t shift) {
  return _rotl64(x, (int)shift);
}
static inline uint64_t kk_bits_rotr64(uint64_t x, uint64_t shift) {
  return _rotr64(x, (int)shift);
}
#else
// most compilers translate these expressions to a direct rotation instruction
static inline uint16_t kk_bits_rotl16(uint16_t x, uint16_t shift) {
  return (x << shift) | (x >> (16 - shift));
}
static inline uint16_t kk_bits_rotr16(uint16_t x, uint16_t shift) {
  return (x >> shift) | (x << (16 - shift));
}
static inline uint32_t kk_bits_rotl32(uint32_t x, uint32_t shift) {
  return (x << shift) | (x >> (32 - shift));
}
static inline uint32_t kk_bits_rotr32(uint32_t x, uint32_t shift) {
  return (x >> shift) | (x << (32 - shift));
}
static inline uint64_t kk_bits_rotl64(uint64_t x, uint64_t shift) {
  return (x << shift) | (x >> (64 - shift));
}
static inline uint64_t kk_bits_rotr64(uint64_t x, uint64_t shift) {
  return (x >> shift) | (x << (64 - shift));
}
#endif

static inline kk_uintx_t kk_bits_rotl(kk_uintx_t x, kk_uintx_t shift) {
  return kk_bitsx(rotl)(x, shift);
}

static inline kk_uintx_t kk_bits_rotr(kk_uintx_t x, kk_uintx_t shift) {
  return kk_bitsx(rotr)(x, shift);
}


/* -----------------------------------------------------------
  `clz` count leading zero bits
  `ctz` count trailing zero bits  
----------------------------------------------------------- */

static inline uint8_t kk_bits_clz32(uint32_t x);
static inline uint8_t kk_bits_ctz32(uint32_t x);
static inline uint8_t kk_bits_ctz64(uint64_t x);
static inline uint8_t kk_bits_ctz64(uint64_t x);

#if defined(__GNUC__)
static inline uint8_t kk_bits_clz32(uint32_t x) {
  return (x==0 ? 32 : __builtin32(clz)(x));
}
static inline uint8_t kk_bits_ctz32(uint32_t x) {
  return (x==0 ? 32 : __builtin32(ctz)(x));
}
#if (KK_INTPTR_SIZE >= 8)
#define HAS_BITS_CLZ64
static inline uint8_t kk_bits_clz64(uint64_t x) {
  return (x==0 ? 64 : __builtin64(clz)(x));
}
static inline uint8_t kk_bits_ctz64(uint64_t x) {
  return (x==0 ? 64 : __builtin64(ctz)(x));
}
#endif

#elif defined(_MSC_VER) && (defined(_M_ARM64) || defined(_M_ARM) || defined(_M_X64) || defined(_M_IX86))
#include <intrin.h>

#if defined(_M_X64) || defined(_M_IX86)
extern bool __has_lzcnt;  // initialized in runtime.c
#endif

static inline uint8_t kk_bits_clz32(uint32_t x) {
  #if !defined(__clang__) && (defined(_M_X64) || defined(_M_IX86))
  if (__has_lzcnt) return (uint8_t)__lzcnt(x);
  #endif
  unsigned long idx;
  return (_BitScanReverse(&idx, x) ? 31 - (uint8_t)idx : 32);
}
static inline uint8_t kk_bits_ctz32(uint32_t x) {
  #if !defined(__clang__) && (defined(_M_X64) || defined(_M_IX86))
  if (__has_lzcnt) return (uint8_t)_tzcnt_u32(x);
  #endif
  unsigned long idx;
  return (_BitScanForward(&idx, x) ? (uint8_t)idx : 32);
}
#if (KK_INTPTR_SIZE >= 8)
#define HAS_BITS_CLZ64
static inline uint8_t kk_bits_clz64(uint64_t x) {
  #if !defined(__clang__) && (defined(_M_X64) || defined(_M_IX86))
  if (__has_lzcnt) return (uint8_t)__lzcnt64(x);
  #endif
  unsigned long idx;
  return (_BitScanReverse64(&idx, x) ? 63 - (uint8_t)idx : 64);
}
static inline uint8_t kk_bits_ctz64(uint64_t x) {
  #if !defined(__clang__) && (defined(_M_X64) || defined(_M_IX86))
  if (__has_lzcnt) return (uint8_t)_tzcnt_u64(x);
  #endif
  unsigned long idx;
  return (_BitScanForward64(&idx, x) ? (uint8_t)idx : 64);
}
#endif

#else
static inline uint8_t kk_bits_ctz32(uint32_t x) {
  // de Bruijn multiplication, see <http://supertech.csail.mit.edu/papers/debruijn.pdf>
  static const unsigned char debruijn[32] = {
    0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
    31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
  };
  return debruijn[((x & -(int32_t)x) * KU32(0x077CB531)) >> 27];
}

static inline uint8_t kk_bits_clz32(uint32_t x) {
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
  return debruijn[(uint32_t)(x * KU32(0x07C4ACDD)) >> 27];
}
#endif

#ifndef HAS_BITS_CLZ64
#define HAS_BITS_CLZ64
static inline uint8_t kk_bits_clz64(uint64_t x) {
  uint8_t cnt = kk_bits_clz32((uint32_t)(x >> 32));
  if (cnt < 32) return cnt;
  return (32 + kk_bits_clz32((uint32_t)x));
}
static inline uint8_t kk_bits_ctz64(uint64_t x) {
  uint8_t cnt = kk_bits_ctz32((uint32_t)x);
  if (cnt < 32) return cnt;
  return (32 + kk_bits_ctz32((uint32_t)(x >> 32)));
}
#endif

static inline uint8_t kk_bits_clz(kk_uintx_t x) {
  return kk_bitsx(clz)(x);
}
static inline uint8_t kk_bits_ctz(kk_uintx_t x) {
  return kk_bitsx(ctz)(x);
}

/* -----------------------------------------------------------
  Byte operations

  - Is there a zero byte in a word? see: <https://graphics.stanford.edu/~seander/bithacks.html#ZeroInWord>
  - Is any byte equal to n?
  - Sum of bytes
----------------------------------------------------------- */

#define kk_bits_one_mask32     KU32(0x01010101)
#define kk_bits_one_mask64     KU64(0x0101010101010101)
#define kk_bits_one_mask       ((~(KUX(0)))/0xFF)         // 0x01010101 ...

#define kk_bits_high_mask32    (kk_bits_one_mask32<<7)      // 0x80808080
#define kk_bits_high_mask64    (kk_bits_one_mask64<<7)      // 0x8080808080808080
#define kk_bits_high_mask      (kk_bits_one_mask<<7)        // 0x80808080 ...

static inline bool kk_bits_has_zero_byte32(uint32_t x) {
  return ((x - kk_bits_one_mask32) & (~x & kk_bits_high_mask32));
}

static inline bool kk_bits_has_zero_byte64(uint64_t x) {
  return ((x - kk_bits_one_mask64) & (~x & kk_bits_high_mask64));
}

// Does `x` contain a zero byte?
static inline bool kk_bits_has_zero_byte(kk_uintx_t x) {
  return kk_bitsx(has_zero_byte)(x);
}

// is there any byte in `x` equal to `n`?
static inline bool kk_bits_has_byte32(uint32_t x, uint8_t n) {
  uint32_t y = n;
  y |= (y << 8);
  y |= (y << 16);
  x ^= y;
  return kk_bits_has_zero_byte32(x);
}

// is there any byte in `x` equal to `n`?
static inline bool kk_bits_has_byte64(uint64_t x, uint8_t n) {
  uint64_t y = n;
  y |= (y << 8);
  y |= (y << 16);
  y |= (y << 32);
  x ^= y;
  return kk_bits_has_zero_byte64(x);
}

// is there any byte in `x` equal to `n`?
static inline bool kk_bits_has_byte(kk_uintx_t x, uint8_t n) {
  return kk_bitsx(has_byte)(x,n);
}


// sum of all the bytes in `x` if it is guaranteed that the sum < 256!
static inline uint8_t kk_bits_byte_sum32(uint32_t x) {
  // perform `x * kk_bits_one_mask`: the highest byte contains the sum of all bytes.
  // note: clang will compile to either shift/adds or a multiply depending on the target
  x += (x << 8);
  x += (x << 16);
  return (x >> 24);
}

// sum of all the bytes in `x` if it is guaranteed that the sum < 256!
static inline uint8_t kk_bits_byte_sum64(uint64_t x) {
  x += (x << 8);
  x += (x << 16);
  x += (x << 32);
  return (x >> 56);
}

// sum of all the bytes in `x` if it is guaranteed that the sum < 256!
static inline uint8_t kk_bits_byte_sum(kk_uintx_t x) {
  return kk_bitsx(byte_sum)(x);
}


/* ---------------------------------------------------------------
  kk_bits_count: population count / hamming weight  (count set bits)
  see <https://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel>
------------------------------------------------------------------ */

static inline uint32_t kk_bits_generic_count32(uint32_t x) {
  x = x - ((x >> 1) & KU32(0x55555555));
  x = (x & KU32(0x33333333)) + ((x >> 2) & KU32(0x33333333));
  x = (x + (x >> 4)) & KU32(0x0F0F0F0F);
  return kk_bits_byte_sum32(x);
}

static inline uint64_t kk_bits_generic_count64(uint64_t x) {
  x = x - ((x >> 1) & KU64(0x5555555555555555));
  x = (x & KU64(0x3333333333333333)) + ((x >> 2) & KU64(0x3333333333333333));
  x = (x + (x >> 4)) & KU64(0x0F0F0F0F0F0F0F0F);
  return kk_bits_byte_sum64(x);
}

#if defined(_MSC_VER) && (defined(_M_X64) || defined(_M_IX86))
#include <intrin.h>
extern bool __has_popcnt;  // initialized in runtime.c

static inline uint32_t kk_bits_count32(uint32_t x) {
  if (__has_popcnt) return __popcnt(x);
  return kk_bits_generic_count32(x);
}
#if (KK_INTPTR_SIZE >= 8)
#define HAS_BITS_COUNT64
static inline uint64_t kk_bits_count64(uint64_t x) {
  if (__has_popcnt) return __popcnt64(x);
  return kk_bits_generic_count64(x);
}
#endif

#elif defined(__GNUC__)
static inline uint32_t kk_bits_count32(uint32_t x) {
  return __builtin32(popcount)(x);
}
#if (KK_INTPTR_SIZE >= 8)
#define HAS_BITS_COUNT64
static inline uint64_t kk_bits_count64(uint64_t x) {
  return __builtin64(popcount)(x);
}
#endif

#else
static inline uint32_t kk_bits_count32(uint32_t x) {
  return kk_bits_generic_count32(x);
}

#define HAS_BITS_COUNT64
static inline uint64_t kk_bits_count64(uint64_t x) {
  return kk_bits_generic_count64(x);
}
#endif

#ifndef HAS_BITS_COUNT64
#define HAS_BITS_COUNT64
static inline uint64_t kk_bits_count64(uint64_t x) {
  return (uint64_t)(kk_bits_count32((uint32_t)x) + kk_bits_count32((uint32_t)(x>>32)));
}
#endif

static inline kk_uintx_t kk_bits_count(kk_uintx_t x) {
  return kk_bitsx(count)(x);
}


/* ---------------------------------------------------------------
  swap bytes
------------------------------------------------------------------ */

#ifdef _MSC_VER
static inline uint16_t kk_bits_bswap16(uint16_t x) {
  return _byteswap_ushort(x);  // in <stdlib.h>
}
static inline uint32_t kk_bits_bswap32(uint32_t x) {
  return _byteswap_ulong(x);
}
static inline uint64_t kk_bits_bswap64(uint64_t x) {
  return _byteswap_uint64(x);
}
#elif defined(__GNUC__)
static inline uint16_t kk_bits_bswap16(uint16_t x) {
  return __builtin_bswap16(x);
}
static inline uint32_t kk_bits_bswap32(uint32_t x) {
  return __builtin_bswap32(x);
}
static inline uint64_t kk_bits_bswap64(uint64_t x) {
  return __builtin_bswap64(x);
}
#else
static inline uint16_t kk_bits_bswap16(uint16_t x) {
  return rotl16(x,8);
}
static inline uint32_t kk_bits_bswap32(uint32_t x) {
  uint32_t hi = kk_bits_bswap16(uint16_t)x);
  uint32_t lo = kk_bits_bswap16(uint16_t)(x >> 16));
  return ((hi << 16) | lo);
}
static inline uint64_t kk_bits_bswap64(uint64_t x) {
  uint64_t hi = kk_bits_bswap32(uint32_t)x);
  uint64_t lo = kk_bits_bswap32(uint32_t)(x >> 32));
  return ((hi << 32) | lo);
}
#endif

/* ---------------------------------------------------------------
  Parity: returns `kk_bits_count(x) % 2`
  see <https://graphics.stanford.edu/~seander/bithacks.html#ParityParallel>
------------------------------------------------------------------ */

#if defined(_MSC_VER) && (defined(_M_X64) || defined(_M_IX86))
static inline bool kk_bits_count_is_even32(uint32_t x) {
  return ((kk_bits_count32(x) & 1) == 0);
}
static inline bool kk_bits_count_is_even64(uint64_t x) {
  return ((kk_bits_count64(x) & 1) == 0);
}

#elif defined(__GNUC__)
static inline bool kk_bits_count_is_even32(uint32_t x) {
  return (__builtin32(parity)(x) == 0);
}
static inline bool kk_bits_count_is_even64(uint64_t x) {
  return (__builtin64(parity)(x) == 0);
}

#else
static inline bool kk_bits_count_is_even32(uint32_t x) {
  x ^= x >> 16;
  x ^= x >> 8;
  x ^= x >> 4;
  x &= 0x0F;
  return (((0x6996 >> x) & 1) == 0);  // 0x6996 = 0b0110100110010110  == "mini" 16 bit lookup table with a bit set if the value has non-even parity
}
static inline bool kk_bits_count_is_even64(uint64_t x) {
  x ^= x >> 32;
  return kk_bits_count_is_even32((uint32_t)x);
}
#endif

static inline bool kk_bits_count_is_even(kk_uintx_t x) {
  return kk_bitsx(count_is_even)(x);
}

/* ---------------------------------------------------------------
  Digits in a decimal representation
------------------------------------------------------------------ */
uint8_t kk_bits_digits32(uint32_t x);
uint8_t kk_bits_digits64(uint64_t x);

static inline uint8_t kk_bits_digits(kk_uintx_t x) {
  return kk_bitsx(digits)(x);
}


#endif // include guard
