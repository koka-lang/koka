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
  return _rotl16(x, (uint8_t)shift & 15);  // in <intrin.h>
}
static inline uint16_t kk_bits_rotr16(uint16_t x, uint16_t shift) {
  return _rotr16(x, (uint8_t)shift & 15);  
}
static inline uint32_t kk_bits_rotl32(uint32_t x, uint32_t shift) {
  return _lrotl(x, (int)shift & 31);
}
static inline uint32_t kk_bits_rotr32(uint32_t x, uint32_t shift) {
  return _lrotr(x, (int)shift & 31);
}
static inline uint64_t kk_bits_rotl64(uint64_t x, uint64_t shift) {
  return _rotl64(x, (int)shift & 63);
}
static inline uint64_t kk_bits_rotr64(uint64_t x, uint64_t shift) {
  return _rotr64(x, (int)shift & 63);
}
#else
// most compilers translate these expressions to a direct rotation instruction
static inline uint16_t kk_bits_rotl16(uint16_t x, uint16_t shift) {
  shift &= 15;
  return (x << shift) | (x >> (16 - shift));
}
static inline uint16_t kk_bits_rotr16(uint16_t x, uint16_t shift) {
  shift &= 15;
  return (x >> shift) | (x << (16 - shift));
}
static inline uint32_t kk_bits_rotl32(uint32_t x, uint32_t shift) {
  shift &= 31;
  return (x << shift) | (x >> (32 - shift));
}
static inline uint32_t kk_bits_rotr32(uint32_t x, uint32_t shift) {
  shift &= 31;
  return (x >> shift) | (x << (32 - shift));
}
static inline uint64_t kk_bits_rotl64(uint64_t x, uint64_t shift) {
  shift &= 63;
  return (x << shift) | (x >> (64 - shift));
}
static inline uint64_t kk_bits_rotr64(uint64_t x, uint64_t shift) {
  shift &= 63;
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

#if defined(__GNUC__)
static inline uint8_t kk_bits_clz32(uint32_t x) {
  return (x==0 ? 32 : __builtin32(clz)(x));
}
static inline uint8_t kk_bits_ctz32(uint32_t x) {
  return (x==0 ? 32 : __builtin32(ctz)(x));
}
#if (KK_INTX_SIZE >= 8)
#define HAS_BITS_CLZ64
static inline uint8_t kk_bits_clz64(uint64_t x) {
  return (x==0 ? 64 : __builtin64(clz)(x));
}
static inline uint8_t kk_bits_ctz64(uint64_t x) {
  return (x==0 ? 64 : __builtin64(ctz)(x));
}
#endif

#elif defined(_MSC_VER) && !defined(__clang_msvc__) && (defined(_M_ARM64) || defined(_M_ARM) || defined(_M_X64) || defined(_M_IX86))
#include <intrin.h>

#if defined(_M_X64) || defined(_M_IX86)
extern bool kk_has_lzcnt;  // initialized in runtime.c
extern bool kk_has_tzcnt;
#endif

static inline uint8_t kk_bits_clz32(uint32_t x) {
  #if defined(_M_X64) || defined(_M_IX86)
  if kk_likely(kk_has_lzcnt) return (uint8_t)__lzcnt(x);
  #endif
  unsigned long idx;
  return (_BitScanReverse(&idx, x) ? 31 - (uint8_t)idx : 32);
}
static inline uint8_t kk_bits_ctz32(uint32_t x) {
  #if defined(_M_X64) || defined(_M_IX86)
  if kk_likely(kk_has_tzcnt) return (uint8_t)_tzcnt_u32(x);
  #endif
  unsigned long idx;
  return (_BitScanForward(&idx, x) ? (uint8_t)idx : 32);
}
#if (KK_INTX_SIZE >= 8)
#define HAS_BITS_CLZ64
static inline uint8_t kk_bits_clz64(uint64_t x) {
  #if defined(_M_X64) || defined(_M_IX86)
  if kk_likely(kk_has_lzcnt) return (uint8_t)__lzcnt64(x);
  #endif
  unsigned long idx;
  return (_BitScanReverse64(&idx, x) ? 63 - (uint8_t)idx : 64);
}
static inline uint8_t kk_bits_ctz64(uint64_t x) {
  #if defined(_M_X64) || defined(_M_IX86)
  if kk_likely(kk_has_tzcnt) return (uint8_t)_tzcnt_u64(x);
  #endif
  unsigned long idx;
  return (_BitScanForward64(&idx, x) ? (uint8_t)idx : 64);
}
#endif

#else
#define KK_BITS_USE_GENERIC_CTZ_CLZ  1
kk_decl_export uint8_t kk_bits_ctz32(uint32_t x);
kk_decl_export uint8_t kk_bits_clz32(uint32_t x);
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

#define kk_bits_one_mask32     KK_U32(0x01010101)
#define kk_bits_one_mask64     KK_U64(0x0101010101010101)
#define kk_bits_one_mask       ((~(KK_UX(0)))/0xFF)         // 0x01010101 ...

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

kk_decl_export uint32_t kk_bits_generic_count32(uint32_t x);
kk_decl_export uint64_t kk_bits_generic_count64(uint64_t x);

#if defined(_MSC_VER) && (defined(_M_X64) || defined(_M_IX86))
#include <intrin.h>
extern bool kk_has_popcnt;  // initialized in runtime.c

static inline uint32_t kk_bits_count32(uint32_t x) {
  if (kk_has_popcnt) return __popcnt(x);
  return kk_bits_generic_count32(x);
}
#if (KK_INTX_SIZE >= 8)
#define HAS_BITS_COUNT64
static inline uint64_t kk_bits_count64(uint64_t x) {
  if (kk_has_popcnt) return __popcnt64(x);
  return kk_bits_generic_count64(x);
}
#endif

#elif defined(__GNUC__)
static inline uint32_t kk_bits_count32(uint32_t x) {
  return __builtin32(popcount)(x);
}
#if (KK_INTX_SIZE >= 8)
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
  return ((uint64_t)kk_bits_count32((uint32_t)x) + kk_bits_count32((uint32_t)(x>>32)));
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

static inline kk_uintx_t kk_bits_bswap(kk_uintx_t u) {
  return kk_bitsx(bswap)(u);
}

/* ---------------------------------------------------------------
  Endian neutral
------------------------------------------------------------------ */

#if defined(KK_IS_LITTLE_ENDIAN)
#define KK_BITS_BSWAP_IF_BE(b,u) (u)
#define KK_BITS_BSWAP_IF_LE(b,u) kk_bits_bswap##b(u)
#else
#define KK_BITS_BSWAP_IF_BE(b,u) kk_bits_bswap##b(u)
#define KK_BITS_BSWAP_IF_LE(b,u) (u)
#endif 

static inline uint16_t kk_bits_bswap_to_le16(uint16_t u) {
  return KK_BITS_BSWAP_IF_BE(16,u);
}
static inline uint16_t kk_bits_bswap_to_be16(uint16_t u) {
  return KK_BITS_BSWAP_IF_LE(16, u);
}
static inline uint32_t kk_bits_bswap_to_le32(uint32_t u) {
  return KK_BITS_BSWAP_IF_BE(32, u);
}
static inline uint32_t kk_bits_bswap_to_be32(uint32_t u) {
  return KK_BITS_BSWAP_IF_LE(32, u);
}
static inline uint64_t kk_bits_bswap_to_le64(uint64_t u) {
  return KK_BITS_BSWAP_IF_BE(64, u);
}
static inline uint64_t kk_bits_bswap_to_be64(uint64_t u) {
  return KK_BITS_BSWAP_IF_LE(64, u);
}
static inline kk_uintx_t kk_bits_bswap_to_le(kk_uintx_t u) {
  return kk_bitsx(bswap_to_le)(u);
}
static inline kk_uintx_t kk_bits_bswap_to_be(kk_uintx_t u) {
  return kk_bitsx(bswap_to_be)(u);
}

static inline uint16_t kk_bits_bswap_from_le16(uint16_t u) {
  return KK_BITS_BSWAP_IF_BE(16, u);
}
static inline uint16_t kk_bits_bswap_from_be16(uint16_t u) {
  return KK_BITS_BSWAP_IF_LE(16, u);
}
static inline uint32_t kk_bits_bswap_from_le32(uint32_t u) {
  return KK_BITS_BSWAP_IF_BE(32, u);
}
static inline uint32_t kk_bits_bswap_from_be32(uint32_t u) {
  return KK_BITS_BSWAP_IF_LE(32, u);
}
static inline uint64_t kk_bits_bswap_from_le64(uint64_t u) {
  return KK_BITS_BSWAP_IF_BE(64, u);
}
static inline uint64_t kk_bits_bswap_from_be64(uint64_t u) {
  return KK_BITS_BSWAP_IF_LE(64, u);
}
static inline kk_uintx_t kk_bits_bswap_from_le(kk_uintx_t u) {
  return kk_bitsx(bswap_from_le)(u);
}
static inline kk_uintx_t kk_bits_bswap_from_be(kk_uintx_t u) {
  return kk_bitsx(bswap_from_be)(u);
}


/* ---------------------------------------------------------------
  Floats to bits
------------------------------------------------------------------ */

static inline uint32_t kk_bits_from_float(float f) {
  uint32_t x;
  memcpy(&x, &f, sizeof(x));  // safe for C aliasing: see <https://gist.github.com/shafik/848ae25ee209f698763cffee272a58f8#how-do-we-type-pun-correctly>
  return x;
}

static inline uint64_t kk_bits_from_double(double d) {
  uint64_t x;
  memcpy(&x, &d, sizeof(x));
  return x;
}

static inline float kk_bits_to_float(uint32_t x) {
  float f;
  memcpy(&f, &x, sizeof(f));
  return f;
}

static inline double kk_bits_to_double(uint64_t x) {
  double d;
  memcpy(&d, &x, sizeof(d));
  return d;
}


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
kk_decl_export uint8_t kk_bits_digits32(uint32_t x);
kk_decl_export uint8_t kk_bits_digits64(uint64_t x);

static inline uint8_t kk_bits_digits(kk_uintx_t x) {
  return kk_bitsx(digits)(x);
}


/* ---------------------------------------------------------------
  midpoint(x,y): the average of x and y, rounded towards x.
  note: written to avoid overflow and UB. See also
  <https://devblogs.microsoft.com/oldnewthing/20220207-00/?p=106223>
------------------------------------------------------------------ */

static inline int32_t kk_bits_midpoint32( int32_t x, int32_t y ) {
  if kk_likely(x <= y) return x + (int32_t)(((uint32_t)y - (uint32_t)x)/2);  
                  else return x - (int32_t)(((uint32_t)x - (uint32_t)y)/2);
}

static inline int64_t kk_bits_midpoint64(int64_t x, int64_t y) {
  if kk_likely(x <= y) return x + (int64_t)(((uint64_t)y - (uint64_t)x)/2);
                  else return x - (int64_t)(((uint64_t)x - (uint64_t)y)/2);
}

static inline kk_intx_t kk_bits_midpoint(kk_intx_t x, kk_intx_t y) {
  return kk_bitsx(midpoint)(x, y);
}

static inline uint32_t kk_bits_umidpoint32( uint32_t x, uint32_t y ) {
  if kk_likely(x <= y) return (x + (y-x)/2);
                  else return (x - (x-y)/2);
}

static inline uint64_t kk_bits_umidpoint64( uint64_t x, uint64_t y ) {
  if kk_likely(x <= y) return (x + (y-x)/2);
                  else return (x - (x-y)/2); 
}

static inline kk_uintx_t kk_bits_umidpoint( kk_uintx_t x, kk_uintx_t y ) {
  return kk_bitsx(umidpoint)(x,y);
}


#endif // include guard
