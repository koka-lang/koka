#pragma once
#ifndef KK_BITS_H
#define KK_BITS_H

/*---------------------------------------------------------------------------
  Copyright 2020-2023, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Define kk_builtin suffixes for gcc/clang
// Note: gcc has `kk_has_builtin` since version 10 so in some cases we also test for __GNUC__>=7 (as on Ubuntu 18)

#define kk_builtin(name)        __builtin_##name
#define kk_has_builtin(name)    __has_builtin(__builtin_##name)

#if (LONG_MAX == INT32_MAX)
#define kk_builtin32(name)       kk_builtin(name##l)
#define kk_has_builtin32(name)   kk_has_builtin(name##l)
#else
#define kk_builtin32(name)       kk_builtin(name)
#define kk_has_builtin32(name)   kk_has_builtin(name)
#endif
#if (LONG_MAX == INT64_MAX)
#define kk_builtin64(name)       kk_builtin(name##l)
#define kk_has_builtin64(name)   kk_has_builtin(name##l)
#else
#define kk_builtin64(name)       kk_builtin(name##ll)
#define kk_has_builtin64(name)   kk_has_builtin(name##ll)
#endif

#if (KK_INTX_SIZE==4)
#define kk_bitsx(name)  kk_bits_##name##32
#else
#define kk_bitsx(name)  kk_bits_##name##64
#endif


/* -----------------------------------------------------------
  Rotations
----------------------------------------------------------- */
#if kk_has_builtin(rotateleft64)
static inline uint16_t kk_bits_rotl16(uint16_t x, int shift) {
  return kk_builtin(rotateleft16)(x, (unsigned)shift & 15);
}
static inline uint16_t kk_bits_rotr16(uint16_t x, int shift) {
  return kk_builtin(rotateright16)(x, (unsigned)shift & 15);
}
static inline uint32_t kk_bits_rotl32(uint32_t x, int shift) {
  return kk_builtin(rotateleft32)(x, (unsigned)shift & 31);
}
static inline uint32_t kk_bits_rotr32(uint32_t x, int shift) {
  return kk_builtin(rotateright32)(x, (unsigned)shift & 31);
}
static inline uint64_t kk_bits_rotl64(uint64_t x, int shift) {
  return kk_builtin(rotateleft64)(x, (unsigned)shift & 63);
}
static inline uint64_t kk_bits_rotr64(uint64_t x, int shift) {
  return kk_builtin(rotateright64)(x, (unsigned)shift & 63);
}
#elif defined(_MSC_VER)
#include <intrin.h>
static inline uint16_t kk_bits_rotl16(uint16_t x, int shift) {
  return _rotl16(x, (uint8_t)shift & 15);  // in <intrin.h>
}
static inline uint16_t kk_bits_rotr16(uint16_t x, int shift) {
  return _rotr16(x, (uint8_t)shift & 15);
}
static inline uint32_t kk_bits_rotl32(uint32_t x, int shift) {
  return _lrotl(x, shift & 31);
}
static inline uint32_t kk_bits_rotr32(uint32_t x, int shift) {
  return _lrotr(x, shift & 31);
}
static inline uint64_t kk_bits_rotl64(uint64_t x, int shift) {
  return _rotl64(x, shift & 63);
}
static inline uint64_t kk_bits_rotr64(uint64_t x, int shift) {
  return _rotr64(x, shift & 63);
}
#else
// most compilers translate these expressions to a direct rotation instruction
// The term `((-mshift)&(N-1)` is written this way instead of `N - mshift` to
// avoid UB when `mshift==0`. See <https://blog.regehr.org/archives/1063>
#define _kk_return_rotate_left(N)  \
  const unsigned int mshift = (unsigned int)(shift) & ((N)-1); \
  return ((x) << mshift) | ((x) >> ((-mshift) & ((N)-1)))

#define _kk_return_rotate_right(N)  \
  const unsigned int mshift = (unsigned int)(shift) & ((N)-1); \
  return ((x) >> mshift) | ((x) << ((-mshift) & ((N)-1)))

static inline uint16_t kk_bits_rotl16(uint16_t x, int shift) {
  _kk_return_rotate_left(16);
}
static inline uint16_t kk_bits_rotr16(uint16_t x, int shift) {
  _kk_return_rotate_right(16);
}
static inline uint32_t kk_bits_rotl32(uint32_t x, int shift) {
  _kk_return_rotate_left(32);
}
static inline uint32_t kk_bits_rotr32(uint32_t x, int shift) {
  _kk_return_rotate_right(32);
}
static inline uint64_t kk_bits_rotl64(uint64_t x, int shift) {
  _kk_return_rotate_left(64);
}
static inline uint64_t kk_bits_rotr64(uint64_t x, int shift) {
  _kk_return_rotate_right(64);
}
#endif

static inline kk_uintx_t kk_bits_rotl(kk_uintx_t x, int shift) {
  return kk_bitsx(rotl)(x, shift);
}

static inline kk_uintx_t kk_bits_rotr(kk_uintx_t x, int shift) {
  return kk_bitsx(rotr)(x, shift);
}


/* -----------------------------------------------------------
  `clz` count leading zero bits   (32/64 for zero)
  `ctz` count trailing zero bits  (32/64 for zero)
----------------------------------------------------------- */

#if kk_has_builtin32(clz) || (__GNUC__ >= 7)
static inline int kk_bits_clz32(uint32_t x) {
  return (x==0 ? 32 : kk_builtin32(clz)(x));
}
static inline int kk_bits_ctz32(uint32_t x) {
  return (x==0 ? 32 : kk_builtin32(ctz)(x));
}
#if kk_has_builtin64(clz) || (__GNUC__ >= 7 && LONG_MAX >= INT64_MAX)
#define KK_BITS_HAS_CLZ64
static inline int kk_bits_clz64(uint64_t x) {
  return (x == 0 ? 64 : kk_builtin64(clz)(x));
}
static inline int kk_bits_ctz64(uint64_t x) {
  return (x==0 ? 64 : kk_builtin64(ctz)(x));
}
#endif

#elif defined(_MSC_VER) && (defined(_M_ARM64) || defined(_M_ARM) || defined(_M_X64) || defined(_M_IX86))
#include <intrin.h>
static inline int kk_bits_clz32(uint32_t x) {
  unsigned long idx;
  return (_BitScanReverse(&idx, x) ? 31 - (int)idx : 32);
}
static inline int kk_bits_ctz32(uint32_t x) {
  unsigned long idx;
  return (_BitScanForward(&idx, x) ? (int)idx : 32);
}
#if (KK_INTX_SIZE >= 8)
#define KK_BITS_HAS_CLZ64
static inline int kk_bits_clz64(uint64_t x) {
  unsigned long idx;
  return (_BitScanReverse64(&idx, x) ? 63 - (int)idx : 64);
}
static inline int kk_bits_ctz64(uint64_t x) {
  unsigned long idx;
  return (_BitScanForward64(&idx, x) ? (int)idx : 64);
}
#endif

#else
#warning "using generiz ctz"
#define KK_BITS_USE_GENERIC_CTZ_CLZ  1
kk_decl_export int kk_bits_ctz32(uint32_t x);
kk_decl_export int kk_bits_clz32(uint32_t x);

#endif

#ifndef KK_BITS_HAS_CLZ64
#define KK_BITS_HAS_CLZ64
static inline int kk_bits_clz64(uint64_t x) {
  int cnt = kk_bits_clz32((uint32_t)(x >> 32));
  if (cnt < 32) return cnt;
  return (32 + kk_bits_clz32((uint32_t)x));
}
static inline int kk_bits_ctz64(uint64_t x) {
  int cnt = kk_bits_ctz32((uint32_t)x);
  if (cnt < 32) return cnt;
  return (32 + kk_bits_ctz32((uint32_t)(x >> 32)));
}
#endif

static inline int kk_bits_clz(kk_uintx_t x) {
  return kk_bitsx(clz)(x);
}
static inline int kk_bits_ctz(kk_uintx_t x) {
  return kk_bitsx(ctz)(x);
}

/* -----------------------------------------------------------
 count leading redundant sign bits (i.e. the number of bits
 following the most significant bit that are identical to it).

 clrsb31(INT32_MAX) == 0
 ...
 clrsb31(1)  == 30
 clrsb32(0)  == 31
 clrsb32(-1) == 31
 clrsb32(-2) == 30
 ...
 clrsb32(INT32_MIN) = 0
----------------------------------------------------------- */

#if kk_has_builtin32(clrsb)
static inline int kk_bits_clrsb32(int32_t x) {
  return kk_builtin32(clrsb)(x);
}
#else
static inline int kk_bits_clrsb32(int32_t x) {
  return kk_bits_clz32((uint32_t)(x<0 ? ~x : x)) - 1;
}
#endif

#if kk_has_builtin64(clrsb)
static inline int kk_bits_clrsb64(int64_t x) {
  return kk_builtin64(clrsb)(x);
}
#else
static inline int kk_bits_clrsb64(int64_t x) {
  return kk_bits_clz64((uint64_t)(x<0 ? ~x : x)) - 1;
}
#endif

static inline int kk_bits_clrsb(kk_intx_t x) {
  return kk_bitsx(clrsb)(x);
}


/* -----------------------------------------------------------
 clear least-significant bit
----------------------------------------------------------- */

#define _kk_bits_clear_lsb(x)  ((x) & ((x)-1))

static inline uint32_t kk_bits_clear_lsb32(uint32_t x) {
  return _kk_bits_clear_lsb(x);
}

static inline uint64_t kk_bits_clear_lsb64(uint64_t x) {
  return _kk_bits_clear_lsb(x);
}

static inline kk_uintx_t kk_bits_clear_lsb(kk_uintx_t x) {
  return kk_bitsx(clear_lsb)(x);
}


/* -----------------------------------------------------------
 keep (only) least-significant bit
----------------------------------------------------------- */

#define _kk_bits_only_keep_lsb(x)  ((x) & (~(x)+1))

static inline uint32_t kk_bits_only_keep_lsb32(uint32_t x) {
  return _kk_bits_only_keep_lsb(x);
}

static inline uint64_t kk_bits_only_keep_lsb64(uint64_t x) {
  return _kk_bits_only_keep_lsb(x);
}

static inline kk_uintx_t kk_bits_only_keep_lsb(kk_uintx_t x) {
  return kk_bitsx(only_keep_lsb)(x);
}

/* -----------------------------------------------------------
  Is a word a power of two? (0 is not a power of two)
----------------------------------------------------------- */

static inline bool kk_bits_is_power_of2_32(uint32_t x) {
  return (x != 0 && kk_bits_clear_lsb32(x) == 0);
}

static inline bool kk_bits_is_power_of2_64(uint64_t x) {
  return (x != 0 && kk_bits_clear_lsb64(x) == 0);
}

static inline bool kk_bits_is_power_of2(kk_uintx_t x) {
  return kk_bitsx(is_power_of2_)(x);
}


/* -----------------------------------------------------------
  Byte operations

  - or-combine: for every byte `b` in a word set it to 0xFF if any bit was set in `b`, or otherwise set it to 0
  - Is there a zero byte in a word? see: <https://graphics.stanford.edu/~seander/bithacks.html#ZeroInWord>
  - Is any byte equal to n?
  - Sum of bytes
----------------------------------------------------------- */

#define kk_mask_bytes_lo_bit32     KK_U32(0x01010101)
#define kk_mask_bytes_lo_bit64     KK_U64(0x0101010101010101)
#define kk_mask_bytes_lo_bit       ((~(KK_UX(0)))/0xFF)         // 0x01010101 ...

#define kk_mask_bytes_hi_bit32    (kk_mask_bytes_lo_bit32<<7)      // 0x80808080
#define kk_mask_bytes_hi_bit64    (kk_mask_bytes_lo_bit64<<7)      // 0x8080808080808080
#define kk_mask_bytes_hi_bit      (kk_mask_bytes_lo_bit<<7)        // 0x80808080 ...

// todo: use orc intrinsic on riscV
#define KK_BITS_USE_GENERIC_ORC  1
kk_decl_export uint32_t kk_bits_orc32(uint32_t x);
kk_decl_export uint64_t kk_bits_orc64(uint64_t x);

static inline kk_uintx_t kk_bits_orc(kk_uintx_t x) {
  return kk_bitsx(orc)(x);
}


#if KK_BITS_HAS_FAST_ORC
static inline bool kk_bits_has_zero_byte32(uint32_t x) {
  return (~kk_bits_orc32(x) != 0);
}

static inline bool kk_bits_has_zero_byte64(uint64_t x) {
  return (~kk_bits_orc64(x) != 0);
}
#else
static inline bool kk_bits_has_zero_byte32(uint32_t x) {
  return ((x - kk_mask_bytes_lo_bit32) &     // high bit set if byte == 0 or > 0x80
          (~x & kk_mask_bytes_hi_bit32));   // high bit set if byte >= 0x80
}

static inline bool kk_bits_has_zero_byte64(uint64_t x) {
  return ((x - kk_mask_bytes_lo_bit64) & (~x & kk_mask_bytes_hi_bit64));
}
#endif

// Does `x` contain a zero byte?
static inline bool kk_bits_has_zero_byte(kk_uintx_t x) {
  return kk_bitsx(has_zero_byte)(x);
}

kk_decl_export bool kk_bits_has_byte32(uint32_t x, uint8_t n);
kk_decl_export bool kk_bits_has_byte64(uint64_t x, uint8_t n);
static inline bool kk_bits_has_byte(kk_uintx_t x, uint8_t n) {
  return kk_bitsx(has_byte)(x,n);
}


// sum of all the bytes in `x` if it is guaranteed that the sum < 256!
kk_decl_export uint8_t kk_bits_byte_sum32(uint32_t x);
kk_decl_export uint8_t kk_bits_byte_sum64(uint64_t x);
static inline uint8_t kk_bits_byte_sum(kk_uintx_t x) {
  return kk_bitsx(byte_sum)(x);
}


/* ---------------------------------------------------------------
  kk_bits_popcount: population count / hamming weight  (count set bits)
  see <https://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel>
------------------------------------------------------------------ */

#if kk_has_builtin32(popcount) || (__GNUC__ >= 7)
static inline int kk_bits_popcount32(uint32_t x) {
  return kk_builtin32(popcount)(x);
}
#if kk_has_builtin64(popcount) || (__GNUC__ >= 7 && LONG_MAX >= INT64_MAX)
static inline int kk_bits_popcount64(uint64_t x) {
  return kk_builtin64(popcount)(x);
}
#else
static inline int kk_bits_popcount64(uint64_t x) {
  return (kk_bits_popcount32((uint32_t)x) + kk_bits_popcount32((uint32_t)(x>>32)));
}
#endif

#else
#define KK_BITS_USE_GENERIC_POPCOUNT
kk_decl_export int kk_bits_generic_popcount32(uint32_t x);
kk_decl_export int kk_bits_generic_popcount64(uint64_t x);

#if defined(_MSC_VER) && (defined(_M_X64) || defined(_M_IX86))
#include <intrin.h>
extern bool kk_has_popcnt;  // initialized in runtime.c

static inline int kk_bits_popcount32(uint32_t x) {
  if kk_likely(kk_has_popcnt) return (int)__popcnt(x);
                         else return kk_bits_generic_popcount32(x);
}

static inline int kk_bits_popcount64(uint64_t x) {
  #if (KK_INTX_SIZE >= 8)
  if kk_likely(kk_has_popcnt) return (int)__popcnt64(x); 
  #endif
  return kk_bits_generic_popcount64(x);
}

#else

static inline int kk_bits_popcount32(uint32_t x) {
  return kk_bits_generic_popcount32(x);
}

static inline int kk_bits_popcount64(uint64_t x) {
  return kk_bits_generic_popcount64(x);
}
#endif

#endif

static inline int kk_bits_popcount(kk_uintx_t x) {
  return kk_bitsx(popcount)(x);
}


/* ---------------------------------------------------------------
  Parity: returns `true` if `kk_bits_popcount(x)` is even.
  see <https://graphics.stanford.edu/~seander/bithacks.html#ParityParallel>
------------------------------------------------------------------ */

#if kk_has_builtin32(parity) || (__GNUC__ >= 7)
static inline bool kk_bits_parity32(uint32_t x) {
  return (kk_builtin32(parity)(x) == 0);
}
#if kk_has_builtin64(parity) || (__GNUC__ >= 7 && LONG_MAX >= INT64_MAX)
#define KK_BITS_HAS_PARITY64
static inline bool kk_bits_parity64(uint64_t x) {
  return (kk_builtin64(parity)(x) == 0);
}
#endif

#elif defined(_MSC_VER) && (defined(_M_X64) || defined(_M_IX86))
static inline bool kk_bits_parity32(uint32_t x) {
  return ((kk_bits_popcount32(x) & 1) == 0);
}
#if (KK_INTX_SIZE>=8)
#define KK_BITS_HAS_PARITY64
static inline bool kk_bits_parity64(uint64_t x) {
  return ((kk_bits_popcount64(x) & 1) == 0);
}
#endif

#else
static inline bool kk_bits_parity32(uint32_t x) {
  x ^= x >> 16;
  x ^= x >> 8;
  x ^= x >> 4;
  x &= 0x0F;
  return (((0x6996 >> x) & 1) == 0);  // 0x6996 = 0b0110100110010110  == "mini" 16 bit lookup table with a bit set if the value has non-even parity
}
#endif

#ifndef KK_BITS_HAS_PARITY64
#define KK_BITS_HAS_PARITY64
static inline bool kk_bits_parity64(uint64_t x) {
  x ^= (x >> 32);
  return kk_bits_parity32((uint32_t)x);
}
#endif

static inline bool kk_bits_parity(kk_uintx_t x) {
  return kk_bitsx(parity)(x);
}


/* ---------------------------------------------------------------
  swap bytes
------------------------------------------------------------------ */

#if kk_has_builtin(bswap32) || (__GNUC__ >= 7)
static inline uint16_t kk_bits_bswap16(uint16_t x) {
  return kk_builtin_bswap16(x);
}
static inline uint32_t kk_bits_bswap32(uint32_t x) {
  return kk_builtin_bswap32(x);
}

#if kk_has_builtin(bswap64) || (__GNUC__ >= 7 && LONG_MAX >= INT64_MAX)
#define KK_BITS_HAS_BSWAP64
static inline uint64_t kk_bits_bswap64(uint64_t x) {
  return kk_builtin_bswap64(x);
}
#endif

#elif defined(_MSC_VER)
static inline uint16_t kk_bits_bswap16(uint16_t x) {
  return _byteswap_ushort(x);  // in <stdlib.h>
}
static inline uint32_t kk_bits_bswap32(uint32_t x) {
  return _byteswap_ulong(x);
}
#if (KK_INTX_SIZE>=8)
#define KK_BITS_HAS_BSWAP64
static inline uint64_t kk_bits_bswap64(uint64_t x) {
  return _byteswap_uint64(x);
}
#endif

#else
static inline uint16_t kk_bits_bswap16(uint16_t x) {
  return kk_bits_rotl16(x,8);
}
static inline uint32_t kk_bits_bswap32(uint32_t x) {
  uint32_t hi = kk_bits_bswap16((uint16_t)x);
  uint32_t lo = kk_bits_bswap16((uint16_t)(x >> 16));
  return ((hi << 16) | lo);
}
#endif

#ifndef KK_BITS_HAS_BSWAP64
#define KK_BITS_HAS_BSWAP64
static inline uint64_t kk_bits_bswap64(uint64_t x) {
  uint64_t hi = kk_bits_bswap32((uint32_t)x);
  uint64_t lo = kk_bits_bswap32((uint32_t)(x >> 32));
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
  Bit reverse
------------------------------------------------------------------ */

#if kk_has_builtin(bitreverse32)
static inline uint32_t kk_bits_reverse32(uint32_t x) {
  return kk_builtin_bitreverse32(x);
}
#if kk_has_builtin(bitreverse64)
static inline uint64_t kk_bits_reverse64(uint64_t x) {
  return kk_builtin_bitreverse64(x);
}
#else
static inline uint64_t kk_bits_reverse64(uint64_t x) {
  uint64_t hi = kk_bits_reverse32((uint32_t)x);
  uint64_t lo = kk_bits_reverse32((uint32_t)(x >> 32));
  return ((hi << 32) | lo);
}
#endif

#else

#define KK_BITS_USE_GENERIC_REVERSE
kk_decl_export uint32_t kk_bits_reverse32(uint32_t x);
kk_decl_export uint64_t kk_bits_reverse64(uint64_t x);

#endif



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

static inline int64_t kk_int64_hi_lo( int32_t hi, int32_t lo ) {
  return (((int64_t)hi << 32) | (uint32_t)lo);
}


/* ---------------------------------------------------------------
  LOG2
------------------------------------------------------------------ */
#define kk_bits_log2_floorN(N) (x<1 ? 0 : N - kk_bits_clz##N(x) - 1)

static inline uint32_t kk_bits_log2_floor32( uint32_t x ) {
  return kk_bits_log2_floorN(32);
}

static inline uint64_t kk_bits_log2_floor64( uint64_t x ) {
  return kk_bits_log2_floorN(64);
}

#define kk_bits_log2_ceilN(N) (log2_floor(x) + (kk_bits_is_power_of2_##N(x) ? 0 : 1))

static inline uint32_t kk_bits_log2_ceil32( uint32_t x ) {
  return kk_bits_log2_ceilN(32);
}

static inline uint64_t kk_bits_log2_ceil64( uint64_t x ) {
  return kk_bits_log2_ceilN(64);
}


/* ---------------------------------------------------------------
  Digits in a decimal representation
------------------------------------------------------------------ */
kk_decl_export int kk_bits_digits32(uint32_t x);
kk_decl_export int kk_bits_digits64(uint64_t x);

static inline int kk_bits_digits(kk_uintx_t x) {
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


/* ---------------------------------------------------------------
  Wide multiplies
------------------------------------------------------------------ */

static inline uint32_t kk_umul32_wide(uint32_t x, uint32_t y, uint32_t* hi) {
  const uint64_t r = (uint64_t)x * y;
  *hi = (uint32_t)(r >> 32);
  return (uint32_t)(r);
}

static inline uint32_t kk_imul32_wide(int32_t x, int32_t y, int32_t* hi) {
  const int64_t r = (int64_t)x * y;
  *hi = (int32_t)(r >> 32);
  return (uint32_t)(r);
}


#if defined(__GNUC__) && defined(__SIZEOF_INT128__)

__extension__ typedef unsigned __int128 kk_uint128_t;
__extension__ typedef __int128 kk_int128_t;

static inline uint64_t kk_umul64_wide(uint64_t x, uint64_t y, uint64_t* hi) {
  kk_uint128_t r = (kk_uint128_t)x * y;
  *hi = (uint64_t)(r >> 64);
  return (uint64_t)(r);
}

static inline uint64_t kk_imul64_wide(int64_t x, int64_t y, int64_t* hi) {
  kk_int128_t r = (kk_int128_t)x * y;
  *hi = (int64_t)(r >> 64);
  return (uint64_t)(r);
}

#elif defined(_MSC_VER) && (_MSC_VER >= 1920) && defined(_M_X64)

#include <intrin.h>
static inline uint64_t kk_umul64_wide(uint64_t x, uint64_t y, uint64_t* hi) {
  return _umul128(x, y, hi);
}

static inline uint64_t kk_imul64_wide(int64_t x, int64_t y, int64_t* hi) {
  return (uint64_t)_mul128(x, y, hi);
}

#elif defined(_MSC_VER) && defined(KK_ARCH_ARM64)

#include <intrin.h>
static inline uint64_t kk_umul64_wide(uint64_t x, uint64_t y, uint64_t* hi) {
  uint64_t lo = x * y;
  *hi = __umulh(x, y);
  return lo;
}

static inline uint64_t kk_imul64_wide(int64_t x, int64_t y, int64_t* hi) {
  uint64_t lo = (uint64_t)(x * y);
  *hi = __mulh(x, y);
  return lo;
}

#else

#define KK_USE_GENERIC_MUL64_WIDE
kk_decl_export uint64_t kk_umul64_wide(uint64_t x, uint64_t y, uint64_t* hi);
kk_decl_export uint64_t kk_imul64_wide(int64_t x, int64_t y, int64_t* hi);

#endif


/* ---------------------------------------------------------------
  Parallel bit extract and deposit
------------------------------------------------------------------ */

#if defined(KK_ARCH_X64) && defined(__BMI2__) && ((defined(_MSC_VER) && !defined(__clang_msvc__)) || defined(__GNUC__))
#define KK_BITS_HAS_FAST_SCATTER_GATHER  1
#include <immintrin.h>
#if defined(__clang_msvc__)
#include <bmi2intrin.h>
#endif

static inline uint32_t kk_bits_gather32(uint32_t x, uint32_t mask) {
  return _pext_u32(x, mask);
}

static inline uint64_t kk_bits_gather64(uint64_t x, uint64_t mask) {
  return _pext_u64(x, mask);
}

static inline uint32_t kk_bits_scatter32(uint32_t x, uint32_t mask) {
  return _pdep_u32(x, mask);
}

static inline uint64_t kk_bits_scatter64(uint64_t x, uint64_t mask) {
  return _pdep_u64(x, mask);
}

#elif defined(KK_ARCH_ARM64) && defined(__ARM_FEATURE_SVE2)
#define KK_BITS_HAS_FAST_SCATTER_GATHER  1
#include <arm_sve.h>

static inline uint32_t kk_bits_gather32(uint32_t x, uint32_t mask) {
  return (uint32_t)svorv_u32(svptrue_b32(), svbext_u32(svdup_u32(x), svdup_u32(mask)));
}

static inline uint64_t kk_bits_gather64(uint64_t x, uint64_t mask) {
  return (uint64_t)svorv_u64(svptrue_b64(), svbext_u64(svdup_u64(x), svdup_u64(mask)));
}

static inline uint32_t kk_bits_scatter32(uint32_t x, uint32_t mask) {
  return (uint32_t)svorv_u32(svptrue_b32(), svbdep_u32(svdup_u32(x), svdup_u32(mask)));
}

static inline uint64_t kk_bits_scatter64(uint64_t x, uint64_t mask) {
  return (uint64_t)svorv_u64(svptrue_b64(), svbdep_u64(svdup_u64(x), svdup_u64(mask)));
}

#else

#define KK_BITS_USE_GENERIC_SCATTER_GATHER
kk_decl_export uint32_t kk_bits_gather32(uint32_t x, uint32_t mask);
kk_decl_export uint64_t kk_bits_gather64(uint64_t x, uint64_t mask);
kk_decl_export uint32_t kk_bits_scatter32(uint32_t x, uint32_t mask);
kk_decl_export uint64_t kk_bits_scatter64(uint64_t x, uint64_t mask);

#endif

static inline kk_uintx_t kk_bits_gather(kk_uintx_t x, kk_uintx_t mask) {
  return kk_bitsx(gather)(x,mask);
}

static inline kk_uintx_t kk_bits_scatter(kk_uintx_t x, kk_uintx_t mask) {
  return kk_bitsx(scatter)(x, mask);
}

/* ---------------------------------------------------------------
  carry-less multiply
------------------------------------------------------------------ */

#if KK_ARCH_X64 && defined(__BMI2__)
#define KK_BITS_HAS_FAST_CLMUL  1
#include <immintrin.h>

static inline uint64_t kk_clmul64(uint64_t x, uint64_t y) {
  const __m128i res = _mm_clmulepi64_si128(_mm_set_epi64x(0, (int64_t)x), _mm_set_epi64x(0, (int64_t)y), 0);
  return _mm_extract_epi64(res, 0); 
}

static inline uint64_t kk_clmul64_wide(uint64_t x, uint64_t y, uint64_t* hi) {
  const __m128i res = _mm_clmulepi64_si128(_mm_set_epi64x(0, (int64_t)x), _mm_set_epi64x(0, (int64_t)y), 0);
  *hi = _mm_extract_epi64(res, 1);
  return _mm_extract_epi64(res, 0); 
}

static inline uint32_t kk_clmul32(uint32_t x, uint32_t y) {
  return (uint32_t)kk_clmul64(x,y);
}

static inline uint32_t kk_clmul32_wide(uint32_t x, uint32_t y, uint32_t* hi) {
  uint64_t z = kk_clmul64(x,y);
  *hi = (uint32_t)(z>>32);
  return (uint32_t)z;
}

#elif KK_ARCH_ARM64 && defined(__ARM_NEON) // (defined(__ARM_FEATURE_SME) || defined(__ARM_FEATURE_SVE))
#define KK_BITS_HAS_FAST_CLMUL  1
#include <arm_neon.h>
#include <arm_acle.h>

static inline uint64_t kk_clmul64(uint64_t x, uint64_t y) {
  const uint64x2_t res = vreinterpretq_u64_p128(vmull_p64((poly64_t)x,(poly64_t)y));
  return (uint64_t)vget_low_u64(res);
}

static inline uint64_t kk_clmul64_wide(uint64_t x, uint64_t y, uint64_t* hi) {
  const uint64x2_t res = vreinterpretq_u64_p128(vmull_p64((poly64_t)x,(poly64_t)y));
  *hi = (uint64_t)vget_high_u64(res);
  return (uint64_t)vget_low_u64(res);
}

static inline uint32_t kk_clmul32(uint32_t x, uint32_t y) {
  return (uint32_t)kk_clmul64(x,y);
}

static inline uint32_t kk_clmul32_wide(uint32_t x, uint32_t y, uint32_t* hi) {
  uint64_t z = kk_clmul64(x,y);
  *hi = (uint32_t)(z>>32);
  return (uint32_t)z;
}

#else

#define KK_BITS_USE_GENERIC_CLMUL  1

uint32_t kk_clmul32(uint32_t x, uint32_t y);
uint64_t kk_clmul64(uint64_t x, uint64_t y);
uint32_t kk_clmul32_wide(uint32_t x, uint32_t y, uint32_t* hi);
uint64_t kk_clmul64_wide(uint64_t x, uint64_t y, uint64_t* hi);

#endif


/* ---------------------------------------------------------------
  Byte and nibble permutation
  todo: use fast riscV intrinsics
------------------------------------------------------------------ */

// todo: use riscV xperm intrinsics
kk_decl_export uint32_t kk_bits_xperm32(uint32_t x, uint32_t indices);
kk_decl_export uint32_t kk_bits_xpermn32(uint32_t x, uint32_t indices);
kk_decl_export uint64_t kk_bits_xperm64(uint64_t x, uint64_t indices);
kk_decl_export uint64_t kk_bits_xpermn64(uint64_t x, uint64_t indices);

static inline kk_uintx_t kk_bits_xperm(kk_uintx_t x, kk_uintx_t indices) {
  return kk_bitsx(xperm)(x,indices);
}

static inline kk_uintx_t kk_bits_xpermn(kk_uintx_t x, kk_uintx_t indices) {
  return kk_bitsx(xpermn)(x,indices);
}


/* ---------------------------------------------------------------
  Bit interleaving: zip and unzip
  // todo: use riscV zip/unzip intrinsics
------------------------------------------------------------------ */
#define kk_mask_odd_bits32  (KK_U32(0x55555555))
#define kk_mask_odd_bits64  (KK_U64(0x5555555555555555))
#define kk_mask_even_bits32 (kk_mask_odd_bits32 << 1)
#define kk_mask_even_bits64 (kk_mask_odd_bits64 << 1)

#if KK_BITS_HAS_FAST_SCATTER_GATHER
// interleave the hi 16-bits and the lo 16-bits of the argument `x` into a
// single 32-bit result where hi is spread over the even bits, and lo over the odd bits.
static inline uint32_t kk_bits_interleave32(uint32_t x) {
  return (kk_bits_scatter32(x>>16,kk_mask_even_bits32) | kk_bits_scatter32(x&0xFFFF,kk_mask_odd_bits32));
}

// de-interleave the bits of the argument `x` where the even bits become the
// hi 16-bits and the odd bits the lo 16-bits of the result
static inline uint32_t kk_bits_deinterleave32(uint32_t x) {
  return ((kk_bits_gather32(x, kk_mask_even_bits32) << 16) | kk_bits_gather32(x, kk_mask_odd_bits32));
}

static inline uint64_t kk_bits_interleave64(uint64_t x) {
  return (kk_bits_scatter64(x>>32, kk_mask_even_bits64) | kk_bits_scatter64(x&KK_U64(0xFFFFFFFF), kk_mask_odd_bits64));
}

static inline uint64_t kk_bits_deinterleave64(uint64_t x) {
  return ((kk_bits_gather64(x, kk_mask_even_bits64) << 32) | kk_bits_gather64(x, kk_mask_odd_bits64));
}

#else

#define KK_BITS_USE_GENERIC_INTERLEAVE
kk_decl_export uint32_t kk_bits_interleave32(uint32_t x);
kk_decl_export uint32_t kk_bits_deinterleave32(uint32_t x);
kk_decl_export uint64_t kk_bits_interleave64(uint64_t x);
kk_decl_export uint64_t kk_bits_deinterleave64(uint64_t x);

#endif

static inline kk_uintx_t kk_bits_interleave(kk_uintx_t x) {
  return kk_bitsx(interleave)(x);
}

static inline kk_uintx_t kk_bits_deinterleave(kk_uintx_t x) {
  return kk_bitsx(deinterleave)(x);
}


#endif // include guard
