/*---------------------------------------------------------------------------
  Copyright 2020-2023, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
#include "kklib.h"

/* ----------------------------------------------------------
  kk_bits_digits
-------------------------------------------------------------*/

static const kk_uintx_t powers_of_10[] = { 
    1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000
#if (KK_INTX_SIZE > 4)
  , KK_UX(10000000000), KK_UX(100000000000), KK_UX(1000000000000), KK_UX(10000000000000), KK_UX(100000000000000)
  , KK_UX(1000000000000000), KK_UX(10000000000000000), KK_UX(100000000000000000), KK_UX(1000000000000000000)
  , KK_UX(10000000000000000000), KK_UX(10000000000000000000)
#endif
};

int kk_bits_digits32(uint32_t u) {
  static const int8_t guess[33] = {
    1, 0, 0, 0, 1, 1, 1, 2, 2, 2,
    3, 3, 3, 3, 4, 4, 4, 5, 5, 5,
    6, 6, 6, 6, 7, 7, 7, 8, 8, 8,
    9, 9, 9
  };
  const int count = guess[32 - kk_bits_clz32(u)]; // = 1 + (KU32(9)*(31 - kk_bits_clz32(u)) >> 5); 
  return (count + (u >= powers_of_10[count] ? 1 : 0));
}

int kk_bits_digits64(uint64_t u) {
  static const int8_t guess[65] = {
    1, 0, 0, 0, 1, 1, 1, 2, 2, 2,
    3, 3, 3, 3, 4, 4, 4, 5, 5, 5,
    6, 6, 6, 6, 7, 7, 7, 8, 8, 8,
    9, 9, 9, 9,10,10,10,11,11,11,
   12,12,12,12,13,13,13,14,14,14,
   15,15,15,15,16,16,16,17,17,17,
   18,18,18,18,19
  };  
  const int count = guess[64 - kk_bits_clz64(u)];  // = 1 + (KU64(1233)*(63 - kk_bits_clz64(u)) >> 12);
  return (count + (u >= powers_of_10[count] ? 1 : 0));
}

/* ----------------------------------------------------------
  generic ctz, clz
-------------------------------------------------------------*/

#if defined(KK_BITS_USE_GENERIC_CTZ_CLZ)

int kk_bits_ctz32(uint32_t x) {
  // de Bruijn multiplication, see <http://supertech.csail.mit.edu/papers/debruijn.pdf>
  static const int8_t debruijn[32] = {
    0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
    31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
  };
  if (x == 0) return 32;
  x = kk_bits_only_keep_lsb32(x);
  return debruijn[(uint32_t)(x * KK_U32(0x077CB531)) >> 27];
}

int kk_bits_clz32(uint32_t x) {  
  // de Bruijn multiplication, see <http://supertech.csail.mit.edu/papers/debruijn.pdf>
  static const int8_t debruijn[32] = {
    31, 22, 30, 21, 18, 10, 29, 2, 20, 17, 15, 13, 9, 6, 28, 1,
    23, 19, 11, 3, 16, 14, 7, 24, 12, 4, 8, 25, 5, 26, 27, 0
  };
  if (x==0) return 32;
  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  return debruijn[(uint32_t)(x * KK_U32(0x07C4ACDD)) >> 27];
}

#endif


/* ----------------------------------------------------------
  or-combine
-------------------------------------------------------------*/
#define kk_mask_odd_pairs32     KK_U32(0x33333333)
#define kk_mask_odd_nibbles32   KK_U32(0x0F0F0F0F)
#define kk_mask_odd_pairs64     KK_U64(0x3333333333333333)
#define kk_mask_odd_nibbles64   KK_U64(0x0F0F0F0F0F0F0F0F)

#if defined(KK_BITS_USE_GENERIC_ORC)
uint32_t kk_bits_orc32(uint32_t x) {
  // set high bit in each byte to `or` of the bits in the byte
  x |= ((x & kk_mask_odd_nibbles32) << 4);
  x |= ((x & kk_mask_odd_pairs32)   << 2);
  x |= ((x & kk_mask_odd_bits32)    << 1);  
  // distribute the high bit back
  x &= kk_mask_bytes_hi_bit32;
  x |= (x >> 1);
  x |= (x >> 2); 
  x |= (x >> 4);
  return x;
}

uint64_t kk_bits_orc64(uint64_t x) {
  // set high bit in each byte to `or` of the bits in the byte
  x |= ((x & kk_mask_odd_nibbles64) << 4);
  x |= ((x & kk_mask_odd_pairs64)   << 2);
  x |= ((x & kk_mask_odd_bits64)    << 1);
  // distribute the high bit back
  x &= kk_mask_bytes_hi_bit64;
  x |= (x >> 1);
  x |= (x >> 2);
  x |= (x >> 4);
  return x;
  return x;
}
#endif

/* ----------------------------------------------------------
  generic popcount
-------------------------------------------------------------*/

#if defined(KK_BITS_USE_GENERIC_POPCOUNT)

int kk_bits_generic_popcount32(uint32_t x) {
  x = x - ((x >> 1) & kk_mask_odd_bits32);
  x = (x & kk_mask_odd_pairs32) + ((x >> 2) & kk_mask_odd_pairs32);
  x = (x + (x >> 4)) & kk_mask_odd_nibbles32;
  return kk_bits_byte_sum32(x);
}

int kk_bits_generic_popcount64(uint64_t x) {
  x = x - ((x >> 1) & kk_mask_odd_bits64);
  x = (x & kk_mask_odd_pairs64) + ((x >> 2) & kk_mask_odd_pairs64);
  x = (x + (x >> 4)) & kk_mask_odd_nibbles64;
  return kk_bits_byte_sum64(x);
}

#endif


/* ----------------------------------------------------------
  wide multiplies
-------------------------------------------------------------*/

#if defined(KK_USE_GENERIC_WIDE_UMUL64)

/* multiply to 64-bit integers `x` and `y` using 32x32 to 64-bit multiplications:

                    yhi : ylo
                    xhi : xlo  *
        ---------------------
                    ahi : alo   // xlo * ylo
              bhi : blo         // xlo * yhi
              chi : clo         // xhi * ylo
        dhi : dlo               // xhi * xhi
             carry              // (ahi + blo + clo) >> 32
        ---------------------+
           hi     :    lo
*/

#define kk_split32(x)   const uint64_t x##lo = (uint32_t)(x); const uint64_t x##hi = (x)>>32;

uint64_t kk_wide_umul64(uint64_t x, uint64_t y, uint64_t* hi) {
  kk_split32(x); kk_split32(y);
  uint64_t a = xlo * ylo;
  uint64_t b = xhi * ylo;
  uint64_t c = xlo * yhi;
  uint64_t d = xhi * yhi;

  kk_split32(a); kk_split32(b); kk_split32(c); kk_unused(alo);
  uint64_t carry = (ahi + blo + clo) >> 32;
  uint64_t lo = a + (blo << 32) + (clo << 32);
  *hi = carry + bhi + chi + d;
  return lo;
}

#endif

#ifdef KK_BITS_USE_GENERIC_REVERSE

uint32_t kk_bits_reverse32(uint32_t x) {
  // from: http://graphics.stanford.edu/~seander/bithacks.html#ReverseParallel
  x = ((x >> 1) & kk_mask_odd_bits32) | ((x & kk_mask_odd_bits32) << 1); // swap odd and even bits
  x = ((x >> 2) & kk_mask_odd_pairs32) | ((x & kk_mask_odd_pairs32) << 2); // swap 2-bit pairs
  x = ((x >> 4) & kk_mask_odd_nibbles32) | ((x & kk_mask_odd_nibbles32) << 4); // swap 4-bit nibbles 
  return kk_bits_bswap32(x);
}

uint64_t kk_bits_reverse64(uint64_t x) {
  x = ((x >> 1) & kk_mask_odd_bits64) | ((x & kk_mask_odd_bits64) << 1); // swap odd and even bits
  x = ((x >> 2) & kk_mask_odd_pairs64) | ((x & kk_mask_odd_pairs64) << 2); // swap 2-bit pairs
  x = ((x >> 4) & kk_mask_odd_nibbles64) | ((x & kk_mask_odd_nibbles64) << 4); // swap 4-bit nibbles 
  return kk_bits_bswap64(x);
}

#endif

/* ----------------------------------------------------------
  generic parallel bit extract / deposit
-------------------------------------------------------------*/

#ifdef KK_BITS_USE_GENERIC_SCATTER_GATHER

static uint32_t kk_bits_scatter32_loop(uint32_t x, uint32_t mask) {
  uint32_t y = 0;
  while (mask != 0) {
    int shift = kk_bits_ctz32(mask);   // find lsb
    mask = kk_bits_clear_lsb32(mask);  // clear lsb
    y |= ((x & 1) << shift);
    x >>= 1;
  }
  return y;
}


uint32_t kk_bits_scatter32(uint32_t x, uint32_t mask) {
  switch (kk_bits_popcount32(mask)) {
  case 0: return 0;
  case 1: return ((x & 1) != 0 ? mask : 0);
  case 2: {
    uint32_t lsb = ((x & 1) != 0 ? kk_bits_only_keep_lsb32(mask) : 0);
    uint32_t msb = ((x & 2) != 0 ? kk_bits_clear_lsb32(mask) : 0);
    return (msb | lsb);
  }
  }
  return kk_bits_scatter32_loop(x, mask);
}

static uint32_t kk_bits_gather32_loop(uint32_t x, uint32_t mask) {
  uint32_t y = 0;
  while (mask != 0) {
    int shift = 31 - kk_bits_clz32(mask);  // find msb
    mask ^= (KK_U32(1) << shift);          // clear msb
    y = (y << 1) | ((x >> shift) & 1);
  }
  return y;
}

uint32_t kk_bits_gather32(uint32_t x, uint32_t mask) {
  switch (kk_bits_popcount32(mask)) {
  case 0: return 0;
  case 1: return ((x & mask) != 0 ? 1 : 0);
  case 2: {
    uint32_t lsb = ((x & kk_bits_only_keep_lsb32(mask)) != 0 ? 1 : 0);
    uint32_t msb = ((x & kk_bits_clear_lsb32(mask)) != 0 ? 2 : 0);
    return (msb | lsb);
  }
  }
  return kk_bits_gather32_loop(x, mask);
}


static uint64_t kk_bits_scatter64_loop(uint64_t x, uint64_t mask) {
  uint64_t y = 0;
  while (mask != 0) {
    int shift = kk_bits_ctz64(mask);   // find lsb
    mask = kk_bits_clear_lsb64(mask);  // clear lsb
    y |= ((x & 1) << shift);
    x >>= 1;
  }
  return y;
}

uint64_t kk_bits_scatter64(uint64_t x, uint64_t mask) {
  switch (kk_bits_popcount64(mask)) {
  case 0: return 0;
  case 1: return ((x & 1) != 0 ? mask : 0);
  case 2: {
    uint64_t lsb = ((x & 1) != 0 ? kk_bits_only_keep_lsb64(mask) : 0);
    uint64_t msb = ((x & 2) != 0 ? kk_bits_clear_lsb64(mask) : 0);
    return (msb | lsb);
  }
  }
  return kk_bits_scatter64_loop(x, mask);
}

static uint64_t kk_bits_gather64_loop(uint64_t x, uint64_t mask) {
  uint64_t y = 0;
  while (mask != 0) {
    int shift = 63 - kk_bits_clz64(mask);  // find msb
    mask ^= (KK_U64(1) << shift);          // clear msb
    y = (y << 1) | ((x >> shift) & 1);
  }
  return y;
}

uint64_t kk_bits_gather64(uint64_t x, uint64_t mask) {
  switch (kk_bits_popcount64(mask)) {
  case 0: return 0;
  case 1: return ((x & mask) != 0 ? 1 : 0);
  case 2: {
    uint64_t lsb = ((x & kk_bits_only_keep_lsb64(mask)) != 0 ? 1 : 0);
    uint64_t msb = ((x & kk_bits_clear_lsb64(mask)) != 0 ? 2 : 0);
    return (msb | lsb);
  }
  }
  return kk_bits_gather64_loop(x, mask);
}

#endif

/* ----------------------------------------------------------
  zip/unzip
-------------------------------------------------------------*/

#ifdef KK_BITS_USE_GENERIC_INTERLEAVE

// scatter bits to odd positions
static inline uint32_t kk_bits_scatter_odd32( uint32_t x ) {
  x = (x ^ (x << 8 )) & KK_U32(0x00FF00FF);
  x = (x ^ (x << 4 )) & kk_mask_odd_nibbles32;
  x = (x ^ (x << 2 )) & kk_mask_odd_pairs32;
  x = (x ^ (x << 1 )) & kk_mask_odd_bits32;
  return x;
}

static inline uint64_t kk_bits_scatter_odd64( uint64_t x ) {
  x = (x ^ (x << 16)) & KK_U64(0x0000FFFF0000FFFF);
  x = (x ^ (x << 8 )) & KK_U64(0x00FF00FF00FF00FF);
  x = (x ^ (x << 4 )) & kk_mask_odd_nibbles64;
  x = (x ^ (x << 2 )) & kk_mask_odd_pairs64;
  x = (x ^ (x << 1 )) & kk_mask_odd_bits64;
  return x;
}

// gather odd bits
static inline uint32_t kk_bits_gather_odd32(uint32_t x) {
  x = x & kk_mask_odd_bits32;
  x = (x ^ (x >> 1 )) & kk_mask_odd_pairs32;
  x = (x ^ (x >> 2 )) & kk_mask_odd_nibbles32;
  x = (x ^ (x >> 4 )) & KK_U32(0x00FF00FF);
  x = (x ^ (x >> 8 )) & KK_U32(0x0000FFFF);
  return x;
}

static inline uint64_t kk_bits_gather_odd64(uint64_t x) {
  x = x & kk_mask_odd_bits64;
  x = (x ^ (x >> 1 )) & kk_mask_odd_pairs64;
  x = (x ^ (x >> 2 )) & kk_mask_odd_nibbles64;
  x = (x ^ (x >> 4 )) & KK_U64(0x00FF00FF00FF00FF);
  x = (x ^ (x >> 8 )) & KK_U64(0x0000FFFF0000FFFF);
  x = (x ^ (x >> 16)) & KK_U64(0x00000000FFFFFFFF);
  return x;
}

uint32_t kk_bits_interleave32(uint32_t x) {
  return ((kk_bits_scatter_odd32(x & 0xFFFF) << 1) | kk_bits_scatter_odd32(x >> 16));  
}

uint32_t kk_bits_deinterleave32(uint32_t x) {
  return ((kk_bits_gather_odd32(x) << 16) | kk_bits_gather_odd32(x >> 1));
}

uint64_t kk_bits_interleave64(uint64_t x) {
  return ((kk_bits_scatter_odd64(x & KK_U64(0xFFFFFFFF)) << 1) | kk_bits_scatter_odd64(x>>32));  
}

uint64_t kk_bits_deinterleave64(uint64_t x) {
  return ((kk_bits_gather_odd64(x) << 32) | kk_bits_gather_odd64(x >> 1));
}

#endif

/* ----------------------------------------------------------
  xperm
-------------------------------------------------------------*/

uint32_t kk_bits_xperm32(uint32_t x, uint32_t indices) {
  uint32_t r = 0;
  for (int i = 0; i < 32; i += 8) {
    uint32_t idx = (indices >> i) & 0x03;
    r |= ((x >> (8*idx)) & 0xFF) << i;
  }
  return r;
}

uint32_t kk_bits_xpermn32(uint32_t x, uint32_t indices) {
  uint32_t r = 0;
  for (int i = 0; i < 32; i += 4) {
    uint32_t idx = (indices >> i) & 0x07;
    r |= ((x >> (4*idx)) & 0x0F) << i;
  }
  return r;
}

uint64_t kk_bits_xperm64(uint64_t x, uint64_t indices) {
  uint64_t r = 0;
  for (int i = 0; i < 64; i += 8) {
    uint64_t idx = (indices >> i) & 0x07;
    r |= ((x >> (8*idx)) & 0xFF) << i;
  }
  return r;
}

uint64_t kk_bits_xpermn64(uint64_t x, uint64_t indices) {
  uint64_t r = 0;
  for (int i = 0; i < 64; i += 4) {
    uint32_t idx = (indices >> i) & 0x0F;
    r |= ((x >> (4*idx)) & 0x0F) << i;
  }
  return r;
}

