/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
#include "kklib.h"

int kk_bits_generic_popcount32(uint32_t x) {
  x = x - ((x >> 1) & KK_U32(0x55555555));
  x = (x & KK_U32(0x33333333)) + ((x >> 2) & KK_U32(0x33333333));
  x = (x + (x >> 4)) & KK_U32(0x0F0F0F0F);
  return kk_bits_byte_sum32(x);
}

int kk_bits_generic_popcount64(uint64_t x) {
  x = x - ((x >> 1) & KK_U64(0x5555555555555555));
  x = (x & KK_U64(0x3333333333333333)) + ((x >> 2) & KK_U64(0x3333333333333333));
  x = (x + (x >> 4)) & KK_U64(0x0F0F0F0F0F0F0F0F);
  return kk_bits_byte_sum64(x);
}


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