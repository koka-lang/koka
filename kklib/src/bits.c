/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
#include "kklib.h"

static const kk_uintx_t powers_of_10[] = { 
    1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000
#if (KK_INTX_SIZE > 4)
  , KUX(10000000000), KUX(100000000000), KUX(1000000000000), KUX(10000000000000), KUX(100000000000000)
  , KUX(1000000000000000), KUX(10000000000000000), KUX(100000000000000000), KUX(1000000000000000000)
  , KUX(10000000000000000000), KUX(10000000000000000000)
#endif
};

uint8_t kk_bits_digits32(uint32_t u) {
  static const uint8_t guess[33] = {
    1, 0, 0, 0, 1, 1, 1, 2, 2, 2,
    3, 3, 3, 3, 4, 4, 4, 5, 5, 5,
    6, 6, 6, 6, 7, 7, 7, 8, 8, 8,
    9, 9, 9
  };
  uint8_t count = guess[32 - kk_bits_clz32(u)]; // = 1 + (KU32(9)*(31 - kk_bits_clz32(u)) >> 5); 
  return (count + (u >= powers_of_10[count] ? 1 : 0));
}

uint8_t kk_bits_digits64(uint64_t u) {
  static const uint8_t guess[65] = {
    1, 0, 0, 0, 1, 1, 1, 2, 2, 2,
    3, 3, 3, 3, 4, 4, 4, 5, 5, 5,
    6, 6, 6, 6, 7, 7, 7, 8, 8, 8,
    9, 9, 9, 9,10,10,10,11,11,11,
   12,12,12,12,13,13,13,14,14,14,
   15,15,15,15,16,16,16,17,17,17,
   18,18,18,18,19
  };  
  uint8_t count = guess[64 - kk_bits_clz64(u)];  // = 1 + (KU64(1233)*(63 - kk_bits_clz64(u)) >> 12);
  return (count + (u >= powers_of_10[count] ? 1 : 0));
}
