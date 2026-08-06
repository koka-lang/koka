/*---------------------------------------------------------------------------
  Copyright 2020-2026, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// -------------------------------------------
// clmul,clmulr,gather,scatter,zip,unzip,orc
// see: kklib/src/bits.c for optimized implementations
// -------------------------------------------

export function _int32_clmul(x,y) {
  let a = $std_core_types._int32_as_uint32(x);
  let b = $std_core_types._int32_as_uint32(y);
  let c = 0;
  for (let i = 0; i < 32; i++) {
    if ((b >> i) & 1) {
      c ^= (a << i);
    }
  }
  return $std_core_types._uint32_as_int32(c|0);
}

export function _int32_clmul_wide(x,y) {
  let a = $std_core_types._int32_as_uint32(x);
  let b = $std_core_types._int32_as_uint32(y);
  let clo = 0;
  let chi = 0;
  for (let i = 0; i < 32; i++) {
    if ((b >> i) & 1) {
      clo ^= (a << i);
      chi ^= (a >> (32 - i));
    }
  }  
  return $std_core_types.Tuple2($std_core_types._uint32_as_int32(chi),$std_core_types._uint32_as_int32(clo));
}


export function _int32_clmulr(x,y) {
  return $std_core_types._int32_breverse(_int32_clmul($std_core_types._int32_breverse(x),$std_core_types._int32_breverse(y)));
}

export function _int32_scatter(x,mask) {
  let a = $std_core_types._int32_as_uint32(x)
  let y = 0
  for (let i = 0; i < 32; i++) {
    if ((mask >> i) & 1) {
      y |= ((a&1) << i)
      a >>= 1
    }
  }
  return $std_core_types._uint32_as_int32(y|0)
}

export function _int32_gather(x,smask) {
  let a = $std_core_types._int32_as_uint32(x)
  let mask = $std_core_types._int32_as_uint32(smask)  
  let y = 0
  let shift = 0
  for (let i = 0; i < 32; i++) {
    if ((mask>>i) & 1) {
      if ((a>>i) & 1) {
        y |= (1 << shift)
      }
      shift++      
    }
  }
  return $std_core_types._uint32_as_int32(y|0)
}

function _int32_scatter_even(x) {
  x = (x | (x << 8 )) & 0x00FF00FF; // even bytes
  x = (x | (x << 4 )) & 0x0F0F0F0F; // even nibbles
  x = (x | (x << 2 )) & 0x33333333; // even pairs
  x = (x | (x << 1 )) & 0x55555555; // even bits
  return x;
}

export function _int32_zip(x) {
  let a = $std_core_types._int32_as_uint32(x) 
  let b = (_int32_scatter_even(a >> 16) << 1) | _int32_scatter_even(a & 0xFFFF);
  return $std_core_types._uint32_as_int32(b)
}

function _int32_gather_even(x) {
  x = x & 0x55555555;
  x = (x | (x >> 1 )) & 0x33333333;
  x = (x | (x >> 2 )) & 0x0F0F0F0F;
  x = (x | (x >> 4 )) & 0x00FF00FF;
  x = (x | (x >> 8 )) & 0x0000FFFF;
  return x;
}

export function _int32_unzip(x) {
  let a = $std_core_types._int32_as_uint32(x)   
  let b = (_int32_gather_even(a >> 1) << 16) | _int32_gather_even(a);
  return $std_core_types._uint32_as_int32(b);
}

export function _int32_orc(x) {
  let a = $std_core_types._int32_as_uint32(x)   
  // set high bit in each byte to `or` of the bits in the byte
  a |= ((a & 0x0F0F0F0F) << 4);
  a |= ((a & 0x33333333) << 2);
  a |= ((a & 0x55555555) << 1);
  // distribute the high bit back
  a &= 0x80808080
  a |= (a >> 1);
  a |= (a >> 2);
  a |= (a >> 4);
  return $std_core_types._uint32_as_int32(a);
}