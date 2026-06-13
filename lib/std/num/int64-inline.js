/*---------------------------------------------------------------------------
  Copyright 2020-2026, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// -------------------------------------------
// clmul,clmulr,gather,scatter,zip,unzip,orc
// -------------------------------------------

export function _int64_clmul(x,y) {
  let a = $std_core_types._int64_as_uint64(x);
  let b = $std_core_types._int64_as_uint64(y);
  let c = 0n;
  for (let i = 0n; i < 64n; i++) {
    if ((b >> i) & 1n) {
      c ^= BigInt.asUintN(64,(a << i));
    }
  }
  return $std_core_types._uint64_as_int64(BigInt.asIntN(64,c));
}

export function _int64_clmul_wide(x,y) {
  let a = $std_core_types._int64_as_uint64(x);
  let b = $std_core_types._int64_as_uint64(y);
  let clo = 0n;
  let chi = 0n;
  for (let i = 0n; i < 64n; i++) {
    if ((b >> i) & 1n) {
      clo ^= BigInt.asUintN(64,a << i);
      chi ^= (a >> (64n - i));
    }
  }  
  return $std_core_types.Tuple2($std_core_types._uint64_as_int64(chi),$std_core_types._uint64_as_int64(clo));
}

export function _int64_clmulr(x,y) {
  return $std_core_types._int64_breverse(_int64_clmul($std_core_types._int64_breverse(x),$std_core_types._int64_breverse(y)));
}


export function _int64_scatter(x,smask) {
  let a = $std_core_types._int64_as_uint64(x)
  let mask = $std_core_types._int64_as_uint64(smask)
  let y = 0n
  for (let i = 0n; i < 64n; i++) {
    if ((mask >> i) & 1n) {
      y |= ((a&1n) << i)
      a >>= 1n
    }
  }
  return BigInt.asIntN(64,y)
}

export function _int64_gather(x,smask) {
  let a = $std_core_types._int64_as_uint64(x)
  let mask = $std_core_types._int64_as_uint64(smask)  
  let y = 0n
  let shift = 0n
  for (let i = 0n; i < 64n; i++) {
    if ((mask>>i) & 1n) {
      if ((a>>i) & 1n) {
        y |= (1n << shift)
      }
      shift++   
    }
  }
  return BigInt.asIntN(64,y)
}

function _int64_scatter_even(x) {
  x = (x | (x << 16n)) & 0x0000FFFF0000FFFFn;
  x = (x | (x << 8n )) & 0x00FF00FF00FF00FFn; // even bytes
  x = (x | (x << 4n )) & 0x0F0F0F0F0F0F0F0Fn; // even nibbles
  x = (x | (x << 2n )) & 0x3333333333333333n; // even pairs
  x = (x | (x << 1n )) & 0x5555555555555555n; // even bits
  return x;
}

export function _int64_zip(x) {
  let a = $std_core_types._int64_as_uint64(x) 
  let b = (_int64_scatter_even(a >> 32n) << 1n) | _int64_scatter_even(a & 0xFFFFFFFFn);
  return $std_core_types._uint64_as_int64(b)
}

function _int64_gather_even(x) {
  x = x & 0x5555555555555555n;
  x = (x | (x >> 1n )) & 0x3333333333333333n;
  x = (x | (x >> 2n )) & 0x0F0F0F0F0F0F0F0Fn;
  x = (x | (x >> 4n )) & 0x00FF00FF00FF00FFn;
  x = (x | (x >> 8n )) & 0x0000FFFF0000FFFFn;
  x = (x | (x >> 16n)) & 0x00000000FFFFFFFFn;
  return x;
}

export function _int64_unzip(x) {
  let a = $std_core_types._int64_as_uint64(x)   
  let b = (_int64_gather_even(a >> 1n) << 32n) | _int64_gather_even(a);
  return $std_core_types._uint64_as_int64(b);
}

export function _int64_orc(x) {
  let a = $std_core_types._int64_as_uint64(x)   
  let lo = $std_core_types._int64_lo(a)
  let hi = $std_core_types._int64_hi(a)
  let olo = $std_num_int32._int32_orc(lo)
  let ohi = $std_num_int32._int32_orc(hi)
  return $std_core_types._int64_hi_lo(ohi,olo)
}

export function _int64_orcx(x) {
  let a = $std_core_types._int64_as_uint64(x)   
  // set high bit in each byte to `or` of the bits in the byte
  a |= ((a & 0x0F0F0F0F0F0F0F0Fn) << 4n);
  a |= ((a & 0x3333333333333333n) << 2n);
  a |= ((a & 0x5555555555555555n) << 1n);
  // distribute the high bit back
  a &= 0x8080808080808080n
  a |= (a >> 1n);
  a |= (a >> 2n);
  a |= (a >> 4n);
  return $std_core_types._uint64_as_int64(a);
}
