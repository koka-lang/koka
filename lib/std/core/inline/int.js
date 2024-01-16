/*---------------------------------------------------------------------------
  Copyright 2012-2024, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

export function _int_parse(s,hex) {
  if (s==="") return $std_core_types.Nothing;
  const cappre  = /^([\-\+])?(0[xX])?(.*)$/.exec(s);
  const sdigits = cappre[3].toLowerCase();
  const sign    = cappre[1] || "";
  if (cappre[2]) hex = true;
  if (hex) {
    const cap = /^[0-9a-f]+$/.exec(sdigits);
    if (!cap) return  $std_core_types.Nothing;
    return $std_core_types.Just( $std_core_types._int_string(sign + "0x" + sdigits) );
  }
  else {
    const rx = /^([0-9]+)(?:\.([0-9]+))?(?:[eE]\+?([0-9]+))?$/;
    const cap = rx.exec(sdigits);
    if (!cap) return $std_core_types.Nothing;
    var sig  = cap[1];
    const frac = cap[2] || "";
    var exp  = (cap[3] ? parseInt(cap[3]) : 0);
    if (frac.length > exp) return $std_core_types.Nothing;
    exp = exp - frac.length;
    sig = sig + frac;
    var x = $std_core_types._int_string(sign + sig);
    if (exp > 0) {
      x = $std_core_types._int_mul_pow10( x, exp );
    }
    return $std_core_types.Just(x);
  }
}
