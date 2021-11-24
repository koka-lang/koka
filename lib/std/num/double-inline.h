/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

static inline kk_std_core_types__tuple2_ kk_double_to_bits( double d, kk_context_t* ctx ) {
  uint64_t u = kk_bits_from_double(d);
  return kk_std_core_types__new_dash__lp__comma__rp_( kk_int32_box((int32_t)u, ctx), kk_int32_box((int32_t)(u >> 32), ctx), ctx );
}

static inline double kk_double_from_bits( int32_t lo, int32_t hi, kk_context_t* ctx ) {
  kk_unused(ctx);
  uint64_t u = (((uint64_t)((uint32_t)hi)) << 32) | (uint32_t)lo;  // note: careful about sign extension
  return kk_bits_to_double(u);
  return d;
}

static inline double kk_prim_parse_double( kk_string_t str, kk_context_t* ctx) {
  const char* s = kk_string_cbuf_borrow(str,NULL);
  char* end;
  double d = strtod(s,&end);
  kk_string_drop(str,ctx);  
  return d;
}
