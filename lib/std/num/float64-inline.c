/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

static inline int64_t kk_double_to_bits( double d, kk_context_t* ctx ) {
  kk_unused(ctx);
  return (int64_t)kk_bits_from_double(d);
}

static inline double kk_double_from_bits( int64_t i, kk_context_t* ctx ) {
  kk_unused(ctx);
  return kk_bits_to_double((uint64_t)i);
}

static inline double kk_prim_parse_double( kk_string_t str, kk_context_t* ctx) {
  const char* s = kk_string_cbuf_borrow(str,NULL,ctx);
  char* end;
  double d = strtod(s,&end);
  kk_string_drop(str,ctx);  
  return d;
}
