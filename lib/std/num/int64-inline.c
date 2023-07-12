/*---------------------------------------------------------------------------
  Copyright 2020-2023, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

static kk_std_core_types__tuple2_ kk_wide_umul64x( int64_t x, int64_t y, kk_context_t* ctx ) {
  uint64_t hi;
  uint64_t lo = kk_wide_umul64((uint64_t)x, (uint64_t)y, &hi);
  return kk_std_core_types__new_dash__lp__comma__rp_( kk_int64_box((int64_t)lo,ctx), kk_int64_box((int64_t)hi,ctx), ctx );
}

static kk_std_core_types__tuple2_ kk_wide_imul64x( int64_t x, int64_t y, kk_context_t* ctx ) {
  int64_t hi;
  uint64_t lo = kk_wide_imul64(x, y, &hi);
  return kk_std_core_types__new_dash__lp__comma__rp_( kk_int64_box((int64_t)lo,ctx), kk_int64_box(hi,ctx), ctx );
}
