/*---------------------------------------------------------------------------
  Copyright 2020-2023, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

static kk_std_core_types__tuple2 kk_umul64x_wide( int64_t x, int64_t y, kk_context_t* ctx ) {
  uint64_t hi;
  uint64_t lo = kk_umul64_wide((uint64_t)x, (uint64_t)y, &hi);
  return kk_std_core_types__new_Tuple2( kk_int64_box((int64_t)hi,ctx), kk_int64_box((int64_t)lo,ctx), ctx );
}

static kk_std_core_types__tuple2 kk_imul64x_wide( int64_t x, int64_t y, kk_context_t* ctx ) {
  int64_t hi;
  uint64_t lo = kk_imul64_wide(x, y, &hi);
  return kk_std_core_types__new_Tuple2( kk_int64_box(hi,ctx), kk_int64_box((int64_t)lo,ctx), ctx );
}

static kk_std_core_types__tuple2 kk_clmul64x_wide( int64_t x, int64_t y, kk_context_t* ctx ) {
  uint64_t hi;
  uint64_t lo = kk_clmul64_wide((uint64_t)x, (uint64_t)y, &hi);
  return kk_std_core_types__new_Tuple2( kk_int64_box((int64_t)hi,ctx), kk_int64_box((int64_t)lo,ctx), ctx );
}
