/*---------------------------------------------------------------------------
  Copyright 2020-2023, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

static kk_std_core_types__tuple2 kk_umul32x_wide( int32_t x, int32_t y, kk_context_t* ctx ) {
  uint32_t hi;
  uint32_t lo = kk_umul32_wide((uint32_t)x, (uint32_t)y, &hi);
  return kk_std_core_types__new_Tuple2( kk_int32_box((int32_t)lo,ctx), kk_int32_box((int32_t)hi,ctx), ctx );
}

static kk_std_core_types__tuple2 kk_imul32x_wide( int32_t x, int32_t y, kk_context_t* ctx ) {
  int32_t hi;
  uint32_t lo = kk_imul32_wide(x, y, &hi);
  return kk_std_core_types__new_Tuple2( kk_int32_box((int32_t)lo,ctx), kk_int32_box(hi,ctx), ctx );
}

static kk_std_core_types__tuple2 kk_clmul32x_wide( int32_t x, int32_t y, kk_context_t* ctx ) {
  uint32_t hi;
  uint32_t lo = kk_clmul32_wide((uint32_t)x, (uint32_t)y, &hi);
  return kk_std_core_types__new_Tuple2( kk_int32_box((int32_t)hi,ctx), kk_int32_box((int32_t)lo,ctx), ctx );
}