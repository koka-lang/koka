/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

static kk_std_core_types__tuple2_ kk_time_unix_now_tuple(kk_context_t* ctx) {
  int64_t asecs;
  int64_t isecs = kk_time_unix_now(&asecs,ctx);
  double frac = (double)asecs * 1e-18;
  double secs = (double)isecs;
  return kk_std_core_types__new_dash__lp__comma__rp_( kk_double_box(secs,ctx), kk_double_box(frac,ctx), ctx );
}

static double kk_time_dresolution(kk_context_t* ctx) {
  int64_t asecs = kk_time_resolution(ctx);
  return (double)asecs * 1e-18;
}
