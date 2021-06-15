/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

static kk_std_core_types__tuple2_ kk_time_unix_now_tuple(kk_context_t* ctx) {
  double frac;
  double secs = kk_time_unix_now(&frac,ctx);
  return kk_std_core_types__new_dash__lp__comma__rp_( kk_double_box(secs,ctx), kk_double_box(frac,ctx), ctx );
}
