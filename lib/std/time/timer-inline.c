/*---------------------------------------------------------------------------
  Copyright 2020, Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

static kk_std_core_types__tuple2_ kk_timer_ticks_tuple(kk_context_t* ctx) {
  double frac;
  double secs = kk_timer_ticks(&frac,ctx);
  return kk_std_core_types__new_dash__lp__comma__rp_( kk_double_box(secs,ctx), kk_double_box(frac,ctx), ctx );
}
