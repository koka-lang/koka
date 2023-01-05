/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Use double-double type for high precision conversion from duration to two doubles.
typedef struct kk_ddouble_s {
  double hi;
  double lo;
} kk_ddouble_t;

static kk_ddouble_t kk_dd_sum(double x, double y) {
  double z = x + y;
  double diff = z - x;
  double err = (x - (z - diff)) + (y - diff);
  kk_ddouble_t dd = { z, err };
  return dd;
}

static kk_ddouble_t kk_dd_quicksum(double x, double y) {
  kk_assert(abs(x) >= abs(y));
  double z = x + y;
  double err = y - (z - x);
  kk_ddouble_t dd = { z, err };
  return dd;
}

static kk_ddouble_t kk_dd_add(kk_ddouble_t x, kk_ddouble_t y) {
  kk_ddouble_t z1 = kk_dd_sum(x.hi, y.hi);
  kk_ddouble_t low = kk_dd_sum(x.lo, y.lo);
  double e1 = z1.lo + low.hi;
  kk_ddouble_t z2 = kk_dd_quicksum(z1.hi, e1);
  double e2 = z2.lo + low.lo;
  return kk_dd_quicksum(z2.hi, e2);
}

static kk_ddouble_t kk_dd_from_int64(int64_t i, double scale) {
  double x = ((double)kk_sar64(i,32) * 0x1p32) * scale;
  double y = (double)((int32_t)i) * scale;
  return kk_dd_sum(x, y);
}

static kk_ddouble_t kk_dd_from_duration(kk_duration_t d) {
  return kk_dd_add(kk_dd_from_int64(d.seconds,1.0), kk_dd_from_int64(d.attoseconds, 1e-18));
}


static kk_std_core_types__tuple2_ kk_time_unix_now_tuple(kk_context_t* ctx) {
  kk_duration_t d = kk_time_unix_now(ctx);
  kk_ddouble_t dd = kk_dd_from_duration(d); 
  return kk_std_core_types__new_dash__lp__comma__rp_( kk_double_box(dd.hi,ctx), kk_double_box(dd.lo,ctx), ctx );
}

static double kk_time_dresolution(kk_context_t* ctx) {
  int64_t asecs = kk_time_resolution(ctx);
  return (double)asecs * 1e-18;
}
