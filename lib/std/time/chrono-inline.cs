/*---------------------------------------------------------------------------
  Copyright 2016-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

static class _Chrono {
  private static long unixEpochTicks = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc).Ticks;
  private static long prev = 0;

  public static __std_core_types._Tuple2_<double,double> UnixNow() {
    long ticks  = DateTime.UtcNow.Ticks - unixEpochTicks;
    long diff   = ticks - prev;
    if (prev != 0 && diff <= 0 && diff >= -TimeSpan.TicksPerSecond) {
      // Negative time step and of less than 1 second.
      // In this case we assume this is a leap second on an OS that
      // jumps back (instead of smearing).
      // Keep increasing by 1 tick until the clock catches up to
      // maintain monotonicity.
      ticks = prev + 1;
    }
    prev = ticks; // thread unsafe is ok here
    double seconds = (double)(ticks / TimeSpan.TicksPerSecond);
    double frac  = (double)(ticks % TimeSpan.TicksPerSecond) / (double)TimeSpan.TicksPerSecond;
    return new __std_core_types._Tuple2_<double,double>( seconds, frac );
  }

  public static double NowResolution() {
    return (1.0 / (double)TimeSpan.TicksPerSecond);
  }
}
