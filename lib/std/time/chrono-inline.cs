/*---------------------------------------------------------------------------
  Copyright 2016-2017 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

static class _Chrono {
  private static long unixEpochTicks = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc).Ticks;
    
  public static __std_core._Tuple2_<double,double> UnixNow() {
    long ticks   = DateTime.UtcNow.Ticks - unixEpochTicks;
    double seconds = (double)(ticks / TimeSpan.TicksPerSecond);
    double frac  = (double)(ticks % TimeSpan.TicksPerSecond) / (double)TimeSpan.TicksPerSecond;
    return new __std_core._Tuple2_<double,double>( seconds, frac );
  }

  public static double NowResolution() {
    return (1.0 / (double)TimeSpan.TicksPerSecond);
  }
}