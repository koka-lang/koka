/*---------------------------------------------------------------------------
  Copyright 2016-2017 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
using System.Diagnostics;

static class _Timer {
  private static Stopwatch ticker = Stopwatch.StartNew();
  private static long frequency = (Stopwatch.Frequency > 0 ? Stopwatch.Frequency : 1000);
  private static double resolution = 1.0 / (double)frequency;

  public static __std_core._Tuple2_<double,double> Ticks() {
    long ticks = ticker.ElapsedTicks;
    // maintain precision ..
    double secs  = (double)(ticks / frequency);
    double fsecs = (double)(ticks % frequency) * resolution;
    return new __std_core._Tuple2_<double,double>(secs,fsecs);
  }

  public static double TicksResolution() {
    return resolution;
  }
}