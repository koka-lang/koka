/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

using System.Diagnostics;
static class _Time
{
  // Time
  static DateTime  epoch  = new DateTime(1970,1,1,0,0,0,0,DateTimeKind.Utc);
  static TimeZone  local  = TimeZone.CurrentTimeZone; // ensure it stays the same

  public static int TimezoneOffset( DateTime d ) {
    return (d.Kind == DateTimeKind.Utc ? 0 : Convert.ToInt32(local.GetUtcOffset(d).TotalMinutes));
  }

  public static double EpochMsecs( DateTime d ) {
    return (d.ToUniversalTime() - epoch).TotalMilliseconds;
  }

  public static DateTime NewFromEpoch( double msecs, bool isUtc ) {
    long eticks = Convert.ToInt64(msecs * 10000.0); // to 100-nanoseconds
    return new DateTime( epoch.Ticks + eticks, isUtc ? DateTimeKind.Utc : DateTimeKind.Local ); 
  }

  public static std_core._Tuple3_<int,int,int> Date( DateTime d ) {
    return new std_core._Tuple3_<int,int,int>(d.Year, d.Month, d.Day);
  }

  public static std_core._Tuple4_<int,int,int,int> Clock( DateTime d ) {
    return new std_core._Tuple4_<int,int,int,int>(d.Hour,d.Minute,d.Second,d.Millisecond);
  }

  // Ticks
  static Stopwatch sw = Stopwatch.StartNew();
  
  public static double Ticks() {
    long   fticks = sw.ElapsedTicks;
    double secs = Convert.ToDouble(fticks/Stopwatch.Frequency) +
                  Convert.ToDouble(fticks%Stopwatch.Frequency)*TicksResolution;
    return (secs * 1000.0);                  
  }

  public static double TicksResolution = 1.0 / Convert.ToDouble(Stopwatch.Frequency); 
}
