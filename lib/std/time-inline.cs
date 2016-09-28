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
    return (d.Kind == DateTimeKind.Utc ? 0 : Convert.ToInt32(local.GetUtcOffset(d).TotalSeconds));
  }

  public static double EpochSecs( DateTime d ) {
    return (d.ToUniversalTime() - epoch).TotalSeconds;
  }

  public static int DayOfWeek( DateTime d ) {
    int w = (int)d.DayOfWeek;
    return (w==0 ? 7 : w);
  }

  public static DateTime New( int year, int month, int day, int hours, int minutes, int seconds, int milliseconds, bool isUtc ) {
    return new DateTime(year,month,day,hours,minutes,seconds,milliseconds, isUtc ? DateTimeKind.Utc : DateTimeKind.Local );
  }

  public static DateTime NewFromEpoch( double secs, bool isUtc ) {
    long eticks = Convert.ToInt64(secs * 1.0e7); // to 100-nanoseconds
    DateTime d = new DateTime( epoch.Ticks + eticks, DateTimeKind.Utc); 
    return (isUtc ? d : d.ToLocalTime());
  }

  public static int Year( DateTime d )    { return d.Year; }
  public static int Month( DateTime d )   { return d.Month; }
  public static int Day( DateTime d )     { return d.Day; }
  public static int Hours( DateTime d )   { return d.Hour; }
  public static int Minutes( DateTime d ) { return d.Minute; }
  public static int Seconds( DateTime d ) { return d.Second; }
  public static int Milliseconds( DateTime d ) { return d.Millisecond; }


  // Ticks
  static Stopwatch sw = Stopwatch.StartNew();
  
  public static double Ticks() {
    long   fticks = sw.ElapsedTicks;
    double secs = Convert.ToDouble(fticks/Stopwatch.Frequency) +
                  Convert.ToDouble(fticks%Stopwatch.Frequency)*TicksResolution;
    return secs;                  
  }

  public static double TicksResolution = 1.0 / Convert.ToDouble(Stopwatch.Frequency); 
}
