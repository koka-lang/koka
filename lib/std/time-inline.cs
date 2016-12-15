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

  public static double TotalSecs( DateTime d ) {
    return (d.ToUniversalTime() - DateTime.MinValue).TotalSeconds;
  }

  public static int DayOfWeek( DateTime d ) {
    int w = (int)d.DayOfWeek;
    return (w==0 ? 7 : w);
  }

  public static DateTime New( int year, int month, int day, int hours, int minutes, int seconds, int milliseconds, bool isUtc ) {
    if (year>=1 && year<=9999 && month>=1 && month<=12 && day>=1 && day<=28 &&
        hours>=0 && hours<=23 && minutes>=0 && minutes<=59 && seconds>=0 && seconds<=59 && 
        milliseconds>=0 && milliseconds<=999) 
    {
      // only call constructor when all arguments are in-range (or an exception is thrown)
      return new DateTime(year,month,day,hours,minutes,seconds,milliseconds, isUtc ? DateTimeKind.Utc : DateTimeKind.Local );
    }
    try {
      // otherwise normalize to prevent exceptions
      int xyear  = (year - 1970);
      int xmonth = (month - 1);
      int xday   = (day - 1);    
      DateTime d = new DateTime(1970,1,1,0,0,0,0, isUtc ? DateTimeKind.Utc : DateTimeKind.Local );    
      // add from large to small
      d = d.AddYears(xyear).AddMonths(xmonth).AddDays(xday);
      d = d.AddHours(hours).AddMinutes(minutes).AddSeconds(seconds).AddMilliseconds(milliseconds);    
      return d;
    }
    catch {
      // if we get out of range return the minimal date
      return DateTime.MinValue;
    }
  }

  public static DateTime NewFromSeconds( double secs, bool isUtc ) {
    try {
      long eticks = Convert.ToInt64(secs * 1.0e7); // to 100-nanoseconds
      DateTime d = new DateTime( eticks, DateTimeKind.Utc); 
      return (isUtc ? d : d.ToLocalTime());
    }
    catch {
      return DateTime.MinValue;
    }
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
