/*---------------------------------------------------------------------------
  Copyright 2016-2017 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

static class _Calendar {
  private static long unixEpochTicks = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc).Ticks;

  public static __std_core._Tuple2_<double,string> LocalUtcDelta( object tzi, double unixSeconds ) {
    TimeZoneInfo tz = tzi as TimeZoneInfo;
    if (tz==null) tz = TimeZoneInfo.Local;
    
    double secs     = Math.Floor(unixSeconds);
    long unixTicks  = (long)secs * TimeSpan.TicksPerSecond + (long)((unixSeconds - secs)*(double)TimeSpan.TicksPerSecond);
    DateTime t      = new DateTime(unixEpochTicks + unixTicks, DateTimeKind.Utc);
    double ofs      = tz.GetUtcOffset( t ).TotalSeconds;
    string abbrv    = (tz.IsDaylightSavingTime(t) ? tz.DaylightName : tz.StandardName); 
    return new __std_core._Tuple2_<double,string>( ofs, abbrv );
  }

  public static TimeZoneInfo LocalTimeZone() {
    return TimeZoneInfo.Local;
  }
}