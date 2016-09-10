/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

static class _Time
{
  static DateTime epoch       = new DateTime(1970,1,1,0,0,0,0,DateTimeKind.Utc);
  static int      localOffset = -Convert.ToInt32(DateTimeOffset.Now.Offset.TotalMinutes);

  private static int Offset( DateTime d ) {
    return (d.Kind == DateTimeKind.Utc ? 0 : localOffset);
  }

  private static std_time._time ToTime( DateTime d ) {
    return new std_time._time(Offset(d),d);
  }

  public static std_time._time Now() {
    return ToTime( DateTime.Now );
  }

  public static double EpochMsecs( object _d ) {
    DateTime d = (DateTime)_d;    
    return (d.ToUniversalTime() - epoch).TotalMilliseconds;
  }

  public static std_core._Tuple3_<int,int,int> Date( std_time._time _t ) {
    DateTime d = (DateTime)_t.xtime;
    return new std_core._Tuple3_<int,int,int>(d.Year, d.Month, d.Day);
  }

  public static std_core._Tuple4_<int,int,int,int> Clock( std_time._time _t ) {
    DateTime d = (DateTime)_t.xtime;
    return new std_core._Tuple4_<int,int,int,int>(d.Hour,d.Minute,d.Second,d.Millisecond);
  }
}
