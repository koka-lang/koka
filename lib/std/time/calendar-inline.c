/*---------------------------------------------------------------------------
  Copyright 2020, Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------------------
  Local time zone UTC delta
--------------------------------------------------------------------------------------------------*/

#if !defined(__STDC_WANT_LIB_EXT1__)
#define __STDC_WANT_LIB_EXT1__    // for gmtime_s
#endif
#if defined(__GLIBC__) && !defined(_GNU_SOURCE)
#define _GNU_SOURCE               // for tm_zone and tm_gmtoff
#endif
#include <time.h>

#if defined(_GNU_SOURCE)
// GNU libc has the tm_zone and tm_gmtoff fields
static long kk_local_utc_delta(double unix_secs, kk_string_t* ptzname, kk_context_t* ctx) {
  const time_t t = (time_t)unix_secs;
  struct tm loctm;
  localtime_r(&t, &loctm);
  if (ptzname != NULL) {
    *ptzname = kk_string_alloc_dup(loctm.tm_zone, ctx);
  }
  return (long)loctm.tm_gmtoff;
}

#else
static long kk_local_utc_delta(double unix_secs, kk_string_t* ptzname, kk_context_t* ctx) {
  // get the UTC delta in a portable way...
  const time_t t = (time_t)unix_secs;
  #if defined(_WIN32) || defined(KK_C11) 
    struct tm gmtm;
    gmtime_s(&gmtm, &t);             
    const time_t loct = mktime(&gmtm);   // interpret gmt as local time
  #else
    struct tm* pgmtm = gmtime(&t);
    const time_t loct = mktime(pgmtm);   // interpret gmt as local time
  #endif
  const time_t utc_delta = t - loct;   // the difference is the utc offset at that time
  if (ptzname != NULL) {
    // getting the timezone name need platform specific code    
    #if defined(_WIN32)
      bool isdst = false;
      struct tm loctm;
      localtime_s(&loctm, &t);
      isdst = (loctm.tm_isdst != 0);
      char tzname[256];
      size_t tznamelen;
      _get_tzname(&tznamelen, tzname, 255, isdst ? 1 : 0); tzname[255] = 0;
      *ptzname = kk_string_alloc_dup(tzname, ctx);
    #elif (_POSIX_C_SOURCE >= 1) || _XOPEN_SOURCE || _POSIX_SOURCE  // tzname
      bool isdst = false;
      struct tm ploctm;
      ploctm = localtime(&t);
      isdst = (ploctm->tm_isdst != 0);
      *ptzname = tzname[isdst ? 1 : 0];
    #else
      // give up :-(
      * ptzname = kk_string_empty();
    #endif    
  }
  return (long)utc_delta;
}
#endif

kk_std_time_calendar__local_timezone kk_local_get_timezone(kk_context_t* ctx) {
  return kk_datatype_from_tag((kk_tag_t)1); // dummy value; we cannot store the local timezone as it is a global :-(
}

kk_std_core_types__tuple2_ kk_local_get_utc_delta_tuple(kk_std_time_calendar__local_timezone tz, double unix_secs, kk_context_t* ctx) {
  kk_string_t tzname;
  long utc_delta = kk_local_utc_delta(unix_secs, &tzname, ctx);
  return kk_std_core_types__new_dash__lp__comma__rp_( kk_double_box((double)utc_delta,ctx), kk_string_box(tzname), ctx );
}
