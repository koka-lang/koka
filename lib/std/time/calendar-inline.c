/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
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

static long kk_local_utc_delta(double unix_secs, kk_string_t* ptzname, kk_context_t* ctx) {
  // get the UTC delta in a somewhat portable way...
  bool isdst = false;
  time_t t = (time_t)unix_secs;
  #if (_WIN32 && KK_INTPTR_SIZE==8)
  if (t < 0) { t += 3155673600; } // try to avoid errors for negative times on some platforms by adding 100 years..
  #endif
  time_t loct = t;    
  #if defined(_GNU_SOURCE)
    // GNU libc has the tm_zone and tm_gmtoff fields
    struct tm loctm;
    if (localtime_r(&t, &loctm) != NULL) {
      isdst = (loctm.tm_isdst != 0);
      loct  = t - loctm.tm_gmtoff + (isdst ? 3600 : 0);
    }
    else {
      loctm.tm_zone = "";
    }
  #elif defined(_WIN32) && !defined(__MINGW32__)
    struct tm gmtm;
    if (gmtime_s(&gmtm, &t) == 0) {      // switched parameters :-(      
      loct = mktime(&gmtm);              // interpret gmt as local time
      struct tm loctm;
      localtime_s(&loctm, &t);           // switched parameters :-(
      isdst = (loctm.tm_isdst != 0);
    }
  #elif defined(__STDC_LIB_EXT1__)
    struct tm gmtm;
    if (gmtime_s(&t, &gmtm) != NULL) {
      loct = mktime(&gmtm);              // interpret gmt as local time
      struct tm loctm;
      localtime_s(&t, &loctm);
      isdst = (loctm.tm_isdst != 0);
    }
  #else
    struct tm* pgmtm = gmtime(&t);
    if (pgmtm != NULL) {
      loct = mktime(pgmtm);                // interpret gmt as local time
      struct tm* ploctm = localtime(&t);
      isdst = (ploctm->tm_isdst != 0);
    }
  #endif
  const time_t utc_delta = t - loct + (isdst ? 3600 : 0);   // the difference is the utc offset at that time
  if (ptzname != NULL) {
    // getting the timezone name
    #if defined(_WIN32) && !defined(__MINGW32__)
      char tzonename[256];
      size_t tznamelen;
      _get_tzname(&tznamelen, tzonename, 255, isdst ? 1 : 0); tzonename[255] = 0;
      *ptzname = kk_string_alloc_from_qutf8(tzonename, ctx);
    #elif defined(_GNU_SOURCE)
      *ptzname = kk_string_alloc_from_qutf8(loctm.tm_zone, ctx);
    #elif (_POSIX_C_SOURCE >= 1) || _XOPEN_SOURCE || _POSIX_SOURCE || __MINGW32__ // tzname
      *ptzname = kk_string_alloc_from_qutf8(tzname[isdst ? 1 : 0], ctx);
    #else
      // give up :-(
      * ptzname = kk_string_empty();
    #endif
  }
  return (long)utc_delta;
}


static kk_std_time_calendar__local_timezone kk_local_get_timezone(kk_context_t* ctx) {
  return kk_datatype_from_tag((kk_tag_t)1); // dummy value; we cannot store the local timezone as it is a global :-(
}

static kk_std_core_types__tuple2_ kk_local_get_utc_delta_tuple(kk_std_time_calendar__local_timezone tz, double unix_secs, kk_context_t* ctx) {
  kk_string_t tzonename;
  long utc_delta = kk_local_utc_delta(unix_secs, &tzonename, ctx);
  return kk_std_core_types__new_dash__lp__comma__rp_( kk_double_box((double)utc_delta,ctx), kk_string_box(tzonename), ctx );
}
