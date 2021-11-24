/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
#include "kklib.h"

#define KK_NSECS_PER_SEC  KK_I64(1000000000)
#define KK_ASECS_PER_NSEC KK_I64(1000000000)
#define KK_ASECS_PER_SEC  (KK_NSECS_PER_SEC * KK_ASECS_PER_NSEC)

/*--------------------------------------------------------------------------------------------------
  Timer ticks
--------------------------------------------------------------------------------------------------*/

kk_decl_export kk_secs_t  kk_timer_ticks(kk_asecs_t* asecs, kk_context_t* ctx);
kk_decl_export kk_asecs_t kk_timer_resolution(kk_context_t* ctx); // in atto seconds

#ifdef WIN32
#include <Windows.h>
static kk_secs_t kk_timer_ticks_prim(kk_asecs_t* asecs, kk_context_t* ctx) {
  LARGE_INTEGER t;
  QueryPerformanceCounter(&t);
  if (ctx->timer_freq == 0) {
    LARGE_INTEGER freq;
    QueryPerformanceFrequency(&freq);
    ctx->timer_freq = freq.QuadPart;
    if (ctx->timer_freq <= 0) { ctx->timer_freq = 1000; }
  }
  kk_assert_internal(ctx->timer_freq != 0);
  // calculate in parts for precision
  kk_secs_t  secs = t.QuadPart / ctx->timer_freq;
  int64_t    frac = t.QuadPart % ctx->timer_freq;
  if (asecs != NULL) {
    int64_t resolution = KK_ASECS_PER_SEC / ctx->timer_freq;
    *asecs = frac * resolution;
  }
  return secs;
}

#else

#include <time.h>

#if (defined(CLOCK_REALTIME) || defined(CLOCK_MONOTONIC))
#if !defined(CLOCK_MONOTONIC)
#define CLOCK_MONOTONIC  CLOCK_REALTIME
#endif

// high res timer
static kk_secs_t kk_timer_ticks_prim(kk_asecs_t* asecs, kk_context_t* ctx) {
  if (ctx->timer_freq == 0) {
    struct timespec tres = { 0, 0 };
    clock_getres(CLOCK_MONOTONIC, &tres);
    if (tres.tv_sec == 0 && tres.tv_nsec > 0 && tres.tv_nsec <= KK_NSECS_PER_SEC && (KK_NSECS_PER_SEC % tres.tv_nsec) == 0) {
      ctx->timer_freq = KK_NSECS_PER_SEC / tres.tv_nsec;
    }
    else {
      ctx->timer_freq = KK_NSECS_PER_SEC;
    }
  }
  struct timespec t;
  clock_gettime(CLOCK_MONOTONIC, &t);
  if (asecs != NULL) {
    *asecs = t.tv_nsec * KK_ASECS_PER_NSEC;
  }
  return t.tv_sec;
}

#else
// low resolution timer
#pragma message("using low-res timer on this platform")
static kk_secs_t kk_timer_ticks_prim(kk_asecs_t* asecs, kk_context_t* ctx) {
  if (ctx->timer_freq == 0) {
    ctx->timer_freq = (int64_t)CLOCKS_PER_SEC;
    if (ctx->timer_freq <= 0) { ctx->timer_freq = 1000; }
  }
  int64_t t = (int64_t)clock();
  // calculate in parts for precision
  int64_t secs = t / ctx->timer_freq;
  int64_t frac = t % ctx->timer_freq;
  if (asecs != NULL) {
    int64_t resolution = KK_ASECS_PER_SEC / ctx->timer_freq;
    *asecs = frac * resolution;
  }
  return secs;
}
#endif
#endif

kk_decl_export kk_secs_t kk_timer_ticks(kk_asecs_t* atto_secs, kk_context_t* ctx) {
  kk_asecs_t asecs;
  kk_secs_t  secs = kk_timer_ticks_prim(&asecs, ctx);
  // init previous and delta
  if (ctx->timer_prev.seconds == 0 && ctx->timer_prev.attoseconds == 0) {
    ctx->timer_prev.seconds = secs;
    ctx->timer_prev.attoseconds = asecs;
    ctx->timer_delta.seconds = secs;
    ctx->timer_delta.attoseconds = asecs;
  }
  // check monotonicity
  if (ctx->timer_prev.seconds > secs || (ctx->timer_prev.seconds == secs && ctx->timer_prev.attoseconds >= asecs)) {
    // ouch, clock ran backward; add 1 nano second and adjust the delta
    ctx->timer_delta.seconds = ctx->timer_prev.seconds - secs;
    ctx->timer_delta.attoseconds = ctx->timer_prev.attoseconds - asecs - KK_NSECS_PER_SEC; // can be negative
  }
  // save time in previous and adjust with the delta
  ctx->timer_prev.seconds = secs;
  ctx->timer_prev.attoseconds = asecs;
  secs -= ctx->timer_delta.seconds;
  asecs -= ctx->timer_delta.attoseconds;
  if (asecs < 0) {
    secs  -= 1;
    asecs += KK_ASECS_PER_SEC;
  }
  kk_assert_internal(secs >= 0 && asecs >= 0);
  if (atto_secs != NULL) *atto_secs = asecs;
  return secs;
}

kk_decl_export kk_asecs_t kk_timer_resolution(kk_context_t* ctx) {
  kk_timer_ticks_prim(NULL, ctx); // initialize
  kk_assert_internal(ctx->timer_freq != 0);
  return (KK_ASECS_PER_SEC / ctx->timer_freq);
}

/*--------------------------------------------------------------------------------------------------
  Current Time
--------------------------------------------------------------------------------------------------*/

#ifdef WIN32
#define KK_100NSECS_PER_SEC  KK_I64(10000000)
#define KK_UNIX_EPOCH        KK_I64(11644473600)  // seconds since 1601-01-01 UTC to 1970-01-01 (Unix epoch)
static kk_secs_t kk_time_unix_now_prim(kk_secs_t* atto_secs, kk_context_t* ctx) {
  FILETIME ft;
  GetSystemTimeAsFileTime(&ft);
  LARGE_INTEGER ti;
  ti.LowPart = ft.dwLowDateTime;
  ti.HighPart = (LONG)ft.dwHighDateTime;
  int64_t t = ti.QuadPart; // t is the time in 100 nano seconds intervals since 1601-01-01 UTC.
  int64_t secs  = (t / KK_100NSECS_PER_SEC) - KK_UNIX_EPOCH;
  int64_t fsecs = (t % KK_100NSECS_PER_SEC);
  if (ctx->time_freq == 0) {
    // initialize
    ctx->time_freq = KK_100NSECS_PER_SEC;
  }
  // done
  if (atto_secs != NULL) {
    *atto_secs = fsecs * 100 * KK_ASECS_PER_NSEC;
  }
  return secs;
}
#else

#include <time.h>
#if defined(CLOCK_REALTIME)
// high res time
static kk_secs_t kk_time_unix_now_prim(kk_asecs_t* asecs, kk_context_t* ctx) {
  if (ctx->time_freq==0) {
    struct timespec tres = { 0, 0 };
    clock_getres(CLOCK_REALTIME, &tres);
    if (tres.tv_sec == 0 && tres.tv_nsec > 0 && tres.tv_nsec <= KK_NSECS_PER_SEC && (tres.tv_nsec % KK_NSECS_PER_SEC) == 0) {
      ctx->time_freq = (KK_NSECS_PER_SEC / tres.tv_nsec);
    }
    else {
      ctx->time_freq = KK_NSECS_PER_SEC;
    }
  }
  struct timespec t;
  clock_gettime(CLOCK_REALTIME, &t);
  if (asecs != NULL) {
    *asecs = t.tv_nsec * KK_ASECS_PER_NSEC;
  }
  return t.tv_sec;
}

#else
// portable 1s resolution time
static kk_secs_t kk_time_unix_now_prim(kk_asecs_t* asecs, kk_context_t* ctx) {
  if (ctx->time_freq == 0) {
    ctx->time_freq = 1; // :-(
  }
  time_t t;
  time(&t);
  if (asecs != NULL) {
    *asecs = 0;
  }
  return t;
}
#endif

#endif

kk_decl_export kk_secs_t kk_time_unix_now(kk_asecs_t* atto_secs, kk_context_t* ctx) {
  kk_asecs_t asecs;
  kk_secs_t  secs = kk_time_unix_now_prim(&asecs, ctx);
  if ((ctx->time_unix_prev.seconds > secs || (ctx->time_unix_prev.seconds == secs && ctx->time_unix_prev.attoseconds >= asecs))
    // time is set backward!
    && ((ctx->time_unix_prev.seconds - secs) <= 1 &&
        (ctx->time_unix_prev.seconds - secs)*KK_ASECS_PER_SEC + (ctx->time_unix_prev.attoseconds - asecs) <= KK_ASECS_PER_SEC)
    // ((secs + frac + 1.0) > (ctx->time_unix_prev.seconds + ctx->time_unix_prev.second_fraction))
    // if it is less the 1 second we add a tiny increment as we assume it is due to leap second smearing
    ) {
    // keep monotonic and allow to catch up
    secs = ctx->time_unix_prev.seconds;
    ctx->time_unix_prev.attoseconds += KK_ASECS_PER_NSEC;
    asecs = ctx->time_unix_prev.attoseconds;
  }
  else {
    // save previous time
    ctx->time_unix_prev.seconds = secs;
    ctx->time_unix_prev.attoseconds = asecs;
  }
  // done
  if (atto_secs != NULL) {
    *atto_secs = asecs;
  }
  return secs;
}

kk_decl_export kk_asecs_t kk_time_resolution(kk_context_t* ctx) {
  kk_time_unix_now_prim(NULL, ctx); // initialize
  kk_assert_internal(ctx->time_freq != 0);
  return (KK_ASECS_PER_SEC / ctx->time_freq);
}
