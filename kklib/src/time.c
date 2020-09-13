/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
#include "kklib.h"

#define KK_NSECS_PER_SEC  KI64(1000000000)


/*--------------------------------------------------------------------------------------------------
  Timer ticks
--------------------------------------------------------------------------------------------------*/

kk_decl_export double kk_timer_ticks(double* secs_frac, kk_context_t* ctx);
kk_decl_export double kk_timer_resolution(kk_context_t* ctx);

#ifdef _WIN32
#include <Windows.h>
static double kk_timer_ticks_prim(double* secs_frac, kk_context_t* ctx) {
  LARGE_INTEGER t;
  QueryPerformanceCounter(&t);
  if (ctx->timer_freq == 0) {
    LARGE_INTEGER freq;
    QueryPerformanceFrequency(&freq);
    if (freq.QuadPart <= 0) {
      freq.QuadPart = 1000;
    }
    ctx->timer_freq = freq.QuadPart;
  }
  kk_assert_internal(ctx->timer_freq != 0);
  // calculate in parts for precision
  int64_t secs = t.QuadPart / ctx->timer_freq;
  int64_t frac = t.QuadPart % ctx->timer_freq;
  if (secs_frac != NULL) {
    *secs_frac = ((double)frac / (double)ctx->timer_freq);
  }
  return (double)secs;
}

#else

#include <time.h>

#if (defined(CLOCK_REALTIME) || defined(CLOCK_MONOTONIC))
#if !defined(CLOCK_MONOTONIC)
#define CLOCK_MONOTONIC  CLOCK_REALTIME
#endif

// high res timer
static double kk_timer_ticks_prim(double* secs_frac, kk_context_t* ctx) {  
  if (ctx->timer_freq == 0) {
    struct timespec tres = { 0, 0 };
    clock_getres(CLOCK_MONOTONIC, &tres);
    if (tres.tv_sec == 0 && tres.tv_nsec > 0 && tres.tv_nsec <= KK_NSECS_PER_SEC && (tres.tv_nsec % KK_NSECS_PER_SEC) == 0) {
      ctx->timer_freq = KK_NSECS_PER_SEC / tres.tv_nsec;
    }
    else {
      ctx->timer_freq = KK_NSECS_PER_SEC;
    }
  }
  struct timespec t;
  clock_gettime(CLOCK_MONOTONIC, &t);
  if (secs_frac != NULL) {
    *secs_frac = (double)t.tv_nsec / 1.0e9;
  }
  return ((double)t.tv_sec);
}

#else
// low resolution timer
static double kk_timer_ticks_prim(double* secs_frac, kk_context_t* ctx) {
  if (ctx->timer_freq == 0) {
    ctx->timer_freq = (int64_t)CLOCKS_PER_SEC;
    if (ctx->timer_freq <= 0) ctx->timer_freq = 1000;
  }
  int64_t t = (int64_t)clock();
  // calculate in parts for precision
  int64_t secs = t / ctx->timer_freq;
  int64_t frac = t % ctx->timer_freq;
  if (secs_frac != NULL) {
    *secs_frac = (double)frac / (double)ctx->timer_freq;
  }
  return (double)secs;
}
#endif
#endif

kk_decl_export double kk_timer_ticks(double* secs_frac, kk_context_t* ctx) {
  double frac;
  double secs = kk_timer_ticks_prim(&frac, ctx);
  // init previous and delta
  if (ctx->timer_prev.seconds == 0) {
    ctx->timer_prev.seconds = secs;
    ctx->timer_prev.second_fraction = frac;
    ctx->timer_delta.seconds = secs;
    ctx->timer_delta.second_fraction = frac;
  }
  // check monotonicity
  if (ctx->timer_prev.seconds > secs || (ctx->timer_prev.seconds == secs && ctx->timer_prev.second_fraction >= frac)) {
    // ouch, clock ran backward; add 1 nano second and adjust the delta
    ctx->timer_delta.seconds = ctx->timer_prev.seconds - secs;
    ctx->timer_delta.second_fraction = ctx->timer_prev.second_fraction - frac - 1e-9; // can be negative    
  }
  // save time in previous and adjust with the delta
  ctx->timer_prev.seconds = secs;
  ctx->timer_prev.second_fraction = frac;
  secs -= ctx->timer_delta.seconds;
  frac -= ctx->timer_delta.second_fraction;
  if (frac < 0.0) {
    secs -= 1.0;
    frac += 1.0;
  }
  kk_assert_internal(secs >= 0.0 && frac >= 0.0);
  if (secs_frac != NULL) *secs_frac = frac;
  return secs;
}

kk_decl_export double kk_timer_resolution(kk_context_t* ctx) {
  kk_timer_ticks_prim(NULL, ctx); // initialize
  kk_assert_internal(ctx->timer_freq != 0);
  return (1.0 / (double)ctx->timer_freq);
}

/*--------------------------------------------------------------------------------------------------
  Time
--------------------------------------------------------------------------------------------------*/

#ifdef _WIN32
#define KK_100NSECS_PER_SEC  KI64(10000000)
#define KK_UNIX_EPOCH        KI64(11644473600)  // seconds since 1601-01-01 UTC to 1970-01-01 (Unix epoch)
static double kk_time_unix_now_prim(double* secs_frac, kk_context_t* ctx) {
  FILETIME ft;
  GetSystemTimeAsFileTime(&ft);
  LARGE_INTEGER ti;
  ti.LowPart = ft.dwLowDateTime;
  ti.HighPart = ft.dwHighDateTime;
  int64_t t = ti.QuadPart; // t is the time in 100 nano seconds intervals since 1601-01-01 UTC.
  int64_t isecs = (t / KK_100NSECS_PER_SEC) - KK_UNIX_EPOCH;
  int64_t ifrac = t % KK_100NSECS_PER_SEC;
  double secs = (double)isecs;
  double frac = (double)ifrac / (double)KK_100NSECS_PER_SEC;
  if (ctx->time_freq == 0.0) {
    // initialize
    ctx->time_freq = KK_100NSECS_PER_SEC;
  }
  // done
  if (secs_frac != NULL) {
    *secs_frac = frac;
  }
  return secs;
}
#else 

#include <time.h>
#ifdef CLOCK_UTC
// high res time
static double kk_time_unix_now_prim(double* secs_frac, kk_context_t* ctx) {
  if (ctx->time_freq==0) {
    struct timespec tres = { 0, 0 };
    clock_getres(CLOCK_UTC, &tres);
    if (tres.tv_sec == 0 && tres.tv_nsec > 0 && tres.tv_nsec <= KK_NSECS_PER_SEC && (tres.tv_nsec % KK_NSECS_PER_SEC) == 0) {
      ctx->time_freq = (KK_NSECS_PER_SEC / tres.tv_nsec);
    }
    else {
      ctx->time_freq = KK_NSECS_PER_SEC;
    }
  }
  struct timespec t;
  clock_gettime(CLOCK_UTC, &t);
  if (secs_frac != NULL) {
    *secs_frac = (double)t.tv_nsec / 1.0e9;
  }
  return ((double)t.tv_sec);
}

#else
// low resolution timer
static double kk_time_unix_now_prim(double* secs_frac, kk_context_t* ctx) {
  if (ctx->time_freq == 0) {
    ctx->time_freq = (int64_t)CLOCKS_PER_SEC;
    if (ctx->time_freq <= 0) ctx->time_freq = 1000;
  }
  int64_t t = (int64_t)clock();
  // calculate in parts for precision
  int64_t secs = t / ctx->time_freq;
  int64_t frac = t % ctx->time_freq;
  if (secs_frac != NULL) {
    *secs_frac = (double)frac / (double)ctx->timer_freq;
  }
  return (double)secs;
}
#endif

#endif

kk_decl_export double kk_time_unix_now(double* secs_frac, kk_context_t* ctx) {
  double frac;
  double secs = kk_time_unix_now_prim(&frac, ctx);
  if ((ctx->time_unix_prev.seconds > secs || (ctx->time_unix_prev.seconds == secs && ctx->time_unix_prev.second_fraction >= frac))
    // time is set backward!
    && ((secs + frac + 1.0) > (ctx->time_unix_prev.seconds + ctx->time_unix_prev.second_fraction))
    // if it is less the 1 second we add a tiny increment as we assume it is due to leap second smearing
    ) {
    // keep monotonic and allow to catch up
    secs = ctx->time_unix_prev.seconds;
    ctx->time_unix_prev.second_fraction += 1.0e-9;
    frac = ctx->time_unix_prev.second_fraction;
  }
  else {
    // save previous time
    ctx->time_unix_prev.seconds = secs;
    ctx->time_unix_prev.second_fraction = frac;
  }
  // done
  if (secs_frac != NULL) {
    *secs_frac = frac;
  }
  return secs;
}

kk_decl_export double kk_time_resolution(kk_context_t* ctx) {
  kk_time_unix_now_prim(NULL, ctx); // initialize
  kk_assert_internal(ctx->time_freq != 0);
  return (1.0 / (double)ctx->time_freq);
}


