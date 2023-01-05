/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
#include "kklib.h"


/*--------------------------------------------------------------------------------------
  Durations
--------------------------------------------------------------------------------------*/

bool kk_duration_is_zero(kk_duration_t x) {
  return (x.seconds == 0 && x.attoseconds == 0);
}

bool kk_duration_is_gt(kk_duration_t x, kk_duration_t y) {
  kk_assert(x.attoseconds >= 0 && y.attoseconds >= 0);
  return (x.seconds > y.seconds || (x.seconds == y.seconds && x.attoseconds > y.attoseconds));
}


#define KK_NSECS_PER_SEC  KK_I64(1000000000)
#define KK_ASECS_PER_NSEC KK_I64(1000000000)
#define KK_ASECS_PER_MSEC (1000000 * KK_ASECS_PER_NSEC)
#define KK_ASECS_PER_SEC  (KK_NSECS_PER_SEC * KK_ASECS_PER_NSEC)

kk_duration_t kk_duration_from_secs(int64_t secs) {
  kk_duration_t d;
  d.seconds = secs;
  d.attoseconds = 0;
  return d;
}

kk_duration_t kk_duration_zero(void) {
  return kk_duration_from_secs(0);
}

kk_duration_t kk_duration_norm(kk_duration_t x) {
  while (x.attoseconds < 0) {
    x.seconds--;
    x.attoseconds += KK_ASECS_PER_SEC;
  }
  while (x.attoseconds >= KK_ASECS_PER_SEC) {
    x.seconds++;
    x.attoseconds -= KK_ASECS_PER_SEC;
  }
  return x;
}

kk_duration_t kk_duration_neg(kk_duration_t x) {
  kk_duration_t d;
  d.seconds = -x.seconds;
  d.attoseconds = -x.attoseconds;
  return kk_duration_norm(d);
}

kk_duration_t kk_duration_add(kk_duration_t x, kk_duration_t y) {
  kk_duration_t z;
  z.seconds = x.seconds + y.seconds;
  z.attoseconds = x.attoseconds + y.attoseconds;
  return kk_duration_norm(z);
}

kk_duration_t kk_duration_sub(kk_duration_t x, kk_duration_t y) {
  return kk_duration_add(x, kk_duration_neg(y));
}

kk_duration_t kk_duration_from_nsecs(int64_t nsecs) {
  kk_duration_t d;
  d.seconds = nsecs / KK_NSECS_PER_SEC;
  d.attoseconds = (nsecs % KK_NSECS_PER_SEC) * KK_ASECS_PER_NSEC;
  return kk_duration_norm(d);
}

/*--------------------------------------------------------------------------------------------------
  Timer ticks
--------------------------------------------------------------------------------------------------*/

#ifdef WIN32
#include <Windows.h>
static kk_duration_t kk_timer_ticks_prim(kk_context_t* ctx) {
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
  kk_duration_t d;
  d.seconds = t.QuadPart / ctx->timer_freq;
  int64_t frac = t.QuadPart % ctx->timer_freq;
  int64_t resolution = KK_ASECS_PER_SEC / ctx->timer_freq;
  d.attoseconds = frac * resolution;
  return kk_duration_norm(d);
}

#else

#include <time.h>

#if (defined(CLOCK_REALTIME) || defined(CLOCK_MONOTONIC))
#if !defined(CLOCK_MONOTONIC)
#define CLOCK_MONOTONIC  CLOCK_REALTIME
#endif

// high res timer
static kk_duration_t kk_timer_ticks_prim(kk_context_t* ctx) {
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
  kk_duration_t d;
  d.seconds = t.tv_sec;
  d.attoseconds = t.tv_nsec * KK_ASECS_PER_NSEC;
  return kk_duration_norm(d);
}

#else
// low resolution timer
#pragma message("using low-res timer on this platform")
static kk_duration_t kk_timer_ticks_prim(kk_context_t* ctx) {
  if (ctx->timer_freq == 0) {
    ctx->timer_freq = (int64_t)CLOCKS_PER_SEC;
    if (ctx->timer_freq <= 0) { ctx->timer_freq = 1000; }
  }
  int64_t t = (int64_t)clock();
  // calculate in parts for precision
  kk_duration_t d;
  d.seconds = t / ctx->timer_freq;
  const int64_t frac = t % ctx->timer_freq;
  const int64_t resolution = KK_ASECS_PER_SEC / ctx->timer_freq;
  d.attoseconds = frac * resolution;
  return kk_duration_norm(d);
}
#endif
#endif

kk_decl_export kk_duration_t kk_timer_ticks(kk_context_t* ctx) {
  const kk_duration_t d = kk_timer_ticks_prim(ctx);
  // init previous and delta
  if kk_unlikely(kk_duration_is_zero(ctx->timer_prev)) {
    ctx->timer_prev = d;
    ctx->timer_delta = d;    
  }
  // check monotonicity
  else if kk_unlikely(kk_duration_is_gt(ctx->timer_prev, d)) {
    // ouch, clock ran backward! 
    // we adjust the delta to return the previous time + 1ns to maintain monotonicity.
    // that is the return value is: d - new_delta == timer_prev + 1ns
    // and thus: new_delta = d - timer_prev - 1ns
    ctx->timer_delta = kk_duration_sub(kk_duration_sub(d, ctx->timer_prev), kk_duration_from_nsecs(1));
  }
  // save time in previous and adjust with the delta
  ctx->timer_prev = d;
  return kk_duration_sub(d, ctx->timer_delta);
}

kk_decl_export kk_asecs_t kk_timer_resolution(kk_context_t* ctx) {
  kk_timer_ticks_prim(ctx); // initialize
  kk_assert_internal(ctx->timer_freq != 0);
  return (KK_ASECS_PER_SEC / ctx->timer_freq);
}

/*--------------------------------------------------------------------------------------------------
  Current Time
--------------------------------------------------------------------------------------------------*/

#ifdef WIN32
#define KK_100NSECS_PER_SEC  KK_I64(10000000)
#define KK_UNIX_EPOCH        KK_I64(11644473600)  // seconds since 1601-01-01 UTC to 1970-01-01 (Unix epoch)
static kk_duration_t kk_time_unix_now_prim(kk_context_t* ctx) {
  FILETIME ft;
  GetSystemTimeAsFileTime(&ft);
  LARGE_INTEGER ti;
  ti.LowPart = ft.dwLowDateTime;
  ti.HighPart = (LONG)ft.dwHighDateTime;
  int64_t t = ti.QuadPart; // t is the time in 100 nano seconds intervals since 1601-01-01 UTC.
  kk_duration_t d;
  d.seconds = (t / KK_100NSECS_PER_SEC) - KK_UNIX_EPOCH;
  d.attoseconds = (t % KK_100NSECS_PER_SEC) * 100 * KK_ASECS_PER_NSEC;
  if (ctx->time_freq == 0) {
    // initialize
    ctx->time_freq = KK_100NSECS_PER_SEC;
  }
  // done
  return kk_duration_norm(d);
}
#else

#include <time.h>
#if defined(CLOCK_REALTIME)
// high res time
static kk_duration_t kk_time_unix_now_prim(kk_context_t* ctx) {
  if (ctx->time_freq==0) {
    struct timespec tres = { 0, 0 };
    clock_getres(CLOCK_REALTIME, &tres);
    if (tres.tv_sec == 0 && tres.tv_nsec > 0 && tres.tv_nsec <= KK_NSECS_PER_SEC) {
      kk_assert((KK_NSECS_PER_SEC % tres.tv_nsec) == 0);  
      ctx->time_freq = (KK_NSECS_PER_SEC / tres.tv_nsec);
    }
    else if (tres.tv_sec == 1 && tres.tv_nsec == 0) {
      ctx->time_freq = 1;
    }
    else {
      kk_assert(false); // should never happen?
      ctx->time_freq = KK_NSECS_PER_SEC;
    }
  }
  struct timespec t;
  clock_gettime(CLOCK_REALTIME, &t);
  kk_duration_t d;
  d.seconds = t.tv_sec;
  d.attoseconds = t.tv_nsec * KK_ASECS_PER_NSEC;
  return kk_duration_norm(d);
}

#else
// portable 1s resolution time
static kk_duration_t kk_time_unix_now_prim(kk_context_t* ctx) {
  if (ctx->time_freq == 0) {
    ctx->time_freq = 1; 
  }
  time_t t;
  time(&t);
  kk_duration_t d;
  d.seconds = t;
  d.attoseconds = 0;
  return kk_duration_norm(d);
}
#endif

#endif

kk_decl_export kk_duration_t kk_time_unix_now(kk_context_t* ctx) {
  kk_duration_t d = kk_time_unix_now_prim(ctx);
  if (kk_duration_is_gt(ctx->time_unix_prev, d)
      // time is set backward!
      // if it is less then 1 second we add a tiny increment as we assume it is due to leap second smearing
      // (so we ensure at least monotonicity during a leap second)
      && !kk_duration_is_gt(ctx->time_unix_prev, kk_duration_add(d,kk_duration_from_secs(1))) ) 
  {
    // keep monotonic and allow to catch up
    d = kk_duration_add(ctx->time_unix_prev, kk_duration_from_nsecs(1));    
  }
  // save previous time
  ctx->time_unix_prev = d;
  return d;
}

kk_decl_export kk_asecs_t kk_time_resolution(kk_context_t* ctx) {
  if (ctx->time_freq == 0) {
    kk_time_unix_now_prim(ctx); // initialize
  }
  kk_assert_internal(ctx->time_freq != 0);
  return (KK_ASECS_PER_SEC / ctx->time_freq);
}
