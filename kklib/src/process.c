/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
#include "kklib.h"

#define KK_USEC_PER_SEC  1000000

// ----------------------------------------------------------------
// Basic timer for convenience; use micro-seconds to avoid doubles
// (2^63-1) us ~= 292471 years
// ----------------------------------------------------------------
#ifdef WIN32
#include <Windows.h>
static kk_usecs_t kk_to_usecs(LARGE_INTEGER t) {
  static LARGE_INTEGER mfreq; // = 0
  if (mfreq.QuadPart == 0) {
    LARGE_INTEGER f;
    QueryPerformanceFrequency(&mfreq);
    //mfreq.QuadPart = f.QuadPart/I64(1000000);
    if (mfreq.QuadPart == 0) mfreq.QuadPart = 1000;
  }
  // calculate in parts to avoid overflow
  int64_t secs = t.QuadPart / mfreq.QuadPart;
  int64_t frac = t.QuadPart % mfreq.QuadPart;
  kk_usecs_t u = secs*KK_USEC_PER_SEC + ((frac*KK_USEC_PER_SEC)/mfreq.QuadPart);
  return u;
}

static kk_usecs_t kk_timer_now(void) {
  LARGE_INTEGER t;
  QueryPerformanceCounter(&t);
  return kk_to_usecs(t);
}
#else
#include <time.h>
#ifdef CLOCK_REALTIME
static kk_usecs_t kk_timer_now(void) {
  struct timespec t;
  clock_gettime(CLOCK_REALTIME, &t);
  return ((kk_usecs_t)t.tv_sec * KK_USEC_PER_SEC) + ((kk_usecs_t)t.tv_nsec/1000);
}
#else
// low resolution timer
static kk_usecs_t kk_timer_now(void) {
  int64_t t = (int64_t)clock();
  // calculate in parts to avoid overflow
  int64_t secs = t / (int64_t)CLOCKS_PER_SEC;
  int64_t frac = t % (int64_t)CLOCKS_PER_SEC;
  return (secs*KK_USEC_PER_SEC + ((frac*KK_USEC_PER_SEC)/CLOCKS_PER_SEC);
}
#endif
#endif

static kk_usecs_t kk_timer_diff;

kk_timer_t kk_timer_start(void) {
  if (kk_timer_diff == 0) {
    kk_timer_t t0 = kk_timer_now();
    kk_timer_diff = kk_timer_now() - t0;
    if (kk_timer_diff==0) kk_timer_diff = 1;
  }
  return kk_timer_now();
}

kk_usecs_t kk_timer_end(kk_timer_t start) {
  kk_usecs_t end = kk_timer_now();
  return (end - start - kk_timer_diff);
}


// --------------------------------------------------------
// Basic process statistics
// --------------------------------------------------------

#if defined(WIN32)
#include <Windows.h>
#include <Psapi.h>
#pragma comment(lib,"psapi.lib")

static kk_msecs_t kk_filetime_msecs(const FILETIME* ftime) {
  ULARGE_INTEGER i;
  i.LowPart = ftime->dwLowDateTime;
  i.HighPart = ftime->dwHighDateTime;
  kk_msecs_t msecs = (i.QuadPart / 10000); // FILETIME is in 100 nano seconds
  return msecs;
}

void kk_process_info(kk_msecs_t* utime, kk_msecs_t* stime, size_t* peak_rss, size_t* page_faults, size_t* page_reclaim, size_t* peak_commit) {
  FILETIME ct;
  FILETIME ut;
  FILETIME st;
  FILETIME et;
  GetProcessTimes(GetCurrentProcess(), &ct, &et, &st, &ut);
  *utime = kk_filetime_msecs(&ut);
  *stime = kk_filetime_msecs(&st);

  PROCESS_MEMORY_COUNTERS info;
  GetProcessMemoryInfo(GetCurrentProcess(), &info, sizeof(info));
  *peak_rss = (size_t)info.PeakWorkingSetSize;
  *page_faults = (size_t)info.PageFaultCount;
  *peak_commit = (size_t)info.PeakPagefileUsage;
  *page_reclaim = 0;
}

#elif defined(__unix__) || defined(__unix) || defined(unix) || (defined(__APPLE__) && defined(__MACH__)) || defined(__HAIKU__)
#include <stdio.h>
#include <unistd.h>
#include <sys/resource.h>

#if defined(__APPLE__) && defined(__MACH__)
#include <mach/mach.h>
#endif

#if defined(__HAIKU__)
#include <kernel/OS.h>
#endif

static kk_msecs_t kk_timeval_secs(const struct timeval* tv) {
  return ((kk_msecs_t)tv->tv_sec * 1000L) + ((kk_msecs_t)tv->tv_usec / 1000L);
}

void kk_process_info(kk_msecs_t* utime, kk_msecs_t* stime, size_t* peak_rss, size_t* page_faults, size_t* page_reclaim, size_t* peak_commit) {
  struct rusage rusage;
  getrusage(RUSAGE_SELF, &rusage);
#if !defined(__HAIKU__)
#if defined(__APPLE__) && defined(__MACH__)
  *peak_rss = rusage.ru_maxrss;  // apple reports in bytes
#else
  *peak_rss = rusage.ru_maxrss * 1024;
#endif
  *page_faults = rusage.ru_majflt;
  *page_reclaim = rusage.ru_minflt;
  *peak_commit = 0;
#else
  // Haiku does not have (yet?) a way to
  // get these stats per process
  thread_info tid;
  area_info mem;
  ssize_t c;
  *peak_rss = 0;
  *page_faults = 0;
  *page_reclaim = 0;
  *peak_commit = 0;
  get_thread_info(find_thread(0), &tid);
  while (get_next_area_info(tid.team, &c, &mem) == B_OK) {
      *peak_rss += mem.ram_size;
  }
#endif
  *utime = kk_timeval_secs(&rusage.ru_utime);
  *stime = kk_timeval_secs(&rusage.ru_stime);
}

#else
#ifndef __wasi__
// WebAssembly instances are not processes
#pragma message("define a way to get process info")
#endif

static kk_process_info(kk_msecs_t* utime, kk_msecs_t* stime, size_t* peak_rss, size_t* page_faults, size_t* page_reclaim, size_t* peak_commit) {
  *peak_rss = 0;
  *page_faults = 0;
  *page_reclaim = 0;
  *peak_commit = 0;
  *utime = 0;
  *stime = 0;
}
#endif
