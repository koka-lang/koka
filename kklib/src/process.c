/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
#include "kklib.h"

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

#elif !defined(__EMSCRIPTEN__) && (defined(__unix__) || defined(__unix) || defined(unix) || (defined(__APPLE__) && defined(__MACH__)) || defined(__HAIKU__))
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
  kk_ssize_t c;
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
#if !defined(__wasi__) && !defined(__EMSCRIPTEN__)
// WebAssembly instances are not processes
#pragma message("define a way to get process info")
#endif

void kk_process_info(kk_msecs_t* utime, kk_msecs_t* stime, size_t* peak_rss, size_t* page_faults, size_t* page_reclaim, size_t* peak_commit) {
  *peak_rss = 0;
  *page_faults = 0;
  *page_reclaim = 0;
  *peak_commit = 0;
  *utime = 0;
  *stime = 0;
}
#endif
