/*---------------------------------------------------------------------------
  Copyright 2020 Daan Leijen, Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

#include <stdlib.h>
#include "time.h"

static msecs_t clock_diff;

static msecs_t _clock_now(void);

msecs_t _clock_start(void) {
  if (clock_diff == 0.0) {
    msecs_t t0 = _clock_now();
    clock_diff = _clock_now() - t0;
  }
  return _clock_now();
}

msecs_t _clock_end(msecs_t start) {
  msecs_t end = _clock_now();
  return (end - start - clock_diff);
}


// ----------------------------------------------------------------
// Basic timer for convenience; use milli-seconds to avoid doubles
// ----------------------------------------------------------------
#ifdef _WIN32
#include <windows.h>
static msecs_t to_msecs(LARGE_INTEGER t) {
  static LARGE_INTEGER mfreq; // = 0
  if (mfreq.QuadPart == 0LL) {
    LARGE_INTEGER f;
    QueryPerformanceFrequency(&f);
    mfreq.QuadPart = f.QuadPart/1000LL;
    if (mfreq.QuadPart == 0) mfreq.QuadPart = 1;
  }
  return (msecs_t)(t.QuadPart / mfreq.QuadPart);
}

msecs_t _clock_now(void) {
  LARGE_INTEGER t;
  QueryPerformanceCounter(&t);
  return to_msecs(t);
}
#else
#include <time.h>
#ifdef CLOCK_REALTIME
msecs_t _clock_now(void) {
  struct timespec t;
  clock_gettime(CLOCK_REALTIME, &t);
  return ((msecs_t)t.tv_sec * 1000) + ((msecs_t)t.tv_nsec / 1000000);
}
#else
// low resolution timer
msecs_t _clock_now(void) {
  return ((msecs_t)clock() / ((msecs_t)CLOCKS_PER_SEC / 1000));
}
#endif
#endif
