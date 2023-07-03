#pragma once
#ifndef KK_PROCESS_H
#define KK_PROCESS_H

/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/
typedef int64_t kk_msecs_t;
void kk_process_info(kk_msecs_t* utime, kk_msecs_t* stime, 
                     size_t* peak_rss, size_t* page_faults, size_t* page_reclaim, size_t* peak_commit);


#endif // include guard
