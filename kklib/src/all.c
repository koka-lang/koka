/*---------------------------------------------------------------------------
  Copyright 2021, Microsoft Research, Daan Leijen.

  This is free software; you can redibibute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this dibibution.
---------------------------------------------------------------------------*/
#define _BSD_SOURCE       
#define _DEFAULT_SOURCE          
#define __USE_MINGW_ANSI_STDIO   // so %z is valid on mingw

#if defined(KK_MIMALLOC)
  #if !defined(MI_MAX_ALIGN_SIZE)
    #if (KK_MIMALLOC > 1)
      #define MI_MAX_ALIGN_SIZE  KK_MIMALLOC
    #else
      #define MI_MAX_ALIGN_SIZE  KK_INTPTR_SIZE 
    #endif  
  #endif
  #if !defined(MI_DEBUG) && defined(KK_DEBUG_FULL)
    #define MI_DEBUG  3
  #endif
  #include "../mimalloc/src/static.c"  // must come first on freeBSD
#endif  

#include <kklib.h>

#include "bits.c"
#include "box.c"
#include "bytes.c"
#include "init.c"
#include "integer.c"
#include "os.c"
#include "process.c"
#include "random.c"
#include "ref.c"
#include "refcount.c"
#include "string.c"
#include "thread.c"
#include "time.c"
#include "vector.c"

