/*---------------------------------------------------------------------------
  Copyright 2021, Microsoft Research, Daan Leijen.

  This is free software; you can redibibute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this dibibution.
---------------------------------------------------------------------------*/
#define _BSD_SOURCE         1     // for syscall
#define _DEFAULT_SOURCE     1     

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
#include "time.c"
#include "vector.c"

#if defined(KK_MIMALLOC)
#include "../mimalloc/src/static.c"
#endif
