/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

#include <uv.h>

#define UV_OK 0
#define kk_uv_status_code_t kk_uv_status_dash_code__uv_status_code

// Map a libuv status code to a Koka Error value with uv status code enum
kk_std_core_exn__error kk_uv_error_from_errno( int err, kk_context_t* ctx );
