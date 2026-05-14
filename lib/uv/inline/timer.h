/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

#ifdef __EMSCRIPTEN__
#include <emscripten.h>

//////////////////////////////////////////////////////
// Create the wrapper type
// For wasm, we need to keep extra information
//  such as repeat time and the timer id from JS
////////////////////////////////////////////////////// 
typedef struct kk_wasm_timer_s {
  kk_function_t callback;
  int64_t repeat_ms;
  int timer;
} kk_wasm_timer_t;

EMSCRIPTEN_KEEPALIVE void wasm_timer_callback(kk_wasm_timer_t* timer_info);
#else
#include <uv.h>
//////////////////////////////////////////////////////
// Create the UV wrapper type (see core.h for the macro)
//////////////////////////////////////////////////////
declare_uv_handle(uv_timer)
#endif
