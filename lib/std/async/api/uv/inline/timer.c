/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// #include "../../../../../kklib/include/kklib.h"
// #include "core.h"

#include <uv.h>

static void kk_uv_timer_callback(uv_timer_t* t) {
  kk_uv_handle_callback((uv_handle_t*)t);
}

static void kk_uv_timer_dispose(uv_handle_t* h, void* arg, kk_context_t* ctx) {
  uv_timer_stop((uv_timer_t*)h);
  kk_uv_handle_close(h);
}

kk_std_core_exn__error kk_timer_setup(kk_uv_loop_t loop, int64_t millisecs, kk_function_t cb, kk_context_t* ctx) {
  uv_timer_t* t;
  int err = kk_uv_handle_create(sizeof(uv_timer_t), cb, (uv_handle_t**)&t, ctx);
  if (err!=0) return kk_error_from_uv_errno(err,ctx);
  err = uv_timer_init(kk_uv_loop(loop,ctx),t);
  if (err!=0) { kk_uv_handle_free((uv_handle_t*)t,ctx); return kk_error_from_uv_errno(err,ctx); }
  err = uv_timer_start(t, &kk_uv_timer_callback, (millisecs < 0 ? 0 : (uint64_t)millisecs), 0 /* no repeat */);
  if (err!=0) { kk_uv_handle_close((uv_handle_t*)t); return kk_error_from_uv_errno(err,ctx); }
  return kk_result_uv_handle_dispose((uv_handle_t*)t,NULL,&kk_uv_timer_dispose,ctx);
}

kk_std_core_exn__error kk_immediate_setup(kk_uv_loop_t loop, kk_function_t cb, kk_context_t* ctx) {
  return kk_timer_setup(loop,0,cb,ctx);
}



