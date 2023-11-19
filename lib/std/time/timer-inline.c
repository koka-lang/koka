// #include "std_time_timer.h";
#include "std_core_types.h"

/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

static kk_std_core_types__tuple2_ kk_timer_ticks_tuple(kk_context_t* _ctx) {
  kk_duration_t d = kk_timer_ticks(_ctx);
  // the conversion has about 15 digits of precision
  // we cannot do this more precisely as the api expects the fraction between 0.0 and 2.0 (for leap seconds).  
  double secs = (double)d.seconds;
  double frac = (double)d.attoseconds * 1e-18;
  return kk_std_core_types__new_dash__lp__comma__rp_( kk_double_box(secs, _ctx), kk_double_box(frac, _ctx), _ctx );
}

static double kk_timer_dresolution(kk_context_t* _ctx) {
  int64_t asecs = kk_timer_resolution(_ctx);
  return (double)asecs * 1e-18;
}

kk_std_time_timer__timer kk_timer_init(kk_context_t* _ctx) {
  uv_timer_t* t = kk_malloc(sizeof(uv_timer_t), _ctx);
  uv_timer_init(uvloop(), t);
  return kk_std_time_timer__new_Timer((kk_std_core_types__intptr__t) t, _ctx);
}

kk_unit_t kk_timer_stop(kk_std_time_timer__timer timer, kk_context_t* _ctx) {
  uv_timer_t* uv_timer = (uv_timer_t*)timer.internal;
  kk_uv_callback_t* wrapper = (kk_uv_callback_t*)uv_timer->data;
  kk_function_drop(wrapper->callback, _ctx);
  kk_free(wrapper, _ctx);
  uv_timer_stop(uv_timer);
  return kk_Unit;
}

void kk_uv_timer_unit_cb(uv_timer_t* uv_timer) {
  kk_context_t* _ctx = kk_get_context();
  kk_uv_callback_t* wrapper = (kk_uv_callback_t*)uv_timer->data;
  kk_function_t callback = wrapper->callback;
  if (uv_timer_get_repeat(uv_timer) == 0) {
    kk_free(wrapper, _ctx);
  }
  kk_function_t cb = kk_function_dup(callback, _ctx);
  kk_function_call(void, (kk_function_t, kk_context_t*), cb, (cb, _ctx), _ctx);
}

kk_std_core__error kk_timer_start(kk_std_time_timer__timer timer, int64_t timeout, int64_t repeat, kk_function_t cb, kk_context_t* _ctx) {
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*)timer.internal, _ctx); 
  int ret = uv_timer_start((uv_timer_t*)timer.internal, kk_uv_timer_unit_cb, timeout, repeat);
  if (ret < 0) {
    kk_free(wrapper, _ctx);
    return kk_async_error_from_errno(ret, _ctx);
  } else {
    return kk_std_core__new_Ok(kk_unit_box(kk_Unit), _ctx);
  }
}

kk_std_core__error kk_timer_again(kk_std_time_timer__timer timer, kk_context_t* _ctx) {
  int ret = uv_timer_again((uv_timer_t*)timer.internal);
  if (ret < 0) {
    return kk_async_error_from_errno(ret, _ctx);
  } else {
    return kk_std_core__new_Ok(kk_unit_box(kk_Unit), _ctx);
  }
}

kk_unit_t kk_timer_set_repeat(kk_std_time_timer__timer timer, int64_t repeat, kk_context_t* _ctx) {
  uv_timer_set_repeat((uv_timer_t*)timer.internal, repeat);
  return kk_Unit;
}

int64_t kk_timer_get_repeat(kk_std_time_timer__timer timer, kk_context_t* _ctx) {
  uint64_t repeat = uv_timer_get_repeat((uv_timer_t*)timer.internal);
  return repeat;
}

int64_t kk_timer_get_due_in(kk_std_time_timer__timer timer, kk_context_t* _ctx) {
  uint64_t due_in = uv_timer_get_due_in((uv_timer_t*)timer.internal);
  return due_in;
}