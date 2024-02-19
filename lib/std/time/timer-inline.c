// #include "std_time_timer.h";
#include "std_core_types.h"

/*---------------------------------------------------------------------------
  Copyright 2020-2021, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

static kk_std_core_types__tuple2 kk_timer_ticks_tuple(kk_context_t* _ctx) {
  kk_duration_t d = kk_timer_ticks(_ctx);
  // the conversion has about 15 digits of precision
  // we cannot do this more precisely as the api expects the fraction between 0.0 and 2.0 (for leap seconds).
  double secs = (double)d.seconds;
  double frac = (double)d.attoseconds * 1e-18;
  return kk_std_core_types__new_Tuple2( kk_double_box(secs, _ctx), kk_double_box(frac, _ctx), _ctx );
}

static double kk_timer_dresolution(kk_context_t* _ctx) {
  int64_t asecs = kk_timer_resolution(_ctx);
  return (double)asecs * 1e-18;
}

#if __EMSCRIPTEN__
EMSCRIPTEN_KEEPALIVE void wasm_timer_callback(kk_wasm_timer_t* timer_info){
  kk_context_t* _ctx = kk_get_context();
  kk_function_t callback = timer_info->callback;
  kk_function_t cb = kk_function_dup(callback, _ctx);
  kk_function_call(kk_unit_t, (kk_function_t, kk_context_t*), callback, (callback, _ctx), _ctx);
  if (timer_info->repeat_ms == 0) {
    kk_free(timer_info, _ctx);
  }
}

EM_JS(int, start_timer, (int64_t timeout, int64_t repeat, kk_wasm_timer_t* timer_info), {
  function cb() {
    _wasm_timer_callback(timer_info);
  }
  const rp = Number(repeat);
  const msx = Number(timeout);
  if (rp != 0) {
    return setInterval(cb, rp);
  } else {
    return setTimeout(cb, msx);
  }
});

EM_JS(void, stop_timer, (int timer, bool repeating), {
  if (timer) {
    if (repeating) {
      clearInterval(timer);
    } else {
      clearTimeout(timer);
    }
  }
});

kk_std_time_timer__timer kk_timer_init(kk_context_t* _ctx) {
  kk_wasm_timer_t* timer_info = kk_malloc(sizeof(kk_wasm_timer_t), _ctx); 
  return kk_std_time_timer__new_Timer((intptr_t)timer_info, _ctx);
}

kk_unit_t kk_timer_stop(kk_std_time_timer__timer timer, kk_context_t* _ctx) {
  kk_wasm_timer_t* timer_info = (kk_wasm_timer_t*)timer.internal;
  if (timer_info == nullptr) return kk_Unit;
  if (timer_info->timer != 0) {
    stop_timer(timer_info->timer, timer_info->repeat_ms != 0);
  }
  kk_function_drop(timer_info->callback, _ctx);
  kk_std_time_timer__timer_drop(timer, _ctx);
  kk_free(timer_info, _ctx);
  return kk_Unit;
}

kk_std_core_exn__error kk_timer_start(kk_std_time_timer__timer timer, int64_t timeout, int64_t repeat, kk_function_t cb, kk_context_t* _ctx) {
  kk_wasm_timer_t* timer_info = (kk_wasm_timer_t*)timer.internal;
  timer_info->callback = cb;
  timer_info->repeat_ms = repeat;
  int ret = start_timer(timeout, repeat, timer_info);
  if (ret < 0) {
    kk_free(timer_info, _ctx);
    kk_function_drop(cb, _ctx);
    kk_define_string_literal(, err_msg, 22, "Failed to start timer", _ctx);
    return kk_std_core_exn__new_Error( kk_std_core_exn__new_Exception( err_msg, kk_std_core_exn__new_ExnSystem(kk_reuse_null, 0, kk_integer_from_int(-1,_ctx), _ctx), _ctx), _ctx );
  } else {
    timer_info->timer = ret;
    return kk_std_core_exn__new_Ok(kk_unit_box(kk_Unit), _ctx);
  }
}

#else
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

kk_std_core_exn__error kk_timer_start(kk_std_time_timer__timer timer, int64_t timeout, int64_t repeat, kk_function_t cb, kk_context_t* _ctx) {
  uv_handle_t* handle = (uv_handle_t*)timer.internal;
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, handle, _ctx); 
  int ret = uv_timer_start((uv_timer_t*)handle, kk_uv_timer_unit_cb, timeout, repeat);
  if (ret < 0) {
    kk_free(wrapper, _ctx);
    return kk_async_error_from_errno(ret, _ctx);
  } else {
    return kk_std_core_exn__new_Ok(kk_unit_box(kk_Unit), _ctx);
  }
}

kk_std_core_exn__error kk_timer_again(kk_std_time_timer__timer timer, kk_context_t* _ctx) {
  int ret = uv_timer_again((uv_timer_t*)timer.internal);
  if (ret < 0) {
    return kk_async_error_from_errno(ret, _ctx);
  } else {
    return kk_std_core_exn__new_Ok(kk_unit_box(kk_Unit), _ctx);
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
#endif