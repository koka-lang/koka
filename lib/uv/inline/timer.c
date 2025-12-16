
#if __EMSCRIPTEN__

void kk_handle_free(void *p, kk_block_t *block, kk_context_t *_ctx) {
    kk_wasm_timer_t* hndcb = (kk_wasm_timer_t*)p;
    kk_free(hndcb, kk_context()); // Free the memory used for the callback and box
    kk_free(block, kk_context()); // Free the block memory
}

#define kk_tm_to_uv(hnd) kk_owned_handle_to_uv_handle(wasm_timer, hnd)

EMSCRIPTEN_KEEPALIVE void wasm_timer_callback(kk_wasm_timer_t* timer_info){
  kk_context_t* _ctx = kk_get_context();
  kk_function_t callback = timer_info->callback;
  if (timer_info->repeat_ms == 0) {
    kk_unit_t res = kk_unit_callback(callback, kk_context());
    return;
  } else {
    callback = kk_function_dup(callback, kk_context());
    kk_unit_t res = kk_unit_callback(callback, kk_context());
    return;
  }
}

EM_JS(int, start_timer, (kk_wasm_timer_t* timer_info, int64_t timeout, int64_t repeat), {
  function wasm_callback() {
    _wasm_timer_callback(timer_info);
  }
  const n_repeat = Number(repeat);
  const n_timeout = Number(timeout);
  if (n_repeat != 0) {
    return setInterval(wasm_callback, n_repeat);
  } else {
    return setTimeout(wasm_callback, n_timeout);
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

kk_uv_timer__timer kk_wasm_timer_init(kk_context_t* _ctx) {
  kk_wasm_timer_t* timer_info = kk_malloc(sizeof(kk_wasm_timer_t), kk_context());
  kk_uv_timer__timer t = uv_handle_to_owned_kk_handle(timer_info, kk_handle_free, timer, Timer);
  timer_info->callback = kk_function_null(kk_context());
  return t;
}

kk_unit_t kk_wasm_timer_finish(kk_uv_timer__timer timer, kk_context_t* _ctx) {
  kk_wasm_timer_t* timer_info = kk_tm_to_uv(timer);
  if (kk_likely(!kk_function_is_null(timer_info->callback, kk_context()))) {
    kk_function_drop(timer_info->callback, kk_context());
  }
  kk_uv_timer__timer_drop(timer, kk_context());
  return kk_Unit;
}

kk_unit_t kk_wasm_timer_stop(kk_uv_timer__timer timer, kk_context_t* _ctx) {
  kk_wasm_timer_t* timer_info = kk_tm_to_uv(timer);
  if (kk_likely(timer_info->timer != 0)) {
    stop_timer(timer_info->timer, timer_info->repeat_ms != 0);
  }
  return kk_Unit;
}

kk_std_core_exn__error kk_wasm_timer_start(kk_uv_timer__timer timer, int64_t timeout, int64_t repeat, kk_function_t callback, kk_context_t* _ctx) {
  kk_wasm_timer_t* timer_info = kk_tm_to_uv(timer);
  timer_info->callback = callback;
  timer_info->repeat_ms = repeat;
  timer_info->timer = start_timer(timer_info, timeout, repeat);
  return kk_std_core_exn__new_Ok(kk_unit_box(kk_Unit), kk_context());
}

#else

#define kk_tm_to_uv(hnd) kk_owned_handle_to_uv_handle(timer, hnd)

// Initialize the timer handle
kk_uv_timer__timer kk_libuv_timer_init(kk_context_t* _ctx) {
  kk_timer_t* handle = kk_malloc(sizeof(kk_timer_t), kk_context());
  handle->callback = kk_function_null(kk_context());
  // Wrap the uv / kk struct in a reference counted box value type
  kk_uv_timer__timer t = uv_handle_to_owned_kk_handle(handle, kk_timer_free, timer, Timer);
  uv_timer_init(uvloop(), (uv_timer_t*)handle); // Timer initialization never fails
  return t;
}

// Stop / pause the timer (doesn't clean up) - the timer can be restarted with the same callback with kk_libuv_timer_again
kk_unit_t kk_libuv_timer_stop(kk_uv_timer__timer timer, kk_context_t* _ctx) {
  uv_timer_t* uv_timer = (uv_timer_t*)kk_tm_to_uv(timer);
  uv_timer_stop(uv_timer);
  return kk_Unit;
}

// Actually clean up the timer 
// This drops the callback first in case it is holding onto the timer - as it does for uv/timer/timer()
kk_unit_t kk_libuv_timer_finish(kk_uv_timer__timer timer, kk_context_t* _ctx) {
  kk_timer_t* kk_timer = kk_tm_to_uv(timer);
  if (kk_likely(!kk_function_is_null(kk_timer->callback, kk_context()))) {
    kk_function_drop(kk_timer->callback, kk_context());
  }
  kk_uv_timer__timer_drop(timer, kk_context());
  return kk_Unit;
}

// The uv callback for the timer
void kk_uv_timer_unit_callback(uv_timer_t* uv_timer) {
  kk_context_t* _ctx = kk_get_context();
  kk_timer_t* kk_timer = (kk_timer_t*)uv_timer;
  kk_function_t callback = kk_timer->callback; // Get the callback
  if (uv_timer_get_repeat(uv_timer) == 0) { // If this is a one-shot timer, just call the callback
    kk_unit_callback(callback, kk_context());
    return;
  } else { // Otherwise, we need to dup the callback, as it will be called again
    callback = kk_function_dup(callback, kk_context());
    kk_unit_callback(callback, kk_context());
    return;
  }
}

kk_std_core_exn__error kk_libuv_timer_start(kk_uv_timer__timer timer, int64_t timeout, int64_t repeat, kk_function_t callback, kk_context_t* _ctx) {
  kk_timer_t* uv_timer = kk_tm_to_uv(timer);
  // TODO: Drop previous callback if any?
  uv_timer->callback = callback;
  int status = uv_timer_start((uv_timer_t*)uv_timer, kk_uv_timer_unit_callback, timeout, repeat);
  // On error, report, and drop callback
  kk_uv_check_err_drops(status, {
    uv_timer->callback = kk_function_null(kk_context());
    kk_function_drop(callback, kk_context());
  })
}

kk_std_core_exn__error kk_libuv_timer_again(kk_uv_timer__timer timer, kk_context_t* _ctx) {
  int status = uv_timer_again((uv_timer_t*)kk_tm_to_uv(timer));
  kk_uv_check(status)
}

kk_unit_t kk_libuv_timer_set_repeat(kk_uv_timer__timer timer, int64_t repeat, kk_context_t* _ctx) {
  uv_timer_set_repeat((uv_timer_t*)kk_tm_to_uv(timer), repeat);
  return kk_Unit;
}

int64_t kk_libuv_timer_get_repeat(kk_uv_timer__timer timer, kk_context_t* _ctx) {
  uint64_t repeat = uv_timer_get_repeat((uv_timer_t*)kk_tm_to_uv(timer));
  return repeat;
}

int64_t kk_libuv_timer_get_due_in(kk_uv_timer__timer timer, kk_context_t* _ctx) {
  uint64_t due_in = uv_timer_get_due_in((uv_timer_t*)kk_tm_to_uv(timer));
  return due_in;
}
#endif

//////////////////////////////////////////////////////
// Commonalities between emscripten / UV
// Patch over the differences in a few cases
////////////////////////////////////////////////////// 
kk_uv_timer__timer kk_timer_init(kk_context_t* _ctx) {
  #ifdef __EMSCRIPTEN__
    return kk_wasm_timer_init(kk_context());
  #else
    return kk_libuv_timer_init(kk_context());
  #endif
}

kk_unit_t kk_timer_stop(kk_uv_timer__timer timer, kk_context_t* _ctx) {
  #ifdef __EMSCRIPTEN__
    return kk_wasm_timer_stop(timer, kk_context());
  #else
    return kk_libuv_timer_stop(timer, kk_context());
  #endif
}

kk_unit_t kk_timer_finish(kk_uv_timer__timer timer, kk_context_t* _ctx) {
  #ifdef __EMSCRIPTEN__
    return kk_wasm_timer_finish(timer, kk_context());
  #else
    return kk_libuv_timer_finish(timer, kk_context());
  #endif
}

kk_std_core_exn__error kk_timer_start(kk_uv_timer__timer timer, int64_t timeout, int64_t repeat, kk_function_t callback, kk_context_t* _ctx) {
  #ifdef __EMSCRIPTEN__
    return kk_wasm_timer_start(timer, timeout, repeat, callback, kk_context());
  #else
    return kk_libuv_timer_start(timer, timeout, repeat, callback, kk_context());
  #endif
}

kk_std_core_exn__error kk_timer_again(kk_uv_timer__timer timer, kk_context_t* _ctx) {
  #ifdef __EMSCRIPTEN__
    return kk_std_core_exn__new_Ok(kk_unit_box(kk_Unit), kk_context());
  #else
    return kk_libuv_timer_again(timer, kk_context());
  #endif
}

kk_unit_t kk_timer_set_repeat(kk_uv_timer__timer timer, int64_t repeat, kk_context_t* _ctx) {
  #ifdef __EMSCRIPTEN__
    return kk_Unit;
  #else
    return kk_libuv_timer_set_repeat(timer, repeat, kk_context());
  #endif
}

int64_t kk_timer_get_repeat(kk_uv_timer__timer timer, kk_context_t* _ctx) {
  #ifdef __EMSCRIPTEN__
    return -1;
  #else
    return kk_libuv_timer_get_repeat(timer, kk_context());
  #endif
}

int64_t kk_timer_get_due_in(kk_uv_timer__timer timer, kk_context_t* _ctx) {
  #ifdef __EMSCRIPTEN__
    return -1;
  #else
    return kk_libuv_timer_get_due_in(timer, kk_context());
  #endif
}