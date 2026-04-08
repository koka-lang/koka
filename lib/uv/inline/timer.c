
#if __EMSCRIPTEN__

void kk_handle_free(void *p, kk_block_t *block, kk_context_t *_ctx) {
    kk_wasm_timer_t* hndcb = (kk_wasm_timer_t*)p;
    kk_free(hndcb, kk_context()); // Free the memory used for the callback and box
    kk_free(block, kk_context()); // Free the block memory
}

#define kk_tm_borrow_internal(hnd) kk_borrow_internal_as(wasm_timer, hnd)

EMSCRIPTEN_KEEPALIVE void wasm_timer_callback(kk_wasm_timer_t* timer_info){
  kk_context_t* _ctx = kk_get_context();
  kk_function_t callback = timer_info->callback;
  if (timer_info->repeat_ms == 0) {
    timer_info->callback = kk_function_null(kk_context());
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
  kk_box_t timer_box = kk_cptr_raw_box(&kk_handle_free, (void*)timer_info, kk_context());
  kk_uv_timer__timer t = kk_uv_timer__new_Timer(timer_box, kk_context());
  timer_info->callback = kk_function_null(kk_context());
  return t;
}

kk_unit_t kk_wasm_timer_stop(kk_uv_timer__timer timer, kk_context_t* _ctx) {
  kk_wasm_timer_t* timer_info = kk_tm_borrow_internal(timer);
  if (kk_likely(timer_info->timer != 0)) {
    stop_timer(timer_info->timer, timer_info->repeat_ms != 0);
  }
  if (kk_likely(!kk_function_is_null(timer_info->callback, kk_context()))) {
    kk_function_drop(timer_info->callback, kk_context());
    timer_info->callback = kk_function_null(kk_context());
  }
  return kk_Unit;
}

kk_std_core_exn__error kk_wasm_timer_start(kk_uv_timer__timer timer, int64_t timeout, int64_t repeat, kk_function_t callback, kk_context_t* _ctx) {
  kk_wasm_timer_t* timer_info = kk_tm_borrow_internal(timer);
  if (kk_unlikely(!kk_function_is_null(timer_info->callback, kk_context()))) {
    // If there's already a callback, the timer is still busy on a previous request
    kk_function_drop(callback, kk_context());
    return kk_uv_error_from_errno(UV_EBUSY, kk_context());
  }
  timer_info->callback = callback;
  timer_info->repeat_ms = repeat;
  timer_info->timer = start_timer(timer_info, timeout, repeat);
  return kk_std_core_types__new_Ok(kk_unit_box(kk_Unit), kk_context());
}

#else

#define kk_tm_borrow_internal(hnd) kk_borrow_internal_as(timer, hnd)

// Initialize the timer handle
kk_uv_timer__timer kk_libuv_timer_init(kk_context_t* _ctx) {
  kk_timer_t* handle = kk_malloc(sizeof(kk_timer_t), kk_context());
  handle->callback = kk_function_null(kk_context());
  // Wrap the uv / kk struct in a reference counted box value type
  kk_uv_timer__timer t = kk_uv_timer__new_Timer(kk_timer_box(handle, _ctx), _ctx);
  uv_timer_init(uvloop(), &(handle->uv)); // Timer initialization never fails
  return t;
}

// Stop timer and remove callback - the timer can be reused with kk_libuv_timer_start
kk_unit_t kk_libuv_timer_stop(kk_uv_timer__timer timer, kk_context_t* _ctx) {
  kk_timer_t* kk_timer = kk_tm_borrow_internal(timer);
  if (kk_likely(!kk_function_is_null(kk_timer->callback, kk_context()))) {
    kk_function_drop(kk_timer->callback, kk_context());
    kk_timer->callback = kk_function_null(kk_context());
  }
  uv_timer_stop(&kk_timer->uv);
  return kk_Unit;
}

// The uv callback for the timer
void kk_uv_timer_unit_callback(uv_timer_t* uv_timer) {
  kk_context_t* _ctx = kk_get_context();
  kk_timer_t* kk_timer = (kk_timer_t*)uv_timer;
  kk_function_t callback = kk_timer->callback; // Get the callback
  if (uv_timer_get_repeat(uv_timer) == 0) { // If this is a one-shot timer, remove it
    kk_timer->callback = kk_function_null(kk_context());
  } else { // Otherwise, we need to dup the callback, as it will be called again
    callback = kk_function_dup(callback, kk_context());
  }
  kk_unit_callback(callback, kk_context());
}

kk_std_core_exn__error kk_libuv_timer_start(kk_uv_timer__timer timer, int64_t timeout, int64_t repeat, kk_function_t callback, kk_context_t* _ctx) {
  kk_timer_t* uv_timer = kk_tm_borrow_internal(timer);
  int status = UV_OK;

  if (kk_unlikely(!kk_function_is_null(uv_timer->callback, kk_context()))) {
    // If there's already a callback, the timer is still busy with a previous request
    status = UV_EBUSY;
  } else {
    uv_timer->callback = callback;
    status = uv_timer_start((uv_timer_t*)uv_timer, kk_uv_timer_unit_callback, timeout, repeat);
  }

  if (status != UV_OK) {
    uv_timer->callback = kk_function_null(kk_context());
    kk_function_drop(callback, kk_context());
    return kk_uv_error_from_errno(status, kk_context());
  } else {
    return kk_std_core_types__new_Ok(kk_unit_box(kk_Unit), kk_context());
  }
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

kk_std_core_exn__error kk_timer_start(kk_uv_timer__timer timer, int64_t timeout, int64_t repeat, kk_function_t callback, kk_context_t* _ctx) {
  #ifdef __EMSCRIPTEN__
    return kk_wasm_timer_start(timer, timeout, repeat, callback, kk_context());
  #else
    return kk_libuv_timer_start(timer, timeout, repeat, callback, kk_context());
  #endif
}
