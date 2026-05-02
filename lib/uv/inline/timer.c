
#if __EMSCRIPTEN__

void kk_handle_free(void *p, kk_block_t *block, kk_context_t *_ctx) {
    kk_unused(block); // block is freed by the runtime after this call
    kk_wasm_timer_t* hndcb = (kk_wasm_timer_t*)p;
    kk_free(hndcb, _ctx); // Free the payload memory
    kk_wasm_loop_unref(_ctx); // Handle freed, drop loop ref
}

// For wasm, the internal pointer is directly a kk_wasm_timer_t* (no embedded uv struct offset)
#define kk_tm_borrow_internal(hnd) ((kk_wasm_timer_t*)kk_cptr_unbox_borrowed(hnd.internal, kk_context()))

EMSCRIPTEN_KEEPALIVE void wasm_timer_callback(kk_wasm_timer_t* timer_info){
  kk_context_t* _ctx = kk_get_context();
  kk_function_t callback = timer_info->callback;
  if (timer_info->repeat_ms == 0) {
    timer_info->callback = kk_function_null(_ctx);
    kk_unit_t res = kk_unit_callback(callback, _ctx);
    return;
  } else {
    callback = kk_function_dup(callback, _ctx);
    kk_unit_t res = kk_unit_callback(callback, _ctx);
    return;
  }
}

EM_JS(int, start_timer, (kk_wasm_timer_t* timer_info, int64_t interval), {
  function wasm_callback() {
    _wasm_timer_callback(timer_info);
  }
  return setInterval(wasm_callback, Number(interval));
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
  kk_wasm_loop_ref(_ctx); // Keep loop alive while this handle exists
  kk_wasm_timer_t* timer_info = kk_malloc(sizeof(kk_wasm_timer_t), _ctx);
  kk_box_t timer_box = kk_cptr_raw_box(&kk_handle_free, (void*)timer_info, _ctx);
  kk_uv_timer__timer t = kk_uv_timer__new_Timer(timer_box, _ctx);
  timer_info->callback = kk_function_null(_ctx);
  return t;
}

kk_unit_t kk_wasm_timer_stop(kk_uv_timer__timer timer, kk_context_t* _ctx) {
  kk_wasm_timer_t* timer_info = kk_tm_borrow_internal(timer);
  if (kk_likely(timer_info->timer != 0)) {
    stop_timer(timer_info->timer, timer_info->repeat_ms != 0);
  }
  if (kk_likely(!kk_function_is_null(timer_info->callback, _ctx))) {
    kk_function_drop(timer_info->callback, _ctx);
    timer_info->callback = kk_function_null(_ctx);
  }
  return kk_Unit;
}

kk_std_core_exn__error kk_wasm_timer_start(kk_uv_timer__timer timer, int64_t interval, kk_function_t callback, kk_context_t* _ctx) {
  kk_wasm_timer_t* timer_info = kk_tm_borrow_internal(timer);
  if (kk_unlikely(!kk_function_is_null(timer_info->callback, _ctx))) {
    // If there's already a callback, the timer is still busy on a previous request
    kk_function_drop(callback, _ctx);
    return kk_wasm_error("timer is busy", _ctx);
  }
  timer_info->callback = callback;
  timer_info->repeat_ms = interval;
  timer_info->timer = start_timer(timer_info, interval);
  return kk_std_core_types__new_Ok(kk_unit_box(kk_Unit), _ctx);
}

// One-shot variant: schedules a single fire after `timeout` ms via setTimeout.
// `repeat_ms == 0` makes `wasm_timer_callback` null the callback after firing.
EM_JS(int, start_timeout_timer, (kk_wasm_timer_t* timer_info, int64_t timeout), {
  function wasm_callback() {
    _wasm_timer_callback(timer_info);
  }
  return setTimeout(wasm_callback, Number(timeout));
});

kk_std_core_exn__error kk_wasm_timer_start_once(kk_uv_timer__timer timer, int64_t timeout, kk_function_t callback, kk_context_t* _ctx) {
  kk_wasm_timer_t* timer_info = kk_tm_borrow_internal(timer);
  if (kk_unlikely(!kk_function_is_null(timer_info->callback, _ctx))) {
    kk_function_drop(callback, _ctx);
    return kk_wasm_error("timer is busy", _ctx);
  }
  timer_info->callback = callback;
  timer_info->repeat_ms = 0; // marks one-shot
  timer_info->timer = start_timeout_timer(timer_info, timeout);
  return kk_std_core_types__new_Ok(kk_unit_box(kk_Unit), _ctx);
}

#else

#define kk_tm_borrow_internal(hnd) kk_borrow_internal_as(timer, hnd)

// Initialize the timer handle
kk_uv_timer__timer kk_libuv_timer_init(kk_context_t* _ctx) {
  kk_timer_t* handle = kk_malloc(sizeof(kk_timer_t), _ctx);
  handle->callback = kk_function_null(_ctx);
  // Wrap the uv / kk struct in a reference counted box value type
  kk_uv_timer__timer t = kk_uv_timer__new_Timer(kk_timer_box(handle, _ctx), _ctx);
  uv_timer_init(uvloop(), &(handle->uv)); // Timer initialization never fails
  return t;
}

// Stop timer and remove callback - the timer can be reused with kk_libuv_timer_start
kk_unit_t kk_libuv_timer_stop(kk_uv_timer__timer timer, kk_context_t* _ctx) {
  kk_timer_t* kk_timer = kk_tm_borrow_internal(timer);
  if (kk_likely(!kk_function_is_null(kk_timer->callback, _ctx))) {
    kk_function_drop(kk_timer->callback, _ctx);
    kk_timer->callback = kk_function_null(_ctx);
  }
  uv_timer_stop(&kk_timer->uv);
  return kk_Unit;
}

// The uv callback for the timer.
//
// Note: the user callback is allowed to call `stop` (or even start a fresh
// repeat) on this same timer while we are still inside this frame. libuv
// permits this; we only access `kk_timer->callback` again when the next
// fire happens (uv reschedules itself), so the re-entrancy is safe.
void kk_uv_timer_unit_callback(uv_timer_t* uv_timer) {
  kk_context_t* _ctx = kk_get_context();
  kk_timer_t* kk_timer = (kk_timer_t*)uv_timer;
  kk_function_t callback = kk_timer->callback; // Get the callback
  if (uv_timer_get_repeat(uv_timer) == 0) { // If this is a one-shot timer, remove it
    kk_timer->callback = kk_function_null(_ctx);
  } else { // Otherwise, we need to dup the callback, as it will be called again
    callback = kk_function_dup(callback, _ctx);
  }
  kk_unit_callback(callback, _ctx);
}

kk_std_core_exn__error kk_libuv_timer_start(kk_uv_timer__timer timer, int64_t interval, kk_function_t callback, kk_context_t* _ctx) {
  kk_timer_t* uv_timer = kk_tm_borrow_internal(timer);
  int status = UV_OK;

  if (kk_unlikely(!kk_function_is_null(uv_timer->callback, _ctx))) {
    // If there's already a callback, the timer is still busy with a previous request
    status = UV_EBUSY;
  } else {
    uv_timer->callback = callback;
    status = uv_timer_start((uv_timer_t*)uv_timer, kk_uv_timer_unit_callback, interval, interval);
  }

  if (status != UV_OK) {
    uv_timer->callback = kk_function_null(_ctx);
    kk_function_drop(callback, _ctx);
    return kk_uv_error_from_errno(status, _ctx);
  } else {
    return kk_std_core_types__new_Ok(kk_unit_box(kk_Unit), _ctx);
  }
}

// One-shot: schedules `callback` to fire once after `timeout` ms.
// The unit-callback nulls `kk_timer->callback` when uv_timer_get_repeat==0,
// so the timer self-cleans after firing.
kk_std_core_exn__error kk_libuv_timer_start_once(kk_uv_timer__timer timer, int64_t timeout, kk_function_t callback, kk_context_t* _ctx) {
  kk_timer_t* uv_timer = kk_tm_borrow_internal(timer);
  int status = UV_OK;

  if (kk_unlikely(!kk_function_is_null(uv_timer->callback, _ctx))) {
    status = UV_EBUSY;
  } else {
    uv_timer->callback = callback;
    status = uv_timer_start((uv_timer_t*)uv_timer, kk_uv_timer_unit_callback, timeout, 0);
  }

  if (status != UV_OK) {
    uv_timer->callback = kk_function_null(_ctx);
    kk_function_drop(callback, _ctx);
    return kk_uv_error_from_errno(status, _ctx);
  } else {
    return kk_std_core_types__new_Ok(kk_unit_box(kk_Unit), _ctx);
  }
}

#endif

//////////////////////////////////////////////////////
// Commonalities between emscripten / UV
// Patch over the differences in a few cases
////////////////////////////////////////////////////// 
kk_uv_timer__timer kk_timer_init(kk_context_t* _ctx) {
  #ifdef __EMSCRIPTEN__
    return kk_wasm_timer_init(_ctx);
  #else
    return kk_libuv_timer_init(_ctx);
  #endif
}

kk_unit_t kk_timer_stop(kk_uv_timer__timer timer, kk_context_t* _ctx) {
  #ifdef __EMSCRIPTEN__
    return kk_wasm_timer_stop(timer, _ctx);
  #else
    return kk_libuv_timer_stop(timer, _ctx);
  #endif
}

// Start a repeating timer with `interval` ms between fires. The first fire is
// also after `interval` ms, matching JS `setInterval` semantics on every backend.
kk_std_core_exn__error kk_timer_start(kk_uv_timer__timer timer, int64_t interval, kk_function_t callback, kk_context_t* _ctx) {
  #ifdef __EMSCRIPTEN__
    return kk_wasm_timer_start(timer, interval, callback, _ctx);
  #else
    return kk_libuv_timer_start(timer, interval, callback, _ctx);
  #endif
}

// Start a one-shot timer that fires once after `timeout` ms.
// The timer's callback is automatically released after firing.
kk_std_core_exn__error kk_timer_start_once(kk_uv_timer__timer timer, int64_t timeout, kk_function_t callback, kk_context_t* _ctx) {
  #ifdef __EMSCRIPTEN__
    return kk_wasm_timer_start_once(timer, timeout, callback, _ctx);
  #else
    return kk_libuv_timer_start_once(timer, timeout, callback, _ctx);
  #endif
}

// Cancel a timer scheduled via `set-timeout` (called from clear-timeout).
// Owns `boxed_timer` (the boxed timer struct returned by `set-timeout`).
// `kk_timer_stop` only *borrows* the timer (the Koka decl uses `^t`), so we
// must explicitly drop it after stopping; the drop schedules `uv_close`
// which is what reclaims the underlying uv handle.
kk_unit_t kk_clear_timeout(kk_box_t boxed_timer, kk_context_t* _ctx) {
  kk_uv_timer__timer timer = kk_uv_timer__timer_unbox(boxed_timer, KK_OWNED, _ctx);
  kk_timer_stop(timer, _ctx);
  kk_uv_timer__timer_drop(timer, _ctx);
  return kk_Unit;
}
