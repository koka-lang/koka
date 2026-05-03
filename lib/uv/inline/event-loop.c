/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

#ifdef __EMSCRIPTEN__
#include <emscripten.h>
#include <emscripten/html5.h>
//////////////////////////////////////////////////////
// Event Loop for Emscripten
//////////////////////////////////////////////////////

// Sentinel box: when its refcount reaches zero, its free function cancels
// the main loop (this is how a wasm program exits cleanly once there is no
// more work to do).
//
// Lifetime story:
//   - `kk_wasm_sentinel_init` creates the box (refcount = 1).
//   - `kk_emscripten_loop_run` drops the initial reference; from that point
//     the refcount equals the number of in-flight handles.
//   - Each handle init dups the sentinel (`kk_wasm_loop_ref`).
//   - Each handle free drops it via `kk_wasm_loop_unref`, which *defers* the
//     drop to the next JS event-loop tick (see comment there for why).
static kk_box_t kk_wasm_loop_sentinel = { .box = 0 };

static void kk_wasm_sentinel_free(void* p, kk_block_t* block, kk_context_t* _ctx) {
  kk_unused(p);
  kk_unused(block);
  emscripten_cancel_main_loop();
}

// Initialize the sentinel (called once from loop init)
static void kk_wasm_sentinel_init(kk_context_t* _ctx) {
  kk_wasm_loop_sentinel = kk_cptr_raw_box(&kk_wasm_sentinel_free, NULL, _ctx);
}

// Dup the sentinel to keep the loop alive (called from handle init)
void kk_wasm_loop_ref(kk_context_t* _ctx) {
  kk_box_dup(kk_wasm_loop_sentinel, _ctx);
}

static void kk_wasm_loop_unref_deferred(void* userData) {
  kk_unused(userData);
  kk_box_drop(kk_wasm_loop_sentinel, kk_get_context());
}

// Drop the sentinel (called from handle free).
//
// We defer the decrement to the next JS event-loop tick rather than dropping
// synchronously. Reason: when a JS callback resumes Koka and Koka transitions
// between handles (e.g. one timer fires and immediately a new `wait`/timer is
// scheduled), the dropped handle's `kk_handle_free` runs *before* the next
// handle is created. If we decremented synchronously, the sentinel could
// briefly hit zero and fire its free fn (cancelling the main loop) even
// though more work was about to be scheduled in the same synchronous burst.
//
// Deferring to the next tick lets any new handle dup'd in the same burst
// land first, keeping refcount > 0 across the transition. When user code is
// genuinely done (no new handles created), the deferred drops settle the
// refcount to zero on the next tick and the loop cancels normally.
//
// Note: we cannot use a "user-action lifetime" ref instead, because on wasm
// `kk_emscripten_loop_run` calls `emscripten_set_main_loop(..., true)` which
// never returns -- so there is no point at which Koka can drop a wider ref
// to signal "user code is done"; the sentinel refcount IS that signal.
void kk_wasm_loop_unref(kk_context_t* _ctx) {
  kk_unused(_ctx);
  emscripten_async_call(kk_wasm_loop_unref_deferred, NULL, 0);
}

void one_iter() {
  return;
}

void kk_emscripten_loop_run(kk_context_t* _ctx){
  // Drop our own reference to the sentinel; handles hold the remaining refs.
  // When the last handle is freed, the sentinel's free function cancels this loop.
  kk_box_drop(kk_wasm_loop_sentinel, _ctx);
  emscripten_set_main_loop(one_iter, 0, true);
}
#else

//////////////////////////////////////////////////////
// Allocators 
//////////////////////////////////////////////////////
static inline void* kk_malloc_ctx(size_t size) {
  return kk_malloc(size, kk_get_context());
}

static inline void* kk_realloc_ctx(void* p, size_t size) {
  return kk_realloc(p, size, kk_get_context());
}

static inline void* kk_calloc_ctx(size_t count, size_t size) {
  void* p = kk_malloc(count*size, kk_get_context());
  kk_memset(p, 0, count*size);
  return p;
}

static inline void kk_free_ctx(void* p) {
  kk_free(p, kk_get_context());
}

static inline void kk_uv_alloc_init(kk_context_t* _ctx){
  uv_replace_allocator(kk_malloc_ctx, kk_realloc_ctx, kk_calloc_ctx, kk_free_ctx);
}

//////////////////////////////////////////////////////
// UV Event Loop
//////////////////////////////////////////////////////
static void kk_uv_loop_init(kk_context_t* _ctx) {
  uv_loop_t* loop = kk_malloc(sizeof(uv_loop_t), kk_context());
  kk_set_uv_loop(loop); // Set thread local loop
  uv_loop_init(loop);
}

void kk_uv_loop_run(kk_context_t* _ctx){
  // Run the event loop after the initial startup of the program
  int ret = uv_run(uvloop(), UV_RUN_DEFAULT);
  if (ret != 0){
    kk_warning_message("Event loop closed with status %s\n", uv_err_name(ret));
  }
}

static char* kk_uv_handle_type_str(uv_handle_t* handle) {
  switch (handle->type) {
    case UV_UNKNOWN_HANDLE: return "UNKNOWN";
    case UV_ASYNC: return "ASYNC";
    case UV_CHECK: return "CHECK";
    case UV_FS_EVENT: return "FS_EVENT";
    case UV_FS_POLL: return "FS_POLL";
    case UV_HANDLE: return "HANDLE";
    case UV_IDLE: return "IDLE";
    case UV_NAMED_PIPE: return "NAMED_PIPE";
    case UV_POLL: return "POLL";
    case UV_PREPARE: return "PREPARE";
    case UV_PROCESS: return "PROCESS";
    case UV_STREAM: return "STREAM";
    case UV_TCP: return "TCP";
    case UV_TIMER: return "TIMER";
    case UV_TTY: return "TTY";
    case UV_UDP: return "UDP";
    case UV_SIGNAL: return "SIGNAL";
    case UV_FILE: return "FILE";
    default: return "INVALID";
  }
}

static void kk_uv_loop_walk_cb(uv_handle_t* handle, void* arg) {
  const char* closing_msg = uv_is_closing(handle) ? " [CLOSING]" : "";
  const char* active_msg = uv_is_active(handle) ? " [ACTIVE]" : "";
  kk_warning_message(" - %s handle%s%s\n", kk_uv_handle_type_str(handle), active_msg, closing_msg);
}

static void kk_uv_loop_close(kk_context_t* _ctx) {
  int ret = uv_loop_close(uvloop());
  if (ret != 0) {
    if (ret == UV_EBUSY) {
      kk_warning_message("Event loop closed with open child handles:\n");
      uv_walk(uvloop(), kk_uv_loop_walk_cb, NULL);
    } else {
      kk_warning_message("Event loop close returned error: %s\n", uv_err_name(ret));
    }
  }
  kk_free(uvloop(), _ctx);
}

#endif

//////////////////////////////////////////////////////
// Generic APIs for both emscripten and UV
////////////////////////////////////////////////////// 
static inline void kk_async_alloc_init(kk_context_t* _ctx){
  #if __EMSCRIPTEN__
    return;
  #else 
    return kk_uv_alloc_init(_ctx);
  #endif
}

static void kk_async_loop_init(kk_context_t* _ctx) {
  #if __EMSCRIPTEN__
    kk_wasm_sentinel_init(_ctx);
  #else
    return kk_uv_loop_init(_ctx);
  #endif
}

void kk_async_loop_run(kk_context_t* _ctx){
  // Run the event loop after the initial startup of the program
  #if __EMSCRIPTEN__
    return kk_emscripten_loop_run(_ctx);
  #else 
    return kk_uv_loop_run(_ctx);
  #endif
  
}

static void kk_async_loop_close(kk_context_t* _ctx) {
  #if __EMSCRIPTEN__
    return;
  #else
    return kk_uv_loop_close(_ctx);
  #endif
}
