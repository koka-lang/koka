/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// Call a unit callback
static inline kk_unit_t kk_unit_callback(kk_function_t callback, kk_context_t* _ctx) {
  return kk_function_call(kk_unit_t, (kk_function_t, kk_context_t*), callback, (callback, kk_context()), kk_context());
}

// ----------------------------------------------------------------------------
// Common preamble for any handle / request wrapper.
//
// Both `declare_uv_handle` and `declare_uv_req` lay out their wrapper structs
// with `kk_function_t callback` as the first member. That lets generic helpers
// (kk_uv_any_take_callback, etc.) operate on any wrapper through a
// `kk_uv_any_t*` cast without knowing the concrete type — Koka's per-type
// abstract structs already give us subtype safety at the language level.
// ----------------------------------------------------------------------------
typedef struct kk_uv_any_s {
  kk_function_t callback;
} kk_uv_any_t;

static inline bool kk_uv_any_has_callback(kk_uv_any_t* h, kk_context_t* _ctx) {
  return !kk_function_is_null(h->callback, _ctx);
}

static inline kk_function_t kk_uv_any_take_callback(kk_uv_any_t* h, kk_context_t* _ctx) {
  kk_assert_internal(kk_uv_any_has_callback(h, _ctx));
  kk_function_t cb = h->callback;
  h->callback = kk_function_null(_ctx);
  return cb;
}

static inline kk_function_t kk_uv_any_dup_callback(kk_uv_any_t* h, kk_context_t* _ctx) {
  kk_assert_internal(kk_uv_any_has_callback(h, _ctx));
  return kk_function_dup(h->callback, _ctx);
}

static inline void kk_uv_any_set_callback(kk_uv_any_t* h, kk_function_t cb, kk_context_t* _ctx) {
  kk_assert_internal(!kk_uv_any_has_callback(h, _ctx));
  h->callback = cb;
}

// Drop the callback if any. Used in error paths and on close/stop.
static inline void kk_uv_any_drop_references(kk_uv_any_t* h, kk_context_t* _ctx) {
  if (kk_uv_any_has_callback(h, _ctx)) {
    kk_function_drop(h->callback, _ctx);
    h->callback = kk_function_null(_ctx);
  }
}

#ifdef __EMSCRIPTEN__
#include <emscripten.h>

// Loop sentinel ref counting — defined in event-loop.c
void kk_wasm_loop_ref(kk_context_t* _ctx);
void kk_wasm_loop_unref(kk_context_t* _ctx);

// Construct a simple internal error for the wasm/emscripten backend
static inline kk_std_core_exn__error kk_wasm_error(const char* msg, kk_context_t* _ctx) {
  kk_string_t s = kk_string_alloc_dup_valid_utf8(msg, _ctx);
  kk_string_t n = kk_string_alloc_dup_valid_utf8("wasm", _ctx);
  return kk_std_core_types__new_Error(
    kk_std_core_exn__exception_box(
      kk_std_core_exn__new_Exception(s, kk_std_core_exn__new_ExnInternal(kk_reuse_null, 0, n, _ctx), _ctx), _ctx), _ctx);
}

#else
#include <uv.h>

// Thread local uv loop variable
static kk_decl_thread uv_loop_t* kk_uv_loop_default;
void kk_set_uv_loop(uv_loop_t* loop);
uv_loop_t* uvloop();

// UV Okay status code
#define UV_OK 0
// Map a libuv status code to a Koka Error value with uv status code enum
kk_std_core_exn__error kk_uv_error_from_errno( int err, kk_context_t* ctx );

// Forward declarations: defined further below, after declare_uv_handle_base(uv_handle).
static inline void kk_uv_handle_close_cb(uv_handle_t* uvhnd);
static inline void kk_uv_handle_free_fn(void* p, kk_block_t* block, kk_context_t* _ctx);

// ----------------------------------------------------------------------------
// Handle / request struct generation
//
// `declare_uv_handle_base(uv_type)` defines `kk_<type>_t` plus casts:
//   - `<type>_as_kk(uv_<type>_t*)` — cast back from a libuv callback's pointer
//     to the wrapping kk struct (uses offsetof, works regardless of the
//     uv struct's offset).
//   - `kk_<type>_unbox_borrowed(kk_box_t)` — extract a borrowed pointer from
//     a Koka-side boxed any.
//   - `kk_<type>_as_any(kk_<type>_t*)` — cast to the common preamble for
//     generic callback helpers.
//
// `declare_uv_handle(uv_type)` builds on the base with handle-specific casts
// (to/from kk_uv_handle_t) and a `kk_<type>_box` that wraps in a refcounted
// Koka box whose drop schedules `uv_close`.
//
// `declare_uv_req(uv_type)` is similar but does not produce a `_box`: requests
// are short-lived and freed by their completion callback, not via Koka's
// refcount machinery.
// ----------------------------------------------------------------------------

#define declare_uv_handle_base(uv_type) \
  typedef struct kk_##uv_type##_s { \
    kk_function_t callback; \
    uv_type##_t   uv; \
  } kk_##uv_type##_t; \
  static inline kk_##uv_type##_t* uv_type##_as_kk(uv_type##_t* p) { \
    return (kk_##uv_type##_t*) (((char*)p) - offsetof(kk_##uv_type##_t, uv)); \
  } \
  static inline kk_##uv_type##_t* kk_##uv_type##_unbox_borrowed(kk_box_t box, kk_context_t* _ctx) { \
    return ((kk_##uv_type##_t*) kk_cptr_unbox_borrowed(box, _ctx)); \
  } \
  static inline kk_uv_any_t* kk_##uv_type##_as_any(kk_##uv_type##_t* p) { \
    return (kk_uv_any_t*) p; \
  }

// The generic uv_handle wrapper (callback + uv_handle_t). Used by close
// callback machinery which only needs to free the wrapper memory.
declare_uv_handle_base(uv_handle)

#define declare_uv_handle(uv_type) \
  declare_uv_handle_base(uv_type) \
  static inline kk_uv_handle_t* kk_##uv_type##_as_handle(kk_##uv_type##_t* p) { \
    return (kk_uv_handle_t*) p; \
  } \
  static inline kk_uv_handle_t* uv_type##_as_kk_handle(uv_type##_t* p) { \
    return kk_##uv_type##_as_handle(uv_type##_as_kk(p)); \
  } \
  static inline kk_box_t kk_##uv_type##_box(kk_##uv_type##_t* p, kk_context_t* _ctx) { \
    return kk_cptr_raw_box(&kk_uv_handle_free_fn, (void*) p, _ctx); \
  }

#define declare_uv_req(uv_type) \
  declare_uv_handle_base(uv_type)

// ----------------------------------------------------------------------------
// Handle-typed wrappers around the kk_uv_any_t helpers, for the common case
// of operating on a long-lived handle through `kk_uv_handle_t*`.
// ----------------------------------------------------------------------------

#define kk_uv_handle_take_callback(h, _ctx) kk_uv_any_take_callback(kk_uv_handle_as_any(h), _ctx)
#define kk_uv_handle_dup_callback(h, _ctx)  kk_uv_any_dup_callback(kk_uv_handle_as_any(h), _ctx)
#define kk_uv_handle_set_callback(h, cb, _ctx) kk_uv_any_set_callback(kk_uv_handle_as_any(h), cb, _ctx)
#define kk_uv_handle_drop_references(h, _ctx)  kk_uv_any_drop_references(kk_uv_handle_as_any(h), _ctx)

// Try to set a callback; returns UV_EBUSY if one is already set, UV_OK otherwise.
// Useful for guarding against concurrent operations on the same handle.
static inline int kk_uv_handle_try_set_callback(kk_uv_handle_t* h, kk_function_t cb, kk_context_t* _ctx) {
  kk_uv_any_t* a = kk_uv_handle_as_any(h);
  if (kk_uv_any_has_callback(a, _ctx)) {
    return UV_EBUSY;
  }
  kk_uv_any_set_callback(a, cb, _ctx);
  return UV_OK;
}

// ----------------------------------------------------------------------------
// Allocation helpers
// ----------------------------------------------------------------------------

// Allocate a wrapper and run uv_<type>_init. On failure the wrapper is freed
// and `status` is non-zero; on success the callback slot is initialized to
// null. Caller is then responsible for boxing or freeing the handle.
//
// Use as: malloc_and_init_handle(uv_timer, hnd, status, uvloop(), &hnd->uv);
#define malloc_and_init_handle(uv_type, hnd, status, ...) \
  kk_##uv_type##_t* hnd = kk_malloc(sizeof(kk_##uv_type##_t), _ctx); \
  status = uv_type##_init(__VA_ARGS__); \
  if (status != UV_OK) { \
    kk_free(hnd, _ctx); \
  } else { \
    hnd->callback = kk_function_null(_ctx); \
  }

// Allocate a request wrapper and stash the callback. Requests don't need
// uv_*_init; they're populated and submitted in one shot.
#define malloc_req(uv_type, hnd, cb) \
  kk_##uv_type##_t* hnd = kk_malloc(sizeof(kk_##uv_type##_t), _ctx); \
  hnd->callback = cb;

// ----------------------------------------------------------------------------
// Close / free
// ----------------------------------------------------------------------------

// Close callback that libuv calls once a handle has been fully closed.
// Frees the wrapper memory; the callback field must already be null.
static inline void kk_uv_handle_close_cb(uv_handle_t* uvhnd) {
  kk_context_t* _ctx = kk_get_context();
  kk_uv_handle_t* kk_hnd = uv_handle_as_kk(uvhnd);
  kk_assert_internal(kk_function_is_null(kk_hnd->callback, _ctx));
  kk_free(kk_hnd, _ctx);
}

// Box drop function for refcounted handles. Schedules uv_close which
// eventually invokes kk_uv_handle_close_cb to free the memory.
static inline void kk_uv_handle_free_fn(void* p, kk_block_t* block, kk_context_t* _ctx) {
  kk_unused(block);
  kk_unused(_ctx);
  kk_uv_handle_t* hnd = (kk_uv_handle_t*) p;
  uv_close(&hnd->uv, kk_uv_handle_close_cb);
}

// Borrow the `internal` struct of a koka wrapper. Convenience wrapper around
// `kk_<type>_unbox_borrowed`; prefer the typed `_unbox_borrowed` directly.
#define kk_borrow_internal_as(uv_tp, hndl) \
  uv_##uv_tp##_as_kk((uv_##uv_tp##_t*) kk_cptr_unbox_borrowed((hndl).internal, kk_context()))

#endif
