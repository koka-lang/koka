// Borrow the `internal` struct of a koka wrapper,
// which should be a pointer to a kk_uv_* wrapper struct
#define kk_borrow_internal_as(uv_tp, hndl) \
  uv_##uv_tp##_as_kk((uv_##uv_tp##_t*)kk_cptr_unbox_borrowed(hndl.internal, kk_context()), kk_context())

// Call a unit callback
static inline kk_unit_t kk_unit_callback(kk_function_t callback, kk_context_t* _ctx) {
  return kk_function_call(kk_unit_t, (kk_function_t, kk_context_t*), callback, (callback, kk_context()), kk_context());
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

// Invariant: handles only have a single outstanding callback
//   multiple concurrent calls using the same handle not supported
//   TODO: Relax this restriction?
#define kk_uv_handle(uv_hnd_tp) \
  typedef struct kk_##uv_hnd_tp##_s { \
    /* The uv handle struct (embedded as first member) */ \
    uv_##uv_hnd_tp##_t uv; \
    /*
       The Koka callback function
       Needs to be dupped every time it is called, so that it always can be called again by libuv
    */ \
    kk_function_t callback; \
  } kk_##uv_hnd_tp##_t; \
  /* get a pointer to the kk_* wrapper containing the given uv struct */ \
  static inline kk_##uv_hnd_tp##_t* uv_##uv_hnd_tp##_as_kk(void *p, kk_context_t *_ctx) { \
    return (kk_##uv_hnd_tp##_t *) (((char*)p) - offsetof(kk_##uv_hnd_tp##_t, uv)); \
  } \
  /* Handles freeing a kk_uv_handle struct (see definition below) */ \
  static inline void kk_##uv_hnd_tp##_free(void *p, kk_block_t *block, kk_context_t *_ctx) { \
      kk_##uv_hnd_tp##_t* kk_handle = (kk_##uv_hnd_tp##_t*)p; \
      uv_handle_t *uv_handle = (uv_handle_t *)(&kk_handle->uv); \
      /* the callback should have been cleaned up prior to this point */ \
      kk_assert_internal(kk_function_is_null(kk_handle->callback, kk_context())); \
      /* free uv_handle using kk_uv_handle_close_callback after uv has cleaned up its state */ \
      uv_close(uv_handle, &kk_uv_handle_close_callback); \
  } \
  /* box a C struct into an `any` koka type */ \
  static inline kk_box_t kk_##uv_hnd_tp##_box(kk_##uv_hnd_tp##_t * hnd, kk_context_t *_ctx) { \
    return kk_cptr_raw_box(&kk_##uv_hnd_tp##_free, (void*)hnd, _ctx); \
  }


// This handles actually freeing the memory when the uv_handle is closed
static inline void kk_uv_handle_close_callback(uv_handle_t* handle) {
  kk_context_t* _ctx = kk_get_context();
  kk_assert_internal(handle->data == NULL);
  kk_free(handle, _ctx); // Free the struct memory
}

#endif
