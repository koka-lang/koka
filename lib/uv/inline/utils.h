// Derive a handle type from a uv type name
#define kk_uv_handle_tp(uv_tp) kk_##uv_tp##_t

// Convert to/from Koka boxed wrapper types, where the `internal` field is an `any` boxed & reference counted C pointer
#define kk_owned_handle_to_uv_handle(uv_tp, hndl) \
  ((kk_uv_handle_tp(uv_tp)*)kk_cptr_unbox_borrowed(hndl.internal, kk_context()))
// `mod` is the module name, `uv_tp` is the uv handle type
// `free_fn` is the function to handle freeing the uv handle.
#define uv_handle_to_owned_kk_handle(hndl, free_fn, mod, Kk_tp) \
  kk_uv_##mod##__new_##Kk_tp(kk_cptr_raw_box(&free_fn, (void*)hndl, kk_context()), kk_context())
// Since the wrapper type is a value struct, we sometimes need to `box/unbox` them as well.
#define uv_handle_to_owned_kk_handle_box(hndl, free_fn, mod, kk_tp, Kk_tp) \
  kk_uv_##mod##__##kk_tp##_box(uv_handle_to_owned_kk_handle(hndl,free_fn,mod,Kk_tp), kk_context())

// Call a unit callback
static inline kk_unit_t kk_unit_callback(kk_function_t callback, kk_context_t* _ctx) {
  return kk_function_call(kk_unit_t, (kk_function_t, kk_context_t*), callback, (callback, kk_context()), kk_context());
}

#ifdef __EMSCRIPTEN__
#include <emscripten.h>
#else
#include <uv.h>

// Thread local uv loop variable
static kk_decl_thread uv_loop_t* kk_uv_loop_default;
void kk_set_uv_loop(uv_loop_t* loop);
uv_loop_t* uvloop();

// UV Okay status code
#define UV_OK 0
// Map a libuv status code to a Koka Error value with uv status code enum
kk_std_core_exn__error kk_uv_async_error_from_errno( int err, kk_context_t* ctx );
// Call a callback with a uv status code wrapped in an Error or Ok (can't be a function due to the type of result not being known)
#define kk_uv_exn_callback(callback, result) \
  kk_function_call(kk_unit_t, (kk_function_t, kk_std_core_exn__error, kk_context_t*), callback, (callback, result, kk_context()), kk_context());
// Call a callback with a uv ok wrapped in an Ok (can't be function due to the type of result not being known)
#define kk_uv_okay_callback(callback, result) \
  kk_uv_exn_callback(callback, kk_std_core_exn__new_Ok(result, kk_context()))
// Call a callback with a uv status code enum value (can't be static inline function due to kk_uv_utils__uv_status_code not being defined yet)
#define kk_uv_status_code_callback(callback, status) \
  kk_function_call(kk_unit_t, (kk_function_t, kk_uv_utils__uv_status_code, kk_context_t*), callback, (callback, kk_uv_utils_int_fs_status_code(status, _ctx), kk_context()), kk_context());
// Call a callback with a uv error wrapped in an Error
static inline void kk_uv_error_callback(kk_function_t callback, int result, kk_context_t* _ctx) {
  kk_uv_exn_callback(callback, kk_uv_async_error_from_errno(result, kk_context()));
}

// Invariant: handles only have a single outstanding callback
//   multiple concurrent calls using the same handle not supported
//   TODO: Relax this restriction?
#define kk_uv_handle(uv_hnd_tp) \
  typedef struct { \
    /* The uv handle struct (embedded as first member) */ \
    uv_##uv_hnd_tp##_t handle; \
    /* 
       The Koka callback function
       Needs to be dupped every time it is called, so that it always can be called again by libuv 
    */ \
    kk_function_t callback; \
  } kk_uv_handle_tp(uv_hnd_tp); \
  /* Handles freeing a kk_uv_handle struct (see definition below) */ \
  static inline void kk_##uv_hnd_tp##_free(void *p, kk_block_t *block, kk_context_t *_ctx) { \
      uv_handle_t *handle = (uv_handle_t *)p; \
      kk_uv_handle_tp(uv_hnd_tp)* hndcb = (kk_uv_handle_tp(uv_hnd_tp)*)handle;  \
      /* the callback should have been cleaned up prior to this point */ \
      kk_assert_internal(kk_function_is_null(hndcb->callback, kk_context())); \
      /* block will be freed by kk_uv_handle_close_callback after uv has cleaned up its state */ \
      handle->data = block; \
      uv_close(handle, &kk_uv_handle_close_callback); \
  } // define the free function


// This handles actually freeing the memory when the uv_handle is closed
static inline void kk_uv_handle_close_callback(uv_handle_t* handle) {
  kk_context_t* _ctx = kk_get_context();
  if (kk_likely(handle->data != NULL)) {
    kk_free(handle->data, kk_context()); // Free the box memory
  }
  kk_free(handle, kk_context()); // Free the struct memory 
}

// TODO: Change all apis to return status code or return error, not a mix
//   Decide which to use for sync errors versus callbacks

// If the status is not OK, drop before returning the status code
#define kk_uv_check_status_drops(status, drops) \
  if (kk_unlikely(status < UV_OK)) { \
    do drops while (0); \
  } \
  return kk_uv_utils_int_fs_status_code(status, kk_context()); \

// Sometimes the return value is a file descriptor which is why this is a < UV_OK check instead of == UV_OK
#define kk_uv_check_return(err, result) \
  if (kk_unlikely(err < UV_OK)) { \
    return kk_uv_async_error_from_errno(err, kk_context()); \
  } else { \
    return kk_std_core_exn__new_Ok(result, kk_context()); \
  }

// Typically used to clean up when an error occurs
#define kk_uv_check_return_err_drops(err, result, drops) \
  if (kk_unlikely(err < UV_OK)) { \
    do drops while (0); \
    return kk_uv_async_error_from_errno(err, kk_context()); \
  } else { \
    return kk_std_core_exn__new_Ok(result, kk_context()); \
  }

// Typically used when cleaning up a handle
#define kk_uv_check_return_ok_drops(err, result, drops) \
  if (kk_unlikely(err < UV_OK)) { \
    return kk_uv_async_error_from_errno(err, kk_context()); \
  } else { \
    do drops while (0); \
    return kk_std_core_exn__new_Ok(result, kk_context()); \
  }

// Check the uv status code and return a kk_std_core_exn__error Ok or Error
#define kk_uv_check(err) kk_uv_check_return(err, kk_unit_box(kk_Unit))

// Check the uv status code and return a kk_std_core_exn__error Ok or Error
// Dropping the references if it was an error
#define kk_uv_check_err_drops(err, drops) \
  kk_uv_check_return_err_drops(err, kk_unit_box(kk_Unit), drops)

// Check the uv status code and return a kk_std_core_exn__error Ok or Error
// Dropping the references if the result is Okay
#define kk_uv_check_ok_drops(err, drops) \
  kk_uv_check_return_ok_drops(err, kk_unit_box(kk_Unit), drops)

#endif
