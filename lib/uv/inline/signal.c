#include "kklib/box.h"

static kk_uv_signal__uv_signal kk_uv_signal_alloc(kk_context_t* _ctx){
  uv_signal_t* sig = kk_malloc(sizeof(uv_signal_t), _ctx);
  uv_signal_init(uvloop(), sig);
  return uv_handle_to_owned_kk_handle(sig, kk_handle_free, signal, signal);
}

static void kk_uv_signal_callback(uv_signal_t* sig, int signum){
  kk_uv_hnd_get_callback(sig, hnd, callback)
  kk_function_dup(callback, _ctx);
  kk_box_dup(hnd, _ctx);
  kk_function_call(void, (kk_function_t, kk_uv_signal__uv_signal, kk_context_t*), callback, (callback, kk_uv_signal__new_Uv_signal(hnd, kk_context()), _ctx), _ctx);
}

static void kk_uv_signal_oneshot_callback(uv_signal_t* sig, int signum){
  kk_uv_hnd_get_callback(sig, hnd, callback)
  kk_unit_callback(callback)
}

static int32_t kk_uv_signal_start(kk_uv_signal__uv_signal handle, kk_function_t callback, int32_t signum, kk_context_t* _ctx){
  kk_set_hnd_cb(uv_signal_t, handle, uvhnd, callback)
  return uv_signal_start(uvhnd, kk_uv_signal_callback, signum);
}

static int32_t kk_uv_signal_start_oneshot(kk_uv_signal__uv_signal handle, kk_function_t callback, int32_t signum, kk_context_t* _ctx){
  kk_set_hnd_cb(uv_signal_t, handle, uvhnd, callback)
  return uv_signal_start_oneshot(uvhnd, kk_uv_signal_oneshot_callback, signum);
}

static int32_t kk_uv_signal_stop(kk_uv_signal__uv_signal handle, kk_context_t* _ctx){
  uv_signal_t* sig = kk_owned_handle_to_uv_handle(uv_signal_t, handle);
  int32_t result = uv_signal_stop(sig);
  kk_box_drop(handle.internal, kk_context());
  return result;
}

static int32_t kk_uv_signal_num(kk_uv_signal__uv_signal handle, kk_context_t* _ctx){
  return kk_owned_handle_to_uv_handle(uv_signal_t, handle)->signum;
}
