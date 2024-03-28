#include "std_os_event_dash_loop.h"

static kk_std_os_signal__uv_signal kk_uv_signal_alloc(kk_context_t* _ctx){
  uv_signal_t* sig = kk_malloc(sizeof(uv_signal_t), _ctx);
  uv_signal_init(uvloop(), sig);
  return kk_std_os_signal__new_Uv_signal((intptr_t)sig, kk_context());
}

static void kk_uv_signal_callback(uv_signal_t* sig, int signum){
  kk_context_t* _ctx = kk_get_context();
  kk_function_t callback = kk_function_from_ptr(sig->data, _ctx);
  kk_function_dup(callback, _ctx);
  kk_function_call(void, (kk_function_t, kk_std_os_signal__uv_signal, kk_context_t*), callback, (callback, kk_std_os_signal__new_Uv_signal((intptr_t)sig, kk_context()), _ctx), _ctx);
}

static void kk_uv_signal_oneshot_callback(uv_signal_t* sig, int signum){
  kk_context_t* _ctx = kk_get_context();
  kk_function_t callback = kk_function_from_ptr(sig->data, _ctx);
  kk_function_call(void, (kk_function_t, kk_context_t*), callback, (callback, _ctx), _ctx);
  uv_close((uv_handle_t*)sig, NULL);
}

static int32_t kk_uv_signal_start(kk_std_os_signal__uv_signal handle, kk_function_t callback, int32_t signum, kk_context_t* _ctx){
  ((uv_handle_t*)handle.internal)->data = kk_function_as_ptr(callback, _ctx);
  return uv_signal_start((uv_signal_t*)handle.internal, kk_uv_signal_callback, signum);
}

static int32_t kk_uv_signal_start_oneshot(kk_std_os_signal__uv_signal handle, kk_function_t callback, int32_t signum, kk_context_t* _ctx){
  ((uv_handle_t*)handle.internal)->data = kk_function_as_ptr(callback, _ctx);
  return uv_signal_start_oneshot((uv_signal_t*)handle.internal, kk_uv_signal_oneshot_callback, signum);
}

static int32_t kk_uv_signal_stop(kk_std_os_signal__uv_signal handle, kk_context_t* _ctx){
  void* callback = ((uv_signal_t*)handle.internal)->data;
  int32_t result = uv_signal_stop((uv_signal_t*)handle.internal);
  if (callback != NULL){
    kk_function_drop(kk_function_from_ptr(callback, _ctx), _ctx);
  }
  return result;
}

static int32_t kk_uv_signal_num(kk_std_os_signal__uv_signal handle, kk_context_t* _ctx){
  return ((uv_signal_t*)handle.internal)->signum;
}