
static kk_std_os_signal__uvSignal kk_uv_signal_alloc(kk_context_t* _ctx){
  uv_signal_t* sig = kk_malloc(sizeof(uv_signal_t), _ctx);
  uv_signal_init(uvloop(), sig);
  return kk_std_os_signal__new_UvSignal((intptr_t)sig, kk_context());
}

static void kk_uv_signal_callback(uv_signal_t* sig, int signum){
  kk_context_t* _ctx = kk_get_context();
  kk_uv_callback_t* wrapper = (kk_uv_callback_t*)sig->data;
  kk_function_t callback = wrapper->callback;
  kk_function_call(void, (kk_function_t, kk_std_os_signal__uvSignal, kk_context_t*), callback, (callback, kk_std_os_signal__new_UvSignal((intptr_t)sig, kk_context()), _ctx), _ctx);
}

static void kk_uv_signal_oneshot_callback(uv_signal_t* sig, int signum){
  kk_context_t* _ctx = kk_get_context();
  kk_uv_callback_t* wrapper = (kk_uv_callback_t*)sig->data;
  kk_function_t callback = wrapper->callback;
  kk_function_call(void, (kk_function_t, kk_context_t*), callback, (callback, _ctx), _ctx);
  uv_close(sig, NULL);
  kk_free(sig, _ctx);
  kk_free(wrapper, _ctx);
}

static int32_t kk_uv_signal_start(kk_std_os_signal__uvSignal handle, kk_function_t callback, int32_t signum, kk_context_t* _ctx){
  kk_uv_callback_t* wrapper = kk_new_uv_callback(callback, (uv_handle_t*)handle.internal, _ctx);
  return uv_signal_start((uv_signal_t*)handle.internal, kk_uv_signal_callback, signum);
}

static int32_t kk_uv_signal_start_oneshot(kk_std_os_signal__uvSignal handle, kk_function_t callback, int32_t signum, kk_context_t* _ctx){
  kk_uv_callback_t* wrapper = kk_new_uv_callback(callback, (uv_handle_t*)handle.internal, _ctx);
  return uv_signal_start_oneshot((uv_signal_t*)handle.internal, kk_uv_signal_oneshot_callback, signum);
}

static int32_t kk_uv_signal_stop(kk_std_os_signal__uvSignal handle, kk_context_t* _ctx){
  kk_uv_callback_t* wrapper = ((uv_signal_t*)handle.internal)->data;
  int32_t result = uv_signal_stop((uv_signal_t*)handle.internal);
  kk_free((uv_signal_t*)handle.internal, _ctx);
  if (wrapper != NULL){
    kk_free(wrapper, _ctx);
  }
  return result;
}

static int32_t kk_uv_signal_num(kk_std_os_signal__uvSignal handle, kk_context_t* _ctx){
  return ((uv_signal_t*)handle.internal)->signum;
}