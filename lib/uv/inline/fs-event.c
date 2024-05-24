void kk_uv_fs_event_callback(uv_fs_event_t* handle, const char *filename, int events, int status) {
  kk_uv_hnd_get_callback(handle, hnd, callback)
  kk_function_dup(callback, kk_context());
  if (status < 0) {
    // TODO: Does the callback really need duping for this condition?
    kk_uv_error_callback(callback, status)  
  } else {
    kk_uv_okay_callback(callback, kk_std_core_types__tuple2_box(kk_std_core_types__new_Tuple2(kk_string_box(kk_string_alloc_from_utf8(filename, _ctx)), kk_int32_box(events, _ctx), _ctx), _ctx))
  }
}

kk_std_core_exn__error kk_uv_fs_event_init(kk_context_t* _ctx) {
  uv_fs_event_t* fs_event = kk_malloc(sizeof(uv_fs_event_t), _ctx);
  int status = uv_fs_event_init(uvloop(), fs_event);
  kk_uv_check_return_err_drops(status, uv_handle_to_owned_kk_handle_box(fs_event, kk_handle_free, fs_dash_event, fs_event), {kk_free(fs_event, _ctx);})
}

kk_std_core_exn__error kk_uv_fs_event_start(kk_uv_fs_dash_event__uv_fs_event handle, kk_string_t path, int32_t interval, kk_function_t callback, kk_context_t* _ctx) {
  kk_set_hnd_cb(uv_fs_event_t, handle, fs_event, callback)
  kk_ssize_t len;
  kk_uv_check_err_drops(uv_fs_event_start(fs_event, kk_uv_fs_event_callback, kk_string_cbuf_borrow(path, &len, _ctx), interval), {kk_function_drop(callback, _ctx);})
}

kk_std_core_exn__error kk_uv_fs_event_stop(kk_uv_fs_dash_event__uv_fs_event handle, kk_context_t* _ctx) {
  uv_fs_event_t* hnd = kk_owned_handle_to_uv_handle(uv_fs_event_t, handle);
  kk_uv_check_ok_drops(uv_fs_event_stop(hnd), {kk_box_drop(handle.internal, _ctx);})
}

kk_std_core_exn__error kk_uv_fs_event_getpath(kk_uv_fs_dash_event__uv_fs_event handle, kk_context_t* _ctx) {
  uv_fs_event_t* fs_event = kk_owned_handle_to_uv_handle(uv_fs_event_t, handle);
  size_t len = 2048;
  char* path = kk_malloc(len, _ctx);
  int status = uv_fs_event_getpath(fs_event, path, &len);
  kk_uv_check_return_err_drops(status, kk_string_box(kk_string_alloc_raw_len(len, (const char*)path, true, _ctx)), {kk_free(path, kk_context());})
}