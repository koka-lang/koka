void kk_uv_fs_poll_callback(uv_fs_poll_t* handle, int status, const uv_stat_t* prev, const uv_stat_t* curr) {
  kk_uv_hnd_get_callback(handle, hnd, callback)
  kk_function_dup(callback, kk_context());
  if (status < 0) {
    // TODO: Does the callback really need duping here?
    kk_uv_error_callback(callback, status)
  } else {
    kk_box_t prevb = kk_uv_file__fstat_box(kk_uv_stat_from_uv_stat(prev, _ctx), _ctx);
    kk_box_t currb = kk_uv_file__fstat_box(kk_uv_stat_from_uv_stat(curr, _ctx), _ctx);
    kk_uv_okay_callback(callback, kk_std_core_types__tuple2_box(kk_std_core_types__new_Tuple2(prevb, currb, _ctx), _ctx))
  }
}

kk_std_core_exn__error kk_uv_fs_poll_init(kk_context_t* _ctx) {
  uv_fs_poll_t* fs_poll = kk_malloc(sizeof(uv_fs_poll_t), _ctx);
  int status = uv_fs_poll_init(uvloop(), fs_poll);
  kk_uv_check_return_err_drops(status, uv_handle_to_owned_kk_handle_box(fs_poll, kk_handle_free, poll, fs_poll), {kk_free(fs_poll, _ctx);})
}

kk_std_core_exn__error kk_uv_fs_poll_start(kk_uv_poll__uv_fs_poll handle, kk_string_t path, kk_integer_t interval, kk_function_t callback, kk_context_t* _ctx) {
  kk_set_hnd_cb(uv_fs_poll_t, handle, fs_poll, callback)
  kk_ssize_t len;
  kk_uv_check_err_drops(uv_fs_poll_start(fs_poll, kk_uv_fs_poll_callback, kk_string_cbuf_borrow(path, &len, _ctx), kk_integer_clamp32(interval, _ctx)), {kk_function_drop(callback, _ctx);})
}

kk_std_core_exn__error kk_uv_fs_poll_stop(kk_uv_poll__uv_fs_poll handle, kk_context_t* _ctx) {
  uv_fs_poll_t* hnd = kk_owned_handle_to_uv_handle(uv_fs_poll_t, handle);
  kk_uv_check_ok_drops(uv_fs_poll_stop(hnd), {kk_box_drop(handle.internal, kk_context());})
}

kk_std_core_exn__error kk_uv_fs_poll_getpath(kk_uv_poll__uv_fs_poll handle, kk_context_t* _ctx) {
  uv_fs_poll_t* fs_poll = kk_owned_handle_to_uv_handle(uv_fs_poll_t, handle);
  size_t len = 2048;
  char* path = kk_malloc(len, _ctx);
  int status = uv_fs_poll_getpath(fs_poll, path, &len);
  kk_uv_check_return_err_drops(status, kk_string_box(kk_string_alloc_raw_len(len, (const char*)path, true, _ctx)), {kk_free(path, _ctx);})
}
