// #include "std_os_stream.h"
#include "std_os_event_dash_loop.h"

void kk_uv_alloc_callback(uv_handle_t* handle, size_t suggested_size, uv_buf_t* buf) {
  buf->base = kk_malloc(suggested_size, kk_get_context());
  buf->len = suggested_size;
}

static void kk_uv_shutdown_callback(uv_shutdown_t* req, int status){
  kk_context_t* _ctx = kk_get_context();
  kk_function_t callback = kk_function_from_ptr(req->data, _ctx);
  kk_function_call(void, (kk_function_t, kk_std_os_event_dash_loop__uv_status_code, kk_context_t*), callback, (callback, kk_std_os_event_dash_loop_int_fs_status_code(status, _ctx), _ctx), _ctx);
  kk_free(req, _ctx);
}

static int kk_uv_shutdown(kk_std_os_stream__uv_stream handle, kk_function_t callback, kk_context_t* _ctx){
  uv_shutdown_t* uv_req = kk_malloc(sizeof(uv_shutdown_t), _ctx);
  uv_req->data = kk_function_as_ptr(callback, _ctx);
  return uv_shutdown(uv_req, (uv_stream_t*)handle.internal, kk_uv_shutdown_callback);
}

static void kk_uv_connection_callback(uv_stream_t* server, int status){
  kk_context_t* _ctx = kk_get_context();
  kk_function_t callback = kk_function_from_ptr(server->data, _ctx);
  kk_function_call(void, (kk_function_t, kk_std_os_event_dash_loop__uv_status_code, kk_context_t*), callback, (callback, kk_std_os_event_dash_loop_int_fs_status_code(status, _ctx), _ctx), _ctx);
}

static int kk_uv_listen(kk_std_os_stream__uv_stream stream, int32_t backlog, kk_function_t callback, kk_context_t* _ctx){
  uv_stream_t* uvstream = (uv_stream_t*)stream.internal;
  uvstream->data = kk_function_as_ptr(callback, _ctx);
  return uv_listen(uvstream, backlog, kk_uv_connection_callback);
}

static int kk_uv_accept(kk_std_os_stream__uv_stream server, kk_std_os_stream__uv_stream client, kk_context_t* _ctx) {  
  return uv_accept((uv_stream_t*)server.internal, (uv_stream_t*)client.internal);
}

static void kk_uv_read_callback(uv_stream_t* stream, ssize_t nread, const uv_buf_t* buf){
  kk_context_t* _ctx = kk_get_context();
  kk_function_t callback = kk_function_from_ptr(stream->data, _ctx);
  kk_bytes_t bytes = kk_bytes_alloc_len((kk_ssize_t)nread,nread, buf->base,NULL, _ctx);
  kk_function_call(void, (kk_function_t, kk_bytes_t, kk_context_t*), callback, (callback, bytes, _ctx), _ctx);
}

static int kk_uv_read_start(kk_std_os_stream__uv_stream stream, kk_function_t read_cb, kk_context_t* _ctx){
  uv_stream_t* uvstream = (uv_stream_t*)stream.internal;
  uvstream->data = kk_function_as_ptr(read_cb, _ctx); 
  return uv_read_start(uvstream, kk_uv_alloc_callback, kk_uv_read_callback);
}

static kk_std_os_event_dash_loop__uv_status_code kk_uv_read_stop(kk_std_os_stream__uv_stream stream, kk_context_t* _ctx){
  return kk_std_os_event_dash_loop_int_fs_status_code(uv_read_stop((uv_stream_t*)stream.internal), _ctx);
}

static void kk_uv_write_callback(uv_write_t* write, int status){
  kk_context_t* _ctx = kk_get_context();
  kk_function_t callback = kk_function_from_ptr(write->data, _ctx);
  // TODO Free bytes?
  kk_function_call(void, (kk_function_t, kk_std_os_event_dash_loop__uv_status_code, kk_context_t*), callback, (callback, kk_std_os_event_dash_loop_int_fs_status_code(status, _ctx), _ctx), _ctx);
}

static void kk_uv_write(kk_std_os_stream__uv_stream stream, kk_std_core_types__list buffs, kk_function_t cb, kk_context_t* _ctx){
  uv_write_t* write = kk_malloc(sizeof(uv_write_t), _ctx);
  write->data = kk_function_as_ptr(cb, _ctx);
  int list_len;
  const uv_buf_t* uv_buffs = kk_bytes_list_to_uv_buffs(buffs, &list_len, _ctx);
  uv_write(write, (uv_stream_t*)stream.internal, uv_buffs, list_len, kk_uv_write_callback);
}

static void kk_uv_write2(kk_std_os_stream__uv_stream handle, kk_std_core_types__list buffs, kk_std_os_stream__uv_stream send_handle, kk_function_t cb, kk_context_t* _ctx){
  uv_write_t* write = kk_malloc(sizeof(uv_write_t), _ctx);
  write->data = kk_function_as_ptr(cb, _ctx);
  int list_len;
  const uv_buf_t* uv_buffs = kk_bytes_list_to_uv_buffs(buffs, &list_len, _ctx);
  uv_write2(write, (uv_stream_t*)handle.internal, uv_buffs, list_len, (uv_stream_t*)send_handle.internal, kk_uv_write_callback);
}

static int32_t kk_uv_try_write(kk_std_os_stream__uv_stream stream, kk_std_core_types__list buffs, kk_context_t* _ctx){
  int list_len;
  const uv_buf_t* uv_buffs = kk_bytes_list_to_uv_buffs(buffs, &list_len, _ctx);
  return uv_try_write((uv_stream_t*)stream.internal, uv_buffs, list_len);
}

static int32_t kk_uv_try_write2(kk_std_os_stream__uv_stream handle, kk_std_core_types__list buffs, kk_std_os_stream__uv_stream send_handle, kk_context_t* _ctx){
  int list_len;
  const uv_buf_t* uv_buffs = kk_bytes_list_to_uv_buffs(buffs, &list_len, _ctx);
  return uv_try_write2((uv_stream_t*)handle.internal, uv_buffs, list_len, (uv_stream_t*)send_handle.internal);
}

static int32_t kk_uv_is_readable(kk_std_os_stream__uv_stream stream, kk_context_t* _ctx){
  return uv_is_readable((uv_stream_t*)stream.internal);
}

static int32_t kk_uv_is_writable(kk_std_os_stream__uv_stream stream, kk_context_t* _ctx){
  return uv_is_writable((uv_stream_t*)stream.internal);
}

static kk_std_os_event_dash_loop__uv_status_code kk_uv_stream_set_blocking(kk_std_os_stream__uv_stream stream, bool blocking, kk_context_t* _ctx){
  return kk_std_os_event_dash_loop_int_fs_status_code(uv_stream_set_blocking((uv_stream_t*)stream.internal, blocking), _ctx);
}

static int32_t kk_uv_stream_get_write_queue_size(kk_std_os_stream__uv_stream stream, kk_context_t* _ctx){
  return uv_stream_get_write_queue_size((uv_stream_t*)stream.internal);
}