
#include "std_os_uv.h"

static kk_std_os_file_dash_uv__uvFsReq kk_uv_fs_init(kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  return kk_std_os_file_dash_uv__new_UvFsReq((intptr_t)fs_req, kk_context());
}

static kk_unit_t kk_uv_fs_req_cleanup(kk_std_os_file_dash_uv__uvFsReq req, kk_context_t* _ctx) {
  uv_fs_req_cleanup((uv_fs_t*)req.internal);
  return kk_Unit;
}

static void kk_std_os_fs_unit_cb(uv_fs_t* req) {
  kk_context_t* _ctx = kk_get_context();
  kk_uv_callback_t* wrapper = (kk_uv_callback_t*)req->data;
  kk_function_t callback = wrapper->callback;
  ssize_t result = req->result;
  kk_free(wrapper, _ctx);
  uv_fs_req_cleanup(req);
  if (result < 0) {
    kk_function_call(void, (kk_function_t, kk_std_core__error, kk_context_t*), callback, (callback, kk_async_error_from_errno(req->result, _ctx), _ctx), _ctx);
  } else {
    kk_function_call(void, (kk_function_t, kk_std_core__error, kk_context_t*), callback, (callback, kk_std_core__new_Ok(kk_unit_box(kk_Unit), _ctx), _ctx), _ctx);
  }
}

static kk_unit_t kk_uv_fs_close(kk_std_os_file_dash_uv__uvFile file, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*)fs_req, _ctx);
  uv_fs_close(uvloop(), fs_req, (uv_file)(file.internal), kk_std_os_fs_unit_cb);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_close_sync(kk_std_os_file_dash_uv__uvFile file, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  int status = uv_fs_close(uvloop(), &fs_req, (uv_file)(file.internal), NULL);
  if (status < 0) {
    kk_async_error_from_errno(status, _ctx);
  } else {
    return kk_std_core__new_Ok(kk_unit_box(kk_Unit), _ctx);
  }
}

static void kk_std_os_file_open_cb(uv_fs_t* req) {
  kk_context_t* _ctx = kk_get_context();
  kk_uv_callback_t* wrapper = (kk_uv_callback_t*)req->data;
  kk_function_t callback = wrapper->callback;
  ssize_t result = req->result;
  kk_free(wrapper, _ctx);
  uv_fs_req_cleanup(req);
  if (result < 0) {
    kk_function_call(void, (kk_function_t, kk_std_core__error, kk_context_t*), callback, (callback, kk_async_error_from_errno(result, _ctx), _ctx), _ctx);
  } else {
    kk_function_call(void, (kk_function_t, kk_std_core__error, kk_context_t*), callback, (callback, kk_std_core__new_Ok(kk_std_os_file_dash_uv__uvFile_box(kk_std_os_file_dash_uv__new_UvFile((intptr_t)result, _ctx), _ctx), _ctx), _ctx), _ctx);
  }
}

static kk_unit_t kk_uv_fs_open(kk_string_t path, int32_t flags, int32_t mode, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*)fs_req, _ctx);
  kk_ssize_t len;
  uv_fs_open(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), flags, mode, kk_std_os_file_open_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_open_sync(kk_string_t path, int32_t flags, int32_t mode, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int fd = uv_fs_open(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), flags, mode, NULL);
  kk_string_drop(path, _ctx);
  if (fd < 0) {
    return kk_async_error_from_errno(fd, _ctx);
  } else {
    return kk_std_core__new_Ok(kk_std_os_file_dash_uv__uvFile_box(kk_std_os_file_dash_uv__new_UvFile((intptr_t)fd, _ctx), _ctx), _ctx);
  }
}

typedef struct kk_uv_buff_callback_s {
  kk_function_t callback;
  kk_bytes_raw_t bytes;
} kk_uv_buff_callback_t;

static void kk_free_bytes(void* p, kk_block_t* bytes, kk_context_t* _ctx) {
  kk_free(((kk_bytes_raw_t)bytes)->cbuf, _ctx);
}

static inline kk_uv_buff_callback_t* kk_new_uv_buff_callback(kk_function_t cb, uv_handle_t* handle, int32_t num_bytes, kk_context_t* _ctx) {
  kk_uv_buff_callback_t* c = kk_malloc(sizeof(kk_uv_buff_callback_t), _ctx);
  c->callback = cb;
  uint8_t* buff = kk_malloc(num_bytes, _ctx);
  c->bytes = kk_datatype_as(kk_bytes_raw_t, kk_bytes_alloc_raw_len((kk_ssize_t)num_bytes, buff, false, _ctx), _ctx);
  c->bytes->free = &kk_free_bytes;
  handle->data = c;
  return c;
}

static void kk_std_os_file_buff_cb(uv_fs_t* req) {
  kk_context_t* _ctx = kk_get_context();
  kk_uv_buff_callback_t* wrapper = (kk_uv_buff_callback_t*)req->data;
  kk_function_t callback = wrapper->callback;
  kk_bytes_raw_t bytes = wrapper->bytes;
  kk_bytes_t bts = kk_datatype_from_base(&bytes->_base, _ctx);
  // kk_info_message("Clength %d", req->result);
  ssize_t result = req->result;
  kk_free(wrapper, _ctx);
  uv_fs_req_cleanup(req);
  if (result < 0) {
    kk_bytes_drop(bts, _ctx);
    kk_function_call(void, (kk_function_t, kk_std_core__error, kk_context_t*), callback, (callback, kk_async_error_from_errno(result, _ctx), _ctx), _ctx);
  } else {
    kk_bytes_t btsadj = kk_bytes_adjust_length(bts, (kk_ssize_t)result + 1, _ctx);
    kk_bytes_set(btsadj, (uint64_t)result, (int8_t)'\0', _ctx);
    // TODO?: Maybe would be better to not add a terminating null byte, and fix it when converting to a string instead?
    kk_std_core_types__tuple2_ tuple = kk_std_core_types__new_dash__lp__comma__rp_(kk_bytes_box(btsadj), kk_integer_box(kk_integer_from_ssize_t(result, _ctx), _ctx), _ctx); /*(1004, 1005)*/
    kk_box_t tupleboxed = kk_std_core_types__tuple2__box(tuple, _ctx);
    kk_function_call(void, (kk_function_t, kk_std_core__error, kk_context_t*), callback, (callback, kk_std_core__new_Ok(tupleboxed, _ctx), _ctx), _ctx);
  }
}

static kk_unit_t kk_uv_fs_read(kk_std_os_file_dash_uv__uvFile file, int32_t num_bytes, int32_t offset, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_buff_callback_t* wrapper = kk_new_uv_buff_callback(cb, (uv_handle_t*)fs_req, num_bytes, _ctx);
  uv_buf_t* uv_buffs = kk_malloc(sizeof(uv_buf_t)*1, _ctx);
  uv_buffs[0].base = (char*)wrapper->bytes->cbuf;
  uv_buffs[0].len = wrapper->bytes->clength;
  uv_fs_read(uvloop(), fs_req, (uv_file)file.internal, uv_buffs, 1, offset, kk_std_os_file_buff_cb);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_read_sync(kk_std_os_file_dash_uv__uvFile file, int32_t num_bytes, int32_t offset, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  uv_buf_t uv_buffs[1] = {0};
  uint8_t* buff = kk_malloc(num_bytes, _ctx);
  kk_bytes_raw_t bytes = kk_datatype_as(kk_bytes_raw_t, kk_bytes_alloc_raw_len((kk_ssize_t)num_bytes, buff, false, _ctx), _ctx);
  bytes->free = &kk_free_bytes;
  uv_buffs[0].base = (char*)bytes->cbuf;
  uv_buffs[0].len = bytes->clength;
  ssize_t result = uv_fs_read(uvloop(), &fs_req, (uv_file)file.internal, uv_buffs, 1, offset, NULL);
  if (result < 0) {
    kk_bytes_drop(kk_datatype_from_base(&bytes->_base, _ctx), _ctx);
    return kk_async_error_from_errno(result, _ctx);
  } else {
    kk_bytes_t btsadj = kk_bytes_adjust_length(kk_datatype_from_base(&bytes->_base, _ctx), (kk_ssize_t)result, _ctx);
    kk_std_core_types__tuple2_ tuple = kk_std_core_types__new_dash__lp__comma__rp_(kk_bytes_box(btsadj), kk_integer_box(kk_integer_from_ssize_t(result, _ctx), _ctx), _ctx); /*(1004, 1005)*/
    kk_box_t tupleboxed = kk_std_core_types__tuple2__box(tuple, _ctx);
    return kk_std_core__new_Ok(tupleboxed, _ctx);
  }
}

static kk_unit_t kk_uv_fs_unlink(kk_string_t path, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*)fs_req, _ctx);
  kk_ssize_t len;
  uv_fs_unlink(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_unlink_sync(kk_string_t path, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_unlink(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), NULL);
  kk_string_drop(path, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    return kk_std_core__new_Ok(kk_unit_box(kk_Unit), _ctx);
  }
}

static void kk_std_os_fs_write_cb(uv_fs_t* req) {
  kk_context_t* _ctx = kk_get_context();
  kk_uv_callback_t* wrapper = (kk_uv_callback_t*)req->data;
  kk_function_t callback = wrapper->callback;
  ssize_t result = req->result;
  kk_free(wrapper, _ctx);
  uv_fs_req_cleanup(req);
  if (result < 0) {
    kk_function_call(void, (kk_function_t, kk_std_core__error, kk_context_t*), callback, (callback, kk_async_error_from_errno(result, _ctx), _ctx), _ctx);
  } else {
    kk_function_call(void, (kk_function_t, kk_std_core__error, kk_context_t*), callback, (callback, kk_std_core__new_Ok(kk_integer_box(kk_integer_from_ssize_t(result, _ctx), _ctx), _ctx), _ctx), _ctx);
  }
}

static kk_unit_t kk_uv_fs_write(kk_std_os_file_dash_uv__uvFile file, kk_bytes_t bytes, int64_t offset, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*)fs_req, _ctx);
  uv_buf_t* uv_buffs = kk_bytes_to_uv_buffs(bytes, _ctx);
  uv_fs_write(uvloop(), fs_req, (uv_file)file.internal, uv_buffs, 1, offset, kk_std_os_fs_write_cb);
  kk_bytes_drop(bytes, _ctx);
  kk_std_os_file_dash_uv__uvFile_drop(file, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_write_sync(kk_std_os_file_dash_uv__uvFile file, kk_bytes_t bytes, int64_t offset, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  uv_buf_t* uv_buffs = kk_bytes_to_uv_buffs(bytes, _ctx);
  int64_t result = uv_fs_write(uvloop(), &fs_req, (uv_file)file.internal, uv_buffs, 1, offset, NULL);
  kk_bytes_drop(bytes, _ctx);
  kk_std_os_file_dash_uv__uvFile_drop(file, _ctx);
  if (result < 0) {
    return kk_async_error_from_errno(result, _ctx);
  } else {
    return kk_std_core__new_Ok(kk_integer_box(kk_integer_from_int64(result, _ctx), _ctx), _ctx);
  }
}

static kk_unit_t kk_uv_fs_mkdir(kk_string_t path, int32_t mode, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*)fs_req, _ctx);
  kk_ssize_t len;
  uv_fs_mkdir(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), mode, kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_mkdir_sync(kk_string_t path, int32_t mode, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_mkdir(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), mode, NULL);
  kk_string_drop(path, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    return kk_std_core__new_Ok(kk_unit_box(kk_Unit), _ctx);
  }
}

static void kk_std_os_fs_mkdtemp_cb(uv_fs_t* req) {
  kk_context_t* _ctx = kk_get_context();
  kk_uv_callback_t* wrapper = (kk_uv_callback_t*)req->data;
  kk_function_t callback = wrapper->callback;
  ssize_t result = req->result;
  kk_free(wrapper, _ctx);
  if (result < 0) {
    uv_fs_req_cleanup(req);
    kk_function_call(void, (kk_function_t, kk_std_core__error, kk_context_t*), callback, (callback, kk_async_error_from_errno(result, _ctx), _ctx), _ctx);
  } else {
    kk_string_t str = kk_string_alloc_raw((const char*) req->path, true, _ctx);
    uv_fs_req_cleanup(req);
    kk_function_call(void, (kk_function_t, kk_std_core__error, kk_context_t*), callback, (callback, kk_std_core__new_Ok(kk_string_box(str), _ctx), _ctx), _ctx);
  }
}

static kk_unit_t kk_uv_fs_mkdtemp(kk_string_t tpl, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*)fs_req, _ctx);
  kk_ssize_t len;
  uv_fs_mkdtemp(uvloop(), fs_req, kk_string_cbuf_borrow(tpl, &len, _ctx), kk_std_os_fs_mkdtemp_cb);
  kk_string_drop(tpl, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_mkdtemp_sync(kk_string_t tpl, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_mkdtemp(uvloop(), &fs_req, kk_string_cbuf_borrow(tpl, &len, _ctx), NULL);
  kk_string_drop(tpl, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    kk_string_t str = kk_string_alloc_raw((const char*) fs_req.path, true, _ctx);
    return kk_std_core__new_Ok(kk_string_box(str), _ctx);
  }
}

static void kk_std_os_fs_mkstemp_cb(uv_fs_t* req) {
  kk_context_t* _ctx = kk_get_context();
  kk_uv_callback_t* wrapper = (kk_uv_callback_t*)req->data;
  kk_function_t callback = wrapper->callback;
  ssize_t result = req->result;
  kk_free(wrapper, _ctx);
  if (result < 0) {
    uv_fs_req_cleanup(req);
    kk_function_call(void, (kk_function_t, kk_std_core__error, kk_context_t*), callback, (callback, kk_async_error_from_errno(result, _ctx), _ctx), _ctx);
  } else {
    kk_string_t str = kk_string_alloc_raw((const char*) req->path, true, _ctx);
    uv_fs_req_cleanup(req);
    kk_std_os_file_dash_uv__uvFile file = kk_std_os_file_dash_uv__new_UvFile((intptr_t)result, _ctx);
    kk_std_core_types__tuple2_ tuple = kk_std_core_types__new_dash__lp__comma__rp_(kk_string_box(str), kk_std_os_file_dash_uv__uvFile_box(file, _ctx), _ctx); /*(1004, 1005)*/
    kk_box_t tupleboxed = kk_std_core_types__tuple2__box(tuple, _ctx);
    kk_function_call(void, (kk_function_t, kk_std_core__error, kk_context_t*), callback, (callback, kk_std_core__new_Ok(tupleboxed, _ctx), _ctx), _ctx);
  }
}

static kk_unit_t kk_uv_fs_mkstemp(kk_string_t tpl, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*)fs_req, _ctx);
  kk_ssize_t len;
  uv_fs_mkstemp(uvloop(), fs_req, kk_string_cbuf_borrow(tpl, &len, _ctx), kk_std_os_fs_mkdtemp_cb);
  kk_string_drop(tpl, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_mkstemp_sync(kk_string_t tpl, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_mkstemp(uvloop(), &fs_req, kk_string_cbuf_borrow(tpl, &len, _ctx), NULL);
  kk_string_drop(tpl, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    kk_string_t str = kk_string_alloc_raw((const char*) fs_req.path, true, _ctx);
    kk_std_os_file_dash_uv__uvFile file = kk_std_os_file_dash_uv__new_UvFile((intptr_t)status, _ctx);
    kk_std_core_types__tuple2_ tuple = kk_std_core_types__new_dash__lp__comma__rp_(kk_std_os_file_dash_uv__uvFile_box(file, _ctx), kk_string_box(str), _ctx); /*(1004, 1005)*/
    kk_box_t tupleboxed = kk_std_core_types__tuple2__box(tuple, _ctx);
    return kk_std_core__new_Ok(tupleboxed, _ctx);
  }
}

static kk_unit_t kk_uv_fs_rmdir(kk_string_t path, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*)fs_req, _ctx);
  kk_ssize_t len;
  uv_fs_rmdir(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_rmdir_sync(kk_string_t path, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_rmdir(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), NULL);
  kk_string_drop(path, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    return kk_std_core__new_Ok(kk_unit_box(kk_Unit), _ctx);
  }
}

void kk_std_os_fs_opendir_cb(uv_fs_t* req) {
  kk_context_t* _ctx = kk_get_context();
  kk_uv_callback_t* wrapper = (kk_uv_callback_t*)req->data;
  kk_function_t callback = wrapper->callback;
  ssize_t result = req->result;
  uv_dir_t* dir = (uv_dir_t*)req->ptr;
  kk_free(wrapper, _ctx);
  uv_fs_req_cleanup(req);
  if (result < 0) {
    kk_function_call(void, (kk_function_t, kk_std_core__error, kk_context_t*), callback, (callback, kk_async_error_from_errno(result, _ctx), _ctx), _ctx);
  } else {
    kk_std_os_file_dash_uv__uvDir d = kk_std_os_file_dash_uv__new_UvDir((intptr_t)dir, _ctx);
    kk_function_call(void, (kk_function_t, kk_std_core__error, kk_context_t*), callback, (callback, kk_std_core__new_Ok(kk_std_os_file_dash_uv__uvDir_box(d, _ctx), _ctx), _ctx), _ctx);
  }
}

static kk_unit_t kk_uv_fs_opendir(kk_string_t path, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*)fs_req, _ctx);
  kk_ssize_t len;
  uv_fs_opendir(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_std_os_fs_opendir_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_opendir_sync(kk_string_t path, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_opendir(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), NULL);
  kk_string_drop(path, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    uv_dir_t* dir = (uv_dir_t*)fs_req.ptr;
    kk_std_os_file_dash_uv__uvDir d = kk_std_os_file_dash_uv__new_UvDir((intptr_t)dir, _ctx);
    return kk_std_core__new_Ok(kk_std_os_file_dash_uv__uvDir_box(d, _ctx), _ctx);
  }
}

static kk_unit_t kk_uv_fs_closedir(kk_std_os_file_dash_uv__uvDir dir, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*)fs_req, _ctx);
  uv_fs_closedir(uvloop(), fs_req, (uv_dir_t*)dir.internal, kk_std_os_fs_unit_cb);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_closedir_sync(kk_std_os_file_dash_uv__uvDir dir, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  int status = uv_fs_closedir(uvloop(), &fs_req, (uv_dir_t*)dir.internal, NULL);
  if (status < 0) {
    return kk_async_error_from_errno(fs_req.result, _ctx);
  } else {
    return kk_std_core__new_Ok(kk_unit_box(kk_Unit), _ctx);
  }
}

typedef struct kk_uv_dir_callback_s {
  kk_function_t callback;
  uv_dir_t* uvdir;
} kk_uv_dir_callback_t;


static inline kk_uv_dir_callback_t* kk_new_uv_dir_callback(kk_function_t cb, uv_handle_t* handle, uv_dir_t* uvdir, int32_t num_entries, kk_context_t* _ctx) {
  kk_uv_dir_callback_t* c = kk_malloc(sizeof(kk_uv_dir_callback_t), _ctx);
  c->callback = cb;
  uvdir->dirents = kk_malloc(sizeof(uv_dirent_t)*500, _ctx);
  uvdir->nentries = 500;
  handle->data = c;
  return c;
}

kk_box_t kk_uv_dirent_to_dirent(uv_dirent_t* dirent, kk_box_t* loc, kk_context_t* _ctx) {
  if (loc == NULL) {
    loc = kk_malloc(sizeof(kk_box_t*), _ctx);
  }
  kk_string_t name = kk_string_alloc_raw((const char*) dirent->name, true, _ctx);
  kk_std_os_file_dash_uv__dirent_type type;
  switch (dirent->type) {
    case UV_DIRENT_FILE: type = kk_std_os_file_dash_uv__new_FILE(_ctx); break;
    case UV_DIRENT_DIR: type = kk_std_os_file_dash_uv__new_DIR(_ctx); break;
    case UV_DIRENT_LINK: type = kk_std_os_file_dash_uv__new_LINK(_ctx); break;
    case UV_DIRENT_FIFO: type = kk_std_os_file_dash_uv__new_FIFO(_ctx); break;
    case UV_DIRENT_SOCKET: type = kk_std_os_file_dash_uv__new_SOCKET(_ctx); break;
    case UV_DIRENT_CHAR: type = kk_std_os_file_dash_uv__new_CHAR(_ctx); break;
    case UV_DIRENT_BLOCK: type = kk_std_os_file_dash_uv__new_BLOCK(_ctx); break;
    default: type = kk_std_os_file_dash_uv__new_UNKNOWN__DIRECTORY__ENTRY(_ctx); break;
  }
  kk_box_t box = kk_std_os_file_dash_uv__dirent_box(kk_std_os_file_dash_uv__new_Dirent(name, type, _ctx), _ctx);
  *loc = box;
  return box;
}

kk_vector_t kk_uv_dirents_to_vec(uv_dir_t* uvdir, kk_ssize_t num_entries, kk_context_t* _ctx) {
  kk_box_t* dirs;
  kk_vector_t dirents = kk_vector_alloc_uninit(num_entries, &dirs, _ctx);
  for (kk_ssize_t i = 0; i < num_entries; i++){
    kk_uv_dirent_to_dirent(&(uvdir->dirents[i]), &dirs[i], _ctx);
  }
  return dirents;
}

void kk_std_os_fs_readdir_cb(uv_fs_t* req) {
  kk_context_t* _ctx = kk_get_context();
  kk_uv_dir_callback_t* wrapper = (kk_uv_dir_callback_t*)req->data;
  kk_function_t callback = wrapper->callback;
  ssize_t result = req->result;
  uv_dir_t* uvdir = wrapper->uvdir;
  kk_free(wrapper, _ctx);
  uv_fs_req_cleanup(req);
  if (result < 0) {
    kk_free(uvdir->dirents, _ctx);
    kk_function_call(void, (kk_function_t, kk_std_core__error, kk_context_t*), callback, (callback, kk_async_error_from_errno(result, _ctx), _ctx), _ctx);
  } else {
    kk_vector_t dirents = kk_uv_dirents_to_vec(uvdir, (kk_ssize_t)result, _ctx);
    kk_free(uvdir->dirents, _ctx);
    kk_function_call(void, (kk_function_t, kk_std_core__error, kk_context_t*), callback, (callback, kk_std_core__new_Ok(kk_vector_box(dirents, _ctx), _ctx), _ctx), _ctx);
  }
}

static kk_unit_t kk_uv_fs_readdir(kk_std_os_file_dash_uv__uvDir dir, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  // Read up to 500 entries in the directory
  kk_uv_dir_callback_t* wrapper = kk_new_uv_dir_callback(cb, (uv_handle_t*)fs_req, (uv_dir_t*) dir.internal, 500, _ctx);
  uv_fs_readdir(uvloop(), fs_req, wrapper->uvdir, kk_std_os_fs_readdir_cb);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_readdir_sync(kk_std_os_file_dash_uv__uvDir dir, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  uv_dir_t* uvdir = (uv_dir_t*) dir.internal;
  uvdir->dirents = kk_malloc(sizeof(uv_dirent_t)*500, _ctx);
  uvdir->nentries = 500;
  int status = uv_fs_readdir(uvloop(), fs_req, uvdir, NULL);
  if (status < 0) {
    kk_free(uvdir->dirents, _ctx);
    return kk_async_error_from_errno(status, _ctx);
  } else {
    kk_vector_t dirents = kk_uv_dirents_to_vec(uvdir, (kk_ssize_t)status, _ctx);
    kk_free(uvdir->dirents, _ctx);
    return kk_std_core__new_Ok(kk_vector_box(dirents, _ctx), _ctx);
  }
}

void kk_std_os_fs_scandir_cb(uv_fs_t* req) {
  kk_context_t* _ctx = kk_get_context();
  kk_uv_callback_t* wrapper = (kk_uv_callback_t*)req->data;
  kk_function_t callback = wrapper->callback;
  ssize_t result = req->result;
  kk_free(wrapper, _ctx);
  if (result < 0){
    uv_fs_req_cleanup(req);
    kk_function_call(void, (kk_function_t, kk_std_core__error, kk_context_t*), callback, (callback, kk_async_error_from_errno(result, _ctx), _ctx), _ctx);
  } else {
    kk_box_t box = kk_std_os_file_dash_uv__uvFsReq_box(kk_std_os_file_dash_uv__new_UvFsReq((intptr_t)req, _ctx), _ctx);
    kk_function_call(void, (kk_function_t, kk_std_core__error, kk_context_t*), callback, (callback, kk_std_core__new_Ok(box, _ctx), _ctx), _ctx);
  }
}

static kk_unit_t kk_uv_fs_scandir(kk_string_t path, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*)fs_req, _ctx);
  kk_ssize_t len;
  uv_fs_scandir(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), 0, kk_std_os_fs_scandir_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_scandir_sync(kk_string_t path, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_ssize_t len;
  int status = uv_fs_scandir(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), 0, NULL);
  kk_string_drop(path, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    kk_box_t box = kk_std_os_file_dash_uv__uvFsReq_box(kk_std_os_file_dash_uv__new_UvFsReq((intptr_t)fs_req, _ctx), _ctx);
    return kk_std_core__new_Ok(box, _ctx);
  }
}

static kk_std_core__error kk_uv_fs_scandir_next(kk_std_os_file_dash_uv__uvFsReq req, kk_context_t* _ctx) {
  uv_dirent_t ent = {0};
  int status = uv_fs_scandir_next((uv_fs_t*)req.internal, &ent);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    kk_box_t dirent = kk_uv_dirent_to_dirent(&ent, NULL, _ctx);
    return kk_std_core__new_Ok(dirent, _ctx);
  }
}

kk_box_t kk_uv_stat_from_uv_stat(uv_stat_t* uvstat, kk_context_t* _ctx) {
  kk_std_os_file_dash_uv__fstat stat = kk_std_os_file_dash_uv__new_Fstat(
    kk_reuse_null,
    0, // cpath
    uvstat->st_dev,
    uvstat->st_mode,
    uvstat->st_nlink,
    uvstat->st_uid,
    uvstat->st_gid,
    uvstat->st_rdev,
    uvstat->st_ino,
    uvstat->st_size,
    uvstat->st_blksize,
    uvstat->st_blocks,
    uvstat->st_flags,
    kk_std_os_file_dash_uv__new_Timespec(uvstat->st_atim.tv_sec, uvstat->st_atim.tv_nsec, _ctx),
    kk_std_os_file_dash_uv__new_Timespec(uvstat->st_mtim.tv_sec, uvstat->st_mtim.tv_nsec, _ctx),
    kk_std_os_file_dash_uv__new_Timespec(uvstat->st_ctim.tv_sec, uvstat->st_ctim.tv_nsec, _ctx),
    kk_std_os_file_dash_uv__new_Timespec(uvstat->st_birthtim.tv_sec, uvstat->st_birthtim.tv_nsec, _ctx),
    _ctx
  );
  return kk_std_os_file_dash_uv__fstat_box(stat, _ctx);
}

static void kk_std_os_fs_stat_cb(uv_fs_t* req) {
  kk_context_t* _ctx = kk_get_context();
  kk_uv_callback_t* wrapper = (kk_uv_callback_t*)req->data;
  kk_function_t callback = wrapper->callback;
  ssize_t result = req->result;
  kk_free(wrapper, _ctx);
  uv_fs_req_cleanup(req);
  if (result < 0) {
    kk_free(stat, _ctx);
    kk_function_call(void, (kk_function_t, kk_std_core__error, kk_context_t*), callback, (callback, kk_async_error_from_errno(result, _ctx), _ctx), _ctx);
  } else {
    kk_box_t s = kk_uv_stat_from_uv_stat(&req->statbuf, _ctx);
    kk_function_call(void, (kk_function_t, kk_std_core__error, kk_context_t*), callback, (callback, kk_std_core__new_Ok(s, _ctx), _ctx), _ctx);
  }
}

static kk_unit_t kk_uv_fs_stat(kk_string_t path, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*)fs_req, _ctx);
  kk_ssize_t len;
  uv_fs_stat(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_std_os_fs_stat_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_stat_sync(kk_string_t path, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_stat(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), NULL);
  kk_string_drop(path, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    kk_box_t s = kk_uv_stat_from_uv_stat(&fs_req.statbuf, _ctx);
    return kk_std_core__new_Ok(s, _ctx);
  }
}

static kk_unit_t kk_uv_fs_fstat(kk_std_os_file_dash_uv__uvFile file, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*)fs_req, _ctx);
  uv_fs_fstat(uvloop(), fs_req, (uv_file)file.internal, kk_std_os_fs_stat_cb);
  kk_std_os_file_dash_uv__uvFile_drop(file, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_fstat_sync(kk_std_os_file_dash_uv__uvFile file, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  int status = uv_fs_fstat(uvloop(), &fs_req, (uv_file)file.internal, NULL);
  kk_std_os_file_dash_uv__uvFile_drop(file, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    kk_box_t s = kk_uv_stat_from_uv_stat(&fs_req.statbuf, _ctx);
    return kk_std_core__new_Ok(s, _ctx);
  }
}

static kk_unit_t kk_uv_fs_lstat(kk_string_t path, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*)fs_req, _ctx);
  kk_ssize_t len;
  uv_fs_lstat(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_std_os_fs_stat_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_lstat_sync(kk_string_t path, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_lstat(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), NULL);
  kk_string_drop(path, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    kk_box_t s = kk_uv_stat_from_uv_stat(&fs_req.statbuf, _ctx);
    return kk_std_core__new_Ok(s, _ctx);
  }
}

static kk_unit_t kk_uv_fs_rename(kk_string_t path, kk_string_t new_path, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*) fs_req, _ctx);
  kk_ssize_t len;
  kk_ssize_t new_len;
  uv_fs_rename(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_string_cbuf_borrow(new_path, &new_len, _ctx), kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  kk_string_drop(new_path, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_rename_sync(kk_string_t path, kk_string_t new_path, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  kk_ssize_t new_len;
  int status = uv_fs_rename(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_string_cbuf_borrow(new_path, &new_len, _ctx), NULL);
  kk_string_drop(path, _ctx);
  kk_string_drop(new_path, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    return kk_std_core__new_Ok(kk_unit_box(kk_Unit), _ctx);
  }
}

static kk_unit_t kk_uv_fs_fsync(kk_std_os_file_dash_uv__uvFile file, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*) fs_req, _ctx);
  uv_fs_fsync(uvloop(), fs_req, (uv_file)file.internal, kk_std_os_fs_unit_cb);
  kk_std_os_file_dash_uv__uvFile_drop(file, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_fsync_sync(kk_std_os_file_dash_uv__uvFile file, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  int status = uv_fs_fsync(uvloop(), &fs_req, (uv_file)file.internal, NULL);
  kk_std_os_file_dash_uv__uvFile_drop(file, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    return kk_std_core__new_Ok(kk_unit_box(kk_Unit), _ctx);
  }
}

static kk_unit_t kk_uv_fs_fdatasync(kk_std_os_file_dash_uv__uvFile file, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*) fs_req, _ctx);
  uv_fs_fdatasync(uvloop(), fs_req, (uv_file)file.internal, kk_std_os_fs_unit_cb);
  kk_std_os_file_dash_uv__uvFile_drop(file, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_fdatasync_sync(kk_std_os_file_dash_uv__uvFile file, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  int status = uv_fs_fdatasync(uvloop(), &fs_req, (uv_file)file.internal, NULL);
  kk_std_os_file_dash_uv__uvFile_drop(file, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    return kk_std_core__new_Ok(kk_unit_box(kk_Unit), _ctx);
  }
}

static kk_unit_t kk_uv_fs_ftruncate(kk_std_os_file_dash_uv__uvFile file, int64_t offset, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*) fs_req, _ctx);
  uv_fs_ftruncate(uvloop(), fs_req, (uv_file)file.internal, offset, kk_std_os_fs_unit_cb);
  kk_std_os_file_dash_uv__uvFile_drop(file, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_ftruncate_sync(kk_std_os_file_dash_uv__uvFile file, int64_t offset, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  int status = uv_fs_ftruncate(uvloop(), &fs_req, (uv_file)file.internal, offset, NULL);
  kk_std_os_file_dash_uv__uvFile_drop(file, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    return kk_std_core__new_Ok(kk_unit_box(kk_Unit), _ctx);
  }
}

static kk_unit_t kk_uv_fs_copyfile(kk_string_t path, kk_string_t new_path, int32_t flags, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*) fs_req, _ctx);
  kk_ssize_t len;
  kk_ssize_t new_len;
  uv_fs_copyfile(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_string_cbuf_borrow(new_path, &new_len, _ctx), flags, kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  kk_string_drop(new_path, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_copyfile_sync(kk_string_t path, kk_string_t new_path, int32_t flags, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  kk_ssize_t new_len;
  int status = uv_fs_copyfile(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_string_cbuf_borrow(new_path, &new_len, _ctx), flags, NULL);
  kk_string_drop(path, _ctx);
  kk_string_drop(new_path, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    return kk_std_core__new_Ok(kk_unit_box(kk_Unit), _ctx);
  }
}

static void kk_std_os_fs_int_cb(uv_fs_t* req) {
  kk_context_t* _ctx = kk_get_context();
  kk_uv_callback_t* wrapper = (kk_uv_callback_t*)req->data;
  kk_function_t callback = wrapper->callback;
  ssize_t result = req->result;
  kk_free(wrapper, _ctx);
  uv_fs_req_cleanup(req);
  if (result < 0) {
    kk_function_call(void, (kk_function_t, kk_std_core__error, kk_context_t*), callback, (callback, kk_async_error_from_errno(result, _ctx), _ctx), _ctx);
  } else {
    kk_function_call(void, (kk_function_t, kk_std_core__error, kk_context_t*), callback, (callback, kk_std_core__new_Ok(kk_integer_box(kk_integer_from_int64(result, _ctx), _ctx), _ctx), _ctx), _ctx);
  }
}

static kk_unit_t kk_uv_fs_sendfile(kk_std_os_file_dash_uv__uvFile out_fd, kk_std_os_file_dash_uv__uvFile in_fd, int64_t in_offset, kk_ssize_t length, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  uv_buf_t buf = uv_buf_init(NULL, 0);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*) fs_req, _ctx);
  uv_fs_sendfile(uvloop(), fs_req, (uv_file)out_fd.internal, (uv_file)in_fd.internal, in_offset, (size_t)length, kk_std_os_fs_int_cb);
  kk_std_os_file_dash_uv__uvFile_drop(out_fd, _ctx);
  kk_std_os_file_dash_uv__uvFile_drop(in_fd, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_sendfile_sync(kk_std_os_file_dash_uv__uvFile out_fd, kk_std_os_file_dash_uv__uvFile in_fd, int64_t in_offset, kk_ssize_t length, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  uv_buf_t buf = uv_buf_init(NULL, 0);
  int status = uv_fs_sendfile(uvloop(), &fs_req, (uv_file)out_fd.internal, (uv_file)in_fd.internal, in_offset, (size_t)length, NULL);
  kk_std_os_file_dash_uv__uvFile_drop(out_fd, _ctx);
  kk_std_os_file_dash_uv__uvFile_drop(in_fd, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    return kk_std_core__new_Ok(kk_integer_box(kk_integer_from_int64(fs_req.result, _ctx), _ctx), _ctx);
  }
}

static kk_unit_t kk_uv_fs_access(kk_string_t path, int32_t mode, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*) fs_req, _ctx);
  uv_fs_access(uvloop(), fs_req, kk_string_cbuf_borrow(path, NULL, _ctx), mode, kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_access_sync(kk_string_t path, int32_t mode, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  int status = uv_fs_access(uvloop(), &fs_req, kk_string_cbuf_borrow(path, NULL, _ctx), mode, NULL);
  kk_string_drop(path, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    return kk_std_core__new_Ok(kk_unit_box(kk_Unit), _ctx);
  }
}

static kk_unit_t kk_uv_fs_chmod(kk_string_t path, int32_t mode, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*) fs_req, _ctx);
  uv_fs_chmod(uvloop(), fs_req, kk_string_cbuf_borrow(path, NULL, _ctx), mode, kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_chmod_sync(kk_string_t path, int32_t mode, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  int status = uv_fs_chmod(uvloop(), &fs_req, kk_string_cbuf_borrow(path, NULL, _ctx), mode, NULL);
  kk_string_drop(path, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    return kk_std_core__new_Ok(kk_unit_box(kk_Unit), _ctx);
  }
}

static kk_unit_t kk_uv_fs_fchmod(kk_std_os_file_dash_uv__uvFile file, int32_t mode, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*) fs_req, _ctx);
  uv_fs_fchmod(uvloop(), fs_req, (uv_file)file.internal, mode, kk_std_os_fs_unit_cb);
  kk_std_os_file_dash_uv__uvFile_drop(file, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_fchmod_sync(kk_std_os_file_dash_uv__uvFile file, int32_t mode, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  int status = uv_fs_fchmod(uvloop(), &fs_req, (uv_file)file.internal, mode, NULL);
  kk_std_os_file_dash_uv__uvFile_drop(file, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    return kk_std_core__new_Ok(kk_unit_box(kk_Unit), _ctx);
  }
}

static kk_unit_t kk_uv_fs_utime(kk_string_t path, double atime, double mtime, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*) fs_req, _ctx);
  kk_ssize_t len;
  uv_fs_utime(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), atime, mtime, kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_utime_sync(kk_string_t path, double atime, double mtime, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_utime(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), atime, mtime, NULL);
  kk_string_drop(path, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    return kk_std_core__new_Ok(kk_unit_box(kk_Unit), _ctx);
  }
}

static kk_unit_t kk_uv_fs_futime(kk_std_os_file_dash_uv__uvFile file, double atime, double mtime, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*) fs_req, _ctx);
  kk_ssize_t len;
  uv_fs_futime(uvloop(), fs_req, (uv_file)file.internal, atime, mtime, kk_std_os_fs_unit_cb);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_futime_sync(kk_std_os_file_dash_uv__uvFile file, double atime, double mtime, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_futime(uvloop(), &fs_req, (uv_file)file.internal, atime, mtime, NULL);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    return kk_std_core__new_Ok(kk_unit_box(kk_Unit), _ctx);
  }
}

static kk_unit_t kk_uv_fs_lutime(kk_string_t path, double atime, double mtime, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*) fs_req, _ctx);
  kk_ssize_t len;
  uv_fs_lutime(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), atime, mtime, kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_lutime_sync(kk_string_t path, double atime, double mtime, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_lutime(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), atime, mtime, NULL);
  kk_string_drop(path, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    return kk_std_core__new_Ok(kk_unit_box(kk_Unit), _ctx);
  }
}

static kk_unit_t kk_uv_fs_link(kk_string_t path, kk_string_t new_path, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*) fs_req, _ctx);
  kk_ssize_t len;
  kk_ssize_t new_len;
  uv_fs_link(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_string_cbuf_borrow(new_path, &new_len, _ctx), kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  kk_string_drop(new_path, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_link_sync(kk_string_t path, kk_string_t new_path, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  kk_ssize_t new_len;
  int status = uv_fs_link(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_string_cbuf_borrow(new_path, &new_len, _ctx), NULL);
  kk_string_drop(path, _ctx);
  kk_string_drop(new_path, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    return kk_std_core__new_Ok(kk_unit_box(kk_Unit), _ctx);
  }
}

static kk_unit_t kk_uv_fs_symlink(kk_string_t path, kk_string_t new_path, int32_t flags, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*) fs_req, _ctx);
  kk_ssize_t len;
  kk_ssize_t new_len;
  uv_fs_symlink(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_string_cbuf_borrow(new_path, &new_len, _ctx), flags, kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  kk_string_drop(new_path, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_symlink_sync(kk_string_t path, kk_string_t new_path, int32_t flags, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  kk_ssize_t new_len;
  int status = uv_fs_symlink(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_string_cbuf_borrow(new_path, &new_len, _ctx), flags, NULL);
  kk_string_drop(path, _ctx);
  kk_string_drop(new_path, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    return kk_std_core__new_Ok(kk_unit_box(kk_Unit), _ctx);
  }
}

void kk_std_os_fs_string_cb(uv_fs_t* req) {
  kk_context_t* _ctx = kk_get_context();
  kk_uv_callback_t* wrapper = (kk_uv_callback_t*)req->data;
  kk_function_t callback = wrapper->callback;
  ssize_t result = req->result;
  kk_free(wrapper, _ctx);
  uv_fs_req_cleanup(req);
  if (result < 0) {
    kk_function_call(void, (kk_function_t, kk_std_core__error, kk_context_t*), callback, (callback, kk_async_error_from_errno(result, _ctx), _ctx), _ctx);
  } else {
    kk_string_t s = kk_string_alloc_raw((const char*)req->ptr, true, _ctx);
    kk_function_call(void, (kk_function_t, kk_std_core__error, kk_context_t*), callback, (callback, kk_std_core__new_Ok(kk_string_box(s), _ctx), _ctx), _ctx);
  }
}

static kk_unit_t kk_uv_fs_readlink(kk_string_t path, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*) fs_req, _ctx);
  kk_ssize_t len;
  uv_fs_readlink(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_std_os_fs_string_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_readlink_sync(kk_string_t path, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_readlink(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), NULL);
  kk_string_drop(path, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    kk_string_t s = kk_string_alloc_raw((const char*)fs_req.ptr, true, _ctx);
    return kk_std_core__new_Ok(kk_string_box(s), _ctx);
  }
}

static kk_unit_t kk_uv_fs_realpath(kk_string_t path, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*)fs_req, _ctx);
  kk_ssize_t len;
  uv_fs_realpath(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_std_os_fs_string_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_realpath_sync(kk_string_t path, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_realpath(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), NULL);
  kk_string_drop(path, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    kk_string_t s = kk_string_alloc_raw((const char*)fs_req.ptr, true, _ctx);
    return kk_std_core__new_Ok(kk_string_box(s), _ctx);
  }
}

static kk_unit_t kk_uv_fs_chown(kk_string_t path, int32_t uid, int32_t gid, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*)fs_req, _ctx);
  kk_ssize_t len;
  uv_fs_chown(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), uid, gid, kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_chown_sync(kk_string_t path, int32_t uid, int32_t gid, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_chown(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), uid, gid, NULL);
  kk_string_drop(path, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  }
  return kk_std_core__new_Ok(kk_unit_box(kk_Unit), _ctx);
}

static kk_unit_t kk_uv_fs_fchown(kk_std_os_file_dash_uv__uvFile file, int32_t uid, int32_t gid, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*)fs_req, _ctx);
  uv_fs_fchown(uvloop(), fs_req, (uv_file)file.internal, uid, gid, kk_std_os_fs_unit_cb);
  kk_std_os_file_dash_uv__uvFile_drop(file, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_fchown_sync(kk_std_os_file_dash_uv__uvFile file, int32_t uid, int32_t gid, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  int status = uv_fs_fchown(uvloop(), &fs_req, (uv_file)file.internal, uid, gid, NULL);
  kk_std_os_file_dash_uv__uvFile_drop(file, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  }
  return kk_std_core__new_Ok(kk_unit_box(kk_Unit), _ctx);
}

static kk_unit_t kk_uv_fs_lchown(kk_string_t path, int32_t uid, int32_t gid, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_callback_t* wrapper = kk_new_uv_callback(cb, (uv_handle_t*)fs_req, _ctx);
  kk_ssize_t len;
  uv_fs_lchown(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), uid, gid, kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core__error kk_uv_fs_lchown_sync(kk_string_t path, int32_t uid, int32_t gid, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_lchown(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), uid, gid, NULL);
  kk_string_drop(path, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  }
  return kk_std_core__new_Ok(kk_unit_box(kk_Unit), _ctx);
}

