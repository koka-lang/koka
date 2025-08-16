
#include "kklib/box.h"
#include "uv_event_dash_loop.h"


void kk_free_fs(void* p, kk_block_t* block, kk_context_t* _ctx) {
    uv_fs_t* req = (uv_fs_t*)p;
    uv_fs_req_cleanup(req);
    kk_info_message("Freeing fs request\n", kk_context());
    kk_free(p, _ctx);
}

#define kk_new_fs_req_cb(req, cb) kk_new_req_cb(uv_fs_t, req, cb)
#define kk_fs_to_uv(req) kk_owned_handle_to_uv_handle(uv_fs_t, req)
#define kk_uv_to_fs(req) uv_handle_to_owned_kk_handle(req, kk_free_fs, file, fs_req)
#define kk_uv_to_fs_box(req) uv_handle_to_owned_kk_handle_box(req, kk_free_fs, file, fs_req)

// Need variants of the check macros that cleanup the request before returning
// Sometimes the return value is a file descriptor which is why this is a < UV_OK check instead of == UV_OK
#define kk_uv_fs_check_return(req, err, result) \
  kk_std_core_exn__error internal_ret; \
  if (err < UV_OK) { \
    internal_ret = kk_async_error_from_errno(err, kk_context()); \
  } else { \
    internal_ret = kk_std_core_exn__new_Ok(result, kk_context()); \
  } \
 uv_fs_req_cleanup(req); \
 return internal_ret;
#define kk_uv_fs_check(req, err) kk_uv_fs_check_return(req, err, kk_unit_box(kk_Unit))

#define kk_callback_result(req, cb, res) \
  kk_context_t* _ctx = kk_get_context(); \
  kk_function_t cb = kk_function_from_ptr(req->data, _ctx); \
  ssize_t res = req->result;

static void kk_std_os_fs_unit_cb(uv_fs_t* req) {
  kk_callback_result(req, callback, result)
  uv_fs_req_cleanup(req);
  if (result < 0) {
    kk_uv_error_callback(callback, req->result)
  } else {
    kk_uv_okay_callback(callback, kk_unit_box(kk_Unit))
  }
}

static kk_unit_t kk_uv_fs_close(kk_uv_file__uv_file file, kk_function_t cb, kk_context_t* _ctx) {
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_close(uvloop(), fs_req, (uv_file)(file.internal), kk_std_os_fs_unit_cb);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_close_sync(kk_uv_file__uv_file file, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_uv_check_return(uv_fs_close(uvloop(), &fs_req, (uv_file)(file.internal), NULL), kk_unit_box(kk_Unit))
}

static void kk_std_os_file_open_cb(uv_fs_t* req) {
  kk_callback_result(req, callback, result)
  uv_fs_req_cleanup(req);
  if (result < 0) {
    kk_uv_error_callback(callback, result)
  } else {
    kk_uv_okay_callback(callback, kk_uv_file__uv_file_box(kk_uv_file__new_Uv_file((intptr_t)result, _ctx), _ctx))
  }
}

static kk_unit_t kk_uv_fs_open(kk_string_t path, int32_t flags, int32_t mode, kk_function_t cb, kk_context_t* _ctx) {
  kk_ssize_t len;
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_open(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), flags, mode, kk_std_os_file_open_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_open_sync(kk_string_t path, int32_t flags, int32_t mode, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int fd = uv_fs_open(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), flags, mode, NULL);
  kk_string_drop(path, _ctx);
  kk_uv_fs_check_return(&fs_req, fd, kk_uv_file__uv_file_box(kk_uv_file__new_Uv_file((intptr_t)fd, _ctx), _ctx))
}

static void kk_std_os_file_buff_cb(uv_fs_t* req) {
  kk_context_t* _ctx = kk_get_context();
  kk_uv_buff_callback_t* wrapper = (kk_uv_buff_callback_t*)req->data;
  kk_function_t callback = wrapper->callback;
  kk_bytes_t bytes = wrapper->bytes;
  // kk_info_message("Clength %d", req->result);
  ssize_t result = req->result;
  kk_free(wrapper, _ctx);
  uv_fs_req_cleanup(req);
  if (result < 0) {
    kk_bytes_drop(bytes, _ctx);
    kk_uv_error_callback(callback, result)
  } else {
    kk_bytes_t btsadj = kk_bytes_adjust_length(bytes, (kk_ssize_t)result + 1, _ctx);
    kk_bytes_set(btsadj, (uint64_t)result, (int8_t)'\0', _ctx);
    // TODO?: Maybe would be better to not add a terminating null byte, and fix it when converting to a string instead?
    kk_std_core_types__tuple2 tuple = kk_std_core_types__new_Tuple2(kk_bytes_box(btsadj), kk_integer_box(kk_integer_from_ssize_t(result, _ctx), _ctx), _ctx); /*(1004, 1005)*/
    kk_box_t tupleboxed = kk_std_core_types__tuple2_box(tuple, _ctx);
    kk_uv_okay_callback(callback, tupleboxed)
  }
}

static kk_unit_t kk_uv_fs_read(kk_uv_file__uv_file file, kk_bytes_t bytes, ssize_t offset, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_uv_buff_callback_t* wrapper = kk_new_uv_buff_callback(cb, bytes, (uv_handle_t*)fs_req, _ctx);
  uv_buf_t* uv_buffs = kk_malloc(sizeof(uv_buf_t)*1, _ctx);
  uv_buffs[0].base = (char*)kk_bytes_cbuf_borrow(bytes, &uv_buffs[0].len, _ctx);
  uv_fs_read(uvloop(), fs_req, (uv_file)file.internal, uv_buffs, 1, offset, kk_std_os_file_buff_cb);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_read_sync(kk_uv_file__uv_file file, kk_bytes_t bytes, int32_t offset, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  uv_buf_t uv_buffs[1] = {0};
  uv_buffs[0].base = (char*)kk_bytes_cbuf_borrow(bytes, &uv_buffs[0].len, _ctx);
  ssize_t result = uv_fs_read(uvloop(), &fs_req, (uv_file)file.internal, uv_buffs, 1, offset, NULL);
  if (result < 0) {
    kk_bytes_drop(bytes, _ctx);
    return kk_async_error_from_errno(result, _ctx);
  } else {
    kk_bytes_t btsadj = kk_bytes_adjust_length(bytes, (kk_ssize_t)result, _ctx);
    kk_std_core_types__tuple2 tuple = kk_std_core_types__new_Tuple2(kk_bytes_box(btsadj), kk_integer_box(kk_integer_from_ssize_t(result, _ctx), _ctx), _ctx); /*(1004, 1005)*/
    kk_box_t tupleboxed = kk_std_core_types__tuple2_box(tuple, _ctx);
    return kk_std_core_exn__new_Ok(tupleboxed, _ctx);
  }
}

static kk_unit_t kk_uv_fs_unlink(kk_string_t path, kk_function_t cb, kk_context_t* _ctx) {
  kk_ssize_t len;
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_unlink(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_unlink_sync(kk_string_t path, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_unlink(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), NULL);
  kk_string_drop(path, _ctx);
  kk_uv_fs_check(&fs_req, status)
}

static void kk_std_os_fs_write_cb(uv_fs_t* req) {
  kk_callback_result(req, callback, result)
  uv_fs_req_cleanup(req);
  if (result < 0) {
    kk_uv_error_callback(callback, result)
  } else {
    kk_uv_okay_callback(callback, kk_integer_box(kk_integer_from_ssize_t(result, _ctx), _ctx))
  }
}

static kk_unit_t kk_uv_fs_write(kk_uv_file__uv_file file, kk_bytes_t bytes, int64_t offset, kk_function_t cb, kk_context_t* _ctx) {
  kk_new_fs_req_cb(fs_req, cb)
  uv_buf_t* uv_buffs = kk_bytes_to_uv_buffs(bytes, _ctx);
  uv_fs_write(uvloop(), fs_req, (uv_file)file.internal, uv_buffs, 1, offset, kk_std_os_fs_write_cb);
  kk_bytes_drop(bytes, _ctx);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_write_sync(kk_uv_file__uv_file file, kk_bytes_t bytes, int64_t offset, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  uv_buf_t* uv_buffs = kk_bytes_to_uv_buffs(bytes, _ctx);
  int64_t result = uv_fs_write(uvloop(), &fs_req, (uv_file)file.internal, uv_buffs, 1, offset, NULL);
  kk_bytes_drop(bytes, _ctx);
  kk_uv_fs_check_return(&fs_req, result, kk_integer_box(kk_integer_from_int64(result, _ctx), _ctx))
}

static kk_unit_t kk_uv_fs_mkdir(kk_string_t path, int32_t mode, kk_function_t cb, kk_context_t* _ctx) {
  kk_ssize_t len;
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_mkdir(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), mode, kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_mkdir_sync(kk_string_t path, int32_t mode, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_mkdir(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), mode, NULL);
  kk_string_drop(path, _ctx);
  kk_uv_fs_check(&fs_req, status)
}

static void kk_std_os_fs_mkdtemp_cb(uv_fs_t* req) {
  kk_callback_result(req, callback, result)
  if (result < 0) {
    uv_fs_req_cleanup(req);
    kk_uv_error_callback(callback, result)
  } else {
    kk_string_t str = kk_string_alloc_raw((const char*) req->path, true, _ctx);
    uv_fs_req_cleanup(req);
    kk_uv_okay_callback(callback, kk_string_box(str))
  }
}

static kk_unit_t kk_uv_fs_mkdtemp(kk_string_t tpl, kk_function_t cb, kk_context_t* _ctx) {
  kk_ssize_t len;
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_mkdtemp(uvloop(), fs_req, kk_string_cbuf_borrow(tpl, &len, _ctx), kk_std_os_fs_mkdtemp_cb);
  kk_string_drop(tpl, _ctx);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_mkdtemp_sync(kk_string_t tpl, kk_context_t* _ctx) {
  kk_ssize_t len;
  uv_fs_t fs_req = {0};
  int status = uv_fs_mkdtemp(uvloop(), &fs_req, kk_string_cbuf_borrow(tpl, &len, _ctx), NULL);
  kk_string_drop(tpl, _ctx);
  kk_uv_fs_check_return(&fs_req, status, kk_string_box(kk_string_alloc_from_utf8((const char*) fs_req.path, _ctx)))
}

static void kk_std_os_fs_mkstemp_cb(uv_fs_t* req) {
  kk_callback_result(req, callback, result)
  if (result < 0) {
    uv_fs_req_cleanup(req);
    kk_uv_error_callback(callback, result)
  } else {
    kk_string_t str = kk_string_alloc_raw((const char*) req->path, true, _ctx);
    uv_fs_req_cleanup(req);
    kk_uv_file__uv_file file = kk_uv_file__new_Uv_file((intptr_t)result, _ctx);
    kk_std_core_types__tuple2 tuple = kk_std_core_types__new_Tuple2(kk_string_box(str), kk_uv_file__uv_file_box(file, _ctx), _ctx); /*(1004, 1005)*/
    kk_box_t tupleboxed = kk_std_core_types__tuple2_box(tuple, _ctx);
    kk_uv_okay_callback(callback, tupleboxed)
  }
}

static kk_unit_t kk_uv_fs_mkstemp(kk_string_t tpl, kk_function_t cb, kk_context_t* _ctx) {
  kk_ssize_t len;
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_mkstemp(uvloop(), fs_req, kk_string_cbuf_borrow(tpl, &len, _ctx), kk_std_os_fs_mkdtemp_cb);
  kk_string_drop(tpl, _ctx);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_mkstemp_sync(kk_string_t tpl, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_mkstemp(uvloop(), &fs_req, kk_string_cbuf_borrow(tpl, &len, _ctx), NULL);
  kk_string_drop(tpl, _ctx);
  if (status < 0) {
    return kk_async_error_from_errno(status, _ctx);
  } else {
    kk_string_t str = kk_string_alloc_raw((const char*) fs_req.path, true, _ctx);
    kk_uv_file__uv_file file = kk_uv_file__new_Uv_file((intptr_t)status, _ctx);
    kk_std_core_types__tuple2 tuple = kk_std_core_types__new_Tuple2(kk_uv_file__uv_file_box(file, _ctx), kk_string_box(str), _ctx); /*(1004, 1005)*/
    kk_box_t tupleboxed = kk_std_core_types__tuple2_box(tuple, _ctx);
    return kk_std_core_exn__new_Ok(tupleboxed, _ctx);
  }
}

static kk_unit_t kk_uv_fs_rmdir(kk_string_t path, kk_function_t cb, kk_context_t* _ctx) {
  kk_ssize_t len;
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_rmdir(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_rmdir_sync(kk_string_t path, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_rmdir(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), NULL);
  kk_string_drop(path, _ctx);
  kk_uv_fs_check(&fs_req, status)
}

void kk_std_os_fs_opendir_cb(uv_fs_t* req) {
  kk_callback_result(req, callback, result)
  uv_dir_t* dir = (uv_dir_t*)req->ptr;
  uv_fs_req_cleanup(req);
  if (result < 0) {
    kk_uv_error_callback(callback, result)
  } else {
    kk_uv_file__uv_dir d = kk_uv_file__new_Uv_dir((intptr_t)dir, _ctx);
    kk_uv_okay_callback(callback, kk_uv_file__uv_dir_box(d, _ctx))
  }
}

static kk_unit_t kk_uv_fs_opendir(kk_string_t path, kk_function_t cb, kk_context_t* _ctx) {
  kk_ssize_t len;
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_opendir(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_std_os_fs_opendir_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_opendir_sync(kk_string_t path, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_opendir(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), NULL);
  kk_string_drop(path, _ctx);
  kk_uv_fs_check_return(&fs_req, status, kk_uv_file__uv_dir_box(kk_uv_file__new_Uv_dir((intptr_t)fs_req.ptr, _ctx), _ctx))
}

static kk_unit_t kk_uv_fs_closedir(kk_uv_file__uv_dir dir, kk_function_t cb, kk_context_t* _ctx) {
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_closedir(uvloop(), fs_req, (uv_dir_t*)dir.internal, kk_std_os_fs_unit_cb);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_closedir_sync(kk_uv_file__uv_dir dir, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_uv_fs_check_return(&fs_req, uv_fs_closedir(uvloop(), &fs_req, (uv_dir_t*)dir.internal, NULL), kk_unit_box(kk_Unit))
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
  kk_uv_file__dirent_type type;
  switch (dirent->type) {
    case UV_DIRENT_FILE: type = kk_uv_file__new_FILE(_ctx); break;
    case UV_DIRENT_DIR: type = kk_uv_file__new_DIR(_ctx); break;
    case UV_DIRENT_LINK: type = kk_uv_file__new_LINK(_ctx); break;
    case UV_DIRENT_FIFO: type = kk_uv_file__new_FIFO(_ctx); break;
    case UV_DIRENT_SOCKET: type = kk_uv_file__new_SOCKET(_ctx); break;
    case UV_DIRENT_CHAR: type = kk_uv_file__new_CHAR(_ctx); break;
    case UV_DIRENT_BLOCK: type = kk_uv_file__new_BLOCK(_ctx); break;
    default: type = kk_uv_file__new_UNKNOWN__DIRECTORY__ENTRY(_ctx); break;
  }
  kk_box_t box = kk_uv_file__dirent_box(kk_uv_file__new_Dirent(name, type, _ctx), _ctx);
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
    kk_uv_error_callback(callback, result)
  } else {
    kk_vector_t dirents = kk_uv_dirents_to_vec(uvdir, (kk_ssize_t)result, _ctx);
    kk_free(uvdir->dirents, _ctx);
    kk_uv_okay_callback(callback, kk_vector_box(dirents, _ctx))
  }
}

static kk_unit_t kk_uv_fs_readdir(kk_uv_file__uv_dir dir, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  // Read up to 500 entries in the directory
  kk_uv_dir_callback_t* wrapper = kk_new_uv_dir_callback(cb, (uv_handle_t*)fs_req, (uv_dir_t*) dir.internal, 500, _ctx);
  uv_fs_readdir(uvloop(), fs_req, wrapper->uvdir, kk_std_os_fs_readdir_cb);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_readdir_sync(kk_uv_file__uv_dir dir, kk_context_t* _ctx) {
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
    return kk_std_core_exn__new_Ok(kk_vector_box(dirents, _ctx), _ctx);
  }
}

void kk_std_os_fs_scandir_cb(uv_fs_t* req) {
  kk_callback_result(req, callback, result)
  if (result < 0){
    uv_fs_req_cleanup(req);
    kk_uv_error_callback(callback, result)
  } else { // TODO: This is not correct, we don't want a new reference counted box here
    kk_uv_okay_callback(callback, kk_uv_to_fs_box(req))
  }
}

static kk_unit_t kk_uv_fs_scandir(kk_string_t path, kk_function_t cb, kk_context_t* _ctx) {
  kk_ssize_t len;
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_scandir(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), 0, kk_std_os_fs_scandir_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_scandir_sync(kk_string_t path, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  kk_ssize_t len;
  int status = uv_fs_scandir(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), 0, NULL);
  kk_string_drop(path, _ctx);
  kk_uv_check_return(status, kk_uv_to_fs_box(fs_req))
}

static kk_std_core_exn__error kk_uv_fs_scandir_next(kk_uv_file__uv_fs_req req, kk_context_t* _ctx) {
  uv_dirent_t ent = {0};
  int status = uv_fs_scandir_next(kk_fs_to_uv(req), &ent);
  kk_uv_check_return(status, kk_uv_dirent_to_dirent(&ent, NULL, _ctx))
}

static void kk_std_os_fs_stat_cb(uv_fs_t* req) {
  kk_callback_result(req, callback, result)
  uv_fs_req_cleanup(req);
  if (result < 0) {
    kk_uv_error_callback(callback, result)
  } else {
    kk_box_t s = kk_uv_file__fstat_box(kk_uv_stat_from_uv_stat(&req->statbuf, _ctx), _ctx);
    kk_uv_okay_callback(callback, s)
  }
}

static kk_unit_t kk_uv_fs_stat(kk_string_t path, kk_function_t cb, kk_context_t* _ctx) {
  kk_ssize_t len;
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_stat(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_std_os_fs_stat_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_stat_sync(kk_string_t path, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_stat(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), NULL);
  kk_string_drop(path, _ctx);
  kk_uv_fs_check_return(&fs_req, status, kk_uv_file__fstat_box(kk_uv_stat_from_uv_stat(&fs_req.statbuf, _ctx), _ctx))
}

static kk_unit_t kk_uv_fs_fstat(kk_uv_file__uv_file file, kk_function_t cb, kk_context_t* _ctx) {
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_fstat(uvloop(), fs_req, (uv_file)file.internal, kk_std_os_fs_stat_cb);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_fstat_sync(kk_uv_file__uv_file file, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  int status = uv_fs_fstat(uvloop(), &fs_req, (uv_file)file.internal, NULL);
  kk_uv_fs_check_return(&fs_req, status, kk_uv_file__fstat_box(kk_uv_stat_from_uv_stat(&fs_req.statbuf, _ctx), _ctx))
}

static kk_unit_t kk_uv_fs_lstat(kk_string_t path, kk_function_t cb, kk_context_t* _ctx) {
  kk_ssize_t len;
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_lstat(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_std_os_fs_stat_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_lstat_sync(kk_string_t path, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_lstat(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), NULL);
  kk_string_drop(path, _ctx);
  kk_uv_fs_check_return(&fs_req, status, kk_uv_file__fstat_box(kk_uv_stat_from_uv_stat(&fs_req.statbuf, _ctx), _ctx))
}

static kk_unit_t kk_uv_fs_rename(kk_string_t path, kk_string_t new_path, kk_function_t cb, kk_context_t* _ctx) {
  kk_ssize_t len;
  kk_new_fs_req_cb(fs_req, cb)
  kk_ssize_t new_len;
  uv_fs_rename(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_string_cbuf_borrow(new_path, &new_len, _ctx), kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  kk_string_drop(new_path, _ctx);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_rename_sync(kk_string_t path, kk_string_t new_path, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  kk_ssize_t new_len;
  int status = uv_fs_rename(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_string_cbuf_borrow(new_path, &new_len, _ctx), NULL);
  kk_string_drop(path, _ctx);
  kk_string_drop(new_path, _ctx);
  kk_uv_fs_check(&fs_req, status)
}

static kk_unit_t kk_uv_fs_fsync(kk_uv_file__uv_file file, kk_function_t cb, kk_context_t* _ctx) {
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_fsync(uvloop(), fs_req, (uv_file)file.internal, kk_std_os_fs_unit_cb);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_fsync_sync(kk_uv_file__uv_file file, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  int status = uv_fs_fsync(uvloop(), &fs_req, (uv_file)file.internal, NULL);
  kk_uv_fs_check(&fs_req, status)
}

static kk_unit_t kk_uv_fs_fdatasync(kk_uv_file__uv_file file, kk_function_t cb, kk_context_t* _ctx) {
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_fdatasync(uvloop(), fs_req, (uv_file)file.internal, kk_std_os_fs_unit_cb);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_fdatasync_sync(kk_uv_file__uv_file file, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  int status = uv_fs_fdatasync(uvloop(), &fs_req, (uv_file)file.internal, NULL);
  kk_uv_fs_check(&fs_req, status)
}

static kk_unit_t kk_uv_fs_ftruncate(kk_uv_file__uv_file file, int64_t offset, kk_function_t cb, kk_context_t* _ctx) {
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_ftruncate(uvloop(), fs_req, (uv_file)file.internal, offset, kk_std_os_fs_unit_cb);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_ftruncate_sync(kk_uv_file__uv_file file, int64_t offset, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  int status = uv_fs_ftruncate(uvloop(), &fs_req, (uv_file)file.internal, offset, NULL);
  kk_uv_fs_check(&fs_req, status)
}

static kk_unit_t kk_uv_fs_copyfile(kk_string_t path, kk_string_t new_path, int32_t flags, kk_function_t cb, kk_context_t* _ctx) {
  kk_ssize_t len;
  kk_new_fs_req_cb(fs_req, cb)
  kk_ssize_t new_len;
  uv_fs_copyfile(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_string_cbuf_borrow(new_path, &new_len, _ctx), flags, kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  kk_string_drop(new_path, _ctx);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_copyfile_sync(kk_string_t path, kk_string_t new_path, int32_t flags, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  kk_ssize_t new_len;
  int status = uv_fs_copyfile(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_string_cbuf_borrow(new_path, &new_len, _ctx), flags, NULL);
  kk_string_drop(path, _ctx);
  kk_string_drop(new_path, _ctx);
  kk_uv_fs_check(&fs_req, status)
}

static void kk_std_os_fs_int_cb(uv_fs_t* req) {
  kk_callback_result(req, callback, result)
  uv_fs_req_cleanup(req);
  if (result < 0) {
    kk_uv_error_callback(callback, result)
  } else {
    kk_uv_okay_callback(callback, kk_integer_box(kk_integer_from_int64(result, _ctx), _ctx))
  }
}

static kk_unit_t kk_uv_fs_sendfile(kk_uv_file__uv_file out_fd, kk_uv_file__uv_file in_fd, int64_t in_offset, kk_ssize_t length, kk_function_t cb, kk_context_t* _ctx) {
  uv_fs_t* fs_req = kk_malloc(sizeof(uv_fs_t), _ctx);
  uv_buf_t buf = uv_buf_init(NULL, 0);
  fs_req->data = kk_function_as_ptr(cb, _ctx);
  uv_fs_sendfile(uvloop(), fs_req, (uv_file)out_fd.internal, (uv_file)in_fd.internal, in_offset, (size_t)length, kk_std_os_fs_int_cb);
  kk_uv_file__uv_file_drop(out_fd, _ctx);
  kk_uv_file__uv_file_drop(in_fd, _ctx);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_sendfile_sync(kk_uv_file__uv_file out_fd, kk_uv_file__uv_file in_fd, int64_t in_offset, kk_ssize_t length, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  uv_buf_t buf = uv_buf_init(NULL, 0);
  int status = uv_fs_sendfile(uvloop(), &fs_req, (uv_file)out_fd.internal, (uv_file)in_fd.internal, in_offset, (size_t)length, NULL);
  kk_uv_file__uv_file_drop(out_fd, _ctx);
  kk_uv_file__uv_file_drop(in_fd, _ctx);
  kk_uv_fs_check_return(&fs_req, status, kk_integer_box(kk_integer_from_int64(fs_req.result, _ctx), _ctx))
}

static kk_unit_t kk_uv_fs_access(kk_string_t path, int32_t mode, kk_function_t cb, kk_context_t* _ctx) {
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_access(uvloop(), fs_req, kk_string_cbuf_borrow(path, NULL, _ctx), mode, kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_access_sync(kk_string_t path, int32_t mode, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  int status = uv_fs_access(uvloop(), &fs_req, kk_string_cbuf_borrow(path, NULL, _ctx), mode, NULL);
  kk_string_drop(path, _ctx);
  kk_uv_fs_check(&fs_req, status)
}

static kk_unit_t kk_uv_fs_chmod(kk_string_t path, int32_t mode, kk_function_t cb, kk_context_t* _ctx) {
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_chmod(uvloop(), fs_req, kk_string_cbuf_borrow(path, NULL, _ctx), mode, kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_chmod_sync(kk_string_t path, int32_t mode, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  int status = uv_fs_chmod(uvloop(), &fs_req, kk_string_cbuf_borrow(path, NULL, _ctx), mode, NULL);
  kk_string_drop(path, _ctx);
  kk_uv_fs_check(&fs_req, status)
}

static kk_unit_t kk_uv_fs_fchmod(kk_uv_file__uv_file file, int32_t mode, kk_function_t cb, kk_context_t* _ctx) {
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_fchmod(uvloop(), fs_req, (uv_file)file.internal, mode, kk_std_os_fs_unit_cb);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_fchmod_sync(kk_uv_file__uv_file file, int32_t mode, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  int status = uv_fs_fchmod(uvloop(), &fs_req, (uv_file)file.internal, mode, NULL);
  kk_uv_fs_check(&fs_req, status)
}

static kk_unit_t kk_uv_fs_utime(kk_string_t path, double atime, double mtime, kk_function_t cb, kk_context_t* _ctx) {
  kk_ssize_t len;
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_utime(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), atime, mtime, kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_utime_sync(kk_string_t path, double atime, double mtime, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_utime(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), atime, mtime, NULL);
  kk_string_drop(path, _ctx);
  kk_uv_fs_check(&fs_req, status)
}

static kk_unit_t kk_uv_fs_futime(kk_uv_file__uv_file file, double atime, double mtime, kk_function_t cb, kk_context_t* _ctx) {
  kk_ssize_t len;
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_futime(uvloop(), fs_req, (uv_file)file.internal, atime, mtime, kk_std_os_fs_unit_cb);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_futime_sync(kk_uv_file__uv_file file, double atime, double mtime, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  kk_uv_fs_check_return(&fs_req, uv_fs_futime(uvloop(), &fs_req, (uv_file)file.internal, atime, mtime, NULL), kk_unit_box(kk_Unit))
}

static kk_unit_t kk_uv_fs_lutime(kk_string_t path, double atime, double mtime, kk_function_t cb, kk_context_t* _ctx) {
  kk_ssize_t len;
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_lutime(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), atime, mtime, kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_lutime_sync(kk_string_t path, double atime, double mtime, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_lutime(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), atime, mtime, NULL);
  kk_string_drop(path, _ctx);
  kk_uv_fs_check(&fs_req, status)
}

static kk_unit_t kk_uv_fs_link(kk_string_t path, kk_string_t new_path, kk_function_t cb, kk_context_t* _ctx) {
  kk_ssize_t len;
  kk_new_fs_req_cb(fs_req, cb)
  kk_ssize_t new_len;
  uv_fs_link(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_string_cbuf_borrow(new_path, &new_len, _ctx), kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  kk_string_drop(new_path, _ctx);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_link_sync(kk_string_t path, kk_string_t new_path, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  kk_ssize_t new_len;
  int status = uv_fs_link(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_string_cbuf_borrow(new_path, &new_len, _ctx), NULL);
  kk_string_drop(path, _ctx);
  kk_string_drop(new_path, _ctx);
  kk_uv_fs_check(&fs_req, status)
}

static kk_unit_t kk_uv_fs_symlink(kk_string_t path, kk_string_t new_path, int32_t flags, kk_function_t cb, kk_context_t* _ctx) {
  kk_ssize_t len;
  kk_new_fs_req_cb(fs_req, cb)
  kk_ssize_t new_len;
  uv_fs_symlink(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_string_cbuf_borrow(new_path, &new_len, _ctx), flags, kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  kk_string_drop(new_path, _ctx);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_symlink_sync(kk_string_t path, kk_string_t new_path, int32_t flags, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  kk_ssize_t new_len;
  int status = uv_fs_symlink(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_string_cbuf_borrow(new_path, &new_len, _ctx), flags, NULL);
  kk_string_drop(path, _ctx);
  kk_string_drop(new_path, _ctx);
  kk_uv_fs_check(&fs_req, status)
}

void kk_std_os_fs_string_cb(uv_fs_t* req) {
  kk_callback_result(req, callback, result)
  uv_fs_req_cleanup(req);
  if (result < 0) {
    kk_uv_error_callback(callback, result)
  } else {
    kk_string_t s = kk_string_alloc_raw((const char*)req->ptr, true, _ctx);
    kk_uv_okay_callback(callback, kk_string_box(s))
  }
}

static kk_unit_t kk_uv_fs_readlink(kk_string_t path, kk_function_t cb, kk_context_t* _ctx) {
  kk_ssize_t len;
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_readlink(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_std_os_fs_string_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_readlink_sync(kk_string_t path, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_readlink(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), NULL);
  kk_string_drop(path, _ctx);
  kk_uv_fs_check_return(&fs_req, status, kk_string_box(kk_string_alloc_raw((const char*)fs_req.ptr, true, _ctx)))
}

static kk_unit_t kk_uv_fs_realpath(kk_string_t path, kk_function_t cb, kk_context_t* _ctx) {
  kk_ssize_t len;
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_realpath(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), kk_std_os_fs_string_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_realpath_sync(kk_string_t path, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_realpath(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), NULL);
  kk_string_drop(path, _ctx);
  kk_uv_fs_check_return(&fs_req, status, kk_string_box(kk_string_alloc_raw((const char*)fs_req.ptr, true, _ctx)))
}

static kk_unit_t kk_uv_fs_chown(kk_string_t path, int32_t uid, int32_t gid, kk_function_t cb, kk_context_t* _ctx) {
  kk_ssize_t len;
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_chown(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), uid, gid, kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_chown_sync(kk_string_t path, int32_t uid, int32_t gid, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_chown(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), uid, gid, NULL);
  kk_string_drop(path, _ctx);
  kk_uv_fs_check(&fs_req, status)
}

static kk_unit_t kk_uv_fs_fchown(kk_uv_file__uv_file file, int32_t uid, int32_t gid, kk_function_t cb, kk_context_t* _ctx) {
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_fchown(uvloop(), fs_req, (uv_file)file.internal, uid, gid, kk_std_os_fs_unit_cb);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_fchown_sync(kk_uv_file__uv_file file, int32_t uid, int32_t gid, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  int status = uv_fs_fchown(uvloop(), &fs_req, (uv_file)file.internal, uid, gid, NULL);
  kk_uv_fs_check(&fs_req, status)
}

static kk_unit_t kk_uv_fs_lchown(kk_string_t path, int32_t uid, int32_t gid, kk_function_t cb, kk_context_t* _ctx) {
  kk_ssize_t len;
  kk_new_fs_req_cb(fs_req, cb)
  uv_fs_lchown(uvloop(), fs_req, kk_string_cbuf_borrow(path, &len, _ctx), uid, gid, kk_std_os_fs_unit_cb);
  kk_string_drop(path, _ctx);
  return kk_Unit;
}

static kk_std_core_exn__error kk_uv_fs_lchown_sync(kk_string_t path, int32_t uid, int32_t gid, kk_context_t* _ctx) {
  uv_fs_t fs_req = {0};
  kk_ssize_t len;
  int status = uv_fs_lchown(uvloop(), &fs_req, kk_string_cbuf_borrow(path, &len, _ctx), uid, gid, NULL);
  kk_string_drop(path, _ctx);
  kk_uv_fs_check(&fs_req, status)
}
