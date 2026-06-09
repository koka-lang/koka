/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// #include <kklib.h>
// #include "core.h"

#include <fcntl.h>
#include <uv.h>

int32_t kk_uv_fd_flags_from_filemode( int32_t fmode, kk_context_t* ctx ) {
  kk_unused(ctx);
  if (fmode == 1)       return O_WRONLY | O_CREAT | O_TRUNC;
  else if (fmode == 2)  return O_WRONLY | O_CREAT | O_APPEND;
  else if (fmode == 3)  return O_RDWR   | O_CREAT | O_TRUNC;
  else return O_RDONLY;
}

static void kk_uv_fs_result_call(kk_function_t cb, uv_req_t* _req, kk_context_t* ctx) {
  uv_fs_t* req = (uv_fs_t*)_req;
  // pass request result as the argument
  kk_std_core_exn__error arg;
  if (req->result < 0) {
    arg = kk_error_from_uv_errno(req->result,ctx);      
  }
  else {
    arg = kk_result_ok( kk_ssize_box(req->result,ctx), ctx);
  }
  kk_function_call_error(cb,arg,ctx);
}

static void kk_uv_fd_open_callback(uv_fs_t* req) {
  kk_uv_req_callback((uv_req_t*)req, &kk_uv_fs_result_call);  
}

kk_std_core_exn__error kk_uv_fd_open_setup(kk_uv_loop_t loop, kk_string_t fpath, int32_t flags, int32_t mode, kk_function_t cb, kk_context_t* ctx) {
  uv_fs_t* req; 
  int err = kk_uv_req_create(sizeof(uv_fs_t), cb, (uv_req_t**)&req, ctx );
  if (err!=0) return kk_error_from_uv_errno(err,ctx);
  kk_with_string_as_qutf8_borrow(fpath,cfpath,ctx) {
    err = uv_fs_open(kk_uv_loop(loop,ctx), req, cfpath, flags, mode, &kk_uv_fd_open_callback);
  }
  if (err!=0) { kk_uv_req_free((uv_req_t*)req,ctx); return kk_error_from_uv_errno(err,ctx); }
  return kk_result_uv_req_dispose0((uv_req_t*)req,ctx);
}

static void kk_uv_fd_close_callback(uv_fs_t* req) {
  kk_uv_req_callback((uv_req_t*)req, &kk_uv_fs_result_call);  
}

kk_std_core_exn__error kk_uv_fd_close(kk_uv_loop_t loop, kk_ssize_t fhandle, kk_function_t cb, kk_context_t* ctx ) {
  uv_fs_t* req; 
  int err = kk_uv_req_create(sizeof(uv_fs_t), cb, (uv_req_t**)&req, ctx );
  if (err!=0) return kk_error_from_uv_errno(err,ctx);
  err = uv_fs_close(kk_uv_loop(loop,ctx), req, fhandle, &kk_uv_fd_close_callback); 
  if (err!=0) { kk_uv_req_free((uv_req_t*)req,ctx); return kk_error_from_uv_errno(err,ctx); }
  return kk_result_uv_req_dispose0((uv_req_t*)req,ctx);
} 

static void kk_uv_fd_read_callback( uv_fs_t* req ) {
  kk_uv_req_callback((uv_req_t*)req, &kk_uv_fs_result_call);
}

kk_std_core_exn__error kk_uv_fd_read(kk_uv_loop_t loop, kk_ssize_t fd, kk_bytes_t buf, int64_t offset, kk_function_t cb, kk_context_t* ctx) {
  uv_fs_t* req; 
  int err = kk_uv_req_create(sizeof(uv_fs_t), cb, (uv_req_t**)&req, ctx );
  if (err!=0) return kk_error_from_uv_errno(err,ctx);
  // bytes to uv_buf_t
  kk_ssize_t len = 0;
  const uint8_t* base = kk_bytes_buf_borrow(buf,&len,ctx);
  uv_buf_t bufs[1];
  bufs[0] = uv_buf_init((char*)base,kk_to_size_t(len));
  // and read
  err = uv_fs_read(kk_uv_loop(loop,ctx), req, fd, bufs, 1, offset, &kk_uv_fd_read_callback);
  kk_assert(kk_block_refcount(kk_datatype_as_ptr(buf,ctx)) > 0);
  kk_bytes_drop(buf,ctx); // .. we must hold a reference in the callback as it must stay alive during the async read!
  if (err!=0) { kk_uv_req_free((uv_req_t*)req,ctx); return kk_error_from_uv_errno(err,ctx); }
  return kk_result_uv_req_dispose0((uv_req_t*)req,ctx);
}