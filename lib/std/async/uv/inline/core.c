/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// for IDE
// #include "../../../../../kklib/include/kklib.h"
// // #define __EMSCRIPTEN__
// #include "core.h"

#ifndef __EMSCRIPTEN__
#include <uv.h>
#endif

kk_std_core_exn__error kk_result_ok( kk_box_t val, kk_context_t* ctx ) {
  return kk_std_core_types__new_Ok(val,ctx);
}


// call `() -> ()` callback
static void kk_function_call0( kk_function_t f, kk_context_t* ctx ) {
  kk_function_call(kk_unit_t, (kk_function_t, kk_context_t*), f, (f, ctx), ctx);  // drops f
}

struct kk_dispose_fun_closure_s {
  struct kk_function_s _base;
  uv_handle_t* handle;
  void* arg;
  kk_uv_handle_dispose_fun_t* dispose;
};

static kk_box_t kk_uv_handle_dispose_fun(kk_function_t _fself, kk_context_t* ctx) {
  struct kk_dispose_fun_closure_s* _self = kk_function_as(struct kk_dispose_fun_closure_s*, _fself, ctx);
  uv_handle_t* handle = _self->handle;
  void* arg = _self->arg;
  kk_uv_handle_dispose_fun_t* dispose = _self->dispose;
  kk_function_drop(_fself,ctx);
  dispose(handle,arg,ctx);
  return kk_unit_box(kk_Unit);
}

kk_function_t kk_uv_handle_dispose_fun_create(uv_handle_t* handle, void* arg, kk_uv_handle_dispose_fun_t* dispose, kk_context_t* ctx) {
  struct kk_dispose_fun_closure_s* _self = kk_function_alloc_as(struct kk_dispose_fun_closure_s, 1, ctx);
  _self->_base.fun = kk_kkfun_ptr_box(&kk_uv_handle_dispose_fun, ctx);
  _self->handle = handle;
  _self->arg = arg;
  _self->dispose = dispose;
  return kk_datatype_from_base(&_self->_base, ctx);
}

kk_std_core_exn__error kk_result_uv_handle_dispose( uv_handle_t* handle, void* arg, kk_uv_handle_dispose_fun_t* dispose, kk_context_t* ctx ) {
  kk_function_t dispose_fun = kk_uv_handle_dispose_fun_create(handle,arg,dispose,ctx);
  return kk_result_ok(kk_function_box(dispose_fun,ctx),ctx);
}

kk_std_core_exn__error kk_result_uv_handle_dispose0( uv_handle_t* handle, kk_context_t* ctx ) {
  return kk_result_uv_handle_dispose( handle, NULL, &kk_uv_handle_dispose, ctx);
}

//---------------------------------------
// set allocator

#define KK_CUSTOM_INIT  kk_uv_alloc_init

static void* kk_malloc_ctx(size_t size) {
  return kk_malloc(size, kk_get_context());
}
static void* kk_realloc_ctx(void* p, size_t size) {
  return kk_realloc(p, size, kk_get_context());
}
static void* kk_calloc_ctx(size_t count, size_t size) {
  return kk_zalloc(count*size, kk_get_context());
}
static void kk_free_ctx(void* p) {
  kk_free(p, kk_get_context());
}
static void kk_uv_alloc_init(kk_context_t* _ctx){
  uv_replace_allocator(&kk_malloc_ctx, &kk_realloc_ctx, &kk_calloc_ctx, &kk_free_ctx);
}


//---------------------------------------
// utility

kk_std_core_exn__error kk_error_from_uv_errno( int err, kk_context_t* ctx ) {
  if (err > 0) {
    return kk_error_from_errno(err,ctx);
  }
  else {
    const int syserr = -err; /* uv error codes are negative */
    const char* serr = uv_strerror(err);
    kk_string_t msg = kk_string_alloc_from_qutf8( serr, ctx );
    kk_free(serr,ctx);
    return kk_std_core_types__new_Error( kk_std_core_exn__exception_box( kk_std_core_exn__new_Exception( msg,
                  kk_std_core_exn__new_ExnSystem(kk_reuse_null, 0, kk_integer_from_int(syserr,ctx), ctx), ctx), ctx), ctx );
  }
}

kk_std_core_exn__error kk_result_uv_handle( uv_handle_t* h, kk_context_t* ctx ) {
  return kk_result_ok(kk_cptr_box(h,ctx),ctx);  // not freed, treat as an opaque value and free explicitly
}

int kk_uv_handle_create( size_t sz, kk_function_t cb, uv_handle_t** phandle, kk_context_t* ctx ) {
  uv_handle_t* h = *phandle = (uv_handle_t*)kk_zalloc(sz,ctx);
  if (h==NULL) {
    kk_function_drop(cb,ctx);
    return UV_ENOMEM;
  }
  else {
    h->data = kk_datatype_as_ptr(cb,ctx);
    return 0;
  }
}

void kk_uv_handle_free(uv_handle_t* h, kk_context_t* ctx) {
  if (h->data != NULL) {
    // drop the callback (just in case, should have been set NULL already in handle_close/callback)
    kk_datatype_drop( kk_datatype_from_ptr((kk_ptr_t)(h->data),ctx), ctx );
    h->data = NULL;
  }
  kk_free(h,ctx);
}

static void kk_uv_handle_close_cb(uv_handle_t* h) {
  kk_uv_handle_free(h,kk_get_context());
}

void kk_uv_handle_close(uv_handle_t* h) {
  if (h==NULL) return;
  if (h->data != NULL) {
    // drop the callback function right away
    // if a handle is disposed, uv might still schedule the callback; setting it to NULL prevents the callback still being called
    kk_context_t* ctx = kk_get_context();
    kk_function_t cb = kk_datatype_from_ptr((kk_ptr_t)(h->data),ctx);
    h->data = NULL;
    kk_function_drop(cb,ctx);
  }
  uv_close(h, &kk_uv_handle_close_cb);
}

void kk_uv_handle_dispose(uv_handle_t* handle, void* arg, kk_context_t* ctx) {
  kk_unused(arg);
  kk_unused(ctx);
  if (handle==NULL) return;
  kk_uv_handle_close((uv_handle_t*)handle);
}

void kk_uv_handle_callback(uv_handle_t* h) {
  if (h==NULL) return;
  if (h->data != NULL) {
    kk_context_t* ctx = kk_get_context();
    kk_function_t cb = kk_datatype_from_ptr((kk_ptr_t)(h->data),ctx); // the call drops the `cb`
    h->data = NULL;
    kk_function_call0(cb,ctx);
  }
  kk_uv_handle_close(h);
}


// -----------------------------------------------------
// Event Loop

kk_uv_loop_t kk_uv_loop_init(kk_context_t* ctx) {
  uv_loop_t* const uvloop = (uv_loop_t*)kk_zalloc(sizeof(uv_loop_t),ctx);
  if (uvloop != NULL) {
    uv_loop_init(uvloop);
  }
  return kk_cptr_raw_box(&kk_free_fun,uvloop,ctx);
}

uv_loop_t* kk_uv_loop(kk_uv_loop_t loop_borrowed, kk_context_t* ctx) {
  return (uv_loop_t*)kk_cptr_raw_unbox_borrowed(loop_borrowed, ctx);
}

static char* kk_uv_handle_type_str(uv_handle_t* handle) {
  switch (handle->type) {
    #define XX(code,lc) case UV_##code: return #lc;
    UV_HANDLE_TYPE_MAP(XX)
    #undef XX
    default: return "unknown";
  }
}

static void kk_uv_loop_walk_cb(uv_handle_t* handle, void* arg) {
  const char* closing_msg = uv_is_closing(handle) ? " [closing]" : "";
  const char* active_msg  = uv_is_active(handle) ? " [active]" : "";
  kk_warning_message(" - %s handle%s%s\n", kk_uv_handle_type_str(handle), active_msg, closing_msg);
}


static void kk_uv_loop_done(kk_uv_loop_t loop, kk_context_t* ctx) {
  uv_loop_t* const uvloop = kk_uv_loop(loop,ctx);
  int ret = uv_loop_close(uvloop);
  if (ret != 0) {
    if (ret == UV_EBUSY) {
      kk_warning_message("event loop closed with open child handles:\n");
      uv_walk(uvloop, kk_uv_loop_walk_cb, NULL);
    } else {
      kk_warning_message("event loop close returned error: %s\n", uv_strerror(ret));
    }
  }
  kk_box_drop(loop,ctx);
}

void kk_uv_loop_run(kk_uv_loop_t loop, kk_context_t* ctx) {
  uv_loop_t* const uvloop = kk_uv_loop(loop,ctx);
  int ret = UV_UNKNOWN_HANDLE;
  if (uvloop!=NULL) {
    ret = uv_run(uvloop,UV_RUN_DEFAULT);
  }
  if (ret!=0){
    kk_warning_message("event loop closed: %s\n", uv_strerror(ret));
  }
  kk_uv_loop_done(loop,ctx);
}

