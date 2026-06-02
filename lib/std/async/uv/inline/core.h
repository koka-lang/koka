/*---------------------------------------------------------------------------
  Copyright 2026, Tim Whiting, Microsoft Research, Daan Leijen

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

typedef struct uv_loop_s uv_loop_t;
typedef struct uv_handle_s uv_handle_t;
typedef struct uv_req_s uv_req_t;

// call `() -> ()` callback
static inline void kk_function_call0( kk_function_t f, kk_context_t* ctx ) {
  kk_function_call(kk_unit_t, (kk_function_t, kk_context_t*), f, (f, ctx), ctx);  // drops f
}

// call `(a) -> ()` callback
static inline void kk_function_call_error( kk_function_t f, kk_std_core_exn__error arg, kk_context_t* ctx ) {
  kk_function_call(kk_unit_t, (kk_function_t, kk_std_core_exn__error, kk_context_t*), f, (f, arg, ctx), ctx);  // drops f
}

// ---------------------
// event loop

typedef kk_box_t kk_uv_loop_t;
kk_uv_loop_t kk_uv_loop_init(kk_context_t* ctx);
void         kk_uv_loop_run(kk_uv_loop_t loop, kk_context_t* ctx);
uv_loop_t*   kk_uv_loop( kk_uv_loop_t loop_borrowed, kk_context_t* ctx ); 


// ---------------------
// Handles

// we store a callback (as `kk_function_t`) in the uv handle `data` field.
int  kk_uv_handle_create( size_t sz, kk_function_t cb, uv_handle_t** phandle, kk_context_t* ctx );
void kk_uv_handle_close(uv_handle_t* h);                     // calls uv_close and then kk_uv_handle_free in the next tick
void kk_uv_handle_free(uv_handle_t* h, kk_context_t* ctx);   // free the handle and drop the callback 

void kk_uv_handle_callback(uv_handle_t* h);                  // call callback and closes the handle
                                                             // todo: support repeated callbacks

// ---------------------
// Handle results

typedef void (kk_uv_handle_dispose_fun_t)(uv_handle_t* handle, void* arg, kk_context_t* ctx);
kk_function_t kk_uv_handle_dispose_fun_create(uv_handle_t* handle, void* arg, kk_uv_handle_dispose_fun_t* dispose, kk_context_t* ctx);
void kk_uv_handle_dispose( uv_handle_t* handle, void* arg, kk_context_t* ctx);  // convenience: calls uv_handle_close

kk_std_core_exn__error kk_result_ok( kk_box_t val, kk_context_t* ctx );
kk_std_core_exn__error kk_result_uv_handle( uv_handle_t* h, kk_context_t* ctx );
kk_std_core_exn__error kk_result_uv_handle_dispose( uv_handle_t* handle, void* arg, kk_uv_handle_dispose_fun_t* dispose, kk_context_t* ctx );  
kk_std_core_exn__error kk_result_uv_handle_dispose0( uv_handle_t* handle, kk_context_t* ctx );
kk_std_core_exn__error kk_error_from_uv_errno( int uv_err, kk_context_t* ctx );

// ---------------------
// Requests

// we store a callback (as `kk_function_t`) in the uv request `data` field.
int  kk_uv_req_create( size_t sz, kk_function_t cb, uv_req_t** preq, kk_context_t* ctx );
void kk_uv_req_close(uv_req_t* req);                      // calls uv_cancel and then kk_uv_req_free in the next tick
void kk_uv_req_free(uv_req_t* req, kk_context_t* ctx);    // free the request (and call uv cleanup functions) and drop the callback 

// call the callback with a callback function (to handle different arguments from the request)
typedef void (kk_uv_req_call_fun_t)(kk_function_t cb, uv_req_t* req, kk_context_t* ctx);
void kk_uv_req_callback(uv_req_t* req, kk_uv_req_call_fun_t* call );


// ---------------------
// Request results

typedef void (kk_uv_req_dispose_fun_t)(uv_req_t* req, void* arg, kk_context_t* ctx);
kk_function_t kk_uv_req_dispose_fun_create(uv_req_t* req, void* arg, kk_uv_req_dispose_fun_t* dispose, kk_context_t* ctx);
void kk_uv_req_dispose( uv_req_t* req, void* arg, kk_context_t* ctx);  // convenience: calls uv_cancel, and uv_req_close

kk_std_core_exn__error kk_result_ok( kk_box_t val, kk_context_t* ctx );
kk_std_core_exn__error kk_result_uv_req_dispose( uv_req_t* req, void* arg, kk_uv_req_dispose_fun_t* dispose, kk_context_t* ctx );  
kk_std_core_exn__error kk_result_uv_req_dispose0( uv_req_t* req, kk_context_t* ctx ); // use `kk_uv_req_dispose`
kk_std_core_exn__error kk_error_from_uv_errno( int uv_err, kk_context_t* ctx );
