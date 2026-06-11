/*---------------------------------------------------------------------------
  Copyright 2026, Microsoft Research, Daan Leijen, Tim Whiting

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the LICENSE file at the root of this distribution.
---------------------------------------------------------------------------*/

// #include <kklib.h>
#include <emscripten.h>

//----------------------------------------------
// dispose function closure
//----------------------------------------------

typedef void (kk_em_dispose_fun_t)(void* p, void* arg, kk_context_t* ctx);

struct kk_em_dispose_fun_closure_s {
  struct kk_function_s _base;
  void* p;
  void* arg;
  kk_em_dispose_fun_t* dispose;
};

static kk_box_t kk_em_dispose_fun(kk_function_t _fself, kk_context_t* ctx) {
  struct kk_em_dispose_fun_closure_s* _self = kk_function_as(struct kk_em_dispose_fun_closure_s*, _fself, ctx);
  void* p = _self->p;
  void* arg = _self->arg;
  kk_em_dispose_fun_t* dispose = _self->dispose;
  kk_function_drop(_fself,ctx);
  dispose(p,arg,ctx);
  return kk_unit_box(kk_Unit);
}

kk_function_t kk_em_dispose_fun_create(void* p, void* arg, kk_em_dispose_fun_t* dispose, kk_context_t* ctx) {
  struct kk_em_dispose_fun_closure_s* _self = kk_function_alloc_as(struct kk_em_dispose_fun_closure_s, 1, ctx);
  _self->_base.fun = kk_kkfun_ptr_box(&kk_em_dispose_fun, ctx);
  _self->p = p;
  _self->arg = arg;
  _self->dispose = dispose;
  return kk_datatype_from_base(&_self->_base, ctx);
}

kk_std_core_exn__error kk_result_ok( kk_box_t val, kk_context_t* ctx ) {
  return kk_std_core_types__new_Ok(val,ctx);
}

kk_std_core_exn__error kk_result_dispose( void* req, void* arg, kk_em_dispose_fun_t* dispose, kk_context_t* ctx ) {
  kk_function_t dispose_fun = kk_em_dispose_fun_create((void*)req,arg,dispose,ctx);
  return kk_result_ok(kk_function_box(dispose_fun,ctx),ctx);
}


//----------------------------------------------
// event loop
//----------------------------------------------

typedef struct em_loop_s {
  int64_t refcount;
} em_loop_t;

typedef kk_box_t kk_em_loop_t;

static em_loop_t* kk_em_loop(kk_em_loop_t loop_borrowed, kk_context_t* ctx) {
  return (em_loop_t*)kk_cptr_raw_unbox_borrowed(loop_borrowed, ctx);
}

void kk_em_loop_ref(kk_em_loop_t evloop, kk_context_t* ctx) {
  em_loop_t* loop = kk_em_loop(evloop,ctx);
  if (loop==NULL) return;
  // todo: if (evloop->refcount==0) { error("refcounting after shutdown") }
  loop->refcount++;
  kk_box_drop(evloop,ctx);
}

static void kk_em_loop_delayed_unref(void* ploop) {
  em_loop_t* loop = (em_loop_t*)ploop;
  if (loop==NULL) return;
  // todo: if (evloop->refcount==0) { error("refcounting after shutdown") }
  loop->refcount--; 
  if (loop->refcount == 0) {
    emscripten_cancel_main_loop();
  }
}

void kk_em_loop_unref(kk_em_loop_t evloop, kk_context_t* ctx) {
  em_loop_t* loop = kk_em_loop(evloop,ctx);
  if (loop==NULL) return;
  if (loop->refcount > 1) { // optimize common case
    loop->refcount--;
  }
  else {
    // delayed deference
    emscripten_async_call(&kk_em_loop_delayed_unref, loop, 0);
    kk_box_drop(evloop,ctx);
  }
}

kk_em_loop_t kk_event_loop_init(kk_context_t* ctx) {
  em_loop_t* const evloop = (em_loop_t*)kk_zalloc(sizeof(em_loop_t),ctx);
  if (evloop != NULL) {
    evloop->refcount = 1;
  }
  return kk_cptr_raw_box(&kk_free_fun,evloop,ctx);
}


static void kk_em_loop_iteration() {
  return;
}

void kk_event_loop_run(kk_em_loop_t evloop, kk_context_t* ctx) {
  emscripten_set_main_loop(&kk_em_loop_iteration, 0, true /* infinite */);  
  kk_box_drop(evloop,ctx);
}


//----------------------------------------------
// timer
//----------------------------------------------


static inline void kk_function_call0( kk_function_t f, kk_context_t* ctx ) {
  kk_function_call(kk_unit_t, (kk_function_t, kk_context_t*), f, (f, ctx), ctx);  // drops f
}

typedef struct em_timer_s {
  kk_em_loop_t loop;
  int jstimer;
  kk_function_t cb;
} em_timer_t;

#ifdef __cplusplus
extern "C"
#endif
EMSCRIPTEN_KEEPALIVE void kk_em_timer_callback(em_timer_t* timer) {
  if (timer==NULL) return;
  kk_context_t* ctx = kk_get_context();
  kk_function_call0(timer->cb,ctx);  // drops cb
  kk_em_loop_unref(timer->loop,ctx);
  kk_free(timer,ctx);
}

EM_JS(int, kk_js_set_timeout, (em_timer_t* t, uint64_t millisecs), {
  return setTimeout(() => _kk_em_timer_callback(t), Number(millisecs));
});
EM_JS(void, kk_js_clear_timeout, (int timer), {
  clearTimeout(timer);  
});

void kk_em_timer_dispose(void* ptimer, void* arg, kk_context_t* ctx) {
  kk_unused(arg);
  em_timer_t* t = (em_timer_t*)ptimer;
  if (t==NULL) return;
  kk_js_clear_timeout(t->jstimer);
  kk_function_drop(t->cb,ctx);
  kk_em_loop_unref(t->loop,ctx);
  kk_free(t,ctx);
}

kk_std_core_exn__error kk_timer_setup(kk_em_loop_t evloop, int64_t millisecs, kk_function_t cb, kk_context_t* ctx) {
  if (millisecs < 0) millisecs = 0;
  em_timer_t* t = (em_timer_t*)kk_zalloc(sizeof(em_timer_t),ctx);
  t->cb = cb;
  t->jstimer = kk_js_set_timeout(t,millisecs);
  t->loop = evloop;
  kk_em_loop_ref(evloop,ctx);
  return kk_result_dispose((void*)t,NULL,&kk_em_timer_dispose,ctx);
}

kk_std_core_exn__error kk_immediate_setup(kk_em_loop_t loop, kk_function_t cb, kk_context_t* ctx) {
  return kk_timer_setup(loop,0,cb,ctx);
}

